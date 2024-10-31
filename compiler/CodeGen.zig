const std = @import("std");
const Module = @import("Module.zig");
const Node = @import("Ast.zig").Node;
const NodeIndex = @import("Ast.zig").NodeIndex;
const Symbol = @import("symbol.zig").Symbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const SymbolMap = @import("Sema.zig").SymbolMap;
const Instruction = @import("instruction.zig").Instruction;
const InstructionType = @import("instruction.zig").InstructionType;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const memory_map = @import("memory_map.zig");
const CodeGen = @This();

mapping_mode: MappingMode,
symbols: SymbolMap,
banks: std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u8)) = .empty, // Bank -> BankData

allocator: std.mem.Allocator,

pub fn generate(gen: *CodeGen, module: Module) !void {
    const module_symbols = gen.symbols.get(module.name.?).?;
    for (module_symbols.keys(), module_symbols.values()) |name, *sym| {
        if (sym.* != .function) {
            continue;
        }

        var builder: FunctionBuilder = .{
            .codegen = gen,
            .module = module,
            .symbol = sym.function,
            .symbol_name = name,
        };
        try builder.build();

        sym.function.assembly_data = try builder.genereateAssemblyData();
        try builder.fixLabelRelocs();
        sym.function.instructions = try builder.instructions.toOwnedSlice(gen.allocator);
    }
}

/// Returns the memory-mapped location of a symbol
pub fn symbolLocation(gen: CodeGen, symbol_loc: SymbolLocation) u24 {
    const symbol = gen.symbols.get(symbol_loc.module).?.get(symbol_loc.name).?;
    return switch (symbol) {
        .variable => @panic("TODO"),
        .function => |func_sym| memory_map.bankOffsetToAddr(gen.mapping_mode, 0x80, func_sym.bank_offset),
    };
}

/// Generates byte data for the individual banks
pub fn generateBanks(gen: *CodeGen) !void {
    // TODO: Respect bank and addr min/max constraints
    for (gen.symbols.values()) |module_symbols| {
        for (module_symbols.values()) |*symbol| {
            if (symbol.* == .function) {
                // TODO: Support function bank
                const gop = try gen.banks.getOrPut(gen.allocator, 0x80);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .empty;
                }

                symbol.function.bank_offset = @intCast(gop.value_ptr.items.len);
                try gop.value_ptr.appendSlice(gen.allocator, symbol.function.assembly_data);
                std.log.info("asm {x}", .{symbol.function.assembly_data});
            }
        }
    }
}

/// Allocate the actual bank-data for the ROM
pub fn allocateBanks(gen: CodeGen) ![]const Rom.BankData {
    var bank_data = try gen.allocator.alloc(Rom.BankData, gen.banks.count());
    for (gen.banks.keys(), gen.banks.values(), 0..) |bank, *data, i| {
        bank_data[i] = .{
            .bank = bank,
            .data = try data.toOwnedSlice(gen.allocator),
        };
    }
    return bank_data;
}

/// Resolves relocation of instructions
pub fn resolveRelocations(gen: CodeGen) !void {
    for (gen.symbols.values()) |module_symbols| {
        for (module_symbols.values()) |*symbol| {
            if (symbol.* != .function) {
                continue;
            }

            const func = symbol.function;
            const bank = gen.banks.get(0x80).?;

            for (func.instructions) |info| {
                const reloc = info.reloc orelse continue;
                const target_addr = gen.symbolLocation(reloc.target_sym) + reloc.target_offset;

                switch (reloc.type) {
                    .addr24 => {
                        const operand: *[3]u8 = bank.items[(func.bank_offset + info.offset + @sizeOf(InstructionType))..][0..3];
                        operand.* = @bitCast(target_addr);
                    },
                }
            }
        }
    }
}

/// Index to a target instruction
pub const Label = u16;

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {
        /// 24-bit Long address of the symbol
        addr24,
    };

    type: Type,
    target_sym: SymbolLocation,
    target_offset: u16,
};
/// Branches need to be relocated to point to their label and use the short / long form
const BranchRelocation = struct {
    pub const Type = enum {
        always,
    };

    type: Type,
    target: []const u8,
};

/// Metadata information about an instruction
pub const InstructionInfo = struct {
    instr: Instruction,
    /// Offset from the start of the function. Only defined once the assembly as been emitted
    offset: u16,

    reloc: ?Relocation,
    branch_reloc: ?BranchRelocation,

    a_size: Instruction.SizeMode,
    xy_size: Instruction.SizeMode,

    comments: []const []const u8,
};

/// Builds instruction information from the AST nodes of a function
const FunctionBuilder = struct {
    codegen: *CodeGen,

    module: Module,
    symbol: Symbol.Function,
    symbol_name: []const u8,

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,
    labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

    // Register State
    a_size: Instruction.SizeMode = .none,
    xy_size: Instruction.SizeMode = .none,
    a_reg_id: ?u64 = null,
    x_reg_id: ?u64 = null,
    y_reg_id: ?u64 = null,

    pub fn build(b: *FunctionBuilder) !void {
        try b.handleFnBlock(b.symbol.node);
    }

    const Error = error{OutOfMemory};

    // Node handling
    fn handleFnBlock(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        var child_iter = b.module.ast.iterChildren(node_idx);
        while (child_iter.nextIndex()) |child_idx| {
            const child = b.module.ast.nodes[child_idx];
            switch (child.tag) {
                .block_expr => try b.handleBlock(child_idx),

                .instruction => try b.handleInstruction(child),
                .label => try b.handleLabel(child),
                else => std.debug.panic("Unexpected expression node {}", .{child}),
            }
        }
    }

    fn handleExpression(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        const node = b.module.ast.nodes[node_idx];
        return switch (node.tag) {
            else => std.debug.panic("Unexpected expression node {}", .{node}),

            .block_expr => b.handleBlock(node_idx),
        };
    }

    fn handleBlock(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        const node = b.module.ast.nodes[node_idx];
        std.debug.assert(node.tag == .block_expr);

        var child_iter = b.module.ast.iterChildren(node_idx);
        while (child_iter.nextIndex()) |expr_idx| {
            try b.handleExpression(expr_idx);
        }
    }

    fn handleInstruction(b: *FunctionBuilder, node: Node) Error!void {
        const instr_node = node.tag.instruction;
        std.log.info("instruction {}", .{instr_node});

        const target_register = instr_node.opcode.target_register();
        const register_size = switch (target_register) {
            .none => .none,
            .a => b.a_size,
            .x, .y => b.xy_size,
        };

        var info: InstructionInfo = .{
            .instr = undefined,
            .offset = undefined,

            .reloc = null,
            // TODO: Branch relocations
            .branch_reloc = null,

            .a_size = b.a_size,
            .xy_size = b.xy_size,

            // TODO: Comments
            .comments = &.{},
        };

        compute_info: switch (instr_node.operand) {
            .none => {
                switch (instr_node.opcode) {
                    inline else => |t| {
                        const field = comptime find_field: {
                            @setEvalBranchQuota(100000);
                            for (std.meta.fields(Instruction)) |field| {
                                if (std.mem.eql(u8, field.name, @tagName(t))) {
                                    break :find_field field;
                                }
                            }
                        };

                        if (field.type == void) {
                            info.instr = @unionInit(Instruction, @tagName(t), {});
                            break :compute_info;
                        }
                    },
                }
                unreachable;
            },
            .number => |num| {
                switch (instr_node.opcode) {
                    inline else => |t| {
                        const field = comptime find_field: {
                            @setEvalBranchQuota(100000);
                            for (std.meta.fields(Instruction)) |field| {
                                if (std.mem.eql(u8, field.name, @tagName(t))) {
                                    break :find_field field;
                                }
                            }
                        };

                        if (field.type == Instruction.Imm816) {
                            info.instr = @unionInit(Instruction, @tagName(t), switch (register_size) {
                                .@"8bit" => .{ .imm8 = @intCast(num) },
                                .@"16bit" => .{ .imm16 = num },
                                else => unreachable,
                            });
                            break :compute_info;
                        }
                    },
                }
                unreachable;
            },
            .identifier => |ident| {
                const sym_loc = SymbolLocation.parse(ident, b.module.name.?);

                info.instr = switch (instr_node.opcode) {
                    inline else => |t| @unionInit(Instruction, @tagName(t), undefined),
                };
                switch (instr_node.opcode) {
                    .jml => {
                        info.reloc = .{
                            .type = .addr24,
                            .target_sym = sym_loc,
                            .target_offset = 0,
                        };
                    },
                    else => unreachable,
                }
            },
        }

        try b.instructions.append(b.codegen.allocator, info);
    }

    fn handleLabel(b: *FunctionBuilder, node: Node) Error!void {
        try b.labels.put(b.codegen.allocator, node.tag.label, @intCast(b.instructions.items.len));
    }

    /// Generates the raw assembly bytes for all instructions
    fn genereateAssemblyData(b: *FunctionBuilder) ![]u8 {
        var data: std.ArrayListUnmanaged(u8) = .{};
        // TODO: Figure out good instruction/assembly ratio
        try data.ensureTotalCapacity(b.codegen.allocator, b.instructions.items.len * 2);

        const data_writer = data.writer(b.codegen.allocator);

        for (b.instructions.items) |*info| {
            const target_register = info.instr.target_register();
            const register_size = switch (target_register) {
                .none => .none,
                .a => info.a_size,
                .x, .y => info.xy_size,
            };
            if (target_register != .none) {
                std.debug.assert(register_size != .none);
            }

            info.offset = @intCast(data.items.len);
            try info.instr.write_data(data_writer, register_size);
        }

        return data.toOwnedSlice(b.codegen.allocator);
    }

    /// By default labels point to local functions instead of the current one with an offset
    fn fixLabelRelocs(b: *FunctionBuilder) !void {
        for (b.instructions.items) |*info| {
            const reloc = &(info.reloc orelse continue);
            if (!std.mem.eql(u8, reloc.target_sym.module, b.module.name.?)) continue;

            for (b.labels.keys(), b.labels.values()) |label, instr_index| {
                if (std.mem.eql(u8, label, reloc.target_sym.name)) {
                    reloc.target_sym.name = b.symbol_name;
                    reloc.target_offset = if (instr_index == b.instructions.items.len)
                        @intCast(b.symbol.assembly_data.len)
                    else
                        b.instructions.items[instr_index].offset;
                }
            }
        }
    }
};

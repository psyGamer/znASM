const std = @import("std");
const Module = @import("Module.zig");
const Node = @import("Ast.zig").Node;
const Symbol = @import("symbol.zig").Symbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const SymbolMap = @import("Sema.zig").SymbolMap;
const Instruction = @import("instruction.zig").Instruction;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const memory_map = @import("memory_map.zig");
const CodeGen = @This();

mapping_mode: MappingMode,
symbols: SymbolMap,
allocator: std.mem.Allocator,

pub fn generate(gen: *CodeGen, module: Module) !void {
    for (gen.symbols.get(module.name.?).?.values()) |*sym| {
        if (sym.* != .function) {
            continue;
        }

        var builder: FunctionBuilder = .{
            .codegen = gen,
            .module = module,
            .symbol = sym.function,
        };
        try builder.build();

        sym.function.assembly_data = try builder.genereateAssemblyData();
    }
}

/// Returns the memory-mapped location of a symbol
pub fn symbolLocation(gen: CodeGen, symbol_loc: SymbolLocation) u24 {
    const symbol = gen.symbols.get(symbol_loc.module.?).?.get(symbol_loc.name).?;
    return switch (symbol) {
        .variable => @panic("TODO"),
        .function => |func_sym| func_sym.address,
    };
}

/// Generates byte data for the individual banks
pub fn createBanks(gen: CodeGen) ![]const Rom.BankData {
    // TODO: Respect bank and addr min/max constraints
    var banks: std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u8)) = .empty;
    defer banks.deinit(gen.allocator);

    for (gen.symbols.values()) |module_symbols| {
        for (module_symbols.values()) |*symbol| {
            if (symbol.* == .function) {
                // TODO: Support function bank
                const gop = try banks.getOrPut(gen.allocator, 0x80);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .empty;
                }

                symbol.function.address = memory_map.bankOffsetToAddr(gen.mapping_mode, 0x80, @intCast(gop.value_ptr.items.len));
                try gop.value_ptr.appendSlice(gen.allocator, symbol.function.assembly_data);
                std.log.info("asm {x}", .{symbol.function.assembly_data});
            }
        }
    }

    var bank_data = try gen.allocator.alloc(Rom.BankData, banks.count());
    for (banks.keys(), banks.values(), 0..) |bank, *data, i| {
        bank_data[i] = .{
            .bank = bank,
            .data = try data.toOwnedSlice(gen.allocator),
        };
    }
    return bank_data;
}

/// Redefinable index to a target instruction
pub const Label = struct {
    index: ?u16,

    /// Defines the label to point to the next instruction
    pub fn define(label: *Label, b: *const FunctionBuilder) void {
        label.index = @intCast(b.instructions.items.len);
    }
};

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {};

    type: Type,
    target_sym: Symbol,
    target_offset: u16,
};
/// Branches need to be relocated to point to their label and use the short / long form
const BranchRelocation = struct {
    pub const Type = enum {
        always,
    };

    type: Type,
    target: *Label,
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

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .{},

    // Register State
    a_size: Instruction.SizeMode = .none,
    xy_size: Instruction.SizeMode = .none,
    a_reg_id: ?u64 = null,
    x_reg_id: ?u64 = null,
    y_reg_id: ?u64 = null,

    pub fn build(b: *FunctionBuilder) !void {
        const nodes = b.module.ast.nodes;

        for (nodes[b.symbol.node].children.items) |expr_idx| {
            try b.handleExpression(nodes[expr_idx]);
        }
    }

    const Error = error{OutOfMemory};

    // Expression handling
    fn handleExpression(b: *FunctionBuilder, node: Node) Error!void {
        return switch (node.tag) {
            .root, .module, .global_var_decl, .fn_def => std.debug.panic("Unexpected expression node {}", .{node}),

            .block_expr => b.handleBlock(node),
            .instruction => b.handleInstruction(node),
        };
    }

    fn handleBlock(b: *FunctionBuilder, node: Node) Error!void {
        std.debug.assert(node.tag == .block_expr);
        const nodes = b.module.ast.nodes;

        for (node.children.items) |expr_idx| {
            try b.handleExpression(nodes[expr_idx]);
        }
    }
    fn handleInstruction(b: *FunctionBuilder, node: Node) Error!void {
        const instr_node = node.tag.instruction;
        std.log.info("instruction {}", .{instr_node});

        const instr = switch (instr_node.opcode) {
            inline else => |t| b: {
                const field = comptime find_field: {
                    @setEvalBranchQuota(10000);
                    for (std.meta.fields(Instruction)) |field| {
                        if (std.mem.eql(u8, field.name, @tagName(t))) {
                            break :find_field field;
                        }
                    }
                };
                if (@bitSizeOf(field.type) != 0) {
                    @panic("Unsuppored instruction: " ++ @tagName(t));
                }
                break :b @unionInit(Instruction, @tagName(t), {});
            },
        };

        try b.instructions.append(b.codegen.allocator, .{
            .instr = instr,
            .offset = undefined,

            // TODO: Relocations
            .reloc = null,
            .branch_reloc = null,

            .a_size = b.a_size,
            .xy_size = b.xy_size,

            // TODO: Comments
            .comments = &.{},
        });
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
};

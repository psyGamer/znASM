const std = @import("std");
const Module = @import("Module.zig");
const Node = @import("Ast.zig").Node;
const NodeIndex = @import("Ast.zig").NodeIndex;
const Symbol = @import("symbol.zig").Symbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Sema = @import("Sema.zig");
const Instruction = @import("instruction.zig").Instruction;
const InstructionType = @import("instruction.zig").InstructionType;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const memory_map = @import("memory_map.zig");
const znasm_builtin = @import("builtin.zig");
const crc32 = @import("util/crc32.zig");
const CodeGen = @This();

mapping_mode: MappingMode,
sema: *Sema,
banks: std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u8)) = .empty, // Bank -> BankData

allocator: std.mem.Allocator,

pub fn generate(gen: *CodeGen) !void {
    const symbols = gen.sema.symbols.items(.sym);

    for (gen.sema.modules) |module| {
        const module_symbols = gen.sema.symbol_map.get(module.name.?).?;
        for (module_symbols.keys(), module_symbols.values()) |name, sym_idx| {
            const sym = &symbols[sym_idx];
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

            sym.function.bank = memory_map.getRealBank(gen.mapping_mode, builder.target_bank);
            std.log.info("bank {}", .{sym.function.bank});
            sym.function.assembly_data = try builder.genereateAssemblyData();
            try builder.fixLabelRelocs();
            sym.function.instructions = try builder.instructions.toOwnedSlice(gen.allocator);

            const labels = try gen.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
            for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
                label.* = .{ label_name, index };
            }
            sym.function.labels = labels;
        }
    }
}

/// Returns the memory-mapped location of a symbol
pub fn symbolLocation(gen: CodeGen, symbol_loc: SymbolLocation) u24 {
    const symbol_idx = gen.sema.symbol_map.get(symbol_loc.module).?.get(symbol_loc.name).?;
    const symbol = gen.sema.symbols.items(.sym)[symbol_idx];

    return switch (symbol) {
        .variable => @panic("TODO"),
        .function => |func_sym| memory_map.bankOffsetToAddr(gen.mapping_mode, func_sym.bank, func_sym.bank_offset),
    };
}

/// Generates byte data for the individual banks
pub fn generateBanks(gen: *CodeGen) !void {
    for (gen.sema.symbols.items(.sym)) |*sym| {
        if (sym.* == .function) {
            const func = &sym.function;

            const gop = try gen.banks.getOrPut(gen.allocator, func.bank);
            if (!gop.found_existing) {
                gop.value_ptr.* = .empty;
            }

            func.bank_offset = @intCast(gop.value_ptr.items.len);
            try gop.value_ptr.appendSlice(gen.allocator, func.assembly_data);
            std.log.info("asm {x}", .{func.assembly_data});
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
    for (gen.sema.symbols.items(.sym)) |*sym| {
        if (sym.* != .function) {
            continue;
        }

        const func = sym.function;
        const bank = gen.banks.get(func.bank).?;

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

/// Writes a .mlb symbol file for Mesen2
pub fn writeMlbSymbols(gen: CodeGen, writer: std.fs.File.Writer) !void {
    const symbols = gen.sema.symbols.items(.sym);

    var comments: std.ArrayListUnmanaged([]const u8) = .empty;
    defer comments.deinit(gen.allocator);

    var module_index: usize = 0;
    for (gen.sema.symbol_map.keys(), gen.sema.symbol_map.values()) |module_name, module_symbols| {
        if (std.mem.eql(u8, module_name, znasm_builtin.module)) {
            continue;
        }

        const module = gen.sema.modules[module_index];
        module_index += 1;

        var src_fbs = std.io.fixedBufferStream(module.source);
        const src_reader = src_fbs.reader();

        for (module_symbols.keys(), module_symbols.values()) |symbol_name, symbol_idx| {
            const symbol = symbols[symbol_idx];
            const is_vector = symbol_name[0] == '@';

            // Usually format module@symbol, except for vectors which would have a double @
            const debug_sym_name = if (is_vector)
                symbol_name
            else
                try std.fmt.allocPrint(gen.allocator, "{s}@{s}", .{ module_name, symbol_name });
            defer if (!is_vector) gen.allocator.free(debug_sym_name);

            switch (symbol) {
                .function => |func| {
                    src_fbs.pos = module.ast.token_locs[module.ast.node_tokens[func.node]].start;

                    const func_offset = memory_map.bankToRomOffset(gen.mapping_mode, func.bank) + func.bank_offset;

                    for (func.instructions, 0..) |info, info_idx| {
                        // Find labels
                        const label: []const u8 = get_label: {
                            if (info_idx == 0) {
                                break :get_label debug_sym_name;
                            }

                            for (func.labels) |label| {
                                const name, const idx = label;
                                if (info_idx == idx) {
                                    break :get_label name;
                                }
                            }

                            break :get_label "";
                        };

                        // Find comments
                        comments.clearRetainingCapacity();
                        while (src_fbs.pos < info.source) {
                            const line_start = src_fbs.pos;
                            try src_reader.skipUntilDelimiterOrEof('\n');
                            const line_end = src_fbs.pos;

                            const line = std.mem.trim(u8, module.source[line_start..line_end], "\n\r");
                            const comment_start = std.mem.indexOf(u8, line, "//") orelse continue;

                            try comments.append(gen.allocator, line[(comment_start + "//".len)..]);
                        }

                        // Write symbols
                        try writer.print("SnesPrgRom:{x}:{s}", .{ func_offset + info.offset, label });

                        for (comments.items, 0..) |comment, i| {
                            if (i == 0) {
                                try writer.writeByte(':');
                            } else {
                                try writer.writeAll("\\n");
                            }

                            try writer.writeAll(comment);
                        }

                        try writer.writeByte('\n');
                    }
                },
                else => @panic("Unsupported"),
            }
        }
    }
}

/// Genrates .cdl debug data for Mesen2
pub fn generateCdlData(gen: CodeGen, rom: []const u8) ![]const u8 {
    const CdlHeader = extern struct {
        magic: [5]u8 align(1) = "CDLv2".*,
        crc: u32 align(1),
    };
    const CdlFlags = packed struct(u8) {
        // Generic flags
        code: bool = false,
        data: bool = false,
        jump_target: bool = false,
        sub_entry_point: bool = false,

        // SNES specific flags
        index_mode_8: bool = false,
        memory_mode_8: bool = false,
        gsu: bool = false,
        cx4: bool = false,
    };

    const cdl_data = try gen.allocator.alloc(u8, rom.len + @sizeOf(CdlHeader));
    @memset(cdl_data, 0xCC);

    const cdl_header: *CdlHeader = @ptrCast(@alignCast(cdl_data.ptr));
    // Specific hash algorithm used by Mesen2 (See https://github.com/SourMesen/Mesen2/blob/master/Utilities/CRC32.cpp)
    cdl_header.* = .{ .crc = crc32.Crc32SliceBy8(0x04C11DB7).hash(rom) };

    const cdl_flags: []CdlFlags = @ptrCast(cdl_data[(@sizeOf(CdlHeader) - 0)..]);
    @memset(cdl_flags, .{});

    for (gen.sema.symbols.items(.sym)) |sym| {
        switch (sym) {
            .function => |func| {
                const func_offset = memory_map.bankToRomOffset(gen.mapping_mode, func.bank) + func.bank_offset;
                for (func.instructions, 0..) |info, info_idx| {
                    const has_label_target = get_label: {
                        for (func.labels) |label| {
                            _, const idx = label;
                            if (info_idx == idx) {
                                break :get_label true;
                            }
                        }
                        break :get_label false;
                    };

                    cdl_flags[func_offset + info.offset] = .{
                        .code = true,
                        .jump_target = has_label_target,
                        .sub_entry_point = info_idx == 0,

                        .index_mode_8 = info.xy_size == .@"8bit",
                        .memory_mode_8 = info.a_size == .@"8bit",
                    };
                }
            },
            else => @panic("Unsupported"),
        }
    }

    return cdl_data;
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

    /// Starting source-offset which caused this instruction
    source: usize,
};

/// Builds instruction information from the AST nodes of a function
const FunctionBuilder = struct {
    codegen: *CodeGen,

    module: Module,
    symbol: Symbol.Function,
    symbol_name: []const u8,

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,
    labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

    target_bank: u8 = 0x80, // 0x80 is the first bank, so default to that

    // Register State
    a_size: Instruction.SizeMode = .none,
    xy_size: Instruction.SizeMode = .none,
    a_reg_id: ?u64 = null,
    x_reg_id: ?u64 = null,
    y_reg_id: ?u64 = null,

    pub fn build(b: *FunctionBuilder) !void {
        _ = b; // autofix
        // try b.handleFnDef(b.symbol.node);
    }

    const Error = error{OutOfMemory};

    // Node handling
    fn handleFnDef(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        const node = b.module.ast.nodes[node_idx];
        std.debug.assert(node.tag == .fn_def);

        var child_iter = b.module.ast.iterChildren(node_idx);
        while (child_iter.nextIndex()) |child_idx| {
            const child = b.module.ast.nodes[child_idx];
            std.log.info("c {}", .{child});
            switch (child.tag) {
                .bank_attr => |bank| b.target_bank = bank,
                .block_scope => try b.handleBlockScope(child_idx),

                else => std.debug.panic("Unexpected expression node {}", .{child}),
            }
        }
    }

    fn handleBlockScope(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        const node = b.module.ast.nodes[node_idx];
        std.debug.assert(node.tag == .block_scope);

        var child_iter = b.module.ast.iterChildren(node_idx);
        while (child_iter.next()) |child| {
            switch (child.tag) {
                // .expr => try b.handleBlock(child),
                .instruction => try b.handleInstruction(child),
                .label => try b.handleLabel(child),

                else => std.debug.panic("Unexpected expression node {}", .{child}),
            }
        }
    }

    fn handleExpression(b: *FunctionBuilder, node_idx: NodeIndex) Error!void {
        _ = b; // autofix
        _ = node_idx; // autofix
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

            .source = b.module.ast.tokens[node.main_token].loc.start,
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

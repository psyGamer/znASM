const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Symbol = @import("symbol.zig").Symbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Sema = @import("Sema.zig");
const Instruction = @import("instruction.zig").Instruction;
const InstructionType = @import("instruction.zig").InstructionType;
const Ir = @import("ir.zig").Ir;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const memory_map = @import("memory_map.zig");
const crc32 = @import("util/crc32.zig");
const CodeGen = @This();

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {
        /// Relative 8-bit offset to the symbol
        rel8,
        /// 16-bit Absolute address of the symbol
        addr16,
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

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode,

    /// Source-node for this instruction
    source: NodeIndex,
};

mapping_mode: MappingMode,
sema: *Sema,
banks: std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u8)) = .empty, // Bank -> BankData

allocator: std.mem.Allocator,

pub fn generate(gen: *CodeGen) !void {
    const symbols = gen.sema.symbols.slice();

    for (symbols.items(.sym), symbols.items(.loc)) |*sym, loc| {
        if (sym.* != .function) {
            continue;
        }

        var builder: FunctionBuilder = .{
            .codegen = gen,
            .symbol = sym.function,
            .symbol_location = loc,
        };
        try builder.build();

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
                .rel8 => {
                    const current_addr = memory_map.bankOffsetToAddr(gen.mapping_mode, func.bank, func.bank_offset) + info.offset + info.instr.size();
                    const rel_offset: i8 = @intCast(@as(i32, @intCast(target_addr)) - @as(i32, @intCast(current_addr)));
                    const operand: *[1]u8 = bank.items[(func.bank_offset + info.offset + 1)..][0..1];
                    operand.* = @bitCast(rel_offset);
                },
                .addr16 => {
                    const absolute_addr: u16 = @truncate(target_addr);
                    const operand: *[2]u8 = bank.items[(func.bank_offset + info.offset + 1)..][0..2];
                    operand.* = @bitCast(absolute_addr);
                },
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
    const WordWrapIter = struct {
        const max_comment_length = 60;

        line: []const u8,
        index: usize = 0,

        pub fn next(iter: *@This()) ?[]const u8 {
            const start = iter.index;
            const end = @min(iter.line.len, iter.index + max_comment_length);

            var last_valid = start;
            while (iter.index < end) : (iter.index += 1) {
                if (iter.index == iter.line.len - 1) {
                    last_valid = iter.line.len;
                    break;
                }
                if (iter.line[iter.index] == ' ') {
                    last_valid = iter.index;
                }
            }

            if (last_valid == start) {
                return null;
            }

            iter.index = last_valid + 1;
            return iter.line[start..last_valid];
        }
    };

    const symbols = gen.sema.symbols.items(.sym);

    var comments: std.ArrayListUnmanaged([]const u8) = .empty;
    defer comments.deinit(gen.allocator);

    for (gen.sema.symbol_map.keys(), gen.sema.symbol_map.values(), 0..) |module_name, module_symbols, module_index| {
        const module = gen.sema.modules[module_index];

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
                        defer {
                            for (comments.items) |comment| {
                                gen.allocator.free(comment);
                            }
                            comments.clearRetainingCapacity();
                        }

                        // Document comments
                        if (info_idx == 0) {
                            const fn_def = module.ast.node_data[func.node].fn_def;
                            const data = module.ast.extraData(Ast.Node.FnDefData, fn_def.extra);

                            for (data.doc_comment_start..data.doc_comment_end) |extra_idx| {
                                const node_idx = module.ast.extra_data[extra_idx];
                                const token_idx = module.ast.node_tokens[node_idx];
                                std.debug.assert(module.ast.token_tags[token_idx] == .doc_comment);
                                const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                                if (comments.items.len == 0) {
                                    try comments.append(gen.allocator, try gen.allocator.dupe(u8, " Documentation:"));
                                }

                                var iter: WordWrapIter = .{ .line = doc_comment };
                                while (iter.next()) |line| {
                                    try comments.append(gen.allocator, try std.fmt.allocPrint(gen.allocator, "   {s}", .{line}));
                                }
                            }
                        }

                        // Regular comments
                        const info_source = module.ast.token_locs[module.ast.node_tokens[info.source]].start;
                        while (src_fbs.pos < info_source) {
                            const line_start = src_fbs.pos;
                            try src_reader.skipUntilDelimiterOrEof('\n');
                            const line_end = src_fbs.pos;

                            const comment_line = module.source[line_start..line_end];
                            const comment_start = std.mem.indexOf(u8, comment_line, "//") orelse continue;
                            const comment = std.mem.trim(u8, comment_line[(comment_start + "//".len)..], " \t\n\r");

                            if (info_idx == 0 and comments.items.len != 0) {
                                // Separate from doc-comments
                                try comments.append(gen.allocator, try gen.allocator.dupe(u8, ""));
                            }

                            var iter: WordWrapIter = .{ .line = comment };
                            while (iter.next()) |line| {
                                try comments.append(gen.allocator, try std.fmt.allocPrint(gen.allocator, " {s}", .{line}));
                            }
                        }

                        // Write label
                        if (info_idx == 0) {
                            // Function label
                            try writer.print("SnesPrgRom:{x}:{s}", .{ func_offset + info.offset, debug_sym_name });
                        } else {
                            for (func.labels) |label| {
                                const name, const idx = label;
                                if (info_idx == idx) {
                                    // Jump label (prefixed to be unique)
                                    try writer.print("SnesPrgRom:{x}:{s}__{s}", .{ func_offset + info.offset, name, debug_sym_name });
                                    break;
                                }
                            } else {
                                // No label
                                try writer.print("SnesPrgRom:{x}:", .{func_offset + info.offset});
                            }
                        }

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

                        .index_mode_8 = info.idx_size == .@"8bit",
                        .memory_mode_8 = info.mem_size == .@"8bit",
                    };
                }
            },
            else => @panic("Unsupported"),
        }
    }

    return cdl_data;
}

/// Builds instruction information from the AST nodes of a function
const FunctionBuilder = struct {
    /// Index to a target instruction
    pub const Label = u16;

    codegen: *CodeGen,

    symbol: Symbol.Function,
    symbol_location: SymbolLocation,

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,
    labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode = .none,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode = .none,

    pub fn build(b: *FunctionBuilder) !void {
        for (b.symbol.ir) |ir| {
            switch (ir.tag) {
                .instruction => try b.handleInstruction(ir),
                .label => try b.handleLabel(ir),
            }
        }
    }

    fn handleInstruction(b: *FunctionBuilder, ir: Ir) !void {
        const instruction = ir.tag.instruction;
        try b.instructions.append(b.codegen.allocator, .{
            .instr = instruction.instr,
            .offset = undefined,

            .reloc = instruction.reloc,
            .branch_reloc = null,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleLabel(b: *FunctionBuilder, ir: Ir) !void {
        try b.labels.put(b.codegen.allocator, ir.tag.label, @intCast(b.instructions.items.len));
    }

    /// Generates the raw assembly bytes for all instructions
    fn genereateAssemblyData(b: *FunctionBuilder) ![]u8 {
        var data: std.ArrayListUnmanaged(u8) = .{};
        // TODO: Figure out good instruction/assembly ratio
        try data.ensureTotalCapacity(b.codegen.allocator, b.instructions.items.len * 2);

        const data_writer = data.writer(b.codegen.allocator);

        for (b.instructions.items) |*info| {
            const size_type = info.instr.size_type();
            const size_mode = switch (size_type) {
                .none => .none,
                .mem => info.mem_size,
                .idx => info.idx_size,
            };
            if (size_type != .none) {
                std.debug.assert(size_mode != .none);
            }

            info.offset = @intCast(data.items.len);
            try info.instr.write_data(data_writer, size_mode);
        }

        return data.toOwnedSlice(b.codegen.allocator);
    }

    /// By default labels point to a symbol in the current module, instead of the current one with an offset
    fn fixLabelRelocs(b: *FunctionBuilder) !void {
        for (b.instructions.items) |*info| {
            const reloc = &(info.reloc orelse continue);
            if (!std.mem.eql(u8, reloc.target_sym.module, b.symbol_location.module)) continue;

            for (b.labels.keys(), b.labels.values()) |label, instr_index| {
                if (std.mem.eql(u8, label, reloc.target_sym.name)) {
                    reloc.target_sym = b.symbol_location;
                    reloc.target_offset = if (instr_index == b.instructions.items.len)
                        @intCast(b.symbol.assembly_data.len)
                    else
                        b.instructions.items[instr_index].offset;
                }
            }
        }
    }
};

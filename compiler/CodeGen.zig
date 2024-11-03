const std = @import("std");
const memory_map = @import("memory_map.zig");
const crc32 = @import("util/crc32.zig");
const rich = @import("util/rich.zig");

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
pub const BranchRelocation = struct {
    pub const Type = enum {
        always,
        carry_set,
        carry_clear,
        overflow_set,
        overflow_clear,
        equal,
        not_equal,
        plus,
        minus,
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

        try builder.resolveBranchRelocs();
        sym.function.assembly_data = try builder.genereateAssemblyData();
        try builder.fixLabelRelocs();
        sym.function.instructions = try builder.instructions.toOwnedSlice(gen.allocator);

        const labels = try gen.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
        for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
            label.* = .{ label_name, index };
        }
        sym.function.labels = labels;
        builder.labels.deinit(gen.allocator);
    }
}

pub fn deinit(gen: *CodeGen) void {
    for (gen.banks.values()) |*bank_data| {
        bank_data.deinit(gen.allocator);
    }
    gen.banks.deinit(gen.allocator);
}

/// Returns the memory-mapped location of a symbol
pub fn symbolLocation(gen: CodeGen, symbol_loc: SymbolLocation) u24 {
    const symbol_idx = gen.sema.symbol_map.get(symbol_loc.module).?.get(symbol_loc.name).?;
    const symbol = gen.sema.symbols.items(.sym)[symbol_idx];

    return switch (symbol) {
        .function => |func_sym| memory_map.bankOffsetToAddr(gen.mapping_mode, func_sym.bank, func_sym.bank_offset),
        .constant => |const_sym| memory_map.bankOffsetToAddr(gen.mapping_mode, const_sym.bank, const_sym.bank_offset),
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
/// Returns whether the operation was successful
pub fn resolveRelocations(gen: CodeGen) !bool {
    const highlight = "bold bright_magenta";
    var has_errors = false;

    const symbols = gen.sema.symbols.slice();
    for (symbols.items(.sym), symbols.items(.loc)) |*sym, loc| {
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
                    const offset = @as(i32, @intCast(target_addr)) - @as(i32, @intCast(current_addr));
                    if (offset < std.math.minInt(i8) or offset > std.math.maxInt(i8)) {
                        try gen.reportLinkerError(info, loc, "Offset [" ++ highlight ++ "]{}[reset] is does not fit into a [" ++ highlight ++ "]signed 8-bit number", .{offset});
                        has_errors = true;
                        continue;
                    }
                    const rel_offset: i8 = @intCast(offset);
                    const operand: *[1]u8 = bank.items[(func.bank_offset + info.offset + 1)..][0..1];
                    operand.* = @bitCast(rel_offset);
                },
                .addr16 => {
                    if (memory_map.getRealBank(gen.mapping_mode, @intCast(target_addr >> 16)) != func.bank) {
                        try gen.reportLinkerError(info, loc, "Target address [" ++ highlight ++ "]${x:0>6}[reset] is not in the same bank as the calling code ([" ++ highlight ++ "]${x:0>2}[reset]) ", .{ target_addr, func.bank });
                        has_errors = true;
                        continue;
                    }
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

    return !has_errors;
}

/// Reports a linker error
/// This doesn't use the error style, like in Ast/Sema, since the linker is just relocations
fn reportLinkerError(gen: CodeGen, info: InstructionInfo, sym_loc: SymbolLocation, comptime fmt: []const u8, args: anytype) !void {
    const module = get_module: {
        for (gen.sema.modules) |module| {
            if (std.mem.eql(u8, module.name.?, sym_loc.module)) {
                break :get_module module;
            }
        }
        unreachable;
    };

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);
    const writer = stderr.writer();

    const token_idx = module.ast.node_tokens[info.source];
    const token_loc = module.ast.token_locs[token_idx];
    const src_loc = std.zig.findLineColumn(module.source, token_loc.start);

    try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", .{ module.source_path, src_loc.line + 1, src_loc.column + 1 });
    try rich.print(writer, tty_config, fmt, args);
    try writer.writeByte('\n');

    try writer.writeAll(src_loc.source_line);
    try writer.writeByte('\n');

    try tty_config.setColor(writer, .green);
    try writer.writeByteNTimes(' ', src_loc.column);
    try writer.writeByte('^');
    try writer.writeByteNTimes('~', token_loc.end -| token_loc.start -| 1);
    try tty_config.setColor(writer, .reset);

    try writer.writeByte('\n');
    try writer.writeByte('\n');
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
                .function => |func_sym| {
                    src_fbs.pos = module.ast.token_locs[module.ast.node_tokens[func_sym.node]].start;

                    const func_offset = memory_map.bankToRomOffset(gen.mapping_mode, func_sym.bank) + func_sym.bank_offset;

                    for (func_sym.instructions, 0..) |info, info_idx| {
                        defer {
                            for (comments.items) |comment| {
                                gen.allocator.free(comment);
                            }
                            comments.clearRetainingCapacity();
                        }

                        // Document comments
                        if (info_idx == 0) {
                            const fn_def = module.ast.node_data[func_sym.node].fn_def;
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
                            for (func_sym.labels) |label| {
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
                .constant => |const_sym| {
                    // Document comments
                    const const_def = module.ast.node_data[const_sym.node].const_def;
                    const data = module.ast.extraData(Ast.Node.ConstDefData, const_def.extra);

                    for (data.doc_comment_start..data.doc_comment_end) |extra_idx| {
                        const node_idx = module.ast.extra_data[extra_idx];
                        const token_idx = module.ast.node_tokens[node_idx];
                        std.debug.assert(module.ast.token_tags[token_idx] == .doc_comment);
                        const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                        var iter: WordWrapIter = .{ .line = doc_comment };
                        while (iter.next()) |line| {
                            try comments.append(gen.allocator, try std.fmt.allocPrint(gen.allocator, "   {s}", .{line}));
                        }
                    }
                },
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
            .function => |func_sym| {
                const func_offset = memory_map.bankToRomOffset(gen.mapping_mode, func_sym.bank) + func_sym.bank_offset;
                for (func_sym.instructions, 0..) |info, info_idx| {
                    const has_label_target = get_label: {
                        for (func_sym.labels) |label| {
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
            .constant => |const_sym| {
                _ = const_sym; // autofix
                // TODO
            },
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
    mem_size: Instruction.SizeMode = .@"8bit",
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode = .none,

    pub fn build(b: *FunctionBuilder) !void {
        for (b.symbol.ir) |ir| {
            switch (ir.tag) {
                .instruction => try b.handleInstruction(ir),
                .label => try b.handleLabel(ir),
                .branch => try b.handleBranch(ir),
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
    fn handleBranch(b: *FunctionBuilder, ir: Ir) !void {
        try b.instructions.append(b.codegen.allocator, .{
            .instr = undefined,
            .offset = undefined,

            .reloc = null,
            .branch_reloc = ir.tag.branch,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
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

    /// Resolves branch locations into a short / long form
    fn resolveBranchRelocs(b: *FunctionBuilder) !void {
        // Relative offsets to the target instruction to determine short- / long-form
        var reloc_offsets: std.AutoArrayHashMapUnmanaged(usize, i32) = .{};
        defer reloc_offsets.deinit(b.codegen.allocator);

        for (b.instructions.items, 0..) |info, i| {
            if (info.branch_reloc != null) {
                // Default to long-form, lower to short-form later
                try reloc_offsets.put(b.codegen.allocator, i, std.math.maxInt(i32));
            }
        }

        const short_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.bra.size(),
            .carry_set = comptime Instruction.bcs.size(),
            .carry_clear = comptime Instruction.bcc.size(),
            .overflow_set = comptime Instruction.bvs.size(),
            .overflow_clear = comptime Instruction.bvc.size(),
            .equal = comptime Instruction.beq.size(),
            .not_equal = comptime Instruction.bne.size(),
            .plus = comptime Instruction.bpl.size(),
            .minus = comptime Instruction.bmi.size(),
        });
        const long_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.jmp.size(),
            .carry_set = comptime Instruction.bcc.size() + Instruction.jmp.size(),
            .carry_clear = comptime Instruction.bcs.size() + Instruction.jmp.size(),
            .overflow_set = comptime Instruction.bvc.size() + Instruction.jmp.size(),
            .overflow_clear = comptime Instruction.bvs.size() + Instruction.jmp.size(),
            .equal = comptime Instruction.bne.size() + Instruction.jmp.size(),
            .not_equal = comptime Instruction.beq.size() + Instruction.jmp.size(),
            .plus = comptime Instruction.bmi.size() + Instruction.jmp.size(),
            .minus = comptime Instruction.bpl.size() + Instruction.jmp.size(),
        });

        // Interativly lower to short-form
        var changed = true;
        while (changed) {
            changed = false;

            for (reloc_offsets.keys(), reloc_offsets.values()) |source_idx, *relative_offset| {
                // If its's already short, don't mark this as a change, but still recalculate the offset
                const already_short = relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8);

                const reloc = b.instructions.items[source_idx].branch_reloc.?;
                const target_idx = b.labels.get(reloc.target).?;

                // Calculate offset to target
                const min = @min(source_idx + 1, target_idx);
                const max = @max(source_idx + 1, target_idx);

                relative_offset.* = 0;
                for (b.instructions.items[min..max], min..max) |info, i| {
                    if (info.branch_reloc) |other_reloc| {
                        const other_offset = reloc_offsets.get(i).?;

                        if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                            relative_offset.* += short_sizes.get(other_reloc.type);
                        } else {
                            relative_offset.* += long_sizes.get(other_reloc.type);
                        }
                    } else {
                        relative_offset.* += info.instr.size();
                    }
                }
                if (target_idx <= source_idx) {
                    relative_offset.* = -relative_offset.*;
                }

                if (!already_short and relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8)) {
                    changed = true;
                }
            }
        }

        // Calculate target offsets (for jumps)
        var target_offsets: std.AutoArrayHashMapUnmanaged(usize, u16) = .{};
        defer target_offsets.deinit(b.codegen.allocator);

        for (reloc_offsets.keys()) |source_idx| {
            const reloc = b.instructions.items[source_idx].branch_reloc.?;
            const target_idx = b.labels.get(reloc.target).?;

            var offset: usize = 0;
            for (b.instructions.items[0..target_idx], 0..) |info, i| {
                if (info.branch_reloc) |other_reloc| {
                    const other_offset = reloc_offsets.get(i).?;

                    if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                        offset += short_sizes.get(other_reloc.type);
                    } else {
                        offset += long_sizes.get(other_reloc.type);
                    }
                } else {
                    offset += info.instr.size();
                }
            }

            try target_offsets.put(b.codegen.allocator, source_idx, @intCast(offset));
        }

        // Insert instructions (reversed to avoid shifting following indices)
        var it = std.mem.reverseIterator(reloc_offsets.keys());

        while (it.next()) |source_idx| {
            const info = &b.instructions.items[source_idx];
            const reloc = info.branch_reloc.?;

            const relative_offset = reloc_offsets.get(source_idx).?;
            const target_offset = target_offsets.get(source_idx).?;

            const use_short = relative_offset >= std.math.minInt(i8) and relative_offset <= std.math.maxInt(i8);

            if (use_short) {
                info.instr = switch (reloc.type) {
                    .always => .{ .bra = @intCast(relative_offset) },
                    .carry_set => .{ .bcs = @intCast(relative_offset) },
                    .carry_clear => .{ .bcc = @intCast(relative_offset) },
                    .overflow_set => .{ .bvs = @intCast(relative_offset) },
                    .overflow_clear => .{ .bvc = @intCast(relative_offset) },
                    .equal => .{ .beq = @intCast(relative_offset) },
                    .not_equal => .{ .bne = @intCast(relative_offset) },
                    .plus => .{ .bpl = @intCast(relative_offset) },
                    .minus => .{ .bmi = @intCast(relative_offset) },
                };
            } else {
                const jmp_size = comptime InstructionType.jmp.size();
                info.instr = switch (reloc.type) {
                    .always => .{ .jmp = undefined },
                    .carry_set => .{ .bcc = jmp_size },
                    .carry_clear => .{ .bcs = jmp_size },
                    .overflow_set => .{ .bvc = jmp_size },
                    .overflow_clear => .{ .bvs = jmp_size },
                    .equal => .{ .bne = jmp_size },
                    .not_equal => .{ .beq = jmp_size },
                    .plus => .{ .bmi = jmp_size },
                    .minus => .{ .bpl = jmp_size },
                };

                const jmp_reloc: Relocation = .{
                    .type = .addr16,
                    .target_sym = b.symbol_location,
                    .target_offset = target_offset,
                };

                if (reloc.type == .always) {
                    info.reloc = jmp_reloc;
                } else {
                    try b.instructions.insert(b.codegen.allocator, source_idx + 1, .{
                        .instr = .{ .jmp = undefined },
                        .offset = undefined,

                        .reloc = jmp_reloc,
                        .branch_reloc = null,

                        .mem_size = info.mem_size,
                        .idx_size = info.idx_size,

                        .source = info.source,
                    });
                }
            }
        }
    }
};

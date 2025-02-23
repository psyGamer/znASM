const std = @import("std");
const memory_map = @import("memory_map.zig");
const rich = @import("util/rich.zig");
const crc32 = @import("util/crc32.zig");

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Symbol = @import("symbol.zig").Symbol;
const SymbolIndex = @import("Sema.zig").SymbolIndex;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Sema = @import("Sema.zig");
const Opcode = @import("instruction.zig").Opcode;
const Instruction = @import("instruction.zig").Instruction;
const InstructionInfo = @import("CodeGen.zig").InstructionInfo;
const Ir = @import("codegen/AssemblyIr.zig");
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;

const Linker = @This();

pub const Error = struct {
    pub const Tag = enum {
        // Extra: no_space_left
        no_space_left,
        // Extra: offset
        offset_too_large_i8,
        offset_too_large_i16,
        // Extra: different_bank
        different_bank,
    };

    tag: Tag,

    /// Notes are associated with the previous error
    is_note: bool = false,

    node: Ast.NodeIndex,
    symbol_location: SymbolLocation,

    extra: union {
        none: void,
        no_space_left: struct {
            addr_min: u24,
            addr_max: u24,
            size: u16,
        },
        offset: i32,
        different_bank: struct {
            source_bank: u8,
            target_addr: u24,
        },
    } = .{ .none = {} },
};

mapping_mode: MappingMode,
sema: *Sema,

rom_banks: std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u8)) = .empty, // Bank -> BankData
rom_bank_data: []const Rom.BankData = &.{},

errors: std.ArrayListUnmanaged(Error) = .empty,

allocator: std.mem.Allocator,

pub fn process(sema: *Sema) !?Linker {
    var linker: Linker = .{
        .mapping_mode = sema.mapping_mode,
        .sema = sema,
        .allocator = sema.allocator,
    };
    errdefer linker.deinit();

    try linker.resolveSymbolAddresses();
    try linker.resolveRelocations();

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    if (try linker.detectErrors(stderr.writer(), tty_config)) {
        linker.deinit();
        return null;
    }

    try linker.allocateBanks();

    return linker;
}

pub fn deinit(link: *Linker) void {
    for (link.rom_banks.values()) |*bank_data| {
        bank_data.deinit(link.allocator);
    }
    link.rom_banks.deinit(link.allocator);

    for (link.rom_bank_data) |bank_data| {
        link.allocator.free(bank_data.data);
    }
    link.allocator.free(link.rom_bank_data);

    link.errors.deinit(link.allocator);
}

/// Returns the memory-mapped location of a symbol
pub fn symbolLocation(link: Linker, symbol_idx: SymbolIndex) u24 {
    const symbol = link.sema.getSymbol(symbol_idx).*;

    return switch (symbol) {
        .function => |func_sym| memory_map.bankOffsetToAddr(link.mapping_mode, func_sym.bank, func_sym.bank_offset),
        .constant => |const_sym| memory_map.bankOffsetToAddr(link.mapping_mode, const_sym.bank, const_sym.bank_offset),
        .variable => |var_sym| memory_map.wramOffsetToAddr(var_sym.wram_offset),
        .register => |reg_sym| reg_sym.address,
        .@"struct", .@"packed", .@"enum" => unreachable,
    };
}

/// Resolves the final addresses of symbols
pub fn resolveSymbolAddresses(link: *Linker) !void {
    const used_ram = try link.allocator.alloc(bool, std.math.maxInt(u17));
    defer link.allocator.free(used_ram);
    @memset(used_ram, false);

    symbol_loop: for (link.sema.symbols.items, 0..) |*symbol, symbol_idx| {
        // Skip unreferenced symbols
        if (symbol.common().analyze_status == .pending) {
            continue;
        }
        std.debug.assert(symbol.common().analyze_status == .done);

        switch (symbol.*) {
            .function => |*func_sym| {
                const gop = try link.rom_banks.getOrPut(link.allocator, func_sym.bank);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .empty;
                }

                func_sym.bank_offset = @intCast(gop.value_ptr.items.len);
                try gop.value_ptr.appendSlice(link.allocator, func_sym.assembly_data);
                std.log.info("asm {x}", .{func_sym.assembly_data});
            },
            .constant => |*const_sym| {
                _ = const_sym; // autofix
            },
            .variable => |*var_sym| {
                // TODO: Place variables from high addr-variance to low addr-variance and from large to small
                const var_size = var_sym.type.byteSize(link.sema);
                const possible = used_ram[var_sym.wram_offset_min..var_sym.wram_offset_max];

                var last_start: u16 = 0;
                for (possible, 0..) |used, i| {
                    if (used) {
                        last_start = @intCast(i + 1);
                        continue;
                    }

                    if (i - last_start + 1 == var_size) {
                        var_sym.wram_offset = var_sym.wram_offset_min + last_start;
                        @memset(possible[last_start..(i + 1)], true);
                        continue :symbol_loop;
                    }
                }

                try link.errors.append(link.allocator, .{
                    .tag = .no_space_left,
                    .node = var_sym.common.node,
                    .symbol_location = link.sema.getSymbolLocation(.cast(symbol_idx)),
                    .extra = .{ .no_space_left = .{
                        .addr_min = memory_map.wramOffsetToAddr(var_sym.wram_offset_min),
                        .addr_max = memory_map.wramOffsetToAddr(var_sym.wram_offset_max),
                        .size = var_size,
                    } },
                });
            },
            .register, .@"struct", .@"packed", .@"enum" => {}, // Don't require space in the ROM
        }
    }
}

/// Allocate the actual bank-data for the ROM
pub fn allocateBanks(link: *Linker) !void {
    const bank_data = try link.allocator.alloc(Rom.BankData, link.rom_banks.count());

    for (link.rom_banks.keys(), link.rom_banks.values(), 0..) |bank, *data, i| {
        bank_data[i] = .{
            .bank = bank,
            .data = try data.toOwnedSlice(link.allocator),
        };
    }

    link.rom_bank_data = bank_data;
}

/// Resolves relocation of instructions
pub fn resolveRelocations(link: *Linker) !void {
    for (link.sema.symbols.items, 0..) |*symbol, symbol_idx| {
        if (symbol.* != .function) {
            continue;
        }

        const func = symbol.function;
        const bank = link.rom_banks.get(func.bank).?;

        for (func.instructions) |info| {
            const reloc = info.reloc orelse continue;
            const target_addr = link.symbolLocation(reloc.target_symbol) + reloc.target_offset;

            switch (reloc.type) {
                .rel8 => {
                    const current_addr = memory_map.bankOffsetToAddr(link.mapping_mode, func.bank, func.bank_offset) + info.offset + info.instr.size(.none);
                    const offset = @as(i32, @intCast(target_addr)) - @as(i32, @intCast(current_addr));
                    if (offset < std.math.minInt(i8) or offset > std.math.maxInt(i8)) {
                        try link.errors.append(link.allocator, .{
                            .tag = .offset_too_large_i8,
                            .node = info.source,
                            .symbol_location = link.sema.getSymbolLocation(.cast(symbol_idx)),
                            .extra = .{ .offset = offset },
                        });
                        continue;
                    }
                    const rel_offset: i8 = @intCast(offset);
                    const operand: *[1]u8 = bank.items[(func.bank_offset + info.offset + 1)..][0..1];
                    operand.* = @bitCast(rel_offset);
                },
                .addr16 => {
                    if (!memory_map.isAddressAccessibleInBank(link.mapping_mode, target_addr, func.bank)) {
                        try link.errors.append(link.allocator, .{
                            .tag = .different_bank,
                            .node = info.source,
                            .symbol_location = link.sema.getSymbolLocation(.cast(symbol_idx)),
                            .extra = .{ .different_bank = .{
                                .source_bank = func.bank,
                                .target_addr = target_addr,
                            } },
                        });
                        continue;
                    }
                    const absolute_addr: u16 = @truncate(target_addr);
                    const operand: *[2]u8 = bank.items[(func.bank_offset + info.offset + 1)..][0..2];
                    operand.* = @bitCast(absolute_addr);
                },
                .addr24 => {
                    const operand: *[3]u8 = bank.items[(func.bank_offset + info.offset + @sizeOf(Opcode))..][0..3];
                    operand.* = @bitCast(target_addr);
                },
            }
        }
    }
}

/// Reports any existng errors and returns `true` if there are any
pub fn detectErrors(link: Linker, writer: std.fs.File.Writer, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (link.errors.items) |err| {
        if (!err.is_note) {
            try writer.writeByte('\n');
        }

        const module = get_module: {
            for (link.sema.modules) |module| {
                if (std.mem.eql(u8, module.name, err.symbol_location.module)) {
                    break :get_module module;
                }
            }
            unreachable;
        };

        const token_idx = module.ast.nodeToken(err.node);
        const token_loc = module.ast.tokenLoc(token_idx);
        const src_loc = std.zig.findLineColumn(module.source, token_loc.start);

        const args = .{ module.source_path, src_loc.line + 1, src_loc.column + 1 };
        if (err.is_note) {
            try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", args);
        } else {
            try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", args);
        }

        try renderError(writer, tty_config, err);
        try writer.writeByte('\n');

        try writer.writeAll(src_loc.source_line);
        try writer.writeByte('\n');

        try tty_config.setColor(writer, .green);
        try writer.writeByteNTimes(' ', src_loc.column);
        try writer.writeByte('^');
        try writer.writeByteNTimes('~', token_loc.end - token_loc.start - 1);
        try tty_config.setColor(writer, .reset);
        try writer.writeByte('\n');
    }

    return link.errors.items.len != 0;
}
fn renderError(writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    const highlight = "bold bright_magenta";

    return switch (err.tag) {
        .no_space_left => rich.print(writer, tty_config, "No more space left in address range [" ++ highlight ++ "]${x:0>6}...${x:0>6}[reset], to fit symbol [" ++ highlight ++ "]{s}[reset] of size [" ++ highlight ++ "]{d}", .{
            err.extra.no_space_left.addr_min,
            err.extra.no_space_left.addr_max,
            err.symbol_location,
            err.extra.no_space_left.size,
        }),

        .offset_too_large_i8 => rich.print(writer, tty_config, "Offset [" ++ highlight ++ "]{d}[reset] is does not fit into a [" ++ highlight ++ "]signed 8-bit number", .{err.extra.offset}),
        .offset_too_large_i16 => rich.print(writer, tty_config, "Offset [" ++ highlight ++ "]{d}[reset] is does not fit into a [" ++ highlight ++ "]signed 16-bit number", .{err.extra.offset}),

        .different_bank => rich.print(writer, tty_config, "Target address [" ++ highlight ++ "]${x:0>6}[reset] is not in the same bank as the calling code ([" ++ highlight ++ "]${x:0>2}[reset]) ", .{
            err.extra.different_bank.target_addr,
            err.extra.different_bank.source_bank,
        }),
    };
}

/// Writes a .mlb symbol file for Mesen2
pub fn writeMlbSymbols(link: Linker, writer: std.fs.File.Writer) !void {
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

    var comments: std.ArrayListUnmanaged([]const u8) = .empty;
    defer comments.deinit(link.allocator);

    for (link.sema.modules) |module| {
        var src_fbs = std.io.fixedBufferStream(module.source);
        const src_reader = src_fbs.reader();

        for (module.symbol_map.keys(), module.symbol_map.values()) |symbol_name, symbol_idx| {
            const symbol = link.sema.getSymbol(symbol_idx).*;
            const is_vector = symbol_name[0] == '@';

            // Skip unreferenced symbols
            if (symbol.commonConst().analyze_status == .pending) {
                continue;
            }
            std.debug.assert(symbol.commonConst().analyze_status == .done);

            // Usually format module@symbol, except for vectors which would have a double @
            const debug_sym_name = if (is_vector)
                symbol_name
            else
                try std.fmt.allocPrint(link.allocator, "{s}@{s}", .{ module.name, symbol_name });
            defer if (!is_vector) link.allocator.free(debug_sym_name);

            switch (symbol) {
                .function => |func_sym| {
                    src_fbs.pos = module.ast.tokenLoc(module.ast.nodeToken(func_sym.common.node)).start;

                    const func_offset = memory_map.bankToRomOffset(link.mapping_mode, func_sym.bank) + func_sym.bank_offset;

                    for (func_sym.instructions, 0..) |info, info_idx| {
                        defer {
                            for (comments.items) |comment| {
                                link.allocator.free(comment);
                            }
                            comments.clearRetainingCapacity();
                        }

                        // Document comments
                        if (info_idx == 0) {
                            const fn_def = module.ast.nodeData(func_sym.common.node).fn_def;
                            const data = module.ast.readExtraData(Ast.Node.FnDefData, fn_def.extra);

                            for (@intFromEnum(data.doc_comment_start)..@intFromEnum(data.doc_comment_end)) |extra_idx| {
                                const node_idx: NodeIndex = .cast(module.ast.extra_data[extra_idx]);
                                const token_idx = module.ast.nodeToken(node_idx);
                                std.debug.assert(module.ast.tokenTag(token_idx) == .doc_comment);
                                const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                                if (comments.items.len == 0) {
                                    try comments.append(link.allocator, try link.allocator.dupe(u8, " Documentation:"));
                                }

                                var iter: WordWrapIter = .{ .line = doc_comment };
                                while (iter.next()) |line| {
                                    try comments.append(link.allocator, try std.fmt.allocPrint(link.allocator, "   {s}", .{line}));
                                }
                            }
                        }

                        // Regular comments
                        const info_source = module.ast.tokenLoc(module.ast.nodeToken(info.source)).start;
                        while (src_fbs.pos < info_source) {
                            const line_start = src_fbs.pos;
                            try src_reader.skipUntilDelimiterOrEof('\n');
                            const line_end = src_fbs.pos;

                            const comment_line = module.source[line_start..line_end];
                            const comment_start = std.mem.indexOf(u8, comment_line, "//") orelse continue;
                            const comment = std.mem.trim(u8, comment_line[(comment_start + "//".len)..], " \t\n\r");

                            if (info_idx == 0 and comments.items.len != 0) {
                                // Separate from doc-comments
                                try comments.append(link.allocator, try link.allocator.dupe(u8, ""));
                            }

                            var iter: WordWrapIter = .{ .line = comment };
                            while (iter.next()) |line| {
                                try comments.append(link.allocator, try std.fmt.allocPrint(link.allocator, " {s}", .{line}));
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
                    const const_def = module.ast.nodeData(const_sym.common.node).const_def;
                    const data = module.ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

                    for (@intFromEnum(data.doc_comment_start)..@intFromEnum(data.doc_comment_end)) |extra_idx| {
                        const node_idx: NodeIndex = .cast(module.ast.extra_data[extra_idx]);
                        const token_idx = module.ast.nodeToken(node_idx);
                        std.debug.assert(module.ast.tokenTag(token_idx) == .doc_comment);
                        const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                        var iter: WordWrapIter = .{ .line = doc_comment };
                        while (iter.next()) |line| {
                            _ = line; // autofix
                            // try comments.append(link.allocator, try std.fmt.allocPrint(link.allocator, "   {s}", .{line}));
                        }
                    }

                    // TODO: Write symbols
                },
                .variable => |var_sym| {
                    defer comments.clearRetainingCapacity();

                    // Document comments
                    const var_def = module.ast.nodeData(var_sym.common.node).var_def;
                    const data = module.ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

                    for (@intFromEnum(data.doc_comment_start)..@intFromEnum(data.doc_comment_end)) |extra_idx| {
                        const node_idx: NodeIndex = .cast(module.ast.extra_data[extra_idx]);
                        const token_idx = module.ast.nodeToken(node_idx);
                        std.debug.assert(module.ast.tokenTag(token_idx) == .doc_comment);
                        const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                        var iter: WordWrapIter = .{ .line = doc_comment };
                        while (iter.next()) |line| {
                            try comments.append(link.allocator, line);
                        }
                    }

                    if (var_sym.type.byteSize(link.sema) == 1) {
                        try writer.print("SnesWorkRam:{x}:{s}", .{ var_sym.wram_offset, debug_sym_name });
                    } else {
                        try writer.print("SnesWorkRam:{x}-{x}:{s}", .{ var_sym.wram_offset, var_sym.wram_offset + var_sym.type.byteSize(link.sema) - 1, debug_sym_name });
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
                },
                .register => |reg_sym| {
                    defer comments.clearRetainingCapacity();

                    // Document comments
                    const reg_def = module.ast.nodeData(reg_sym.common.node).reg_def;
                    const data = module.ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

                    for (@intFromEnum(data.doc_comment_start)..@intFromEnum(data.doc_comment_end)) |extra_idx| {
                        const node_idx: NodeIndex = .cast(module.ast.extra_data[extra_idx]);
                        const token_idx = module.ast.nodeToken(node_idx);
                        std.debug.assert(module.ast.tokenTag(token_idx) == .doc_comment);
                        const doc_comment = std.mem.trim(u8, module.ast.tokenSource(token_idx)["///".len..], " \t\n\r");

                        var iter: WordWrapIter = .{ .line = doc_comment };
                        while (iter.next()) |line| {
                            try comments.append(link.allocator, line);
                        }
                    }

                    try writer.print("SnesRegister:{x}:{s}", .{ reg_sym.address, debug_sym_name });
                    for (comments.items, 0..) |comment, i| {
                        if (i == 0) {
                            try writer.writeByte(':');
                        } else {
                            try writer.writeAll("\\n");
                        }

                        try writer.writeAll(comment);
                    }
                    try writer.writeByte('\n');
                },
                .@"struct", .@"packed", .@"enum" => {
                    // Don't have symbols representing them
                },
            }
        }
    }
}

/// Genrates .cdl debug data for Mesen2
pub fn generateCdlData(link: Linker, rom: []const u8) ![]const u8 {
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

    const cdl_data = try link.allocator.alloc(u8, rom.len + @sizeOf(CdlHeader));
    @memset(cdl_data, 0xCC);

    const cdl_header: *CdlHeader = @ptrCast(@alignCast(cdl_data.ptr));
    // Specific hash algorithm used by Mesen2 (See https://github.com/SourMesen/Mesen2/blob/master/Utilities/CRC32.cpp)
    cdl_header.* = .{ .crc = crc32.Crc32SliceBy8(0x04C11DB7).hash(rom) };

    const cdl_flags: []CdlFlags = @ptrCast(cdl_data[(@sizeOf(CdlHeader) - 0)..]);
    @memset(cdl_flags, .{});

    for (link.sema.symbols.items) |symbol| {
        // Skip unreferenced symbols
        if (symbol.commonConst().analyze_status == .pending) {
            continue;
        }
        std.debug.assert(symbol.commonConst().analyze_status == .done);

        switch (symbol) {
            .function => |func_sym| {
                const func_offset = memory_map.bankToRomOffset(link.mapping_mode, func_sym.bank) + func_sym.bank_offset;
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
            .variable, .register => {
                // Not in ROM
            },
            .@"struct", .@"packed", .@"enum" => {
                // Doesn't exist at runtime
            },
        }
    }

    return cdl_data;
}

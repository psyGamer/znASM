const std = @import("std");
const builtin = @import("builtin");
const Rom = @import("Rom.zig");
const MappingMode = Rom.Header.Mode.Map;
const Function = @import("Function.zig");
const Builder = @import("Builder.zig");

const BuildSystem = @This();

allocator: std.mem.Allocator,

mapping_mode: MappingMode,
functions: std.AutoArrayHashMapUnmanaged(Function.SymbolPtr, Function) = .{},

pub fn init(allocator: std.mem.Allocator, mapping_mode: MappingMode) !BuildSystem {
    return .{
        .allocator = allocator,

        .mapping_mode = mapping_mode,
    };
}

/// Generates assembly for the symbol and marks it for inclusion in the ROM
pub fn generate_function(sys: *BuildSystem, sym: Function.Symbol) !void {
    if (comptime builtin.cpu.arch.endian() != .little) {
        @compileError("Currently, znASM only supports compiling for little-endian targets");
    }

    if (sys.functions.contains(sym)) {
        // Already generated
        return;
    }

    std.log.debug("Compiling function '{s}'...", .{get_function_name(sym)});

    // Build function body
    var builder: Builder = .init(sys, sym);
    builder.build();

    // Create function definiton
    const function: Function = .{
        .code = try builder.instruction_data.toOwnedSlice(sys.allocator),
        .code_info = try builder.instruction_info.toOwnedSlice(sys.allocator),
        .symbol_name = builder.symbol_name,
        .source = builder.source_location,
    };

    try sys.functions.put(sys.allocator, sym, function);
}

pub fn write_debug_data(sys: *BuildSystem, rom: []const u8, mlb_writer: anytype, cdl_writer: anytype) !void {
    const CdlFlags = packed struct(u8) {
        // Global CDL flags
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

    const cdl_data = try sys.allocator.alloc(CdlFlags, rom.len);
    defer sys.allocator.free(cdl_data);

    @memset(cdl_data, .{}); // Mark everything as unknown initially

    // Write symbol data / mark regions
    for (sys.functions.values()) |func| {
        @memset(cdl_data[func.offset..(func.offset + func.code.len)], .{ .code = true });

        cdl_data[func.offset].sub_entry_point = true;

        // Set instruction specific flags
        for (func.code_info) |info| {
            const instr_region = cdl_data[(func.offset + info.offset)..(func.offset + info.offset + info.instr.size())];

            if (info.instr == .ora or info.instr == .@"and" or info.instr == .eor or info.instr == .adc or
                info.instr == .bit or info.instr == .lda or info.instr == .cmp or info.instr == .sbc)
            {
                @memset(instr_region, .{ .memory_mode_8 = true });
            } else if (info.instr == .ldy or info.instr == .ldx or info.instr == .cpy or info.instr == .cpx) {
                @memset(instr_region, .{ .index_mode_8 = true });
            }

            // TODO: Jump targets
        }

        if (func.symbol_name == null) {
            continue;
        }

        // Try collecting comments
        var comments: std.AutoArrayHashMapUnmanaged(u32, []const u8) = .{};
        defer comments.deinit(sys.allocator);

        b: {
            const file_path = find_file: {
                for (func.code_info) |info| {
                    if (info.caller_file) |file| {
                        break :find_file file;
                    }
                }
                break :b;
            };

            var src_file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
                std.log.err("Failed collecting comments for function '{s}': '{s}' {}", .{ func.symbol_name.?, file_path, err });
                break :b;
            };
            defer src_file.close();

            const src_reader = src_file.reader();

            // Go to function start
            for (1..func.source.?.line) |_| {
                src_reader.skipUntilDelimiterOrEof('\n') catch break :b;
            }

            var line_buffer: std.ArrayListUnmanaged(u8) = .{};
            defer line_buffer.deinit(sys.allocator);

            var src_line = func.source.?.line;
            const last_line = func.code_info[func.code_info.len - 1].caller_line orelse break :b;

            while (src_line <= last_line) : (src_line += 1) {
                line_buffer.clearRetainingCapacity();
                src_reader.streamUntilDelimiter(line_buffer.writer(sys.allocator), '\n', null) catch break :b;

                const line = std.mem.trim(u8, line_buffer.items, " \t\n\r");
                if (std.mem.startsWith(u8, line, "///")) {
                    try comments.put(sys.allocator, src_line, try sys.allocator.dupe(u8, line["///".len..]));
                } else if (std.mem.startsWith(u8, line, "//")) {
                    try comments.put(sys.allocator, src_line, try sys.allocator.dupe(u8, line["//".len..]));
                }
            }
        }

        // Write symbols
        var comment_line: u32 = 0;
        var comment_buffer: std.ArrayListUnmanaged(u8) = .{};
        defer comment_buffer.deinit(sys.allocator);

        for (func.code_info) |info| {
            comment_buffer.clearRetainingCapacity();
            if (info.caller_line) |caller_line| {
                while (comment_line < caller_line) : (comment_line += 1) {
                    if (comments.get(comment_line)) |comment| {
                        if (comment_buffer.items.len != 0) {
                            try comment_buffer.appendSlice(sys.allocator, "\\n");
                        }
                        try comment_buffer.appendSlice(sys.allocator, comment);
                    }
                }
            }

            const label = if (info.offset == 0)
                func.symbol_name orelse ""
            else
                "";

            if (comment_buffer.items.len == 0 and label.len == 0) {
                continue;
            }

            try mlb_writer.print("SnesPrgRom:{x}:{s}:{s}\n", .{ func.offset + info.offset, label, comment_buffer.items });
        }
    }

    // Specific hash algorithm used by Mesen2 (See https://github.com/SourMesen/Mesen2/blob/master/Utilities/CRC32.cpp)
    const crc = std.hash.crc.Crc(u32, .{
        .polynomial = 0x77073096,
        .initial = 0x00000000,
        .reflect_input = false,
        .reflect_output = false,
        .xor_output = 0x00000000,
    });

    try cdl_writer.writeAll("CDLv2");
    try cdl_writer.writeInt(u32, crc.hash(rom), .little);
    try cdl_writer.writeAll(@ptrCast(cdl_data));
}

/// Calculates the real (non-mirrored) memory-mapped address of a symbol
pub fn symbol_location(sys: BuildSystem, sym: Function.Symbol) u24 {
    if (sys.functions.get(sym)) |func| {
        switch (sys.mapping_mode) {
            .lorom => {
                const bank: u8 = @intCast(func.offset / 0x8000 + 0x80);
                const addr: u16 = @intCast(func.offset % 0x8000 + 0x8000);
                return @as(u24, bank) << 16 | addr;
            },
            .hirom => {
                @panic("TODO: HiROM");
            },
            .exhirom => {
                @panic("TODO: ExHiROM");
            },
        }
        return func.offset;
    } else {
        std.debug.panic("Tried to get offset of unknown symbol '{s}'", .{get_function_name(sym)});
    }
}

// Slight hack to get the function name at comptime
fn get_function_name(func: anytype) []const u8 {
    const S = struct {
        fn Dummy(f: anytype) type {
            return struct {
                fn warpper() void {
                    f();
                }
            };
        }
    };

    const name = @typeName(S.Dummy(func));
    const start = (std.mem.indexOfScalar(u8, name, '\'') orelse unreachable) + 1;
    const end = std.mem.indexOfScalarPos(u8, name, start, '\'') orelse unreachable;
    return name[start..end];
}

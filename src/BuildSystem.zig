const std = @import("std");
const builtin = @import("builtin");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
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
    const function: Function = .{ .code = try builder.instruction_data.toOwnedSlice(sys.allocator) };
    try sys.functions.put(sys.allocator, sym, function);
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

const std = @import("std");
const builtin = @import("builtin");
const Symbol = @import("Function.zig").Symbol;
const Builder = @import("Builder.zig");

const BuildSystem = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) !BuildSystem {
    return .{
        .allocator = allocator,
    };
}

pub fn generate_function(sys: *BuildSystem, sym: Symbol) !void {
    if (comptime builtin.cpu.arch.endian() != .little) {
        @compileError("Currently, znASM only supports compiling for little-endian targets");
    }

    std.log.debug("Compiling function '{s}'...", .{getFunctionName(sym)});

    // Build function body
    var builder: Builder = .init(sys, sym);
    builder.build();
    // builder.emit(.rti);
    // builder.emit(.{ .jmp = .{ .addr = 0x3456 } });

    std.log.info("{x}", .{builder.instruction_data.items});
}

// Slight hack to get the function name at comptime
fn getFunctionName(func: anytype) []const u8 {
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

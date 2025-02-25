//! A processed .znasm file
const std = @import("std");

const Ast = @import("Ast.zig");
const Sema = @import("Sema.zig");
const Symbol = Sema.Symbol;

const Module = @This();

pub const Index = enum(u32) {
    _,

    /// Helper function to cast a generic number to a `Module.Index`
    pub inline fn cast(x: anytype) Index {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(u32, @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(u32, @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a Index"),
        };
    }

    /// Fetches the underlying value
    pub fn get(index: Index, sema: *Sema) *Module {
        return &sema.modules[@intFromEnum(index)];
    }
};

source: [:0]const u8,
source_path: []const u8,

name: []const u8,
ast: Ast,

symbol_map: std.StringArrayHashMapUnmanaged(Symbol.Index),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, source: [:0]const u8, source_path: []const u8) !?Module {
    var ast = try Ast.parse(allocator, source, source_path);

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    if (try ast.detectErrors(stderr.writer(), tty_config)) {
        allocator.free(source);
        ast.deinit(allocator);
        return null;
    }

    return .{
        .source = source,
        .source_path = source_path,

        .name = "",
        .ast = ast,

        .symbol_map = .empty,

        .allocator = allocator,
    };
}

pub fn deinit(module: *Module) void {
    module.allocator.free(module.source);

    module.ast.deinit(module.allocator);
    module.symbol_map.deinit(module.allocator);
}

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

/// Name of this module, defined with `module <name>;`
name: []const u8,

/// Abstract-Syntax-Tree representing this module
ast: Ast,

/// Mapping of symbol names to their indices
symbol_map: std.StringArrayHashMapUnmanaged(Symbol.Index),

pub fn init(ast: Ast) Module {
    return .{ .name = "", .ast = ast, .symbol_map = .empty };
}

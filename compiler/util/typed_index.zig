const std = @import("std");

pub fn TypedIndex(comptime BackingInt: type, none_value: BackingInt) type {
    return enum(BackingInt) {
        const Index = @This();

        none = none_value,
        _,

        /// Helper function to cast a generic number to a this index
        pub inline fn cast(x: anytype) Index {
            return switch (@typeInfo(@TypeOf(x))) {
                .null => .none,
                .int, .comptime_int => @enumFromInt(@as(BackingInt, @intCast(x))),
                .optional => if (x) |value| @enumFromInt(@as(BackingInt, @intCast(value))) else .none,
                else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a " ++ @typeName(Index)),
            };
        }
    };
}

//! Provides utility methods for describing sizes in bytes
const std = @import("std");

/// Returns the size of the specified value of kibibits in bytes
pub fn KiBit(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024 / 8)) {
    return value * 1024 / 8;
}
/// Returns the size of the specified value of kibibytes in bytes
pub fn KiB(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024)) {
    return value * 1024;
}

/// Returns the size of the specified value of mibibits in bytes
pub fn MiBit(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024 * 1024 / 8)) {
    return value * 1024 * 1024 / 8;
}
/// Returns the size of the specified value of mibibytes in bytes
pub fn MiB(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024 * 1024)) {
    return value * 1024 * 1024;
}

/// Returns the size of the specified value of gibibits in bytes
pub fn GiBit(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024 * 1024 * 1024 / 8)) {
    return value * 1024 * 1024 * 1024 / 8;
}
/// Returns the size of the specified value of gibibytes in bytes
pub fn GiB(value: anytype) incrementBits(@TypeOf(value), std.math.log2(1024 * 1024 * 1024)) {
    return value * 1024 * 1024 * 1024;
}

fn incrementBits(comptime orig_type: type, comptime bit_increment: comptime_int) type {
    if (orig_type == comptime_int) return comptime_int;

    const info = @typeInfo(orig_type).int;
    std.debug.assert(info.signedness == .unsigned);

    return @Type(.{
        .int = .{
            .signedness = .unsigned,
            .bits = info.bits + bit_increment,
        },
    });
}

//! Represents the built-in module, required by compiler
const std = @import("std");
const Module = @import("Module.zig");

pub fn init(allocator: std.mem.Allocator) !Module {
    return try Module.init(allocator, try allocator.dupeZ(u8, @embedFile("builtin.znasm")), "<builtin>") orelse unreachable;
}

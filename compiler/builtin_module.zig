//! Represents the built-in module, required by compiler
const std = @import("std");
const Module = @import("Module.zig");

pub fn init(allocator: std.mem.Allocator) !Module {
    return try Module.init(allocator, try allocator.dupeZ(u8, @embedFile("builtin.znasm")), "<builtin>") orelse unreachable;
}

// These names MUST match exactly with those in the source file
pub const module_name = "builtin";
pub const empty_vector_name = "empty_vector";

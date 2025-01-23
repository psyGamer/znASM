//! Represents the built-in module, required by compiler
const std = @import("std");
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Module = @import("Module.zig");

pub fn init(allocator: std.mem.Allocator) !Module {
    return try Module.init(allocator, try allocator.dupeZ(u8, @embedFile("builtin.znasm")), "<builtin>") orelse unreachable;
}

pub const module_name = "BuiltIn";
pub const access_type_symbol = module_name ++ SymbolLocation.separator ++ "AccessType";

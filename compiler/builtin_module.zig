//! Represents the built-in module, required by compiler
const std = @import("std");

const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const TypeExpression = Sema.TypeExpression;

pub fn init(allocator: std.mem.Allocator) !Module {
    return try Module.init(allocator, try allocator.dupeZ(u8, @embedFile("builtin.znasm")), "<builtin>") orelse unreachable;
}

const module: Module.Index = .cast(0);
const module_name = "BuiltIn";
const access_type_symbol = "AccessType";
const variable_location_symbol = "VariableLocation";

/// Storage location for local variables
/// This type **must** be kept in sync with the implementation in `builtin.znasm`
pub const VariableLocation = enum(u8) {
    /// Scratch-memory in the zero page
    scratch = 0,

    /// Stack-frame of current function
    stack = 1,

    var cached_type: TypeExpression.Index = .none;

    /// Resolves the "BuiltIn::VariableLocation" type
    pub fn resolveTypeExpr(sema: *Sema) !TypeExpression.Index {
        if (cached_type != .none) {
            return cached_type;
        }

        std.debug.assert(std.mem.eql(u8, module.get(sema).name, module_name));

        const symbol = module.get(sema).symbol_map.get(variable_location_symbol) orelse unreachable;
        try sema.analyzeSymbol(symbol, module, .none);

        cached_type = @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len)));
        try sema.type_expressions.append(sema.allocator, .{ .@"enum" = symbol });
        return cached_type;
    }
};

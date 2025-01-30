//! Represents the built-in module, required by compiler
const std = @import("std");
const Sema = @import("Sema.zig");
const TypeExpression = @import("sema/type_expression.zig");
const Module = @import("Module.zig");

pub fn init(allocator: std.mem.Allocator) !Module {
    return try Module.init(allocator, try allocator.dupeZ(u8, @embedFile("builtin.znasm")), "<builtin>") orelse unreachable;
}

const module_idx: Sema.ModuleIndex = .cast(0);
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

    var type_idx: ?Sema.TypeExpressionIndex = null;

    /// Resolves the "BuiltIn::VariableLocation" type
    pub fn resolveTypeExpr(sema: *Sema) !Sema.TypeExpressionIndex {
        if (type_idx) |idx| {
            return idx;
        }

        const builtin_module = sema.getModule(module_idx);
        std.debug.assert(std.mem.eql(u8, builtin_module.name, module_name));

        const symbol_idx = builtin_module.symbol_map.get(variable_location_symbol) orelse unreachable;
        try sema.analyzeSymbol(symbol_idx, module_idx, .none);

        type_idx = @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len)));
        try sema.type_expressions.append(sema.allocator, .{ .@"enum" = symbol_idx });
        return type_idx.?;
    }
};

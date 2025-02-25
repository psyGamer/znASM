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
const cpu_mode_symbol = "CpuMode";

/// Storage location for local variables
/// This type **must** be kept in sync with the implementation in `builtin.znasm`
pub const VariableLocation = enum(u8) {
    /// Scratch-memory in the zero page
    scratch = 0,

    /// Stack-frame of current function
    stack = 1,

    var cached_type: TypeExpression.Index = .none;

    /// Resolves the "BuiltIn::VariableLocation" type
    pub fn resolveType(sema: *Sema) !TypeExpression.Index {
        if (cached_type != .none) {
            return cached_type;
        }

        std.debug.assert(std.mem.eql(u8, module.get(sema).name, module_name));

        const symbol = module.get(sema).symbol_map.get(variable_location_symbol) orelse unreachable;
        try sema.analyzeSymbol(symbol, module, .none);

        cached_type = try sema.addTypeExpression(.{ .@"enum" = symbol });
        return cached_type;
    }
};

/// CPU 6502 emulation mode
/// This type **must** be kept in sync with the implementation in `builtin.znasm`
pub const CpuMode = enum(u8) {
    /// Allow usage of 65C816 features and disable 6502 emulation
    native = 0,

    /// Disallow usage of 65c816 feature and enable 6502 emulation
    emulation = 1,

    var cached_type: TypeExpression.Index = .none;

    /// Resolves the "BuiltIn::CpuMode" type
    pub fn resolveType(sema: *Sema) !TypeExpression.Index {
        if (cached_type != .none) {
            return cached_type;
        }

        std.debug.assert(std.mem.eql(u8, module.get(sema).name, module_name));

        const symbol = module.get(sema).symbol_map.get(cpu_mode_symbol) orelse unreachable;
        try sema.analyzeSymbol(symbol, module, .none);

        cached_type = try sema.addTypeExpression(.{ .@"enum" = symbol });
        return cached_type;
    }
};

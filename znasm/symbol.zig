const Builder = @import("Builder.zig");

// Referencing Symbol.Function would cause a dependency loop...
// See https://github.com/ziglang/zig/issues/12325
pub const FunctionSym = *const fn (b: *Builder) void;

/// Label to a location in ROM / RAM, being functions, global variables, etc.
pub const Symbol = union(enum) {
    pub const Address = u24;
    pub const Function = *const fn (b: *Builder) void;

    address: Address,
    function: Symbol.Function,
};

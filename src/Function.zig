const Builder = @import("Builder.zig");

/// Unique comptime identifier for a function
pub const Symbol = fn (b: *Builder) void;
/// Unique runtime identifier for a function
pub const SymbolPtr = *const fn (b: *Builder) void;

/// Assembly data for the body of this function
code: []const u8,

/// Offset into the ROM
offset: u24 = undefined,

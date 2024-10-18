const Builder = @import("Builder.zig");

/// Unique comptime identifier for a function
pub const Symbol = fn (b: *Builder) void;
/// Unique runtime identifier for a function
pub const SymbolPtr = *const fn (b: *Builder) void;

/// Assembly data for the body of this function
code: []const u8,

/// Metadata information about the assembly instructions
code_info: []const Builder.InstructionInfo,

/// Offset into the ROM
offset: u24 = undefined,

/// Debug symbol name of this function
symbol_name: ?[]const u8,

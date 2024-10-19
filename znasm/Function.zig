const std = @import("std");
const Builder = @import("Builder.zig");

/// Assembly data for the body of this function
code: []const u8,

/// Metadata information about the assembly instructions
code_info: []const Builder.InstructionInfo,

/// Offset into the ROM
offset: u24 = undefined,

/// Debug symbol name of this function
symbol_name: ?[]const u8,

/// Source location of the function definition
source: ?std.builtin.SourceLocation,

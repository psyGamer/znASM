const std = @import("std");
const Builder = @import("Builder.zig");
const SizeMode = @import("instruction.zig").Instruction.SizeMode;

/// Represents how to properly call a function
pub const CallingConvention = struct {
    start_a_size: SizeMode = .none,
    start_xy_size: SizeMode = .none,
    end_a_size: SizeMode = .none,
    end_xy_size: SizeMode = .none,

    /// Input values for this function
    inputs: []const Builder.CallValue,
    /// Output values from this function
    outputs: []Builder.CallValue,
    /// Values which are modified by this function, potentially leaving them in an invalid state
    clobbers: []const Builder.CallValue,
};

/// Assembly data for the body of this function
code: []const u8,

/// Metadata information about the assembly instructions
code_info: []const Builder.InstructionInfo,

/// Info on how to call this function
call_conv: CallingConvention,

/// Offset into the ROM
offset: u24 = undefined,

/// Debug symbol name of this function
symbol_name: ?[]const u8,

/// Source location of the function definition
source: ?std.builtin.SourceLocation,

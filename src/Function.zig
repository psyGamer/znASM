const Builder = @import("Builder.zig");

/// A symbol is a unique identifier for a function
// pub const Symbol = *const fn (b: Builder) void;
pub const Symbol = fn (b: *Builder) void;

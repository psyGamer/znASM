const std = @import("std");

pub fn compile(config: Config, allocator: std.mem.Allocator, writer: anytype) !void {
    _ = writer; // autofix
    var build_system: BuildSystem = try .init(allocator);

    // Generate vector functions
    try build_system.generate_function(config.vectors.native.cop);
}

// Types
pub const Config = @import("RomConfig.zig");
pub const Rom = @import("SnesRom.zig");

// Util
pub const size = @import("util/size.zig");

// Internal
const BuildSystem = @import("BuildSystem.zig");

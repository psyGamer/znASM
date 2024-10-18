const std = @import("std");

pub fn compile(config: Config, allocator: std.mem.Allocator, writer: anytype, mlb_writer: anytype, cdl_writer: anytype) !void {
    var build_system: BuildSystem = try .init(allocator, config.mode.map);

    // Generate vector functions
    try build_system.generate_function(Config.EmptyVector);
    try build_system.generate_function(config.vectors.native.cop);
    try build_system.generate_function(config.vectors.native.brk);
    try build_system.generate_function(config.vectors.native.nmi);
    try build_system.generate_function(config.vectors.native.irq);
    try build_system.generate_function(config.vectors.emulation.cop);
    try build_system.generate_function(config.vectors.emulation.nmi);
    try build_system.generate_function(config.vectors.emulation.reset);
    try build_system.generate_function(config.vectors.emulation.irqbrk);

    // Setup CODE segment
    var code_segment: Rom.Segment = .{
        .bank = switch (config.mode.map) {
            .lorom => 0x80,
            .hirom => 0xC0,
            .exhirom => 0x40,
        },
        .size = 0,
    };

    for (build_system.functions.values()) |func| {
        code_segment.size += @intCast(func.code.len);
    }

    try code_segment.allocate(allocator);
    defer code_segment.free(allocator);

    for (build_system.functions.values()) |*func| {
        func.offset = @intCast(code_segment.data.items.len);
        code_segment.data.appendSliceAssumeCapacity(func.code);
    }

    // Generate ROM
    const rom: Rom = .{
        .header = try .init(config),
        .vectors = .{
            .native_cop = @truncate(build_system.symbol_location(config.vectors.native.cop)),
            .native_brk = @truncate(build_system.symbol_location(config.vectors.native.brk)),
            .native_abort = @truncate(build_system.symbol_location(Config.EmptyVector)), // Unused
            .native_nmi = @truncate(build_system.symbol_location(config.vectors.native.nmi)),
            .native_irq = @truncate(build_system.symbol_location(config.vectors.native.irq)),
            .emulation_cop = @truncate(build_system.symbol_location(config.vectors.emulation.cop)),
            .emulation_abort = @truncate(build_system.symbol_location(Config.EmptyVector)), // Unused
            .emulation_nmi = @truncate(build_system.symbol_location(config.vectors.emulation.nmi)),
            .emulation_reset = @truncate(build_system.symbol_location(config.vectors.emulation.reset)),
            .emulation_irqbrk = @truncate(build_system.symbol_location(config.vectors.emulation.irqbrk)),
        },
        .segments = &.{code_segment},
    };

    const rom_data = try rom.generate(allocator);
    defer allocator.free(rom_data);

    build_system.resolve_relocations(rom_data);

    try writer.writeAll(rom_data);

    // Write debug data
    if (@TypeOf(mlb_writer) != @TypeOf(null) and @TypeOf(cdl_writer) != @TypeOf(null)) {
        try build_system.write_debug_data(rom_data, mlb_writer, cdl_writer);
    }
}

// Types
pub const Config = @import("RomConfig.zig");
pub const Rom = @import("Rom.zig");
pub const Builder = @import("Builder.zig");

// Util
pub const size = @import("util/size.zig");

// Internal
const BuildSystem = @import("BuildSystem.zig");

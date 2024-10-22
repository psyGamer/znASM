const std = @import("std");
const builtin = @import("builtin");

comptime {
    if (builtin.cpu.arch.endian() != .little) {
        @compileError("Currently, znASM only supports assemblying on little-endian host targets");
    }
}

pub fn compile(config: Config, allocator: std.mem.Allocator, writer: anytype, mlb_writer: anytype, cdl_writer: anytype) !void {
    var build_system: BuildSystem = try .init(allocator, config.mode.map);

    // Generate vector functions
    try build_system.register_symbol(Config.empty_vector);
    try build_system.register_symbol(config.vectors.native.cop);
    try build_system.register_symbol(config.vectors.native.brk);
    try build_system.register_symbol(config.vectors.native.nmi);
    try build_system.register_symbol(config.vectors.native.irq);
    try build_system.register_symbol(config.vectors.emulation.cop);
    try build_system.register_symbol(config.vectors.emulation.nmi);
    try build_system.register_symbol(config.vectors.emulation.reset);
    try build_system.register_symbol(config.vectors.emulation.irqbrk);

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

    // Setup DATA segment
    // TODO: Setup DATA segments for intelligently
    var data_segment: Rom.Segment = .{
        .bank = switch (config.mode.map) {
            .lorom => 0x80,
            .hirom => 0xC0,
            .exhirom => 0x40,
        },
        .size = 0,
    };

    for (build_system.data.values()) |data| {
        data_segment.size += @intCast(data.data.len);
    }

    try data_segment.allocate(allocator);
    defer data_segment.free(allocator);

    for (build_system.data.values()) |*data| {
        data.offset = @intCast(code_segment.size + data_segment.data.items.len);
        data_segment.data.appendSliceAssumeCapacity(data.data);
    }

    // Generate ROM
    const rom: Rom = .{
        .header = try .init(config),
        .vectors = .{
            .native_cop = @truncate(build_system.symbol_location(.{ .function = config.vectors.native.cop })),
            .native_brk = @truncate(build_system.symbol_location(.{ .function = config.vectors.native.brk })),
            .native_abort = @truncate(build_system.symbol_location(.{ .function = Config.empty_vector })), // Unused
            .native_nmi = @truncate(build_system.symbol_location(.{ .function = config.vectors.native.nmi })),
            .native_irq = @truncate(build_system.symbol_location(.{ .function = config.vectors.native.irq })),
            .emulation_cop = @truncate(build_system.symbol_location(.{ .function = config.vectors.emulation.cop })),
            .emulation_abort = @truncate(build_system.symbol_location(.{ .function = Config.empty_vector })), // Unused
            .emulation_nmi = @truncate(build_system.symbol_location(.{ .function = config.vectors.emulation.nmi })),
            .emulation_reset = @truncate(build_system.symbol_location(.{ .function = config.vectors.emulation.reset })),
            .emulation_irqbrk = @truncate(build_system.symbol_location(.{ .function = config.vectors.emulation.irqbrk })),
        },
        .segments = &.{ code_segment, data_segment },
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

// Symbols
const symbol = @import("symbol.zig");
pub const Address = symbol.Symbol.Address;
pub const Data = symbol.DataSymbol;

// Util
pub const size = @import("util/size.zig");

// Internal
const BuildSystem = @import("BuildSystem.zig");

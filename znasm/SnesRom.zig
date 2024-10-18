//! Contains all the data of the ROM which will be written to a file
const std = @import("std");
const Config = @import("RomConfig.zig");

pub const Header = extern struct {
    // The header needs to be exactly 32 bytes large
    comptime {
        std.debug.assert(@sizeOf(@This()) == 32);
    }

    pub const Mode = packed struct(u8) {
        pub const Speed = enum(u1) { slow = 0, fast = 1 };
        pub const Map = enum(u4) { lorom = 0, hirom = 1, exhirom = 5 };

        map: Map,
        speed: Speed,
        _: u3 = 0b001,
    };
    pub const Chipset = packed struct(u8) {
        pub const Components = enum(u4) {
            rom = 0x0,
            rom_ram = 0x1,
            rom_ram_battery = 0x2,
            rom_coprocessor = 0x3,
            rom_coprocessor_ram = 0x4,
            rom_coprocessor_ram_battery = 0x5,
            rom_coprocessor_battery = 0x6,
        };
        pub const Coprocessor = enum(u4) {
            /// No coprocessor
            pub const none: @This() = @enumFromInt(0x0);

            dsp = 0x0,
            gsu = 0x1, // SuperFX
            obc1 = 0x2,
            sa_1 = 0x3,
            s_dd1 = 0x4,
            s_rtc = 0x5,
            other = 0xE, // Super Game Boy / Satellaview
            custom = 0xF, // specified with $FFBF
        };

        components: Components,
        coprocessor: Coprocessor = Coprocessor.none,
    };
    pub const Country = enum(u8) {
        /// Uses NTSC 60 FPS
        japan = 0,
        /// Uses NTSC 60 FPS
        usa = 1,
        /// Uses PAL 50 FPS
        europe = 2,
    };

    title: [21]u8,
    mode: Mode,
    chipset: Chipset,
    rom_size_log2_kb: u8,
    ram_size_log2_kb: u8,
    country: Country,
    _developer_id: u8 = 0, // unsupported
    rom_version: u8,

    checksum: u16 = 0x0000,
    checksum_complement: u16 = 0xFFFF,
};

pub const InterruptVectors = extern struct {
    // The vector table needs to be exactly 32 bytes large
    comptime {
        std.debug.assert(@sizeOf(@This()) == 32);
    }

    _native_pad: u32 = 0,
    native_cop: u16,
    native_brk: u16,
    native_abort: u16,
    native_nmi: u16,
    _native_none: u16 = 0,
    native_irq: u16,

    _emulation_pad: u32 = 0,
    emulation_cop: u16,
    _emulation_none: u16 = 0,
    emulation_abort: u16,
    emulation_nmi: u16,
    emulation_reset: u16,
    emulation_irqbrk: u16,
};

pub const Segment = struct {
    pub const Mode = enum { rom, ram };

    name: []const u8,
    start: u24,
    size: u16,
    mode: Mode,
};

const Self = @This();

header: Header,
vectors: InterruptVectors,

segments: []const Segment = &.{},
segment_data: []std.ArrayListUnmanaged(u8) = &.{},

pub fn init(config: Config) !Self {
    var rom: Self = .{
        .header = undefined,
        .vectors = .{
            .native_cop = 0,
            .native_brk = 0,
            .native_abort = 0,
            .native_nmi = 0,
            .native_irq = 0,

            .emulation_cop = 0,
            .emulation_abort = 0,
            .emulation_nmi = 0,
            .emulation_reset = 0,
            .emulation_irqbrk = 0,
        },
    };

    // Generate header
    if (config.title.len > rom.header.title.len) {
        std.log.err("ROM title is too long! Expected {} characters at most, found {}", .{ rom.header.title.len, config.title.len });
        return error.InvalidConfig;
    }
    // Fill rest with spaces
    @memset(&rom.header.title, ' ');
    @memcpy(rom.header.title[0..config.title.len], config.title);

    rom.header.mode = config.mode;
    rom.header.chipset = config.chipset;
    rom.header.country = config.country;
    rom.header.rom_version = config.version;

    rom.header.rom_size_log2_kb = @intCast(std.math.log2(config.rom_size / 1024));
    rom.header.ram_size_log2_kb = @intCast(if (config.ram_size == 0) 0 else std.math.log2(config.ram_size / 1024));

    return rom;
}

pub fn allocateSegments(self: *Self, allocator: std.mem.Allocator, segments: []const Segment) !void {
    self.segments = segments;
    self.segment_data = try allocator.alloc(std.ArrayListUnmanaged(u8), segments.len);

    for (segments, self.segment_data) |seg, *seg_data| {
        seg_data.* = try std.ArrayListUnmanaged(u8).initCapacity(allocator, seg.size);
        // Initialize with zeros
        @memset(seg_data.items[0..seg.size], 0x00);
    }
}

pub fn findSegment(self: Self, segment_name: []const u8) ?struct { Segment, *std.ArrayListUnmanaged(u8) } {
    for (self.segments, self.segment_data) |seg, *seg_data| {
        if (std.mem.eql(u8, seg.name, segment_name)) {
            return .{ seg, seg_data };
        }
    }
    return null;
}

pub fn generate(self: Self, allocator: std.mem.Allocator) ![]u8 {
    if (self.header.rom_size_log2_kb < 5) {
        std.log.err("Cannot generate ROM without at least 1 bank (32KiB). Found {}", .{std.fmt.fmtIntSizeBin(@as(usize, @intCast(1024)) << @intCast(self.header.rom_size_log2_kb))});
        return error.InvalidROM;
    }

    // Write header data
    std.log.info("== ROM Header ==", .{});
    std.log.info("  Title: '{s}'", .{self.header.title});
    std.log.info("  Mode:", .{});
    switch (self.header.mode.map) {
        .lorom => std.log.info("    Map: LoROM", .{}),
        .hirom => std.log.info("    Map: HiROM", .{}),
        .exhirom => std.log.info("    Map: ExHiROM", .{}),
    }
    switch (self.header.mode.speed) {
        .slow => std.log.info("    Speed: Slow (2.68MHz)", .{}),
        .fast => std.log.info("    Speed: Fast (3.58MHz)", .{}),
    }
    std.log.info("  ROM size: {}", .{std.fmt.fmtIntSizeBin(@as(usize, @intCast(1024)) << @intCast(self.header.rom_size_log2_kb))});
    // RAM size of 0 = No RAM
    std.log.info("  RAM size: {}", .{std.fmt.fmtIntSizeBin(if (self.header.ram_size_log2_kb == 0) 0 else @as(usize, @intCast(1024)) << @intCast(self.header.ram_size_log2_kb))});

    const rom_size = @as(usize, @intCast(1024)) << @intCast(self.header.rom_size_log2_kb);
    const rom_data = try allocator.alloc(u8, rom_size);

    @memset(rom_data, 0x00);

    var fbs = std.io.fixedBufferStream(rom_data);
    var writer = fbs.writer();

    // Write segments
    for (self.segments, self.segment_data) |seg, seg_data| {
        // Only write ROM segments
        if (seg.mode != .rom) {
            continue;
        }

        try writer.writeAll(seg_data.items[0..seg.size]);
    }

    // Different header location based on mapping mode
    const header_location: u24 = switch (self.header.mode.map) {
        .lorom => 0x007FC0,
        .hirom => 0x00FFC0,
        .exhirom => 0x40FFc0,
    };
    fbs.pos = header_location;

    // Write header + interrupt vectors
    try writer.writeStruct(self.header);
    try writer.writeStruct(self.vectors);

    // Compute checksum
    var checksum: u16 = 0;
    for (rom_data) |byte| {
        checksum +%= byte;
    }
    const checksum_complement = checksum ^ 0xFFFF;

    fbs.pos = header_location + @offsetOf(Header, "checksum");
    try writer.writeInt(u16, checksum, .little);
    try writer.writeInt(u16, checksum_complement, .little);

    std.log.info("  Checksum: 0x{x:0>4} | 0x{x:0>4}", .{ checksum, checksum_complement });

    return rom_data;
}

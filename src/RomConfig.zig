const rom = @import("SnesRom.zig");

title: []const u8,
mode: rom.Header.Mode,
chipset: rom.Header.Chipset,
country: rom.Header.Country,
rom_size: u24,
ram_size: u24,
version: u8,

vectors: struct {
    native: struct {
        cop: ?[]const u8 = null,
        brk: ?[]const u8 = null,
        nmi: ?[]const u8 = null,
        irq: ?[]const u8 = null,
    },
    emulation: struct {
        cop: ?[]const u8 = null,
        nmi: ?[]const u8 = null,
        reset: ?[]const u8 = null,
        irqbrl: ?[]const u8 = null,
    },
},

segments: []const rom.Segment,

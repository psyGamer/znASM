//! Handles various calculations related to memory mapping
const std = @import("std");
const MappingMode = @import("Rom.zig").Header.Mode.Map;

/// Gets the size of banks for the mapping mode
pub fn bankSize(mode: MappingMode) u16 {
    return switch (mode) {
        .lorom => 0x8000,
        .hirom, .exhirom => 0xFFFF,
    };
}

/// Returns the non-mirrored memory-mapped target bank
pub fn getRealBank(mode: MappingMode, bank: u8) u8 {
    return switch (mode) {
        .lorom => switch (bank) {
            0x00...0x7D => bank + 0x80,
            0x7E...0x7F => unreachable,
            0x80...0xFF => bank,
        },
        .hirom => switch (bank) {
            0x00...0x3F => bank + (0xC0 - 0x00),
            0x40...0x7F => unreachable,
            0x80...0xBF => bank + (0xC0 - 0x80),
            0xC0...0xFF => bank,
        },
        .exhirom => switch (bank) {
            0x00...0x3D => bank + (0x40 - 0x00),
            0x3E...0x7D => bank,
            0x7E...0x7F => unreachable,
            0x80...0xBF => bank + (0xC0 - 0x80),
            0xC0...0xFF => bank,
        },
    };
}

/// Maps a bank with the offset from the first accessible byte to the actual address
pub fn bankOffsetToAddr(mode: MappingMode, bank: u8, offset: u16) u24 {
    return switch (mode) {
        .lorom => @as(u24, bank) << 16 | (offset + 0x8000),
        .hirom => @as(u24, bank) << 16 | offset,
        .exhirom => if (bank >= 0x40)
            @as(u24, bank) << 16 | offset
        else
            @as(u24, bank) << 16 | (offset + 0x8000),
    };
}

/// Gets the ROM-offset for the first address of a bank
/// The bank must be non-mirrored
pub fn bankToRomOffset(mode: MappingMode, bank: u8) u24 {
    switch (mode) {
        .lorom => {
            std.debug.assert(bank >= 0x80 and bank <= 0xFF);
            return (@as(u24, bank) - 0x80) * 0x8000;
        },
        .hirom => {
            std.debug.assert(bank >= 0xC0 and bank <= 0xFF);
            return (@as(u24, bank) - 0xC0) * 0xFFFF;
        },
        .exhirom => {
            std.debug.assert((bank >= 0x3E and bank <= 0x7D) or (bank >= 0xC0 and bank <= 0xFF));
            if (bank >= 0xC0 and bank <= 0xFF) {
                return (@as(u24, bank) - 0xC0) * 0xFFFF;
            } else if (@as(u24, bank) >= 0x40 and bank <= 0x7D) {
                return (@as(u24, bank) - 0x40) * 0xFFFF + 0x400000;
            } else {
                return (@as(u24, bank) - 0x3E) * 0xFFFF + 0x400000 + 0x3E0000;
            }
        },
    }
}

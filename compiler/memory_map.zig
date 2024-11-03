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

/// Returns the non-mirrored memory-mapped target bank for a ROM address
pub fn getRealRomBank(mode: MappingMode, bank: u8) u8 {
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
/// Returns the non-mirrored memory-mapped target bank for a ROM address
/// If the source bank is not mapped, returns `null`
pub fn mapRealRomBank(mode: MappingMode, bank: u8) ?u8 {
    return switch (mode) {
        .lorom => switch (bank) {
            0x00...0x7D => bank + 0x80,
            0x7E...0x7F => null,
            0x80...0xFF => bank,
        },
        .hirom => switch (bank) {
            0x00...0x3F => bank + (0xC0 - 0x00),
            0x40...0x7F => null,
            0x80...0xBF => bank + (0xC0 - 0x80),
            0xC0...0xFF => bank,
        },
        .exhirom => switch (bank) {
            0x00...0x3D => bank + (0x40 - 0x00),
            0x3E...0x7D => bank,
            0x7E...0x7F => null,
            0x80...0xBF => bank + (0xC0 - 0x80),
            0xC0...0xFF => bank,
        },
    };
}

/// Returns the non-mirrored memory-mapped target bank for a RAM address
pub fn getRealRamBank(bank: u8) u8 {
    return switch (bank) {
        0x00...0x3F => 0x7E,
        0x40...0x7D => unreachable,
        0x7E...0x7F => bank,
        0x80...0xBF => 0x7E,
        0xC0...0xFF => unreachable,
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

/// Maps an offset into Work-RAM to the actual address
pub fn wramOffsetToAddr(wram_offset: u17) u24 {
    return 0x7E0000 + @as(u24, wram_offset);
}

/// Checks if the target address is accessible in the specified bank
pub fn isAddressAccessibleInBank(mapping_mode: MappingMode, target_addr: u24, bank: u8) bool {
    const target_bank: u8 = @intCast(target_addr >> 16);
    const target_addr16: u16 = @truncate(target_addr);

    if (target_bank == bank) {
        return true;
    }

    return switch (target_bank) {
        0x00...0x3F, 0x80...0xBF => switch (target_addr16) {
            0x0000...0x1FFF => switch (bank) {
                0x00...0x3F, 0x7E, 0x80...0xBF => true,
                else => false,
            },
            0x2000...0x5FFF => switch (bank) {
                0x00...0x3F, 0x80...0xBF => true,
                else => false,
            },
            0x8000...0xFFFF => switch (mapping_mode) {
                .lorom => target_bank + 0x80 == bank,
                .hirom => target_bank + 0x80 == bank or target_bank + 0xC0 == bank,
                .exhirom => target_bank + 0x40 == bank and target_bank != 0x3E and target_bank != 0x3F,
            },
            else => false,
        },
        0x40...0x7D => switch (target_addr16) {
            0x0000...0xFFFF => switch (mapping_mode) {
                .lorom => target_bank + 0x80 == bank and target_addr16 >= 0x8000,
                .hirom => false,
                .exhirom => target_bank - 0x40 == bank,
            },
        },
        0x7E => switch (target_addr16) {
            0x0000...0x1FFF => switch (bank) {
                0x00...0x3F, 0x80...0xBF => true,
                else => false,
            },
            else => false,
        },
        0x7F => false,
        0xC0...0xFF => switch (target_addr16) {
            0x0000...0xFFFF => switch (mapping_mode) {
                .lorom => target_bank - 0x80 == bank and target_addr16 >= 0x8000,
                .hirom => target_bank - 0x40 == bank or target_bank - 0xC0 == bank,
                .exhirom => target_bank - 0x40 == bank,
            },
        },
    };
}

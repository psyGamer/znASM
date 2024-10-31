//! Handles various calculations related to memory mapping
const MappingMode = @import("Rom.zig").Header.Mode.Map;

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

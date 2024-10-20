const znasm = @import("znasm");
const Builder = znasm.Builder;

const reg = @import("reg.zig");

pub const InterruptConfig = packed struct(u8) {
    joypad_autoread: bool,
    _pad0: u3 = 0,
    screen_interrupt: enum(u2) {
        disabled = 0b00,
        h_eql = 0b01,
        v_eql_h_zero = 0b10,
        v_eql_h_eql = 0b11,
    },
    _pad1: u1 = 0,
    vblank_interrupt: bool,
};
pub fn set_interrupt_config(b: *Builder, config: InterruptConfig) void {
    b.store_value(.@"8bit", reg.NMITIMEN, @bitCast(config));
}

pub const RomSpeed = enum(u8) {
    /// 2.68 MHz ROM access cycle (8 cycles)
    slow = 0x00,
    /// 3.58 MHz ROM access cycle (6 cycles)
    fast = 0x01,
};
pub fn set_rom_access_speed(b: *Builder, speed: RomSpeed) void {
    b.store_value(.@"8bit", reg.MEMSEL, @intFromEnum(speed));
}

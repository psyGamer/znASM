const znasm = @import("znasm");
const Builder = znasm.Builder;

// PPU Registers

pub const INIDISP: znasm.FixedAddress = .init(0x2100, null);

pub const ScreenConfig = packed struct(u8) {
    pub const force_blank = .{ .brightness = 0, .f_blank = true };

    brightness: u4,
    _: u3 = 0,
    f_blank: bool,
};
pub fn set_screen_config(b: *Builder, config: ScreenConfig) void {
    const value: u8 = @bitCast(config);
    if (value == 0) {
        b.store_zero(INIDISP);
    } else {
        var a = b.reg_a8();
        a = .load_store(b, INIDISP, value);
    }
}

// Memory Mapped I/O Registers

pub const NMITIMEN: znasm.FixedAddress = .init(0x4200, null);

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
    const value: u8 = @bitCast(config);
    if (value == 0) {
        b.store_zero(NMITIMEN);
    } else {
        var a = b.reg_a8();
        a = .load_store(b, NMITIMEN, value);
    }
}

pub const HDMAEN: znasm.FixedAddress = .init(0x420C, null);

pub const HdmaEnabledConfig = packed struct(u8) {
    pub const disable_all: HdmaEnabledConfig = .{};
    pub const enable_0: HdmaEnabledConfig = .{ .channel_0 = true };
    pub const enable_1: HdmaEnabledConfig = .{ .channel_1 = true };
    pub const enable_2: HdmaEnabledConfig = .{ .channel_2 = true };
    pub const enable_3: HdmaEnabledConfig = .{ .channel_3 = true };
    pub const enable_4: HdmaEnabledConfig = .{ .channel_4 = true };
    pub const enable_5: HdmaEnabledConfig = .{ .channel_5 = true };
    pub const enable_6: HdmaEnabledConfig = .{ .channel_6 = true };
    pub const enable_7: HdmaEnabledConfig = .{ .channel_7 = true };

    channel_0: bool = false,
    channel_1: bool = false,
    channel_2: bool = false,
    channel_3: bool = false,
    channel_4: bool = false,
    channel_5: bool = false,
    channel_6: bool = false,
    channel_7: bool = false,
};
pub fn set_hdma_enabled(b: *Builder, config: HdmaEnabledConfig) void {
    const value: u8 = @bitCast(config);
    if (value == 0) {
        b.store_zero(HDMAEN);
    } else {
        var a = b.reg_a8();
        a = .load_store(b, HDMAEN, value);
    }
}

pub const MEMSEL: znasm.FixedAddress = .init(0x420D, null);

pub const RomSpeed = enum(u8) {
    /// 2.68 MHz ROM access cycle (8 cycles)
    slow = 0x00,
    /// 3.58 MHz ROM access cycle (6 cycles)
    fast = 0x01,
};
pub fn set_rom_access_speed(b: *Builder, speed: RomSpeed) void {
    const value: u8 = @intFromEnum(speed);
    if (value == 0) {
        b.store_zero(HDMAEN);
    } else {
        var a = b.reg_a8();
        a = .load_store(b, HDMAEN, value);
    }
}

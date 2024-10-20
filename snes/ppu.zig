const znasm = @import("znasm");
const Builder = znasm.Builder;

const reg = @import("reg.zig");

pub const screen_width = 256;
pub const screen_height = 224;
pub const screen_width_tiles = 32;
pub const screen_height_tiles = 28;

pub const ScreenConfig = packed struct(u8) {
    pub const force_blank = .{ .brightness = 0, .f_blank = true };

    brightness: u4,
    _: u3 = 0,
    f_blank: bool,
};
pub fn set_screen(b: *Builder, config: ScreenConfig) void {
    b.store_value(.@"8bit", reg.INIDISP, @bitCast(config));
}

const znasm = @import("znasm");
const Builder = znasm.Builder;

const reg = @import("reg.zig");

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
    b.store_value(.@"8bit", reg.HDMAEN, @bitCast(config));
}

pub fn workram_dma(b: *Builder) void {
    _ = b; // autofix
}

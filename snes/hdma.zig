const znasm = @import("znasm");
const Builder = znasm.Builder;
const Addr = znasm.Address;

const reg = @import("reg.zig");

/// Controls which HDMA channel are currently enabled
pub const HDMA_ENABLE: Addr = reg.HDMAEN;

pub const ChannelConfig = packed struct(u8) {
    pub const disable_all: ChannelConfig = .{};
    pub const enable_0: ChannelConfig = .{ .channel_0 = true };
    pub const enable_1: ChannelConfig = .{ .channel_1 = true };
    pub const enable_2: ChannelConfig = .{ .channel_2 = true };
    pub const enable_3: ChannelConfig = .{ .channel_3 = true };
    pub const enable_4: ChannelConfig = .{ .channel_4 = true };
    pub const enable_5: ChannelConfig = .{ .channel_5 = true };
    pub const enable_6: ChannelConfig = .{ .channel_6 = true };
    pub const enable_7: ChannelConfig = .{ .channel_7 = true };

    channel_0: bool = false,
    channel_1: bool = false,
    channel_2: bool = false,
    channel_3: bool = false,
    channel_4: bool = false,
    channel_5: bool = false,
    channel_6: bool = false,
    channel_7: bool = false,
};

/// Enalbes/Disables HDMA for the specified channels
pub fn set_enabled(b: *Builder, register: Builder.Register, config: ChannelConfig) void {
    b.store_value(.@"8bit", register, HDMA_ENABLE, @bitCast(config));
}

x_pos: u16,
y_pos: u16,
x_spd: u16,
y_spd: u16,

const Player = @This();

pub var player: znasm.Global(Player) = undefined;

pub fn init(b: *znasm.Builder) void {
    player.write_x_pos(b, 69);
    player.write_y_pos(b, 420);

    b.call(update, .{player.read_x_pos(b)});
}

pub fn update(b: *znasm.Builder, times: znasm.Param(u16)) void {
    b.emitNop();

    b.e
}

const Symbol = @import("symbol.zig").Symbol;
const Register = @This();

addr: u16,

pub fn init(addr: u16) @This() {
    return .{
        .addr = addr,
    };
}

pub fn symbol(reg: Register) Symbol {
    return .{ .register = reg.addr };
}

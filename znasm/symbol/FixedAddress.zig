//! A fixed address to something in ROM / RAM
const Symbol = @import("../symbol.zig").Symbol;
const Register = @This();

addr: u16,
bank: ?u8,

pub fn init(addr: u16, bank: ?u8) @This() {
    return .{
        .addr = addr,
        .bank = bank,
    };
}

pub fn symbol(reg: Register) Symbol {
    return .{
        .register = @as(u24, (reg.bank orelse 0x00)) << 16 | reg.addr,
    };
}

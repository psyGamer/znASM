//! Represents the Y register
const std = @import("std");
const Builder = @import("../Builder.zig");
const IndexingMode = @import("../instruction.zig").Instruction.IndexingMode;
const Register = @import("../Register.zig");

const RegY = @This();

builder: *Builder,

/// Used to check if the register is in an undefined state
id: u64,

pub fn load(b: *Builder, value: anytype) RegY {
    std.debug.assert(b.xy_indexing != .none);

    if (@TypeOf(value) == u8 or value >= 0 and value <= std.math.maxInt(u8)) {
        if (b.xy_indexing != .@"8bit") {
            std.debug.panic("Trying to load 8-bit value while in 16-bit mode", .{});
        }

        b.emit(.{ .ldy = .{ .imm8 = value } });
    } else if (@TypeOf(value) == u16 or value >= 0 and value <= std.math.maxInt(u16)) {
        if (b.xy_indexing != .@"16bit") {
            std.debug.panic("Trying to load 16-bit value while in 8-bit mode", .{});
        }

        b.emit(.{ .ldy = .{ .imm16 = value } });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported value type '{s}'", .{@typeName(@TypeOf(value))}));
    }

    b.reg_y = .{
        .builder = b,
        .id = b.register_id(),
    };
    return b.reg_y.?;
}

pub fn load_store(b: *Builder, target: anytype, value: anytype) RegY {
    const reg = load(b, value);
    reg.store(target);
    return reg;
}

pub fn store(reg: RegY, target: anytype) void {
    if (reg.id != reg.builder.reg_y.?.id) {
        @panic("Y Register is in an undefined state");
    }

    if (@TypeOf(target) == Register) {
        // TODO: Ensure target is in the same bank
        reg.builder.emit_extra(.{ .sty_addr16 = undefined }, .{
            .type = .addr16,
            .target_sym = target.symbol(),
            .target_offset = 0,
        });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported target address'{s}'", .{@typeName(@TypeOf(target))}));
    }
}

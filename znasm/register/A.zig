//! Represents the A register
const std = @import("std");
const Builder = @import("../Builder.zig");
const IndexingMode = @import("../instruction.zig").Instruction.IndexingMode;
const Register = @import("../Register.zig");

const RegA = @This();

mode: IndexingMode,
builder: *Builder,

/// Used to check if the register is in an undefined state
id: u64,

pub fn load(b: *Builder, value: anytype) RegA {
    const prev_reg = b.reg_a orelse @panic("A Register wasn't defined before loading");

    if (@TypeOf(value) == u8 or value >= 0 and value <= std.math.maxInt(u8)) {
        if (prev_reg.mode != .@"8bit") {
            std.debug.panic("Trying to load 8-bit value while in 16-bit mode", .{});
        }

        b.emit(.{ .lda = .{ .imm8 = value } });
    } else if (@TypeOf(value) == u16 or value >= 0 and value <= std.math.maxInt(u16)) {
        if (prev_reg.mode != .@"16bit") {
            std.debug.panic("Trying to load 16-bit value while in 8-bit mode", .{});
        }

        b.emit(.{ .lda = .{ .imm16 = value } });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported value type '{s}'", .{@typeName(@TypeOf(value))}));
    }

    b.reg_a = .{
        .mode = prev_reg.mode,
        .builder = prev_reg.builder,
        .id = b.register_id(),
    };
    return b.reg_a.?;
}

pub fn load_store(b: *Builder, target: anytype, value: anytype) RegA {
    const reg = load(b, value);
    reg.store(target);
    return reg;
}

pub fn store(reg: RegA, target: anytype) void {
    if (reg.id != reg.builder.reg_a.?.id) {
        @panic("A Register is in an undefined state");
    }

    if (@TypeOf(target) == Register) {
        // TODO: Ensure target is in the same bank
        reg.builder.emit_extra(.{ .sta_addr16 = undefined }, .{
            .type = .addr16,
            .target_sym = target.symbol(),
            .target_offset = 0,
        });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported target address'{s}'", .{@typeName(@TypeOf(target))}));
    }
}

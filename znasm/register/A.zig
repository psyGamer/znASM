//! Represents the A register
const std = @import("std");
const Builder = @import("../Builder.zig");
const IndexingMode = @import("../instruction.zig").Instruction.IndexingMode;

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

    // TODO: Wrap registers / memory address into some custom type
    if (@TypeOf(target) == u8 or target >= 0 and target <= std.math.maxInt(u8)) {
        reg.builder.emit(.{ .sta_addr8 = target });
    } else if (@TypeOf(target) == u16 or target >= 0 and target <= std.math.maxInt(u16)) {
        reg.builder.emit(.{ .sta_addr16 = target });
    } else if (@TypeOf(target) == u24 or target >= 0 and target <= std.math.maxInt(u24)) {
        reg.builder.emit(.{ .sta_addr24 = target });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported target address'{s}'", .{@typeName(@TypeOf(target))}));
    }
}

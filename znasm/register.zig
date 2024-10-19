//! General representation for the A / X / Y registers
const std = @import("std");
const Builder = @import("Builder.zig");
const SizeMode = @import("instruction.zig").Instruction.SizeMode;
const FixedAddress = @import("symbol/FixedAddress.zig");

var register_id_prng: std.Random.DefaultPrng = .init(0x00);

pub fn Register(comptime reg_type: enum { a, x, y }) type {
    return struct {
        const Reg = @This();
        const name = switch (reg_type) {
            .a => "A Accumulator Register",
            .x => "X Index Register",
            .y => "Y Index Register",
        };

        builder: *Builder,

        /// Used to check if the register is in an undefined state
        id: u64,

        /// Creates a new register state, invalidating the previous one
        pub fn next(b: *Builder) Reg {
            const id = register_id_prng.next();

            switch (reg_type) {
                .a => b.a_reg_id = id,
                .x => b.x_reg_id = id,
                .y => b.y_reg_id = id,
            }

            return .{
                .builder = b,
                .id = id,
            };
        }

        /// Loads the value into the register
        pub fn load(b: *Builder, value: anytype) Reg {
            if (reg_type == .a) {
                std.debug.assert(b.a_size != .none);
            } else {
                std.debug.assert(b.xy_size != .none);
            }

            const size_mode = switch (reg_type) {
                .a => b.a_size,
                .x, .y => b.xy_size,
            };

            if (@TypeOf(value) == u8 or value >= 0 and value <= std.math.maxInt(u8)) {
                if (size_mode != .@"8bit") {
                    std.debug.panic("Trying to load 8-bit value while in 16-bit mode", .{});
                }

                b.emit(switch (reg_type) {
                    .a => .{ .lda = .{ .imm8 = value } },
                    .x => .{ .ldx = .{ .imm8 = value } },
                    .y => .{ .ldy = .{ .imm8 = value } },
                });
            } else if (@TypeOf(value) == u16 or value >= 0 and value <= std.math.maxInt(u16)) {
                if (size_mode != .@"16bit") {
                    std.debug.panic("Trying to load 16-bit value while in 8-bit mode", .{});
                }

                b.emit(switch (reg_type) {
                    .a => .{ .lda = .{ .imm16 = value } },
                    .x => .{ .ldx = .{ .imm16 = value } },
                    .y => .{ .ldy = .{ .imm16 = value } },
                });
            } else {
                @compileError(std.fmt.comptimePrint("Unsupported value type '{s}'", .{@typeName(@TypeOf(value))}));
            }

            return .next(b);
        }

        /// Stores the current value into the target
        pub fn store(reg: Reg, target: anytype) void {
            reg.ensure_valid();

            if (@TypeOf(target) == FixedAddress) {
                // TODO: Ensure target is in the same bank
                reg.builder.emit_extra(switch (reg_type) {
                    .a => .{ .sta_addr16 = undefined },
                    .x => .{ .stx_addr16 = undefined },
                    .y => .{ .sty_addr16 = undefined },
                }, .{
                    .type = .addr16,
                    .target_sym = target.symbol(),
                    .target_offset = 0,
                });
            } else {
                @compileError(std.fmt.comptimePrint("Unsupported target address'{s}'", .{@typeName(@TypeOf(target))}));
            }
        }

        /// Combines a load and a store
        pub fn load_store(b: *Builder, target: anytype, value: anytype) Reg {
            const reg = load(b, value);
            reg.store(target);
            return reg;
        }

        /// Ensures the register doesn't contained undefiend data
        fn ensure_valid(reg: Reg) void {
            const curr_id = switch (reg_type) {
                .a => reg.builder.a_reg_id,
                .x => reg.builder.x_reg_id,
                .y => reg.builder.y_reg_id,
            };
            if (reg.id != curr_id) {
                @panic(std.fmt.comptimePrint("{s} is in an undefiend state", .{name}));
            }
        }

        // Shorthands for clearing meaning in the calling code
        pub const a8 = if (reg_type == .a) set_8bit else @compileError(std.fmt.comptimePrint("Cannot use function a8 with {s}", .{name}));
        pub const a16 = if (reg_type == .a) set_16bit else @compileError(std.fmt.comptimePrint("Cannot use function a16 with {s}", .{name}));
        pub const x8 = if (reg_type == .x) set_8bit else @compileError(std.fmt.comptimePrint("Cannot use function x8 with {s}", .{name}));
        pub const x16 = if (reg_type == .x) set_16bit else @compileError(std.fmt.comptimePrint("Cannot use function x16 with {s}", .{name}));
        pub const y8 = if (reg_type == .y) set_8bit else @compileError(std.fmt.comptimePrint("Cannot use function y8 with {s}", .{name}));
        pub const y16 = if (reg_type == .y) set_16bit else @compileError(std.fmt.comptimePrint("Cannot use function y16 with {s}", .{name}));

        fn set_8bit(b: *Builder) Reg {
            return change_size(b, .@"8bit");
        }
        fn set_16bit(b: *Builder) Reg {
            return change_size(b, .@"16bit");
        }

        fn change_size(b: *Builder, size: SizeMode) Reg {
            const curr_size = switch (reg_type) {
                .a => b.a_size,
                .x, .y => b.xy_size,
            };

            if (curr_size == size) {
                // Nothing to do
                return .{
                    .builder = b,
                    .id = switch (reg_type) {
                        .a => b.a_reg_id,
                        .x => b.x_reg_id,
                        .y => b.y_reg_id,
                    } orelse @panic(std.fmt.comptimePrint("{s} is uninitialized", .{name})),
                };
            }

            b.change_status_flags(switch (reg_type) {
                .a => .{ .a_8bit = size == .@"8bit" },
                .x, .y => .{ .xy_8bit = size == .@"8bit" },
            });

            return .next(b);
        }
    };
}

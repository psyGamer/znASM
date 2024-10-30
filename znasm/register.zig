//! General representation for the A / X / Y registers

const std = @import("std");
const Builder = @import("Builder.zig");
const Symbol = @import("symbol.zig").Symbol;
const SizeMode = @import("instruction.zig").Instruction.SizeMode;

pub const RegA = Register(.a);
pub const RegX = Register(.x);
pub const RegY = Register(.y);

var register_id_prng: std.Random.DefaultPrng = .init(0x00);

fn Register(comptime reg_type: enum { a, x, y }) type {
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
            const size_mode = switch (reg_type) {
                .a => b.a_size,
                .x, .y => b.xy_size,
            };

            switch (size_mode) {
                .none => @panic("Cannot load without a defined register size"),
                .@"8bit" => {
                    if (value < std.math.minInt(u8) or value > std.math.maxInt(u8)) {
                        @panic("Trying to load 16-bit value while in 8-bit mode");
                    }

                    b.emit(switch (reg_type) {
                        .a => .{ .lda = .{ .imm8 = @intCast(value) } },
                        .x => .{ .ldx = .{ .imm8 = @intCast(value) } },
                        .y => .{ .ldy = .{ .imm8 = @intCast(value) } },
                    });
                },
                .@"16bit" => {
                    b.emit(switch (reg_type) {
                        .a => .{ .lda = .{ .imm16 = value } },
                        .x => .{ .ldx = .{ .imm16 = value } },
                        .y => .{ .ldy = .{ .imm16 = value } },
                    });
                },
            }

            b.clobbers.put(b.build_system.allocator, switch (reg_type) {
                .a => .a,
                .x => .x,
                .y => .y,
            }, {}) catch @panic("Out of memory");

            return .next(b);
        }

        /// Loads the value of the relocation into the register
        pub fn load_reloc(b: *Builder, reloc: Builder.InstructionInfo.Relocation) Reg {
            if (reg_type == .a) {
                std.debug.assert(b.a_size != .none);
            } else {
                std.debug.assert(b.xy_size != .none);
            }

            const size_mode = switch (reg_type) {
                .a => b.a_size,
                .x, .y => b.xy_size,
            };

            const reloc_size: Builder.AddrSize = switch (reloc.type) {
                .imm8, .rel8, .addr_l, .addr_h, .addr_bank => .@"8bit",
                .imm16, .addr16 => .@"16bit",
                .addr24 => @panic("Cannot load 24-bit value into register"),
            };

            if (reloc_size == .@"8bit") {
                if (size_mode != .@"8bit") {
                    std.debug.panic("Trying to load 8-bit value while in 16-bit mode", .{});
                }

                b.emit_reloc(switch (reloc.type) {
                    .imm8, .rel8, .addr_l, .addr_h, .addr_bank => switch (reg_type) {
                        .a => .lda,
                        .x => .ldx,
                        .y => .ldy,
                    },
                    .imm16, .addr16, .addr24 => unreachable,
                }, reloc);
            } else {
                if (size_mode != .@"16bit") {
                    std.debug.panic("Trying to load 16-bit value while in 8-bit mode", .{});
                }

                b.emit_reloc(switch (reloc.type) {
                    .imm8, .rel8, .addr_l, .addr_h, .addr_bank => unreachable,
                    .imm16 => switch (reg_type) {
                        .a => .lda,
                        .x => .ldx,
                        .y => .ldy,
                    },
                    .addr16 => @panic("TODO"),
                    .addr24 => unreachable,
                }, reloc);
            }

            b.clobbers.put(b.build_system.allocator, switch (reg_type) {
                .a => .a,
                .x => .x,
                .y => .y,
            }, {}) catch @panic("Out of memory");

            return .next(b);
        }

        /// Stores the current value into the target
        pub fn store(reg: Reg, target: anytype) void {
            reg.store_offset(target, 0x00);
        }

        /// Stores the current value into the target + offset
        pub fn store_offset(reg: Reg, target: anytype, offset: u16) void {
            reg.ensure_valid();

            if (@TypeOf(target) == Symbol.Address) {
                // TODO: Handle symbols in other banks
                reg.builder.emit_reloc(switch (reg_type) {
                    .a => .sta_addr16,
                    .x => .stx_addr16,
                    .y => .sty_addr16,
                }, .{
                    .type = .addr16,
                    .target_sym = .{ .address = target },
                    .target_offset = offset,
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

        /// Transfers the current value into the target register
        pub fn transfer_to(reg: Reg, target: enum { a, x, y, direct_page, stack_ptr }) void {
            switch (reg_type) {
                .a => {
                    switch (target) {
                        .a => return, // No-Op
                        .x => {
                            if (reg.builder.a_size != reg.builder.xy_size) {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.tax);
                            _ = RegX.next(reg.builder);
                        },
                        .y => {
                            if (reg.builder.a_size != reg.builder.xy_size) {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.tay);
                            _ = RegY.next(reg.builder);
                        },
                        .direct_page => {
                            if (reg.builder.a_size != .@"16bit") {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.tcd);
                        },
                        .stack_ptr => {
                            if (reg.builder.a_size != .@"16bit") {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.tcs);
                        },
                    }
                },
                .x => {
                    switch (target) {
                        .a => {
                            if (reg.builder.xy_size != reg.builder.a_size) {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.txa);
                            _ = RegA.next(reg.builder);
                        },
                        .x => return, // No-Op
                        .y => {
                            reg.builder.emit(.txy);
                            _ = RegY.next(reg.builder);
                        },
                        .direct_page => @panic("Unsupported register transfer"),
                        .stack_ptr => {
                            if (reg.builder.xy_size != .@"16bit") {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.txs);
                        },
                    }
                },
                .y => {
                    switch (target) {
                        .a => {
                            if (reg.builder.xy_size != reg.builder.a_size) {
                                @panic("Source and Target registers must have same bit-size");
                            }

                            reg.builder.emit(.tya);
                            _ = RegA.next(reg.builder);
                        },
                        .x => {
                            reg.builder.emit(.tyx);
                            _ = RegX.next(reg.builder);
                        },
                        .y => return, // No-Op
                        .direct_page => @panic("Unsupported register transfer"),
                        .stack_ptr => @panic("Unsupported register transfer"),
                    }
                },
            }
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

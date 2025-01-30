const std = @import("std");
const memory_map = @import("memory_map.zig");

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Symbol = @import("symbol.zig").Symbol;
const TypeSymbol = @import("symbol.zig").TypeSymbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const SymbolIndex = Sema.SymbolIndex;
const Sema = @import("Sema.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Ir = @import("ir.zig").Ir;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;

const CodeGen = @This();

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {
        /// Relative signed 8-bit offset to the symbol
        rel8,
        /// 16-bit Absolute address of the symbol
        addr16,
        /// 24-bit Long address of the symbol
        addr24,
    };

    type: Type,
    target_symbol: Sema.SymbolIndex,
    target_offset: u16 = 0,
};
/// Branches need to be relocated to point to their label and use the short / long form
pub const BranchRelocation = struct {
    pub const Type = enum {
        always,
        carry_set,
        carry_clear,
        overflow_set,
        overflow_clear,
        equal,
        not_equal,
        plus,
        minus,
        jump,
        jump_long,
    };

    type: Type,
    target_label: []const u8,
};
/// Metadata information about an instruction
pub const InstructionInfo = struct {
    instr: Instruction,
    /// Offset from the start of the function. Only defined once the assembly as been emitted
    offset: u16 = undefined,

    reloc: ?Relocation = null,
    branch_reloc: ?BranchRelocation = null,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode,

    /// Source-node for this instruction
    source: NodeIndex,
};

pub fn process(sema: *Sema) !void {
    for (sema.symbols.items, 0..) |*symbol, symbol_idx| {
        if (symbol.* != .function) {
            continue;
        }

        var builder: FunctionBuilder = .{
            .sema = sema,
            .symbol_idx = .cast(symbol_idx),

            .mem_size = if (symbol.function.calling_convention.status_flags.mem_8bit) |mem_8bit|
                if (mem_8bit) .@"8bit" else .@"16bit"
            else
                .none,
            .idx_size = if (symbol.function.calling_convention.status_flags.idx_8bit) |idx_8bit|
                if (idx_8bit) .@"8bit" else .@"16bit"
            else
                .none,
        };
        errdefer builder.instructions.deinit(sema.allocator);
        defer builder.labels.deinit(sema.allocator);

        try builder.build();
        try builder.resolveBranchRelocs();
        symbol.function.assembly_data = try builder.genereateAssemblyData();
        // try builder.fixLabelRelocs();
        symbol.function.instructions = try builder.instructions.toOwnedSlice(sema.allocator);

        const labels = try sema.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
        for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
            label.* = .{ label_name, index };
        }
        symbol.function.labels = labels;
    }
}

/// Builds instruction information from the AST nodes of a function
const FunctionBuilder = struct {
    /// Index to a target instruction
    pub const Label = u16;

    sema: *Sema,
    symbol_idx: Sema.SymbolIndex,

    ir_idx: u32 = 0,

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,

    labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode,

    pub fn build(b: *FunctionBuilder) !void {
        try b.emit(.none, .clc);
        try b.emit(.none, .xce);

        while (b.ir_idx < b.function().ir.len) : (b.ir_idx += 1) {
            const ir = b.function().ir[b.ir_idx];
            switch (ir.tag) {
                .instruction => try b.handleInstruction(ir),
                .change_status_flags => try b.handleChangeStatusFlags(ir),
                .store_global, .store_local, .store_push => try b.handleStore(ir),
                .store_operation => unreachable,
                // .load_value => try b.handleLoadValue(ir),
                // .load_variable => try b.handleLoadVariable(ir),
                // .store_variable => try b.handleStoreVariable(ir),
                // .zero_variable => try b.handleZeroVariable(ir),
                // .and_value => try b.handleAndValue(ir),
                // .and_variable => try b.handleAndVariable(ir),
                // .or_value => try b.handleOrValue(ir),
                // .or_variable => try b.handleOrVariable(ir),
                // .shift_accum_left => try b.handleShiftAccumLeft(ir),
                // .shift_accum_right => try b.handleShiftAccumRight(ir),
                // .rotate_accum_left => try b.handleRotateAccumLeft(ir),
                // .rotate_accum_right => try b.handleRotateAccumRight(ir),
                // .clear_bits => try b.handleClearBits(ir),
                inline else => |_, tag| @panic("TODO: " ++ @tagName(tag)),
                .call => try b.handleCall(ir),
                .branch => try b.handleBranch(ir),
                .label => try b.handleLabel(ir),
            }
        }
    }

    inline fn function(b: *FunctionBuilder) *Symbol.Function {
        return &b.sema.getSymbol(b.symbol_idx).function;
    }

    fn handleInstruction(b: *FunctionBuilder, ir: Ir) !void {
        const instruction = ir.tag.instruction;
        try b.instructions.append(b.sema.allocator, .{
            .instr = instruction.instr,
            .reloc = instruction.reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleChangeStatusFlags(b: *FunctionBuilder, ir: Ir) !void {
        const change_flags = ir.tag.change_status_flags;

        var set: Instruction.StatusRegister = .{};
        var clear: Instruction.StatusRegister = .{};

        inline for (std.meta.fields(Instruction.StatusRegister)) |field| {
            if (@field(change_flags, field.name)) |value| {
                if (value) {
                    @field(set, field.name) = true;
                } else {
                    @field(clear, field.name) = true;
                }
            }
        }

        if (set != @as(Instruction.StatusRegister, .{})) {
            try b.emit(ir.node, .{ .sep = set });
        }
        if (clear != @as(Instruction.StatusRegister, .{})) {
            try b.emit(ir.node, .{ .rep = clear });
        }

        if (change_flags.mem_8bit) |mem_8bit| {
            b.mem_size = if (mem_8bit) .@"8bit" else .@"16bit";
        }
        if (change_flags.idx_8bit) |idx_8bit| {
            b.idx_size = if (idx_8bit) .@"8bit" else .@"16bit";
        }
    }

    fn handleStore(b: *FunctionBuilder, ir: Ir) !void {
        const operation_count = switch (ir.tag) {
            inline .store_global, .store_local, .store_push => |info| info.operations,
            else => unreachable,
        };
        const int_reg = switch (ir.tag) {
            inline .store_global, .store_local, .store_push => |info| info.intermediate_register,
            else => unreachable,
        };

        const operations = b.function().ir[(b.ir_idx + 1)..(b.ir_idx + operation_count + 1)];
        std.debug.assert(operations.len != 0);

        b.ir_idx += operation_count;

        var start_idx: usize = 0;
        var curr_offset = operations[start_idx].tag.store_operation.write_offset;
        for (operations, 0..) |op_ir, op_idx| {
            const op = op_ir.tag.store_operation;
            if (op_idx != operations.len - 1 and operations[op_idx + 1].tag.store_operation.write_offset == curr_offset) {
                continue;
            }
            defer if (op_idx != operations.len - 1) {
                start_idx = op_idx + 1;
                curr_offset = operations[start_idx].tag.store_operation.write_offset;
            };

            // Boundry of current write
            var immediate_value: u16 = 0;
            var mask_value: u16 = 0;

            const Variable = packed struct {
                source_type: enum(u2) { none, global, local },
                source: packed union {
                    symbol: SymbolIndex,
                    index: u16,
                },

                bit_size: u16,
            };
            var variables = [_]Variable{.{ .source_type = .none, .source = undefined, .bit_size = undefined }} ** 16;

            // Iterate in reverse, since most-significant bits need to come first
            var iter = std.mem.reverseIterator(operations[start_idx..(op_idx + 1)]);
            while (iter.next()) |curr_op_ir| {
                const curr_op = curr_op_ir.tag.store_operation;
                switch (curr_op.value) {
                    .immediate => |value| {
                        immediate_value |= (value.to(u16) catch unreachable) << @intCast(curr_op.bit_offset % int_reg.bitSize());
                    },
                    .global => |global| {
                        // TODO: Support packed field loads
                        std.debug.assert(global.bit_offset % 8 == 0);

                        variables[curr_op.bit_offset % int_reg.bitSize()] = .{
                            .source_type = .global,
                            .source = .{ .symbol = global.symbol },
                            .bit_size = curr_op.bit_size,
                        };
                    },
                    .local => |local| {
                        // TODO: Support packed field loads
                        std.debug.assert(local.bit_offset % 8 == 0);

                        variables[curr_op.bit_offset % int_reg.bitSize()] = .{
                            .source_type = .local,
                            .source = .{ .index = local.index },
                            .bit_size = curr_op.bit_size,
                        };
                    },
                }

                // Mark written bits
                mask_value |= @as(u16, @intCast((@as(u17, 1) << @intCast(curr_op.bit_size)) - 1)) << @intCast(curr_op.bit_offset % int_reg.bitSize());
            }

            // Next byte will be overwritten anyway
            if (op.write_offset == curr_offset + 1) {
                mask_value |= std.math.maxInt(u8) << 8;
            }

            // Flush current write
            const full_write = switch (int_reg) {
                .a8, .x8, .y8 => mask_value & std.math.maxInt(u8) == std.math.maxInt(u8),
                .a16, .x16, .y16 => mask_value & std.math.maxInt(u16) == std.math.maxInt(u16),
                .none => true,
            };
            const imm_value: Instruction.Imm816 = switch (int_reg) {
                .a8, .x8, .y8 => .{ .imm8 = @intCast(immediate_value) },
                .a16, .x16, .y16 => .{ .imm16 = @intCast(immediate_value) },
                else => undefined,
            };
            std.log.info("FLUSH {} @ {} || {} {b} {}", .{ immediate_value, curr_offset, full_write, mask_value, imm_value });

            if (full_write) {
                // Full register write
                var shift_offset: u8 = std.math.maxInt(u8);
                for (variables, 0..) |variable, bit_offset| {
                    if (variable.source_type == .none) {
                        continue;
                    }

                    if (shift_offset == std.math.maxInt(u8)) {
                        switch (variable.source_type) {
                            .global => try b.emitReloc(ir.node, switch (int_reg) {
                                .a8, .a16 => .{ .lda_addr16 = undefined },
                                .x8, .x16 => .{ .ldx_addr16 = undefined },
                                .y8, .y16 => .{ .ldy_addr16 = undefined },
                                else => unreachable,
                            }, .{
                                .type = .addr16,
                                .target_symbol = variable.source.symbol,
                                .target_offset = 0,
                            }),
                            .local => switch (b.function().local_variables[variable.source.index].location) {
                                .scratch => @panic("TODO"),
                                .stack => |offset| try b.emit(ir.node, .{ .lda_sr = offset }),
                            },
                            .none => unreachable,
                        }
                    } else {
                        std.debug.assert(int_reg == .a8 or int_reg == .a16);

                        for (0..(shift_offset - bit_offset)) |_| {
                            try b.emit(ir.node, .asl_accum);
                        }

                        switch (variable.source_type) {
                            .global => try b.emitReloc(ir.node, .{ .ora_addr16 = undefined }, .{
                                .type = .addr16,
                                .target_symbol = variable.source.symbol,
                                .target_offset = 0,
                            }),
                            .local => switch (b.function().local_variables[variable.source.index].location) {
                                .scratch => @panic("TODO"),
                                .stack => |offset| try b.emit(ir.node, .{ .ora_sr = offset }),
                            },
                            .none => unreachable,
                        }
                    }

                    shift_offset = @intCast(bit_offset);
                }
                if (shift_offset == std.math.maxInt(u8)) {
                    try b.emit(op_ir.node, switch (int_reg) {
                        .a8, .a16 => .{ .lda_imm = imm_value },
                        .x8, .x16 => .{ .ldx_imm = imm_value },
                        .y8, .y16 => .{ .ldy_imm = imm_value },
                        else => unreachable,
                    });
                } else {
                    std.debug.assert(int_reg == .a8 or int_reg == .a16);

                    for (0..shift_offset) |_| {
                        try b.emit(op_ir.node, .asl_accum);
                    }
                    try b.emit(op_ir.node, .{ .ora_imm = imm_value });
                }

                switch (ir.tag) {
                    .store_global => |global| {
                        const instr: Instruction = if (int_reg == .none or (immediate_value == 0 and shift_offset == 0))
                            .{ .stz_addr16 = undefined }
                        else switch (int_reg) {
                            .a8, .a16 => .{ .sta_addr16 = undefined },
                            .x8, .x16 => .{ .stx_addr16 = undefined },
                            .y8, .y16 => .{ .sty_addr16 = undefined },
                            else => unreachable,
                        };

                        try b.emitReloc(op_ir.node, instr, .{
                            .type = .addr16,
                            .target_symbol = global.symbol,
                            .target_offset = curr_offset,
                        });
                    },
                    .store_local => |local| switch (b.function().local_variables[local.index].location) {
                        .scratch => @panic("TODO"),
                        .stack => |offset| try b.emit(ir.node, .{ .sta_sr = offset }),
                    },
                    .store_push => try b.emit(ir.node, .pha),
                    else => unreachable,
                }
            } else {
                // Partial register write
                std.debug.assert(int_reg == .a8 or int_reg == .a16);

                var min_left_shifts: u8 = std.math.maxInt(u8);
                var min_left_offset: u8 = undefined;
                var min_right_shifts: u8 = std.math.maxInt(u8);
                var min_right_offset: u8 = undefined;

                var bit_offset: u8 = 0;
                var shift_offset: u8 = 0;
                var has_variables = false;

                const rotate_size: u8 = switch (int_reg) {
                    // +1 since the carry bit is included when rotating
                    .a8 => 8 + 1,
                    .a16 => 16 + 1,
                    else => unreachable,
                };

                while (true) : (bit_offset += 1) {
                    if (bit_offset == variables.len) {
                        if (min_left_shifts == std.math.maxInt(u8) or min_right_shifts == std.math.maxInt(u8)) {
                            break;
                        }

                        std.log.warn("MinLeft {} @ {}", .{ min_left_shifts, min_left_offset });
                        std.log.warn("MinRight {} @ {}", .{ min_left_shifts, min_left_offset });
                        if (min_left_shifts <= min_right_shifts) {
                            shift_offset = min_left_offset;
                        } else {
                            shift_offset = min_right_offset;
                        }

                        if (has_variables) {} else {}

                        variables[shift_offset].source_type = .none; // Mark variable as used
                        has_variables = true;

                        // Repeat for next variable
                        bit_offset = 0;
                        continue;
                    }

                    if (variables[bit_offset].source_type == .none) {
                        continue;
                    }

                    const left_shifts: u8 = @intCast(@mod(@as(i8, @intCast(bit_offset)) - @as(i8, @intCast(shift_offset)), @as(i8, @intCast(rotate_size))));
                    const right_shifts: u8 = @intCast(@mod(rotate_size - bit_offset + shift_offset, rotate_size));
                    std.log.info("SHIFT at {d} from {d} << {d} | >> {d}", .{ bit_offset, shift_offset, left_shifts, right_shifts });

                    if (left_shifts < min_left_shifts) {
                        min_left_shifts = left_shifts;
                        min_left_offset = bit_offset;
                    }
                    if (right_shifts < min_right_shifts) {
                        min_right_shifts = right_shifts;
                        min_right_offset = bit_offset;
                    }
                }

                if (has_variables) {
                    @panic("TODO");
                } else {
                    switch (ir.tag) {
                        .store_global => |global| try b.emitReloc(op_ir.node, .{ .lda_addr16 = undefined }, .{
                            .type = .addr16,
                            .target_symbol = global.symbol,
                            .target_offset = curr_offset,
                        }),
                        .store_local => |local| switch (b.function().local_variables[local.index].location) {
                            .scratch => @panic("TODO"),
                            .stack => |offset| try b.emit(ir.node, .{ .lda_sr = offset }),
                        },
                        // Value doesn't exist yet
                        .store_push => {},
                        else => unreachable,
                    }

                    switch (ir.tag) {
                        .store_global, .store_local => {
                            try b.emit(op_ir.node, .{ .and_imm = switch (int_reg) {
                                .a8 => .{ .imm8 = @truncate(~mask_value) },
                                .a16 => .{ .imm16 = @truncate(~mask_value) },
                                else => undefined,
                            } });
                            try b.emit(op_ir.node, .{ .ora_imm = imm_value });
                        },
                        // Value doesn't exist yet
                        .store_push => try b.emit(op_ir.node, .{ .lda_imm = imm_value }),
                        else => unreachable,
                    }

                    switch (ir.tag) {
                        .store_global => |global| try b.emitReloc(op_ir.node, .{ .sta_addr16 = undefined }, .{
                            .type = .addr16,
                            .target_symbol = global.symbol,
                            .target_offset = curr_offset,
                        }),
                        .store_local => |local| switch (b.function().local_variables[local.index].location) {
                            .scratch => @panic("TODO"),
                            .stack => |offset| try b.emit(ir.node, .{ .sta_sr = offset }),
                        },
                        .store_push => try b.emit(op_ir.node, .pha),
                        else => unreachable,
                    }
                }
            }
        }
        // const operations

    }

    fn handleLoadValue(b: *FunctionBuilder, ir: Ir) !void {
        const load = ir.tag.load_value;

        try b.instructions.append(b.sema.allocator, .{
            .instr = switch (load.register) {
                .a8, .a16 => .{ .lda_imm = load.value },
                .x8, .x16 => .{ .ldx_imm = load.value },
                .y8, .y16 => .{ .ldy_imm = load.value },
                .none => unreachable,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleLoadVariable(b: *FunctionBuilder, ir: Ir) !void {
        const load = ir.tag.load_variable;
        const target_symbol = b.sema.getSymbol(load.symbol).*;

        const instr: Instruction, const reloc: ?Relocation = switch (load.register) {
            .a8, .a16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .lda_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = load.symbol,
                    .target_offset = load.offset,
                } }
            else
                .{ .{ .lda_addr24 = undefined }, .{
                    .type = .addr24,
                    .target_symbol = load.symbol,
                    .target_offset = load.offset,
                } },

            .x8, .x16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .ldx_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = load.symbol,
                    .target_offset = load.offset,
                } }
            else
                unreachable,

            .y8, .y16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .ldx_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = load.symbol,
                    .target_offset = load.offset,
                } }
            else
                unreachable,

            .none => unreachable,
        };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleStoreVariable(b: *FunctionBuilder, ir: Ir) !void {
        const store = ir.tag.store_variable;
        const target_symbol = b.sema.getSymbol(store.symbol).*;

        const instr: Instruction, const reloc: Relocation = switch (store.register) {
            .a8, .a16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .sta_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = store.symbol,
                    .target_offset = store.offset,
                } }
            else
                .{ .{ .sta_addr24 = undefined }, .{
                    .type = .addr24,
                    .target_symbol = store.symbol,
                    .target_offset = store.offset,
                } },

            .x8, .x16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .stx_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = store.symbol,
                    .target_offset = store.offset,
                } }
            else
                unreachable,

            .y8, .y16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
                .{ .{ .sty_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_symbol = store.symbol,
                    .target_offset = store.offset,
                } }
            else
                unreachable,
        };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleZeroVariable(b: *FunctionBuilder, ir: Ir) !void {
        const store = ir.tag.zero_variable;

        // STZ does not support long addresses
        std.debug.assert(b.sema.isSymbolAccessibleInBank(b.sema.getSymbol(store.symbol).*, b.function().bank));

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .stz_addr16 = undefined },
            .reloc = .{
                .type = .addr16,
                .target_symbol = store.symbol,
                .target_offset = store.offset,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleOrValue(b: *FunctionBuilder, ir: Ir) !void {
        const value = ir.tag.or_value;

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .ora_imm = value },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleOrVariable(b: *FunctionBuilder, ir: Ir) !void {
        const variable = ir.tag.or_variable;
        const target_symbol = b.sema.getSymbol(variable.symbol).*;

        const instr: Instruction, const reloc: Relocation = if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
            .{ .{ .ora_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = variable.symbol,
                .target_offset = variable.offset,
            } }
        else
            .{ .{ .ora_addr24 = undefined }, .{
                .type = .addr24,
                .target_symbol = variable.symbol,
                .target_offset = variable.offset,
            } };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleAndValue(b: *FunctionBuilder, ir: Ir) !void {
        const value = ir.tag.and_value;

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .and_imm = value },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleAndVariable(b: *FunctionBuilder, ir: Ir) !void {
        const variable = ir.tag.and_variable;
        const target_symbol = b.sema.getSymbol(variable.symbol).*;

        const instr: Instruction, const reloc: Relocation = if (b.sema.isSymbolAccessibleInBank(target_symbol, b.function().bank))
            .{ .{ .and_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = variable.symbol,
                .target_offset = variable.offset,
            } }
        else
            .{ .{ .and_addr24 = undefined }, .{
                .type = .addr24,
                .target_symbol = variable.symbol,
                .target_offset = variable.offset,
            } };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleShiftAccumLeft(b: *FunctionBuilder, ir: Ir) !void {
        const shift = ir.tag.shift_accum_left;

        for (0..shift) |_| {
            try b.instructions.append(b.sema.allocator, .{
                .instr = .asl_accum,

                .mem_size = b.mem_size,
                .idx_size = b.idx_size,

                .source = ir.node,
            });
        }
    }
    fn handleShiftAccumRight(b: *FunctionBuilder, ir: Ir) !void {
        const shift = ir.tag.shift_accum_right;

        for (0..shift) |_| {
            try b.instructions.append(b.sema.allocator, .{
                .instr = .lsr_accum,

                .mem_size = b.mem_size,
                .idx_size = b.idx_size,

                .source = ir.node,
            });
        }
    }
    fn handleRotateAccumLeft(b: *FunctionBuilder, ir: Ir) !void {
        const rotate = ir.tag.rotate_accum_left;

        for (0..rotate) |_| {
            try b.instructions.append(b.sema.allocator, .{
                .instr = .rol_accum,

                .mem_size = b.mem_size,
                .idx_size = b.idx_size,

                .source = ir.node,
            });
        }
    }
    fn handleRotateAccumRight(b: *FunctionBuilder, ir: Ir) !void {
        const rotate = ir.tag.rotate_accum_right;

        for (0..rotate) |_| {
            try b.instructions.append(b.sema.allocator, .{
                .instr = .ror_accum,

                .mem_size = b.mem_size,
                .idx_size = b.idx_size,

                .source = ir.node,
            });
        }
    }
    fn handleClearBits(b: *FunctionBuilder, ir: Ir) !void {
        const clear_bits = ir.tag.clear_bits;

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .lda_imm = clear_bits.mask },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .trb_addr16 = undefined },
            .reloc = .{
                .type = .addr16,
                .target_symbol = clear_bits.symbol,
                .target_offset = clear_bits.offset,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleLabel(b: *FunctionBuilder, ir: Ir) !void {
        try b.labels.put(b.sema.allocator, ir.tag.label, @intCast(b.instructions.items.len));
    }
    fn handleCall(b: *FunctionBuilder, ir: Ir) !void {
        const call = ir.tag.call;
        const target_symbol = b.sema.getSymbol(call.target);

        // TODO: Support long jumps
        std.debug.assert(target_symbol.function.bank == b.function().bank);

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .jsr = undefined },
            .reloc = .{
                .type = .addr16,
                .target_symbol = call.target,
                .target_offset = call.target_offset,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleBranch(b: *FunctionBuilder, ir: Ir) !void {
        try b.instructions.append(b.sema.allocator, .{
            .instr = undefined,
            .branch_reloc = ir.tag.branch,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }

    fn emit(b: *FunctionBuilder, node: NodeIndex, instr: Instruction) !void {
        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = node,
        });
    }
    fn emitReloc(b: *FunctionBuilder, node: NodeIndex, instr: Instruction, reloc: Relocation) !void {
        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = node,
        });
    }

    /// Generates the raw assembly bytes for all instructions
    fn genereateAssemblyData(b: *FunctionBuilder) ![]u8 {
        var data: std.ArrayListUnmanaged(u8) = .{};
        // TODO: Figure out good instruction/assembly ratio
        try data.ensureTotalCapacity(b.sema.allocator, b.instructions.items.len * 2);

        const data_writer = data.writer(b.sema.allocator);

        for (b.instructions.items) |*info| {
            const size_type = info.instr.sizeType();
            const size_mode = switch (size_type) {
                .none => .none,
                .mem => info.mem_size,
                .idx => info.idx_size,
            };
            if (size_type != .none) {
                std.debug.assert(size_mode != .none);
            }

            info.offset = @intCast(data.items.len);
            try info.instr.writeData(data_writer, size_mode);
        }

        return data.toOwnedSlice(b.sema.allocator);
    }

    /// By default labels point to a symbol in the current module, instead of the current one with an offset
    fn fixLabelRelocs(b: *FunctionBuilder) !void {
        for (b.instructions.items) |*info| {
            const reloc = &(info.reloc orelse continue);
            if (!std.mem.eql(u8, reloc.target_sym.module, b.symbol_location.module)) continue;

            for (b.labels.keys(), b.labels.values()) |label, instr_index| {
                if (std.mem.eql(u8, label, reloc.target_sym.name)) {
                    reloc.target_symbol = b.symbol_location;
                    reloc.target_offset = if (instr_index == b.instructions.items.len)
                        @intCast(b.function().assembly_data.len)
                    else
                        b.instructions.items[instr_index].offset;
                }
            }
        }
    }

    /// Resolves branch locations into a short / long form
    fn resolveBranchRelocs(b: *FunctionBuilder) !void {
        // Relative offsets to the target instruction to determine short- / long-form
        var reloc_offsets: std.AutoArrayHashMapUnmanaged(usize, i32) = .{};
        defer reloc_offsets.deinit(b.sema.allocator);

        for (b.instructions.items, 0..) |info, i| {
            if (info.branch_reloc != null) {
                // Default to long-form, lower to short-form later
                try reloc_offsets.put(b.sema.allocator, i, std.math.maxInt(i32));
            }
        }

        const short_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.bra.size(.none),
            .carry_set = comptime Instruction.bcs.size(.none),
            .carry_clear = comptime Instruction.bcc.size(.none),
            .overflow_set = comptime Instruction.bvs.size(.none),
            .overflow_clear = comptime Instruction.bvc.size(.none),
            .equal = comptime Instruction.beq.size(.none),
            .not_equal = comptime Instruction.bne.size(.none),
            .plus = comptime Instruction.bpl.size(.none),
            .minus = comptime Instruction.bmi.size(.none),
            .jump = comptime Instruction.jmp.size(.none),
            .jump_long = comptime Instruction.jml.size(.none),
        });
        const long_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.jmp.size(.none),
            .carry_set = comptime Instruction.bcc.size(.none) + Instruction.jmp.size(.none),
            .carry_clear = comptime Instruction.bcs.size(.none) + Instruction.jmp.size(.none),
            .overflow_set = comptime Instruction.bvc.size(.none) + Instruction.jmp.size(.none),
            .overflow_clear = comptime Instruction.bvs.size(.none) + Instruction.jmp.size(.none),
            .equal = comptime Instruction.bne.size(.none) + Instruction.jmp.size(.none),
            .not_equal = comptime Instruction.beq.size(.none) + Instruction.jmp.size(.none),
            .plus = comptime Instruction.bmi.size(.none) + Instruction.jmp.size(.none),
            .minus = comptime Instruction.bpl.size(.none) + Instruction.jmp.size(.none),
            .jump = comptime Instruction.jmp.size(.none),
            .jump_long = comptime Instruction.jml.size(.none),
        });

        // Interativly lower to short-form
        var changed = true;
        while (changed) {
            changed = false;

            for (reloc_offsets.keys(), reloc_offsets.values()) |source_idx, *relative_offset| {
                // If its's already short, don't mark this as a change, but still recalculate the offset
                const already_short = relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8);

                const reloc = b.instructions.items[source_idx].branch_reloc.?;
                const target_idx = b.labels.get(reloc.target_label).?;

                // Calculate offset to target
                const min = @min(source_idx + 1, target_idx);
                const max = @max(source_idx + 1, target_idx);

                relative_offset.* = 0;
                for (b.instructions.items[min..max], min..max) |info, i| {
                    if (info.branch_reloc) |other_reloc| {
                        const other_offset = reloc_offsets.get(i).?;

                        if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                            relative_offset.* += short_sizes.get(other_reloc.type);
                        } else {
                            relative_offset.* += long_sizes.get(other_reloc.type);
                        }
                    } else {
                        const size_type = info.instr.sizeType();
                        const size_mode = switch (size_type) {
                            .none => .none,
                            .mem => info.mem_size,
                            .idx => info.idx_size,
                        };
                        if (size_type != .none) {
                            std.debug.assert(size_mode != .none);
                        }

                        relative_offset.* += info.instr.size(size_mode);
                    }
                }
                if (target_idx <= source_idx) {
                    relative_offset.* = -relative_offset.*;
                }

                if (!already_short and relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8)) {
                    changed = true;
                }
            }
        }

        // Calculate target offsets (for jumps)
        var target_offsets: std.AutoArrayHashMapUnmanaged(usize, u16) = .{};
        defer target_offsets.deinit(b.sema.allocator);

        for (reloc_offsets.keys()) |source_idx| {
            const reloc = b.instructions.items[source_idx].branch_reloc.?;
            const target_idx = b.labels.get(reloc.target_label).?;

            var offset: usize = 0;
            for (b.instructions.items[0..target_idx], 0..) |info, i| {
                if (info.branch_reloc) |other_reloc| {
                    const other_offset = reloc_offsets.get(i).?;

                    if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                        offset += short_sizes.get(other_reloc.type);
                    } else {
                        offset += long_sizes.get(other_reloc.type);
                    }
                } else {
                    const size_type = info.instr.sizeType();
                    const size_mode = switch (size_type) {
                        .none => .none,
                        .mem => info.mem_size,
                        .idx => info.idx_size,
                    };
                    if (size_type != .none) {
                        std.debug.assert(size_mode != .none);
                    }

                    offset += info.instr.size(size_mode);
                }
            }

            try target_offsets.put(b.sema.allocator, source_idx, @intCast(offset));
        }

        // Insert instructions (reversed to avoid shifting following indices)
        var it = std.mem.reverseIterator(reloc_offsets.keys());

        while (it.next()) |source_idx| {
            const info = &b.instructions.items[source_idx];
            const reloc = info.branch_reloc.?;

            const relative_offset = reloc_offsets.get(source_idx).?;
            const target_offset = target_offsets.get(source_idx).?;

            const use_short = relative_offset >= std.math.minInt(i8) and relative_offset <= std.math.maxInt(i8);

            if (use_short) {
                info.instr = switch (reloc.type) {
                    .always => .{ .bra = @intCast(relative_offset) },
                    .carry_set => .{ .bcs = @intCast(relative_offset) },
                    .carry_clear => .{ .bcc = @intCast(relative_offset) },
                    .overflow_set => .{ .bvs = @intCast(relative_offset) },
                    .overflow_clear => .{ .bvc = @intCast(relative_offset) },
                    .equal => .{ .beq = @intCast(relative_offset) },
                    .not_equal => .{ .bne = @intCast(relative_offset) },
                    .plus => .{ .bpl = @intCast(relative_offset) },
                    .minus => .{ .bmi = @intCast(relative_offset) },
                    .jump => .{ .jmp = undefined },
                    .jump_long => .{ .jml = undefined },
                };

                if (reloc.type == .jump) {
                    info.reloc = .{
                        .type = .addr16,
                        .target_symbol = b.symbol_idx,
                        .target_offset = target_offset,
                    };
                } else if (reloc.type == .jump_long) {
                    info.reloc = .{
                        .type = .addr24,
                        .target_symbol = b.symbol_idx,
                        .target_offset = target_offset,
                    };
                }
            } else {
                const jmp_size = comptime Opcode.jmp.size(.none);
                info.instr = switch (reloc.type) {
                    .always, .jump => .{ .jmp = undefined },
                    .carry_set => .{ .bcc = jmp_size },
                    .carry_clear => .{ .bcs = jmp_size },
                    .overflow_set => .{ .bvc = jmp_size },
                    .overflow_clear => .{ .bvs = jmp_size },
                    .equal => .{ .bne = jmp_size },
                    .not_equal => .{ .beq = jmp_size },
                    .plus => .{ .bmi = jmp_size },
                    .minus => .{ .bpl = jmp_size },
                    .jump_long => .{ .jml = undefined },
                };

                const jmp_reloc: Relocation = .{
                    .type = .addr16,
                    .target_symbol = b.symbol_idx,
                    .target_offset = target_offset,
                };

                if (reloc.type == .always or reloc.type == .jump) {
                    info.reloc = jmp_reloc;
                } else if (reloc.type == .jump_long) {
                    info.reloc = .{
                        .type = .addr24,
                        .target_symbol = b.symbol_idx,
                        .target_offset = target_offset,
                    };
                } else {
                    try b.instructions.insert(b.sema.allocator, source_idx + 1, .{
                        .instr = .{ .jmp = undefined },
                        .reloc = jmp_reloc,

                        .mem_size = info.mem_size,
                        .idx_size = info.idx_size,

                        .source = info.source,
                    });
                }
            }
        }
    }
};

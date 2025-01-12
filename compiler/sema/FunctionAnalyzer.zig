const std = @import("std");
const Ast = @import("../Ast.zig");
const Module = @import("../Module.zig");
const NodeIndex = Ast.NodeIndex;
const Sema = @import("../Sema.zig");
const Ir = @import("../ir.zig").Ir;
const Symbol = @import("../symbol.zig").Symbol;
const SymbolIndex = @import("../Sema.zig").SymbolIndex;
const SymbolLocation = @import("../symbol.zig").SymbolLocation;
const TypeSymbol = @import("../symbol.zig").TypeSymbol;
const Instruction = @import("../instruction.zig").Instruction;
const Opcode = @import("../instruction.zig").Opcode;
const Relocation = @import("../CodeGen.zig").Relocation;
const BuiltinFn = @import("BuiltinFn.zig");
const BuiltinVar = @import("BuiltinVar.zig");

const Analyzer = @This();
const Error = Sema.AnalyzeError;

pub const Scope = struct {};
pub const CallingConvention = struct {
    status_flags: Ir.ChangeStatusFlags = .{},
};

sema: *Sema,

symbol: *Symbol.Function,
module_idx: Sema.ModuleIndex,

/// Interrupt vectors cannot rely on a calling convention
is_interrupt_vector: bool,

/// List of intermediate instructions, used to generate machine code
ir: std.ArrayListUnmanaged(Ir) = .empty,

/// Size of the A-Register / Memory instructions
mem_size: Instruction.SizeMode = .none,
/// Size of the X/Y-Registers
idx_size: Instruction.SizeMode = .none,

calling_conv: CallingConvention = .{},

scope_stack: std.ArrayListUnmanaged(Scope) = .empty,

// Used to generate unique labels for loops
while_loops: u16 = 0,

pub fn deinit(ana: *Analyzer) void {
    for (ana.ir.items) |ir| {
        ir.deinit(ana.sema.allocator);
    }
    ana.ir.deinit(ana.sema.allocator);
}

pub inline fn module(ana: *Analyzer) *Module {
    return ana.sema.getModule(ana.module_idx);
}
pub inline fn ast(ana: *Analyzer) *Ast {
    return &ana.module().ast;
}

// Helper functions

pub fn setSizeMode(ana: *Analyzer, node: NodeIndex, target: Instruction.SizeType, mode: Instruction.SizeMode) !void {
    if (target == .mem and ana.mem_size == mode or
        target == .idx and ana.idx_size == mode)
    {
        // Nothing to do
        return;
    }

    // Forward to calling convention
    if (!ana.is_interrupt_vector and target == .mem and ana.mem_size == .none) {
        ana.calling_conv.status_flags.mem_8bit = mode == .@"8bit";
        return;
    }
    if (!ana.is_interrupt_vector and target == .idx and ana.idx_size == .none) {
        ana.calling_conv.status_flags.idx_8bit = mode == .@"8bit";
        return;
    }

    // TODO: Clobber register data
    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .change_status_flags = switch (target) {
            .mem => .{ .mem_8bit = mode == .@"8bit" },
            .idx => .{ .idx_8bit = mode == .@"8bit" },
            else => unreachable,
        } },
        .node = node,
    });
}

// AST node handling

pub fn handleFnDef(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    try ana.handleBlock(ana.ast().nodeData(node_idx).fn_def.block);

    // Implicit return after function body
    // Maintain same status flags as for start of function
    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .change_status_flags = ana.calling_conv.status_flags },
        .node = node_idx,
    });
    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .instruction = .{
            .instr = .rts,
            .reloc = null,
        } },
        .node = node_idx,
    });
}

pub fn handleBlock(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    try ana.scope_stack.append(ana.sema.allocator, .{});
    defer _ = ana.scope_stack.pop();

    const range = ana.ast().nodeData(node_idx).sub_range;
    for (@intFromEnum(range.extra_start)..@intFromEnum(range.extra_end)) |extra_idx| {
        const child_idx: NodeIndex = @enumFromInt(ana.ast().extra_data[extra_idx]);

        switch (ana.ast().nodeTag(child_idx)) {
            .label => try ana.handleLabel(child_idx),
            .assign_statement => try ana.handleAssign(child_idx),
            .call_statement => try ana.handleCall(child_idx),
            .while_statement => try ana.handleWhile(child_idx),
            else => unreachable,
        }
    }
}

pub fn handleLabel(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const label_token = ana.ast().nodeToken(node_idx);
    const label = ana.ast().parseIdentifier(label_token);

    if (ana.module().symbol_map.get(label)) |existing_sym_idx| {
        const existing_sym = ana.sema.getSymbol(existing_sym_idx);

        try ana.sema.errors.append(ana.sema.allocator, .{
            .tag = .duplicate_label,
            .ast = ana.ast(),
            .token = label_token,
        });
        try ana.sema.errors.append(ana.sema.allocator, .{
            .tag = .existing_symbol,
            .is_note = true,
            .ast = ana.ast(),
            .token = ana.ast().nodeToken(existing_sym.common().node),
        });
        return error.AnalyzeFailed;
    }
    for (ana.ir.items) |ir| {
        if (ir.tag != .label) {
            continue;
        }

        const existing_label = ir.tag.label;
        if (std.mem.eql(u8, label, existing_label)) {
            try ana.sema.errors.append(ana.sema.allocator, .{
                .tag = .duplicate_label,
                .ast = ana.ast(),
                .token = label_token,
            });
            try ana.sema.errors.append(ana.sema.allocator, .{
                .tag = .existing_label,
                .is_note = true,
                .ast = ana.ast(),
                .token = ana.ast().nodeToken(ir.node),
            });
            return error.AnalyzeFailed;
        }
    }

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .label = try ana.sema.allocator.dupe(u8, label) },
        .node = node_idx,
    });
}

const Field = union(enum) {
    builtin: BuiltinVar,
    symbol: struct {
        main_symbol: SymbolIndex,
        target_type: TypeSymbol,
        byte_offset: u16,

        /// Offset into a packed struct.
        /// `std.math.maxInt(u8)` indicates a regular struct
        bit_offset: u8 = std.math.maxInt(u8),
    },
};

fn resolveField(ana: *Analyzer, field_access: NodeIndex) Error!Field {
    var main_symbol_idx: SymbolIndex = undefined;
    var current_type: TypeSymbol = undefined;

    const byte_offset: u16 = 0;
    var bit_offset: u8 = 0;
    var is_packed: bool = false;

    const field_range = ana.ast().nodeData(field_access).sub_range;
    for (@intFromEnum(field_range.extra_start)..@intFromEnum(field_range.extra_end)) |extra_idx| {
        const field_token: Ast.TokenIndex = .cast(ana.ast().extra_data[extra_idx]);

        // Main target symbol
        if (extra_idx == @intFromEnum(field_range.extra_start)) {
            if (ana.ast().tokenTag(field_token) == .builtin_ident) {
                // Built-in variable
                if (BuiltinVar.get(ana.ast().parseIdentifier(field_token))) |builtin| {
                    // Built-ins don't support field access
                    std.debug.assert(extra_idx + 1 == @intFromEnum(field_range.extra_end));

                    return .{ .builtin = builtin };
                } else {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .invalid_builtin,
                        .ast = ana.ast(),
                        .token = ana.ast().nodeToken(field_access),
                    });
                    return error.AnalyzeFailed;
                }
            } else {
                // User variable
                const main_symbol, main_symbol_idx = try ana.sema.resolveSymbol(field_token, ana.module_idx);
                current_type = switch (main_symbol.*) {
                    .constant => |const_sym| const_sym.type,
                    .variable => |var_sym| var_sym.type,
                    .register => |reg_sym| reg_sym.type,
                    else => {
                        try ana.sema.errors.append(ana.sema.allocator, .{
                            .tag = .expected_const_var_reg_symbol,
                            .ast = ana.ast(),
                            .token = ana.ast().nodeToken(field_access),
                            .extra = .{ .actual_symbol = main_symbol.* },
                        });
                        return error.AnalyzeFailed;
                    },
                };
                continue;
            }
        }

        // Fields
        const field_name = ana.ast().parseIdentifier(field_token);
        current_type = switch (current_type.raw) {
            .@"packed" => |packed_type_sym| b: {
                is_packed = true;

                const packed_sym = ana.sema.getSymbol(packed_type_sym.symbol_index).@"packed";

                for (packed_sym.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        break :b field.type;
                    }

                    bit_offset += field.type.bitSize();
                }

                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .unknown_field,
                    .ast = ana.ast(),
                    .token = field_token,
                    .extra = .{ .unknown_field = .{ .symbol = current_type } },
                });
                return error.AnalyzeFailed;
            },
            else => {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .no_members,
                    .ast = ana.ast(),
                    .token = ana.ast().nodeToken(field_access),
                    .extra = .{ .type = current_type },
                });
                return error.AnalyzeFailed;
            },
        };
    }

    return .{ .symbol = .{
        .main_symbol = main_symbol_idx,
        .target_type = current_type,
        .byte_offset = byte_offset,
        .bit_offset = if (is_packed) bit_offset else std.math.maxInt(u8),
    } };
}

fn handleAssign(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const assign = ana.ast().nodeData(node_idx).assign_statement;
    const data = ana.ast().readExtraData(Ast.Node.AssignStatementData, assign.extra);

    const target = try ana.resolveField(assign.target);
    const target_type = switch (target) {
        .builtin => |builtin| builtin.type,
        .symbol => |symbol| symbol.target_type,
    };

    const value_expr = try ana.sema.resolveExprValue(target_type, data.value, ana.module_idx);
    defer value_expr.deinit(ana.sema.allocator);

    const opt_register_type = get_register: {
        if (value_expr != .register) {
            if (data.intermediate_register == .none) {
                break :get_register null;
            }

            const register_name = ana.ast().tokenSource(data.intermediate_register);
            break :get_register std.meta.stringToEnum(Sema.RegisterType, register_name) orelse {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .invalid_register,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                });
                return error.AnalyzeFailed;
            };
        } else {
            break :get_register value_expr.register;
        }
    };

    // Forward to built-in implementation
    if (target == .builtin) {
        if (!std.mem.containsAtLeast(?Sema.RegisterType, target.builtin.allowed_registers, 1, &.{opt_register_type})) {
            if (opt_register_type) |register_type| {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .unsupported_register,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                    .extra = .{ .unsupported_register = .{
                        .register = register_type,
                        .message = target.builtin.name,
                    } },
                });
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .supported_registers,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                    .is_note = true,
                    .extra = .{ .supported_registers = target.builtin.allowed_registers },
                });
                return error.AnalyzeFailed;
            } else {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_intermediate_register,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                });
                return error.AnalyzeFailed;
            }
        }

        try target.builtin.write(ana, node_idx, value_expr, opt_register_type);
        return;
    }

    const register_size: u8 = if (opt_register_type) |register_type|
        switch (register_type) {
            .a8, .x8, .y8 => 1,
            .a16, .x16, .y16 => 2,
        }
    else switch (ana.mem_size) {
        .none => @panic("TODO"),
        .@"8bit" => 1,
        .@"16bit" => 2,
    };

    if (target.symbol.bit_offset == std.math.maxInt(u8)) {
        // Regular field
        const target_size = target_type.size();
        if (value_expr == .register) {
            if (target_size != register_size) {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_register_size,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                    .extra = .{ .expected_register_size = .{
                        .expected = target_size * 8,
                        .actual = register_size * 8,
                    } },
                });
                return error.AnalyzeFailed;
            }
        } else {
            if (target_size % register_size != 0) {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_mod_register_size,
                    .ast = ana.ast(),
                    .token = data.intermediate_register,
                    .extra = .{ .expected_register_size = .{
                        .expected = target_size * 8,
                        .actual = register_size * 8,
                    } },
                });
                return error.AnalyzeFailed;
            }
        }

        switch (value_expr) {
            .value, .variable => {
                // Storing zero doesn't require an intermediate register
                if (value_expr == .value and value_expr.value == 0) {
                    // If an intermediate register exists, use it for the size
                    if (opt_register_type) |register_type| {
                        try ana.setSizeMode(node_idx, switch (register_type) {
                            .a8, .a16 => .mem,
                            .x8, .x16, .y8, .y16 => .idx,
                        }, switch (register_type) {
                            .a8, .x8, .y8 => .@"8bit",
                            .a16, .x16, .y16 => .@"16bit",
                        });
                    }

                    // n stores
                    try ana.ir.ensureUnusedCapacity(ana.sema.allocator, @divExact(target_size, register_size));

                    var offset: std.math.Log2Int(TypeSymbol.ComptimeIntValue) = 0;
                    while (offset < target_size) : (offset += @intCast(register_size * 8)) {
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .zero_variable = .{
                                .symbol = target.symbol.main_symbol,
                                .offset = offset + target.symbol.byte_offset,
                            } },
                            .node = node_idx,
                        });
                    }
                } else {
                    const register_type = opt_register_type orelse {
                        try ana.sema.errors.append(ana.sema.allocator, .{
                            .tag = .expected_intermediate_register,
                            .ast = ana.ast(),
                            .token = ana.ast().nodeToken(node_idx),
                        });
                        return error.AnalyzeFailed;
                    };

                    // 1 size change + 2n load/stores
                    try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 1 + @divExact(target_size, register_size) * 2);
                    try ana.setSizeMode(node_idx, switch (register_type) {
                        .a8, .a16 => .mem,
                        .x8, .x16, .y8, .y16 => .idx,
                    }, switch (register_type) {
                        .a8, .x8, .y8 => .@"8bit",
                        .a16, .x16, .y16 => .@"16bit",
                    });

                    var offset: std.math.Log2Int(TypeSymbol.ComptimeIntValue) = 0;
                    while (offset < target_size) : (offset += @intCast(register_size * 8)) {
                        switch (value_expr) {
                            .value => |value| {
                                ana.ir.appendAssumeCapacity(.{
                                    .tag = .{
                                        .load_value = .{
                                            .register = register_type,
                                            .value = switch (register_type) {
                                                .a8, .x8, .y8 => .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(value)) >> offset) },
                                                .a16, .x16, .y16 => .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(value)) >> offset) },
                                            },
                                        },
                                    },
                                    .node = node_idx,
                                });
                            },
                            .variable => |sym_loc| {
                                ana.ir.appendAssumeCapacity(.{
                                    .tag = .{
                                        .load_variable = .{
                                            .register = register_type,
                                            .symbol = sym_loc,
                                            .offset = offset,
                                        },
                                    },
                                    .node = node_idx,
                                });
                            },
                            else => unreachable,
                        }

                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .store_variable = .{
                                .register = register_type,
                                .symbol = target.symbol.main_symbol,
                                .offset = offset + target.symbol.byte_offset,
                            } },
                            .node = node_idx,
                        });
                    }
                }
            },
            .packed_fields => |fields| {
                // If an intermediate register exists, use it for the size
                if (opt_register_type) |register_type| {
                    try ana.setSizeMode(node_idx, switch (register_type) {
                        .a8, .a16 => .mem,
                        .x8, .x16, .y8, .y16 => .idx,
                    }, switch (register_type) {
                        .a8, .x8, .y8 => .@"8bit",
                        .a16, .x16, .y16 => .@"16bit",
                    });
                }

                inline for (&.{ u8, u16 }) |T| {
                    if (register_size == @sizeOf(T)) {
                        var byte_offset: u8 = 0;
                        var bit_offset: u8 = 0;

                        var used_symbols: std.ArrayListUnmanaged(PackedSymbol) = .empty;
                        defer used_symbols.deinit(ana.sema.allocator);

                        while (byte_offset < target_size) : (byte_offset += @sizeOf(T)) {
                            var curr_value: T = 0;
                            used_symbols.clearRetainingCapacity();

                            try ana.resolvePackedFields(T, node_idx, fields, &curr_value, &used_symbols, byte_offset, &bit_offset, 0);

                            if (used_symbols.items.len == 0) {
                                // No variables involved
                                if (curr_value == 0) {
                                    try ana.ir.append(ana.sema.allocator, .{
                                        .tag = .{ .zero_variable = .{
                                            .symbol = target.symbol.main_symbol,
                                            .offset = byte_offset + target.symbol.byte_offset,
                                        } },
                                        .node = node_idx,
                                    });
                                } else {
                                    const register_type = opt_register_type orelse {
                                        try ana.sema.errors.append(ana.sema.allocator, .{
                                            .tag = .expected_intermediate_register,
                                            .ast = ana.ast(),
                                            .token = ana.ast().nodeToken(node_idx),
                                        });
                                        return error.AnalyzeFailed;
                                    };

                                    try ana.ir.append(ana.sema.allocator, .{
                                        .tag = .{ .load_value = .{
                                            .register = register_type,
                                            .value = if (T == u8)
                                                .{ .imm8 = curr_value }
                                            else
                                                .{ .imm16 = curr_value },
                                        } },
                                        .node = node_idx,
                                    });
                                    try ana.ir.append(ana.sema.allocator, .{
                                        .tag = .{ .store_variable = .{
                                            .register = register_type,
                                            .symbol = target.symbol.main_symbol,
                                            .offset = byte_offset + target.symbol.byte_offset,
                                        } },
                                        .node = node_idx,
                                    });
                                }
                            } else {
                                // Some variables involved
                                const register_type = opt_register_type orelse {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .expected_intermediate_register,
                                        .ast = ana.ast(),
                                        .token = ana.ast().nodeToken(node_idx),
                                    });
                                    return error.AnalyzeFailed;
                                };
                                if (register_type != .a8 and register_type != .a16) {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .unsupported_register,
                                        .ast = ana.ast(),
                                        .token = data.intermediate_register,
                                        .extra = .{ .unsupported_register = .{
                                            .register = register_type,
                                            .message = "object-initializer including variables",
                                        } },
                                    });
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .supported_registers,
                                        .ast = ana.ast(),
                                        .token = data.intermediate_register,
                                        .is_note = true,
                                        .extra = .{ .supported_registers = &.{ .a8, .a16 } },
                                    });
                                    return error.AnalyzeFailed;
                                }

                                // Shift in variables
                                var i: u8 = @intCast(used_symbols.items.len - 1);
                                while (true) : (i -= 1) {
                                    const curr_symbol = used_symbols.items[i];

                                    if (i == used_symbols.items.len - 1) {
                                        // First variable
                                        try ana.ir.append(ana.sema.allocator, .{
                                            .tag = .{ .load_variable = .{
                                                .register = register_type,
                                                .symbol = curr_symbol.symbol,
                                                .offset = curr_symbol.byte_offset,
                                            } },
                                            .node = node_idx,
                                        });
                                    } else {
                                        // Other variable
                                        const prev_symbol = used_symbols.items[i + 1];

                                        try ana.ir.append(ana.sema.allocator, .{
                                            .tag = .{ .shift_accum_left = prev_symbol.shift_offset - curr_symbol.shift_offset },
                                            .node = node_idx,
                                        });
                                        try ana.ir.append(ana.sema.allocator, .{
                                            .tag = .{ .or_variable = .{
                                                .symbol = curr_symbol.symbol,
                                                .offset = curr_symbol.byte_offset,
                                            } },
                                            .node = node_idx,
                                        });
                                    }

                                    if (i == 0) break;
                                }

                                try ana.ir.append(ana.sema.allocator, .{
                                    .tag = .{ .shift_accum_left = used_symbols.items[0].shift_offset },
                                    .node = node_idx,
                                });

                                // OR-in constant value
                                if (curr_value != 0) {
                                    try ana.ir.append(ana.sema.allocator, .{
                                        .tag = .{ .or_value = if (T == u8)
                                            .{ .imm8 = curr_value }
                                        else
                                            .{ .imm16 = curr_value } },
                                        .node = node_idx,
                                    });
                                }

                                // Store final value
                                try ana.ir.append(ana.sema.allocator, .{
                                    .tag = .{ .store_variable = .{
                                        .register = register_type,
                                        .symbol = target.symbol.main_symbol,
                                        .offset = byte_offset + target.symbol.byte_offset,
                                    } },
                                    .node = node_idx,
                                });
                            }
                        }
                        return;
                    }
                }
                unreachable;
            },
            .register => |reg| {
                try ana.ir.append(ana.sema.allocator, .{
                    .tag = .{ .store_variable = .{
                        .register = reg,
                        .symbol = target.symbol.main_symbol,
                        .offset = target.symbol.byte_offset,
                    } },
                    .node = node_idx,
                });
            },
        }
    } else {
        // Packed field
        const target_bitsize = target_type.bitSize();

        // Prevent crossing register boundry
        if (target.symbol.bit_offset % register_size * 8 > target.symbol.bit_offset + target_bitsize % register_size * 8) {
            try ana.sema.errors.append(ana.sema.allocator, .{
                .tag = .fields_cross_register_boundry,
                .ast = ana.ast(),
                .token = ana.ast().nodeToken(node_idx),
                .extra = .{ .bit_size = 8 },
            });
            return error.AnalyzeFailed;
        }

        switch (value_expr) {
            .value, .variable => {
                const register_type = opt_register_type orelse {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .expected_intermediate_register,
                        .ast = ana.ast(),
                        .token = ana.ast().nodeToken(node_idx),
                    });
                    return error.AnalyzeFailed;
                };
                if (register_type != .a8 and register_type != .a16) {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .unsupported_register,
                        .ast = ana.ast(),
                        .token = data.intermediate_register,
                        .extra = .{ .unsupported_register = .{
                            .register = register_type,
                            .message = "packed field access",
                        } },
                    });
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .supported_registers,
                        .ast = ana.ast(),
                        .token = data.intermediate_register,
                        .is_note = true,
                        .extra = .{ .supported_registers = &.{ .a8, .a16 } },
                    });
                    return error.AnalyzeFailed;
                }

                // Adjust current memory size mode
                try ana.setSizeMode(node_idx, .mem, switch (register_type) {
                    .a8 => .@"8bit",
                    .a16 => .@"16bit",
                    else => unreachable,
                });

                // Values can be optimized by pre-shifting them
                if (value_expr == .value) {
                    if (value_expr.value == 0) {
                        try ana.ir.append(ana.sema.allocator, .{
                            .tag = .{ .clear_bits = .{
                                .symbol = target.symbol.main_symbol,
                                .offset = target.symbol.byte_offset,
                                .mask = switch (register_size) {
                                    1 => .{ .imm8 = ((@as(u8, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset) },
                                    2 => .{ .imm16 = ((@as(u16, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset) },
                                    else => unreachable,
                                },
                            } },
                            .node = node_idx,
                        });
                    } else {
                        try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 4);

                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .load_variable = .{
                                .register = register_type,
                                .symbol = target.symbol.main_symbol,
                                .offset = target.symbol.byte_offset,
                            } },
                            .node = node_idx,
                        });

                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .and_value = switch (register_size) {
                                1 => .{ .imm8 = ~(((@as(u8, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset)) },
                                2 => .{ .imm16 = ~(((@as(u16, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset)) },
                                else => unreachable,
                            } },
                            .node = node_idx,
                        });
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .or_value = switch (register_size) {
                                1 => .{ .imm8 = @as(u8, @intCast(value_expr.value)) << @intCast(target.symbol.bit_offset) },
                                2 => .{ .imm16 = @as(u16, @intCast(value_expr.value)) << @intCast(target.symbol.bit_offset) },
                                else => unreachable,
                            } },
                            .node = node_idx,
                        });

                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .store_variable = .{
                                .register = register_type,
                                .symbol = target.symbol.main_symbol,
                                .offset = target.symbol.byte_offset,
                            } },
                            .node = node_idx,
                        });
                    }
                    return;
                }

                // Cycle Count
                //     Rotate: 4 + 2n + 2 + 2/4 + 2n + 4 = 12/14 + 4n
                //     Reset:  2 + 6 + 2/4 + 2n + 4 + 4  = 18/20 + 2n
                // Bytes
                //     Rotate: 3 + n + 3 + 3 + n + 3 = 12 + 2n
                //     Reset:  3 + 3 + 3 + n + 3 + 3 = 15 +  n
                const rotate_rights = target.symbol.bit_offset;
                const rotate_lefts = register_size * 8 - target.symbol.bit_offset + 1;

                // Rotate is faster for < 3 rotations; otherwise Reset is >=
                // For = 3 rotations, both implementations are exactly the same
                if (@min(rotate_rights, rotate_lefts) < 3) {
                    // 'Rotate' method
                    try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 6);

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load_variable = .{
                            .register = register_type,
                            .symbol = target.symbol.main_symbol,
                            .offset = target.symbol.byte_offset,
                        } },
                        .node = node_idx,
                    });

                    if (rotate_rights < rotate_lefts) {
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .rotate_accum_right = rotate_rights },
                            .node = node_idx,
                        });
                    } else {
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .rotate_accum_left = rotate_lefts },
                            .node = node_idx,
                        });
                    }

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .and_value = switch (register_size) {
                            1 => .{ .imm8 = ~((@as(u8, 1) << @intCast(target_bitsize)) - 1) },
                            2 => .{ .imm16 = ~((@as(u16, 1) << @intCast(target_bitsize)) - 1) },
                            else => unreachable,
                        } },
                        .node = node_idx,
                    });
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .or_variable = .{
                            .symbol = value_expr.variable,
                        } },
                        .node = node_idx,
                    });

                    if (rotate_rights < rotate_lefts) {
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .rotate_accum_left = rotate_rights },
                            .node = node_idx,
                        });
                    } else {
                        ana.ir.appendAssumeCapacity(.{
                            .tag = .{ .rotate_accum_right = rotate_lefts },
                            .node = node_idx,
                        });
                    }

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .store_variable = .{
                            .register = register_type,
                            .symbol = target.symbol.main_symbol,
                            .offset = target.symbol.byte_offset,
                        } },
                        .node = node_idx,
                    });
                } else {
                    // 'Reset' method
                    try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 5);

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .clear_bits = .{
                            .symbol = target.symbol.main_symbol,
                            .offset = target.symbol.byte_offset,
                            .mask = switch (register_size) {
                                1 => .{ .imm8 = ((@as(u8, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset) },
                                2 => .{ .imm16 = ((@as(u16, 1) << @intCast(target_bitsize)) - 1) << @intCast(target.symbol.bit_offset) },
                                else => unreachable,
                            },
                        } },
                        .node = node_idx,
                    });

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load_variable = .{
                            .register = register_type,
                            .symbol = value_expr.variable,
                        } },
                        .node = node_idx,
                    });
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .shift_accum_left = target.symbol.bit_offset },
                        .node = node_idx,
                    });
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .or_variable = .{
                            .symbol = target.symbol.main_symbol,
                            .offset = target.symbol.byte_offset,
                        } },
                        .node = node_idx,
                    });

                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .store_variable = .{
                            .register = register_type,
                            .symbol = target.symbol.main_symbol,
                            .offset = target.symbol.byte_offset,
                        } },
                        .node = node_idx,
                    });
                }
            },
            .packed_fields => |fields| {
                _ = fields; // autofix
                @panic("TODO");
            },
            .register => |reg| {
                _ = reg; // autofix
                // try ana.ir.append(ana.sema.allocator, .{
                //     .tag = .{ .store_variable = .{
                //         .register = reg,
                //         .symbol = target.symbol.main_symbol,
                //         .offset = target.symbol.byte_offset,
                //     } },
                //     .node = node_idx,
                // });
                @panic("TODO");
            },
        }
    }
}

const PackedSymbol = struct {
    symbol: SymbolIndex,
    byte_offset: u8,
    shift_offset: u8,
};
fn resolvePackedFields(ana: *Analyzer, comptime T: type, node_idx: NodeIndex, fields: []const Sema.ExpressionValue.PackedField, curr_value: *T, used_symbols: *std.ArrayListUnmanaged(PackedSymbol), byte_offset: u8, bit_offset: *u8, base_bit_offset: u8) Error!void {
    while (bit_offset.* < (byte_offset + @sizeOf(T)) * 8) {
        const field = find_field: {
            for (fields) |field| {
                const actual_bit_offset = base_bit_offset + field.bit_offset;
                if (actual_bit_offset <= bit_offset.* and bit_offset.* < actual_bit_offset + field.bit_size) {
                    break :find_field field;
                }
            }

            // No fields remaining
            break;
        };
        defer bit_offset.* = @min(bit_offset.* + field.bit_size, (byte_offset + @sizeOf(T)) * 8);
        const shift_offset: std.math.Log2Int(T) = @intCast(bit_offset.* % @bitSizeOf(T));

        switch (field.value) {
            .value => |value| {
                curr_value.* |= @as(T, @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(value)) >> @intCast(bit_offset.* - field.bit_offset - base_bit_offset))) << shift_offset;
            },
            .variable => |symbol_idx| {
                // If there isn't a shift yet, crossing boundries is fine
                if (shift_offset != 0 and shift_offset + field.bit_size > @bitSizeOf(T)) {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .fields_cross_register_boundry,
                        .ast = ana.ast(),
                        .token = ana.ast().nodeToken(field.node_idx),
                        .extra = .{ .bit_size = 8 },
                    });
                    return error.AnalyzeFailed;
                }

                const target_symbol = ana.sema.getSymbol(symbol_idx);
                const target_type = switch (target_symbol.*) {
                    .variable => |var_sym| var_sym.type,
                    .register => |reg_sym| reg_sym.type,
                    else => unreachable,
                };

                if (target_type.size() % @sizeOf(T) != 0) {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .expected_mod_register_size,
                        .ast = ana.ast(),
                        .token = ana.ast().nodeToken(field.node_idx),
                        .extra = .{ .expected_register_size = .{
                            .expected = target_type.size() * 8,
                            .actual = @sizeOf(T) * 8,
                        } },
                    });
                    return error.AnalyzeFailed;
                }

                try used_symbols.append(ana.sema.allocator, .{
                    .symbol = symbol_idx,
                    .byte_offset = @intCast(@divFloor(bit_offset.* - field.bit_offset - base_bit_offset, @bitSizeOf(T))),
                    .shift_offset = shift_offset,
                });
            },
            .register => |register| {
                _ = register; // autofix
                @panic("TODO");
            },
            .packed_fields => |nested_fields| {
                try ana.resolvePackedFields(T, node_idx, nested_fields, curr_value, used_symbols, byte_offset, bit_offset, field.bit_offset + base_bit_offset);
            },
        }
    }
}

fn handleCall(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const target_ident = ana.ast().nodeToken(node_idx);
    const target_name = ana.ast().tokenSource(target_ident);

    const param_range = ana.ast().nodeData(node_idx).sub_range;
    const params: []const NodeIndex = @ptrCast(ana.ast().extra_data[@intFromEnum(param_range.extra_start)..@intFromEnum(param_range.extra_end)]);

    if (target_name[0] == '@') {
        // Built-in call
        if (BuiltinFn.get(target_name)) |builtin| {
            if (builtin.param_count != params.len) {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_arguments,
                    .ast = ana.ast(),
                    .token = ana.ast().nodeToken(node_idx),
                    .extra = .{ .arguments = .{
                        .expected = builtin.param_count,
                        .actual = @intCast(params.len),
                    } },
                });
                return error.AnalyzeFailed;
            }

            try builtin.handler_fn(ana, node_idx, params);
        } else {
            try ana.sema.errors.append(ana.sema.allocator, .{
                .tag = .invalid_builtin,
                .ast = ana.ast(),
                .token = ana.ast().nodeToken(node_idx),
            });
            return error.AnalyzeFailed;
        }
    } else {
        // Function / macro call
        const symbol, const symbol_idx = try ana.sema.resolveSymbol(target_ident, ana.module_idx);

        switch (symbol.*) {
            .function => |fn_sym| {
                // TODO: Clobber register data
                try ana.ir.append(ana.sema.allocator, .{
                    .tag = .{ .change_status_flags = fn_sym.calling_convention.status_flags },
                    .node = node_idx,
                });

                try ana.ir.append(ana.sema.allocator, .{
                    .tag = .{ .call = .{ .target = symbol_idx } },
                    .node = node_idx,
                });
            },
            // TODO: Support macros
            else => {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_fn_symbol,
                    .ast = ana.ast(),
                    .token = ana.ast().nodeToken(node_idx),
                    .extra = .{ .actual_symbol = symbol.* },
                });
                return error.AnalyzeFailed;
            },
        }
    }
}

fn handleWhile(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const while_statement = ana.ast().nodeData(node_idx).while_statement;

    if (while_statement.condition == .none) {
        // while (true)
        const loop_label = try std.fmt.allocPrint(ana.sema.allocator, "__while{}_loop", .{ana.while_loops});
        ana.while_loops += 1;

        try ana.ir.append(ana.sema.allocator, .{
            .tag = .{ .label = loop_label },
            .node = node_idx,
        });

        try ana.handleBlock(while_statement.block);

        try ana.ir.append(ana.sema.allocator, .{
            .tag = .{ .branch = .{
                .type = .always,
                .target_label = loop_label,
            } },
            .node = node_idx,
        });
    } else {
        // Runtime loop condition
        @panic("TODO");
    }
}

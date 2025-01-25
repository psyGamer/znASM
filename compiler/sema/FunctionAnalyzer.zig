const std = @import("std");
const Ast = @import("../Ast.zig");
const Module = @import("../Module.zig");
const NodeIndex = Ast.NodeIndex;
const Sema = @import("../Sema.zig");
const ExpressionIndex = Sema.ExpressionIndex;
const TypeExpressionIndex = Sema.TypeExpressionIndex;
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
const RegisterType = Ir.RegisterType;

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
    std.debug.assert(target != .none);

    if (target == .mem and ana.mem_size == mode or
        target == .idx and ana.idx_size == mode)
    {
        // Nothing to do
        return;
    }

    switch (target) {
        .none => unreachable,
        .mem => ana.mem_size = mode,
        .idx => ana.idx_size = mode,
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

    if (ana.module().symbol_map.get(label)) |existing_symbol_idx| {
        try ana.sema.errDuplicateSymbol(label_token, ana.module_idx, label, existing_symbol_idx, .label);
        return error.AnalyzeFailed;
    }
    for (ana.ir.items) |ir| {
        if (ir.tag != .label) {
            continue;
        }

        const existing_label = ir.tag.label;
        if (std.mem.eql(u8, label, existing_label)) {
            try ana.sema.emitError(label_token, ana.module_idx, .duplicate_label, .{label});
            try ana.sema.emitNote(ana.ast().nodeToken(ir.node), ana.module_idx, .existing_label, .{});
            return error.AnalyzeFailed;
        }
    }

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .label = try ana.sema.allocator.dupe(u8, label) },
        .node = node_idx,
    });
}

fn handleAssign(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const Target = union(enum) {
        builtin: BuiltinVar,
        global: struct {
            symbol: SymbolIndex,
            type: TypeExpressionIndex,
            bit_offset: u16,
        },
    };

    const assign = ana.ast().nodeData(node_idx).assign_statement;
    const data = ana.ast().readExtraData(Ast.Node.AssignStatementData, assign.extra);

    // Resolve assignment target
    const target: Target = get_target: {
        var symbol_idx: SymbolIndex = undefined;
        var current_type: TypeExpressionIndex = undefined;
        var bit_offset: u16 = 0;

        const field_range = ana.ast().nodeData(assign.target).sub_range;
        for (@intFromEnum(field_range.extra_start)..@intFromEnum(field_range.extra_end)) |extra_idx| {
            const field_token: Ast.TokenIndex = .cast(ana.ast().extra_data[extra_idx]);

            // Main target symbol
            if (extra_idx == @intFromEnum(field_range.extra_start)) {
                if (ana.ast().tokenTag(field_token) == .builtin_ident) {
                    // Built-in variable
                    if (BuiltinVar.get(ana.ast().parseIdentifier(field_token))) |builtin| {
                        // Built-ins don't support field access
                        std.debug.assert(extra_idx + 1 == @intFromEnum(field_range.extra_end));

                        break :get_target .{ .builtin = builtin };
                    } else {
                        try ana.sema.emitError(field_token, ana.module_idx, .invalid_builtin_var, .{ana.ast().tokenSource(field_token)});
                        return error.AnalyzeFailed;
                    }
                } else {
                    // User variable
                    const symbol, symbol_idx = try ana.sema.resolveSymbol(field_token, ana.module_idx);
                    current_type = switch (symbol.*) {
                        .constant => |const_sym| const_sym.type,
                        .variable => |var_sym| var_sym.type,
                        .register => |reg_sym| reg_sym.type,
                        else => {
                            try ana.sema.emitError(field_token, ana.module_idx, .expected_const_var_reg_symbol, .{@tagName(symbol.*)});
                            return error.AnalyzeFailed;
                        },
                    };
                    continue;
                }
            }

            // Fields
            const field_name = ana.ast().parseIdentifier(field_token);
            current_type = switch (ana.sema.getTypeExpression(current_type).*) {
                .@"struct" => |struct_symbol_idx| b: {
                    const struct_sym = ana.sema.getSymbol(struct_symbol_idx).@"struct";
                    for (struct_sym.fields) |field| {
                        if (std.mem.eql(u8, field.name, field_name)) {
                            break :b field.type;
                        }

                        bit_offset += field.type.byteSize(ana.sema) * 8;
                    }

                    try ana.sema.emitError(field_token, ana.module_idx, .unknown_field, .{ field_name, current_type.fmt(ana.sema) });
                    return error.AnalyzeFailed;
                },
                .@"packed" => |packed_symbol_idx| b: {
                    const packed_sym = ana.sema.getSymbol(packed_symbol_idx).@"packed";
                    for (packed_sym.fields) |field| {
                        if (std.mem.eql(u8, field.name, field_name)) {
                            break :b field.type;
                        }

                        bit_offset += field.type.bitSize(ana.sema);
                    }

                    try ana.sema.emitError(field_token, ana.module_idx, .unknown_field, .{ field_name, current_type.fmt(ana.sema) });
                    return error.AnalyzeFailed;
                },
                else => {
                    try ana.sema.emitError(field_token, ana.module_idx, .no_members, .{current_type.fmt(ana.sema)});
                    return error.AnalyzeFailed;
                },
            };
        }

        break :get_target .{ .global = .{
            .symbol = symbol_idx,
            .type = current_type,
            .bit_offset = bit_offset,
        } };
    };
    const target_type: TypeExpressionIndex = switch (target) {
        .builtin => |builtin| b: {
            try ana.sema.type_expressions.append(ana.sema.allocator, builtin.type);
            break :b @enumFromInt(@as(u32, @intCast(ana.sema.type_expressions.items.len - 1)));
        },
        .global => |global| global.type,
    };

    const value_expr: ExpressionIndex = try .resolve(ana.sema, target_type, data.value, ana.module_idx);

    const intermediate_register: RegisterType = get_register: {
        if (data.intermediate_register == .none) {
            break :get_register .none;
        }

        const register_name = ana.ast().parseIdentifier(data.intermediate_register);
        break :get_register std.meta.stringToEnum(RegisterType, register_name) orelse {
            try ana.sema.emitError(data.intermediate_register, ana.module_idx, .invalid_intermediate_register, .{register_name});
            return error.AnalyzeFailed;
        };
    };

    // Forward to built-in implementation
    if (target == .builtin) {
        if (!std.mem.containsAtLeast(RegisterType, target.builtin.allowed_registers, 1, &.{intermediate_register})) {
            if (intermediate_register != .none) {
                return ana.sema.failUnsupportedIntermediateRegister(ana.ast().nodeToken(ana.sema.getExpression(value_expr).node), data.intermediate_register, ana.module_idx, intermediate_register, target.builtin.allowed_registers, target.builtin.name);
            } else {
                try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .expected_intermediate_register, .{});
                return error.AnalyzeFailed;
            }
        }

        try target.builtin.write(ana, node_idx, value_expr, intermediate_register);
        return;
    }

    // Prepare intermediate register
    if (intermediate_register != .none) {
        try ana.setSizeMode(node_idx, switch (intermediate_register) {
            .a8, .a16 => .mem,
            .x8, .x16, .y8, .y16 => .idx,
            .none => unreachable,
        }, switch (intermediate_register) {
            .a8, .x8, .y8 => .@"8bit",
            .a16, .x16, .y16 => .@"16bit",
            .none => unreachable,
        });
    }

    var operations: std.ArrayListUnmanaged(struct { Ir.StoreOperation, NodeIndex }) = .empty;
    defer operations.deinit(ana.sema.allocator);

    try ana.storeValue(&operations, target_type, value_expr, target.global.bit_offset, intermediate_register);
    if (operations.items.len == 0) {
        return; // Nothing to do
    }

    // Validate operations
    var stack_fallback = std.heap.stackFallback(64, ana.sema.allocator);
    const stack_fallback_allocator = stack_fallback.get();

    const written_bytes = try stack_fallback_allocator.alloc(packed struct { any: bool, value: bool, variable: bool, cross_boundry: bool }, target_type.byteSize(ana.sema));
    defer stack_fallback_allocator.free(written_bytes);
    @memset(written_bytes, .{ .any = false, .value = false, .variable = false, .cross_boundry = false });

    for (operations.items) |entry| {
        const op = entry[0];

        const start_idx = @divFloor(op.bit_offset, 8);
        const end_idx = @divFloor(op.bit_offset + op.bit_size - 1, 8);

        for (start_idx..(end_idx + 1)) |i| {
            written_bytes[i].any = true;
        }

        // Check for supported registers
        switch (op.value) {
            .immediate => |value| {
                if (!value.eqlZero()) {
                    if (intermediate_register == .none) {
                        try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .expected_intermediate_register, .{});
                        return error.AnalyzeFailed;
                    }

                    for (start_idx..(end_idx + 1)) |i| {
                        written_bytes[i].value = true;
                    }
                }
            },
            .global => {
                if (intermediate_register == .none) {
                    try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .expected_intermediate_register, .{});
                    return error.AnalyzeFailed;
                }

                for (start_idx..(end_idx + 1)) |i| {
                    written_bytes[i].variable = true;
                }
            },
        }

        // Dectect crossing byte boundry
        if (op.bit_size > 8 or op.bit_offset % 8 > (op.bit_offset + op.bit_size - 1) % 8) {
            for (start_idx..end_idx) |i| {
                written_bytes[i].cross_boundry = true;
            }
        }
    }

    // Ensure writes are possible
    var i: usize = 0;
    while (i < written_bytes.len) : (i += 1) {
        const curr = written_bytes[i];
        if (!curr.any) {
            continue;
        }

        switch (intermediate_register) {
            .x8, .y8, .x16, .y16 => {
                if (curr.value and curr.variable) {
                    return ana.sema.failUnsupportedIntermediateRegister(ana.ast().nodeToken(node_idx), data.intermediate_register, ana.module_idx, intermediate_register, &.{ .a8, .a16 }, "object-initializer with mixed constants and variables");
                }
            },
            else => {},
        }

        // Split boundry-crossing operation
        const last_byte = if (intermediate_register == .a8 or intermediate_register == .x8 or intermediate_register == .y8 or intermediate_register == .none and ana.mem_size != .@"16bit")
            i
        else
            i + 1;
        if (last_byte <= written_bytes.len - 1 and written_bytes[last_byte].cross_boundry) {
            for (operations.items, 0..) |*entry, op_idx| {
                const op = &entry[0];

                if (@divFloor(op.bit_offset, 8) == last_byte and
                    op.bit_size > 8 or op.bit_offset % 8 > (op.bit_offset + op.bit_size - 1) % 8)
                {
                    const orig_value = op.value;

                    const register_size: u16 = switch (intermediate_register) {
                        .none => if (ana.mem_size == .@"16bit") 16 else 8,
                        else => intermediate_register.bitSize(),
                    };

                    const available_size = register_size - (op.bit_offset % register_size);
                    const remaining_size = op.bit_size - available_size;
                    op.bit_size = available_size;
                    op.value = switch (orig_value) {
                        .immediate => |value| b: {
                            var available_mask: std.math.big.int.Mutable = .init(try stack_fallback_allocator.alloc(std.math.big.Limb, 1), 1);
                            defer stack_fallback_allocator.free(available_mask.limbs);

                            const shift_capacity = available_mask.len + (available_size / @typeInfo(std.math.big.Limb).int.bits) + 1;
                            if (available_mask.limbs.len < shift_capacity) {
                                available_mask.limbs = try stack_fallback_allocator.realloc(available_mask.limbs, shift_capacity);
                            }
                            available_mask.shiftLeft(available_mask.toConst(), available_size);

                            const add_capacity = @max(available_mask.limbs.len, std.math.big.int.calcLimbLen(-1)) + 1;
                            if (available_mask.limbs.len < add_capacity) {
                                available_mask.limbs = try stack_fallback_allocator.realloc(available_mask.limbs, add_capacity);
                            }
                            available_mask.addScalar(available_mask.toConst(), -1);

                            var split_value: std.math.big.int.Mutable = .init(try ana.sema.allocator.alloc(std.math.big.Limb, 1), 0);
                            errdefer ana.sema.allocator.free(split_value.limbs);

                            const and_capacity = @min(value.limbs.len, available_mask.limbs.len);
                            if (split_value.limbs.len < and_capacity) {
                                split_value.limbs = try stack_fallback_allocator.realloc(split_value.limbs, and_capacity);
                            }
                            split_value.bitAnd(value, available_mask.toConst());

                            // Shrink allocation to avoid leaking excess capacity
                            if (ana.sema.allocator.resize(split_value.limbs, split_value.len)) {
                                break :b .{ .immediate = split_value.toConst() };
                            } else {
                                const limbs = try ana.sema.allocator.alloc(std.math.big.Limb, split_value.len);
                                @memcpy(limbs, split_value.limbs[0..split_value.len]);
                                defer ana.sema.allocator.free(split_value.limbs);

                                break :b .{ .immediate = .{
                                    .limbs = limbs,
                                    .positive = split_value.positive,
                                } };
                            }
                        },
                        .global => orig_value,
                    };

                    const new_op: Ir.StoreOperation = .{
                        .value = switch (orig_value) {
                            .immediate => |value| b: {
                                var split_value: std.math.big.int.Mutable = .init(try ana.sema.allocator.alloc(std.math.big.Limb, 1), 0);
                                errdefer ana.sema.allocator.free(split_value.limbs);

                                const shift_capacity = value.limbs.len - (available_size / (@sizeOf(std.math.big.Limb) * 8));
                                if (split_value.limbs.len < shift_capacity) {
                                    split_value.limbs = try stack_fallback_allocator.realloc(split_value.limbs, shift_capacity);
                                }
                                split_value.shiftRight(value, available_size);

                                // Shrink allocation to avoid leaking excess capacity
                                if (ana.sema.allocator.resize(split_value.limbs, split_value.len)) {
                                    break :b .{ .immediate = split_value.toConst() };
                                } else {
                                    const limbs = try ana.sema.allocator.alloc(std.math.big.Limb, split_value.len);
                                    @memcpy(limbs, split_value.limbs[0..split_value.len]);
                                    defer ana.sema.allocator.free(split_value.limbs);

                                    break :b .{ .immediate = .{
                                        .limbs = limbs,
                                        .positive = split_value.positive,
                                    } };
                                }
                            },
                            .global => |info| .{ .global = .{
                                .symbol = info.symbol,
                                .bit_offset = available_size,
                            } },
                        },

                        .bit_offset = @intCast((last_byte + 1) * 8),
                        .bit_size = remaining_size,
                    };
                    try operations.insert(ana.sema.allocator, op_idx + 1, .{ new_op, entry[1] });
                }
            }
        }

        if (intermediate_register == .a8 or intermediate_register == .x8 or intermediate_register == .y8 or intermediate_register == .none and ana.mem_size != .@"16bit" or i + 1 >= written_bytes.len) {
            for (operations.items) |*entry| {
                if (@divFloor(entry[0].bit_offset, 8) == i) {
                    entry[0].write_offset = @intCast(i);
                }
            }
            continue; // Otherwise, 8-bit writes always work
        }

        std.debug.assert(!(curr.value and curr.variable));

        const next = written_bytes[i + 1];
        const require_mem = (next.variable) or (curr.variable and next.value);

        if (curr.cross_boundry) {
            // Current write must be complete and not half
            if (require_mem and intermediate_register != .a16) {
                return ana.sema.failUnsupportedIntermediateRegister(ana.ast().nodeToken(node_idx), data.intermediate_register, ana.module_idx, intermediate_register, &.{ .a8, .a16 }, "object-initializer with mixed constants and variables");
            }
        }

        if (!next.any) {
            if (intermediate_register == .none) {
                // Lower memory-size to 8-bits
                try ana.setSizeMode(node_idx, .mem, .@"8bit");

                for (operations.items) |*entry| {
                    if (@divFloor(entry[0].bit_offset, 8) == i) {
                        entry[0].write_offset = @intCast(i);
                    }
                }
                continue;
            } else {
                try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .intermediate_register_too_large, .{@tagName(intermediate_register)});
                return error.AnalyzeFailed;
            }
        }

        if (require_mem and intermediate_register != .a16) {
            // Discard high-byte of current assignment with next write
            if (i + 2 < written_bytes.len and (written_bytes[i + 2].value or written_bytes[i + 2].variable)) {
                for (operations.items) |*entry| {
                    if (@divFloor(entry[0].bit_offset, 8) == i) {
                        entry[0].write_offset = @intCast(i);
                    }
                }
                continue;
            } else {
                return ana.sema.failUnsupportedIntermediateRegister(ana.ast().nodeToken(node_idx), data.intermediate_register, ana.module_idx, intermediate_register, &.{ .a8, .a16, .x8, .y8 }, "byte-aligned field initialization");
            }
        } else {
            for (operations.items) |*entry| {
                const op_idx = @divFloor(entry[0].bit_offset, 8);
                if (op_idx == i or op_idx == i + 1) {
                    entry[0].write_offset = @intCast(i);
                }
            }

            i += 1;
            continue;
        }
    }

    if (intermediate_register == .none and ana.mem_size == .none) {
        // Default to 8-bits since it's more common
        try ana.setSizeMode(node_idx, .mem, .@"8bit");
    }

    try ana.ir.ensureUnusedCapacity(ana.sema.allocator, operations.items.len + 1);
    ana.ir.appendAssumeCapacity(.{
        .tag = .{ .store = .{
            .intermediate_register = intermediate_register,
            .symbol = target.global.symbol,
            .operations = @intCast(operations.items.len),
        } },
        .node = node_idx,
    });

    // Emit in sorted order
    var curr_bit_offset: u16 = 0;
    for (0..operations.items.len) |_| {
        var closest_offset: u16 = std.math.maxInt(u16);
        var closest_idx: u16 = undefined;
        for (operations.items, 0..) |entry, idx| {
            const op = entry[0];

            if (op.bit_offset >= curr_bit_offset and op.bit_offset - curr_bit_offset < closest_offset) {
                closest_offset = op.bit_offset - curr_bit_offset;
                closest_idx = @intCast(idx);
            }
        }

        const op, const op_node = operations.items[closest_idx];
        ana.ir.appendAssumeCapacity(.{
            .tag = .{ .store_operation = op },
            .node = op_node,
        });

        curr_bit_offset = op.bit_offset + op.bit_size;
    }
}

fn storeValue(ana: *Analyzer, operations: *std.ArrayListUnmanaged(struct { Ir.StoreOperation, NodeIndex }), target_type: TypeExpressionIndex, expr_value: ExpressionIndex, bit_offset: u16, intermediate_register: RegisterType) Error!void {
    const expr = ana.sema.getExpression(expr_value);
    switch (expr.value) {
        .immediate => |value| {
            try operations.append(ana.sema.allocator, .{ .{
                .value = .{ .immediate = value },
                .bit_offset = bit_offset,
                .bit_size = target_type.bitSize(ana.sema),
            }, expr.node });
        },
        .symbol => |symbol| {
            try operations.append(ana.sema.allocator, .{
                .{
                    .value = .{
                        .global = .{
                            .symbol = symbol,
                            .bit_offset = 0, // TODO: Support bit_offset for source symbols
                        },
                    },
                    .bit_offset = bit_offset,
                    .bit_size = target_type.bitSize(ana.sema),
                },
                expr.node,
            });
        },
        .field_initializer => |field| {
            try ana.storeValue(operations, field.type, field.value, bit_offset + field.bit_offset, intermediate_register);
        },
        .object_initializer => |fields| {
            for (@intFromEnum(fields.fields_start)..@intFromEnum(fields.fields_end)) |field_idx| {
                try ana.storeValue(operations, target_type, @enumFromInt(@as(u32, @intCast(field_idx))), bit_offset, intermediate_register);
            }
        },
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
                try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .expected_arguments, .{ builtin.param_count, if (builtin.param_count == 1) "" else "s", params.len });
                return error.AnalyzeFailed;
            }

            try builtin.handler_fn(ana, node_idx, params);
        } else {
            try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .invalid_builtin_fn, .{target_name});
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
                try ana.sema.emitError(ana.ast().nodeToken(node_idx), ana.module_idx, .expected_fn_symbol, .{@tagName(symbol.*)});
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

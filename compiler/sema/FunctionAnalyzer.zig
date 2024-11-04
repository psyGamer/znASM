const std = @import("std");
const Ast = @import("../Ast.zig");
const NodeIndex = Ast.NodeIndex;
const Sema = @import("../Sema.zig");
const Ir = @import("../ir.zig").Ir;
const Symbol = @import("../symbol.zig").Symbol;
const SymbolLocation = @import("../symbol.zig").SymbolLocation;
const Instruction = @import("../instruction.zig").Instruction;
const Opcode = @import("../instruction.zig").Opcode;
const Relocation = @import("../CodeGen.zig").Relocation;
const BuiltinFn = @import("BuiltinFn.zig");

const Analyzer = @This();
const Error = Sema.AnalyzeError;

ast: *const Ast,
sema: *Sema,

symbol: *Symbol.Function,
symbol_location: SymbolLocation,

ir: std.ArrayListUnmanaged(Ir) = .empty,

/// Size of the A-Register / Memory instructions
mem_size: Instruction.SizeMode = .none,
/// Size of the X/Y-Registers
idx_size: Instruction.SizeMode = .none,

// Used to generate unique labels for loops
while_loops: u16 = 0,

pub fn handleFnDef(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    try ana.handleBlock(ana.ast.node_data[node_idx].fn_def.block);
}

pub fn handleBlock(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const range = ana.ast.node_data[node_idx].sub_range;
    for (range.extra_start..range.extra_end) |extra_idx| {
        const child_idx = ana.ast.extra_data[extra_idx];

        switch (ana.ast.node_tags[child_idx]) {
            .instruction => try ana.handleInstruction(child_idx),
            .label => try ana.handleLabel(child_idx),
            .call_statement => try ana.handleCall(child_idx),
            .while_statement => try ana.handleWhile(child_idx),
            else => unreachable,
        }
    }
}

pub fn handleInstruction(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const opcode_name = ana.ast.parseIdentifier(ana.ast.node_tokens[node_idx]);

    const opcode = get_opcode: {
        inline for (std.meta.fields(Instruction)) |field| {
            @setEvalBranchQuota(std.meta.fields(Opcode).len * 1000);
            const curr_opcode: Opcode = comptime get_tag: {
                for (std.meta.fields(Opcode)) |tag| {
                    if (std.mem.eql(u8, field.name, tag.name)) {
                        break :get_tag @enumFromInt(tag.value);
                    }
                }
                unreachable;
            };
            const mnemonic = field.name[0..3];

            if (std.mem.eql(u8, mnemonic, opcode_name)) {
                if (field.type == void and ana.ast.isToken(ana.ast.node_tokens[node_idx] + 1, .new_line)) {
                    break :get_opcode curr_opcode;
                }
                if (field.type == Instruction.Imm816) {
                    if (ana.ast.areTokens(ana.ast.node_tokens[node_idx] + 1, &.{ .int_literal, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                    if (ana.ast.areTokens(ana.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }
                if (field.type == Instruction.Addr16) {
                    if (ana.ast.areTokens(ana.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }

                if (curr_opcode == .bra or
                    curr_opcode == .jmp or curr_opcode == .jml or curr_opcode == .jsr or curr_opcode == .jsl)
                {
                    if (ana.ast.areTokens(ana.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }
            }
        }

        try ana.sema.errors.append(ana.sema.allocator, .{
            .tag = .invalid_opcode,
            .ast = ana.ast,
            .token = ana.ast.node_tokens[node_idx],
        });
        return;
    };

    const instr: Instruction, const reloc: ?Relocation = get_instr: switch (opcode) {
        inline else => |t| {
            const field = comptime find_field: {
                for (std.meta.fields(Instruction)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(t))) {
                        break :find_field field;
                    }
                }
                unreachable;
            };

            const operand_token = ana.ast.node_tokens[node_idx] + 1;

            const size_type = opcode.sizeType();
            const curr_size = switch (size_type) {
                .mem => ana.mem_size,
                .idx => ana.idx_size,
                .none => .none,
            };

            if (size_type != .none and curr_size == .none) {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .undefined_size_mode,
                    .ast = ana.ast,
                    .token = operand_token,
                    .extra = .{ .size_type = size_type },
                });
                return error.AnalyzeFailed;
            }

            if (field.type == void) {
                break :get_instr .{ @unionInit(Instruction, field.name, {}), null };
            }
            if (field.type == Instruction.Imm816) {
                if (ana.ast.token_tags[operand_token] == .int_literal) {
                    if (curr_size == .@"8bit") {
                        const value = try ana.sema.parseInt(u8, ana.ast, operand_token);
                        break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm8 = value }), null };
                    }
                    if (curr_size == .@"16bit") {
                        const value = try ana.sema.parseInt(u16, ana.ast, operand_token);
                        break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm16 = value }), null };
                    }
                }

                if (ana.ast.token_tags[operand_token] == .ident and ana.ast.token_tags[operand_token + 1] == .new_line) {
                    const value_sym = ana.sema.lookupSymbol(.{
                        .module = ana.symbol_location.module,
                        .name = ana.ast.parseIdentifier(operand_token),
                    }) orelse {
                        try ana.sema.errors.append(ana.sema.allocator, .{
                            .tag = .undefined_symbol,
                            .ast = ana.ast,
                            .token = operand_token,
                        });
                        return error.AnalyzeFailed;
                    };

                    switch (value_sym) {
                        .constant => |const_sym| {
                            if (curr_size == .@"8bit") {
                                if (const_sym.type != .raw or const_sym.type.raw != .unsigned_int or const_sym.type.raw.unsigned_int > 8) {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .expected_type,
                                        .ast = ana.ast,
                                        .token = operand_token,
                                        .extra = .{ .expected_type = .{
                                            .expected = .{ .raw = .{ .unsigned_int = 8 } },
                                            .actual = const_sym.type,
                                        } },
                                    });
                                    return error.AnalyzeFailed;
                                }

                                const value: u8 = @bitCast(const_sym.value[0..@sizeOf(u8)].*);
                                break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm8 = value }), null };
                            }
                            if (curr_size == .@"16bit") {
                                if (const_sym.type != .raw or const_sym.type.raw != .unsigned_int or const_sym.type.raw.unsigned_int > 16) {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .expected_type,
                                        .ast = ana.ast,
                                        .token = operand_token,
                                        .extra = .{ .expected_type = .{
                                            .expected = .{ .raw = .{ .unsigned_int = 16 } },
                                            .actual = const_sym.type,
                                        } },
                                    });
                                    return error.AnalyzeFailed;
                                }

                                const value_ptr: *const u16 = @ptrCast(@alignCast(const_sym.value.ptr));
                                // Mask-out undefined values for smaller types
                                const value: u16 = value_ptr.* & @as(u16, std.math.maxInt(u16)) >> @intCast((@sizeOf(u16) -| const_sym.value.len) * 8);
                                break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm16 = value }), null };
                            }

                            unreachable;
                        },
                        else => {
                            try ana.sema.errors.append(ana.sema.allocator, .{
                                .tag = .expected_const_var_symbol,
                                .ast = ana.ast,
                                .token = operand_token,
                                .extra = .{ .expected_symbol = value_sym },
                            });
                            return error.AnalyzeFailed;
                        },
                    }
                }
            }
            if (field.type == Instruction.Addr16) {
                if (ana.ast.token_tags[operand_token] == .ident and ana.ast.token_tags[operand_token + 1] == .new_line) {
                    const value_sym_loc: SymbolLocation = .{
                        .module = ana.symbol_location.module,
                        .name = ana.ast.parseIdentifier(operand_token),
                    };
                    const value_sym = ana.sema.lookupSymbol(value_sym_loc) orelse {
                        try ana.sema.errors.append(ana.sema.allocator, .{
                            .tag = .undefined_symbol,
                            .ast = ana.ast,
                            .token = operand_token,
                        });
                        return error.AnalyzeFailed;
                    };

                    switch (value_sym) {
                        .variable => |var_sym| {
                            if (curr_size == .@"8bit") {
                                if (var_sym.type != .raw or var_sym.type.raw != .unsigned_int or var_sym.type.raw.unsigned_int > 8) {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .expected_type,
                                        .ast = ana.ast,
                                        .token = operand_token,
                                        .extra = .{ .expected_type = .{
                                            .expected = .{ .raw = .{ .unsigned_int = 8 } },
                                            .actual = var_sym.type,
                                        } },
                                    });
                                    return error.AnalyzeFailed;
                                }
                            } else if (curr_size == .@"16bit") {
                                if (var_sym.type != .raw or var_sym.type.raw != .unsigned_int or var_sym.type.raw.unsigned_int > 16) {
                                    try ana.sema.errors.append(ana.sema.allocator, .{
                                        .tag = .expected_type,
                                        .ast = ana.ast,
                                        .token = operand_token,
                                        .extra = .{ .expected_type = .{
                                            .expected = .{ .raw = .{ .unsigned_int = 8 } },
                                            .actual = var_sym.type,
                                        } },
                                    });
                                    return error.AnalyzeFailed;
                                }
                            }

                            break :get_instr .{ @unionInit(Instruction, field.name, undefined), .{
                                .type = .addr16,
                                .target_sym = value_sym_loc,
                                .target_offset = 0,
                            } };
                        },
                        else => {
                            @panic("TODO: error");
                            // try ana.sema.errors.append(ana.sema.allocator, .{
                            //     .tag = .expected_const_var_symbol,
                            //     .ast = ana.ast,
                            //     .token = operand_token,
                            //     .extra = .{ .expected_symbol = value_sym },
                            // });
                            // return error.AnalyzeFailed;
                        },
                    }
                }
            }

            if (opcode == .bra or
                opcode == .jmp or opcode == .jml or opcode == .jsr or opcode == .jsl)
            {
                break :get_instr .{ @unionInit(Instruction, field.name, undefined), .{
                    .type = switch (opcode) {
                        .bra => .rel8,
                        .jmp, .jsr => .addr16,
                        .jml, .jsl => .addr24,
                        else => unreachable,
                    },
                    .target_sym = .parse(ana.ast.parseIdentifier(operand_token), ana.symbol_location.module),
                    .target_offset = 0,
                } };
            }

            unreachable;
        },
    };

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .instruction = .{ .instr = instr, .reloc = reloc } },
        .node = node_idx,
    });
}

pub fn handleLabel(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const label_token = ana.ast.node_tokens[node_idx];
    const label = ana.ast.parseIdentifier(label_token);

    if (ana.sema.lookupSymbol(.{ .module = ana.symbol_location.module, .name = label })) |existing_sym| {
        try ana.sema.errors.append(ana.sema.allocator, .{
            .tag = .duplicate_label,
            .ast = ana.ast,
            .token = label_token,
        });
        try ana.sema.errors.append(ana.sema.allocator, .{
            .tag = .existing_symbol,
            .is_note = true,
            .ast = ana.ast,
            .token = ana.ast.node_tokens[
                switch (existing_sym) {
                    .function => |existing_func| existing_func.node,
                    else => @panic("TODO"),
                }
            ],
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
                .ast = ana.ast,
                .token = label_token,
            });
            try ana.sema.errors.append(ana.sema.allocator, .{
                .tag = .existing_label,
                .is_note = true,
                .ast = ana.ast,
                .token = ana.ast.node_tokens[ir.node],
            });
            return error.AnalyzeFailed;
        }
    }

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .label = try ana.sema.allocator.dupe(u8, label) },
        .node = node_idx,
    });
}

fn handleCall(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const target_ident = ana.ast.node_tokens[node_idx];
    const target_name = ana.ast.tokenSource(target_ident);

    const param_range = ana.ast.node_data[node_idx].sub_range;
    const params = ana.ast.extra_data[param_range.extra_start..param_range.extra_end];

    if (target_name[0] == '@') {
        // Built-in call
        if (BuiltinFn.get(target_name)) |builtin| {
            if (builtin.param_count != params.len) {
                try ana.sema.errors.append(ana.sema.allocator, .{
                    .tag = .expected_arguments,
                    .ast = ana.ast,
                    .token = ana.ast.node_tokens[node_idx],
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
                .ast = ana.ast,
                .token = ana.ast.node_tokens[node_idx],
            });
            return error.AnalyzeFailed;
        }
    } else {
        // Function / macro call
        @panic("TODO");
    }
}

fn handleWhile(ana: *Analyzer, node_idx: NodeIndex) Error!void {
    const while_statement = ana.ast.node_data[node_idx].while_statement;

    if (while_statement.condition == Ast.null_node) {
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
                .target = loop_label,
            } },
            .node = node_idx,
        });
    } else {
        // Runtime loop condition
        @panic("TODO");
    }
}

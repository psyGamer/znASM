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
mem_size: Instruction.SizeMode = .@"8bit",
/// Size of the X/Y-Registers
idx_size: Instruction.SizeMode = .none,

// Used to generate unique labels for loops
while_loops: u16 = 0,

pub fn handleFnDef(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    try anal.handleBlock(anal.ast.node_data[node_idx].fn_def.block);
}

pub fn handleBlock(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    const range = anal.ast.node_data[node_idx].sub_range;
    for (range.extra_start..range.extra_end) |extra_idx| {
        const child_idx = anal.ast.extra_data[extra_idx];

        switch (anal.ast.node_tags[child_idx]) {
            .instruction => try anal.handleInstruction(child_idx),
            .label => try anal.handleLabel(child_idx),
            .call_statement => try anal.handleCall(child_idx),
            .while_statement => try anal.handleWhile(child_idx),
            else => unreachable,
        }
    }
}

pub fn handleInstruction(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    const opcode_name = anal.ast.parseIdentifier(anal.ast.node_tokens[node_idx]);

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
                if (field.type == void and anal.ast.isToken(anal.ast.node_tokens[node_idx] + 1, .new_line)) {
                    break :get_opcode curr_opcode;
                }
                if (field.type == Instruction.Imm816) {
                    if (anal.ast.areTokens(anal.ast.node_tokens[node_idx] + 1, &.{ .int_literal, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                    if (anal.ast.areTokens(anal.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }
                if (field.type == Instruction.Addr16) {
                    if (anal.ast.areTokens(anal.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }

                if (curr_opcode == .bra or
                    curr_opcode == .jmp or curr_opcode == .jml or curr_opcode == .jsr or curr_opcode == .jsl)
                {
                    if (anal.ast.areTokens(anal.ast.node_tokens[node_idx] + 1, &.{ .ident, .new_line })) {
                        break :get_opcode curr_opcode;
                    }
                }
            }
        }

        try anal.sema.errors.append(anal.sema.allocator, .{
            .tag = .invalid_opcode,
            .type = .err,
            .ast = anal.ast,
            .token = anal.ast.node_tokens[node_idx],
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

            const operand_token = anal.ast.node_tokens[node_idx] + 1;

            const size_type = opcode.sizeType();
            const curr_size = switch (size_type) {
                .mem => anal.mem_size,
                .idx => anal.idx_size,
                .none => .none,
            };

            if (size_type != .none and curr_size == .none) {
                try anal.sema.errors.append(anal.sema.allocator, .{
                    .tag = .undefined_size_mode,
                    .type = .err,
                    .ast = anal.ast,
                    .token = operand_token,
                    .extra = .{ .size_type = size_type },
                });
                return error.AnalyzeFailed;
            }

            if (field.type == void) {
                break :get_instr .{ @unionInit(Instruction, field.name, {}), null };
            }
            if (field.type == Instruction.Imm816) {
                if (anal.ast.token_tags[operand_token] == .int_literal) {
                    if (curr_size == .@"8bit") {
                        const value = try anal.sema.parseInt(u8, anal.ast, operand_token);
                        break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm8 = value }), null };
                    }
                    if (curr_size == .@"16bit") {
                        const value = try anal.sema.parseInt(u16, anal.ast, operand_token);
                        break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm16 = value }), null };
                    }
                }

                if (anal.ast.token_tags[operand_token] == .ident and anal.ast.token_tags[operand_token + 1] == .new_line) {
                    const value_sym = anal.sema.lookupSymbol(.{
                        .module = anal.symbol_location.module,
                        .name = anal.ast.parseIdentifier(operand_token),
                    }) orelse {
                        try anal.sema.errors.append(anal.sema.allocator, .{
                            .tag = .undefined_symbol,
                            .type = .err,
                            .ast = anal.ast,
                            .token = operand_token,
                        });
                        return error.AnalyzeFailed;
                    };

                    switch (value_sym) {
                        .constant => |const_sym| {
                            if (curr_size == .@"8bit") {
                                if (const_sym.type != .raw or const_sym.type.raw != .unsigned_int or const_sym.type.raw.unsigned_int > 8) {
                                    try anal.sema.errors.append(anal.sema.allocator, .{
                                        .tag = .expected_type,
                                        .type = .err,
                                        .ast = anal.ast,
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
                                    try anal.sema.errors.append(anal.sema.allocator, .{
                                        .tag = .expected_type,
                                        .type = .err,
                                        .ast = anal.ast,
                                        .token = operand_token,
                                        .extra = .{ .expected_type = .{
                                            .expected = .{ .raw = .{ .unsigned_int = 8 } },
                                            .actual = const_sym.type,
                                        } },
                                    });
                                    return error.AnalyzeFailed;
                                }

                                const value: u16 = @bitCast(const_sym.value[0..@sizeOf(u16)].*);
                                break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm16 = value }), null };
                            }

                            unreachable;
                        },
                        else => {
                            try anal.sema.errors.append(anal.sema.allocator, .{
                                .tag = .expected_const_var_symbol,
                                .type = .err,
                                .ast = anal.ast,
                                .token = operand_token,
                                .extra = .{ .expected_symbol = value_sym },
                            });
                            return error.AnalyzeFailed;
                        },
                    }
                }
            }
            if (field.type == Instruction.Addr16) {
                if (anal.ast.token_tags[operand_token] == .ident and anal.ast.token_tags[operand_token + 1] == .new_line) {
                    const value_sym_loc: SymbolLocation = .{
                        .module = anal.symbol_location.module,
                        .name = anal.ast.parseIdentifier(operand_token),
                    };
                    const value_sym = anal.sema.lookupSymbol(value_sym_loc) orelse {
                        try anal.sema.errors.append(anal.sema.allocator, .{
                            .tag = .undefined_symbol,
                            .type = .err,
                            .ast = anal.ast,
                            .token = operand_token,
                        });
                        return error.AnalyzeFailed;
                    };

                    switch (value_sym) {
                        .variable => |var_sym| {
                            if (curr_size == .@"8bit") {
                                if (var_sym.type != .raw or var_sym.type.raw != .unsigned_int or var_sym.type.raw.unsigned_int > 8) {
                                    try anal.sema.errors.append(anal.sema.allocator, .{
                                        .tag = .expected_type,
                                        .type = .err,
                                        .ast = anal.ast,
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
                                    try anal.sema.errors.append(anal.sema.allocator, .{
                                        .tag = .expected_type,
                                        .type = .err,
                                        .ast = anal.ast,
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
                            // try anal.sema.errors.append(anal.sema.allocator, .{
                            //     .tag = .expected_const_var_symbol,
                            //     .type = .err,
                            //     .ast = anal.ast,
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
                    .target_sym = .parse(anal.ast.parseIdentifier(operand_token), anal.symbol_location.module),
                    .target_offset = 0,
                } };
            }

            unreachable;
        },
    };

    try anal.ir.append(anal.sema.allocator, .{
        .tag = .{ .instruction = .{ .instr = instr, .reloc = reloc } },
        .node = node_idx,
    });
}

pub fn handleLabel(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    const label_token = anal.ast.node_tokens[node_idx];
    const label = anal.ast.parseIdentifier(label_token);

    if (anal.sema.lookupSymbol(.{ .module = anal.symbol_location.module, .name = label })) |existing_sym| {
        try anal.sema.errors.append(anal.sema.allocator, .{
            .tag = .duplicate_label,
            .type = .err,
            .ast = anal.ast,
            .token = label_token,
        });
        try anal.sema.errors.append(anal.sema.allocator, .{
            .tag = .existing_symbol,
            .type = .note,
            .ast = anal.ast,
            .token = anal.ast.node_tokens[
                switch (existing_sym) {
                    .function => |existing_func| existing_func.node,
                    else => @panic("TODO"),
                }
            ],
        });
        return error.AnalyzeFailed;
    }
    for (anal.ir.items) |ir| {
        if (ir.tag != .label) {
            continue;
        }

        const existing_label = ir.tag.label;
        if (std.mem.eql(u8, label, existing_label)) {
            try anal.sema.errors.append(anal.sema.allocator, .{
                .tag = .duplicate_label,
                .type = .err,
                .ast = anal.ast,
                .token = label_token,
            });
            try anal.sema.errors.append(anal.sema.allocator, .{
                .tag = .existing_label,
                .type = .note,
                .ast = anal.ast,
                .token = anal.ast.node_tokens[ir.node],
            });
            return error.AnalyzeFailed;
        }
    }

    try anal.ir.append(anal.sema.allocator, .{
        .tag = .{ .label = try anal.sema.allocator.dupe(u8, label) },
        .node = node_idx,
    });
}

fn handleCall(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    const target_ident = anal.ast.node_tokens[node_idx];
    const target_name = anal.ast.tokenSource(target_ident);

    const param_range = anal.ast.node_data[node_idx].sub_range;
    const params = anal.ast.extra_data[param_range.extra_start..param_range.extra_end];

    if (target_name[0] == '@') {
        // Built-in call
        if (BuiltinFn.get(target_name)) |builtin| {
            if (builtin.param_count != params.len) {
                try anal.sema.errors.append(anal.sema.allocator, .{
                    .tag = .expected_arguments,
                    .type = .err,
                    .ast = anal.ast,
                    .token = anal.ast.node_tokens[node_idx],
                    .extra = .{ .arguments = .{
                        .expected = builtin.param_count,
                        .actual = @intCast(params.len),
                    } },
                });
                return error.AnalyzeFailed;
            }

            try builtin.handler_fn(anal, node_idx, params);
        } else {
            try anal.sema.errors.append(anal.sema.allocator, .{
                .tag = .invalid_builtin,
                .type = .err,
                .ast = anal.ast,
                .token = anal.ast.node_tokens[node_idx],
            });
            return error.AnalyzeFailed;
        }
    } else {
        // Function / macro call
        @panic("TODO");
    }
}

fn handleWhile(anal: *Analyzer, node_idx: NodeIndex) Error!void {
    const while_statement = anal.ast.node_data[node_idx].while_statement;

    if (while_statement.condition == Ast.null_node) {
        // while (true)
        const loop_label = try std.fmt.allocPrint(anal.sema.allocator, "__while{}_loop", .{anal.while_loops});
        anal.while_loops += 1;

        try anal.ir.append(anal.sema.allocator, .{
            .tag = .{ .label = loop_label },
            .node = node_idx,
        });

        try anal.handleBlock(while_statement.block);

        try anal.ir.append(anal.sema.allocator, .{
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

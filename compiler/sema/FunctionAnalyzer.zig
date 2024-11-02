const std = @import("std");
const Ast = @import("../Ast.zig");
const NodeIndex = Ast.NodeIndex;
const Sema = @import("../Sema.zig");
const Ir = @import("../ir.zig").Ir;
const Symbol = @import("../symbol.zig").Symbol;
const SymbolLocation = @import("../symbol.zig").SymbolLocation;
const Instruction = @import("../instruction.zig").Instruction;
const InstructionType = @import("../instruction.zig").InstructionType;
const Relocation = @import("../CodeGen.zig").Relocation;
const BuiltinFn = @import("BuiltinFn.zig");

const Analyzer = @This();

ast: *const Ast,
sema: *Sema,

func: *Symbol.Function,
sym_loc: SymbolLocation,

ir: std.ArrayListUnmanaged(Ir) = .empty,

// State

/// Size of the A-Register / Memory instructions
mem_size: Instruction.SizeMode = .none,
/// Size of the X/Y-Registers
idx_size: Instruction.SizeMode = .none,

pub fn handleFnDef(anal: *Analyzer, node_idx: NodeIndex) !void {
    try anal.handleBlock(anal.ast.node_data[node_idx].fn_def.block);
}

pub fn handleBlock(anal: *Analyzer, node_idx: NodeIndex) !void {
    const range = anal.ast.node_data[node_idx].sub_range;
    for (range.extra_start..range.extra_end) |extra_idx| {
        const child_idx = anal.ast.extra_data[extra_idx];

        switch (anal.ast.node_tags[child_idx]) {
            .instruction => try anal.handleInstruction(child_idx),
            .label => try anal.handleLabel(child_idx),
            .call => try anal.handleCall(child_idx),
            else => unreachable,
        }
    }
}

pub fn handleInstruction(anal: *Analyzer, node_idx: NodeIndex) !void {
    const opcode_name = anal.ast.parseIdentifier(anal.ast.node_tokens[node_idx]);

    const opcode = get_opcode: {
        inline for (std.meta.fields(Instruction)) |field| {
            @setEvalBranchQuota(std.meta.fields(InstructionType).len * 1000);
            const curr_opcode: InstructionType = comptime get_tag: {
                for (std.meta.fields(InstructionType)) |tag| {
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
                if (field.type == Instruction.Imm816 and anal.ast.areTokens(anal.ast.node_tokens[node_idx] + 1, &.{ .int_literal, .new_line })) {
                    break :get_opcode curr_opcode;
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

            if (field.type == void) {
                break :get_instr .{ @unionInit(Instruction, field.name, {}), null };
            }
            if (field.type == Instruction.Imm816) {
                const size_type = opcode.size_type();
                const curr_size = switch (size_type) {
                    .mem => anal.mem_size,
                    .idx => anal.idx_size,
                    .none => unreachable,
                };

                if (curr_size == .none) {
                    try anal.sema.errors.append(anal.sema.allocator, .{
                        .tag = .undefined_size_mode,
                        .type = .err,
                        .ast = anal.ast,
                        .token = operand_token,
                        .extra = .{ .size_type = size_type },
                    });
                    return;
                }
                if (curr_size == .@"8bit") {
                    const value = try anal.sema.parseInt(u8, anal.ast, operand_token);
                    break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm8 = value }), null };
                }
                if (curr_size == .@"16bit") {
                    const value = try anal.sema.parseInt(u16, anal.ast, operand_token);
                    break :get_instr .{ @unionInit(Instruction, field.name, .{ .imm16 = value }), null };
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
                    .target_sym = .parse(anal.ast.parseIdentifier(operand_token), anal.sym_loc.module),
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

pub fn handleLabel(anal: *Analyzer, node_idx: NodeIndex) !void {
    const label_token = anal.ast.node_tokens[node_idx];
    const label = anal.ast.parseIdentifier(label_token);

    if (anal.sema.lookupSymbol(.{ .module = anal.sym_loc.module, .name = label })) |existing_sym| {
        try anal.sema.errors.append(anal.sema.allocator, .{
            .tag = .duplicate_label,
            .type = .err,
            .ast = anal.ast,
            .token = label_token,
        });
        try anal.sema.errors.append(anal.sema.allocator, .{
            .tag = .existing_sym,
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
        .tag = .{ .label = label },
        .node = node_idx,
    });
}

fn handleCall(anal: *Analyzer, node_idx: NodeIndex) !void {
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

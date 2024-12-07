//! Provides built-in functions to call
const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const Instruction = @import("../instruction.zig").Instruction;
const BranchRelocation = @import("../CodeGen.zig").BranchRelocation;
const Analyzer = @import("FunctionAnalyzer.zig");
const BuiltinFn = @This();

const HandlerFn = fn (*Analyzer, NodeIndex, []const NodeIndex) Sema.AnalyzeError!void;

name: []const u8,
param_count: u8,
handler_fn: *const HandlerFn,

/// Retrieves the built-in function with the specified name
pub fn get(name: []const u8) ?BuiltinFn {
    for (&all) |builtin| {
        if (std.mem.eql(u8, name, builtin.name)) {
            return builtin;
        }
    }

    return null;
}

pub const all = [_]BuiltinFn{
    .{
        .name = "@setMemorySize",
        .param_count = 1,
        .handler_fn = sizeHandler(.mem),
    },
    .{
        .name = "@setIndexSize",
        .param_count = 1,
        .handler_fn = sizeHandler(.idx),
    },
    .{
        .name = "@branchAlways",
        .param_count = 1,
        .handler_fn = branchHandler(.always),
    },
    .{
        .name = "@branchCarrySet",
        .param_count = 1,
        .handler_fn = branchHandler(.carry_set),
    },
    .{
        .name = "@branchCarryClear",
        .param_count = 1,
        .handler_fn = branchHandler(.carry_clear),
    },
    .{
        .name = "@branchOverflowSet",
        .param_count = 1,
        .handler_fn = branchHandler(.overflow_set),
    },
    .{
        .name = "@branchOverflowClear",
        .param_count = 1,
        .handler_fn = branchHandler(.overflow_clear),
    },
    .{
        .name = "@branchEqual",
        .param_count = 1,
        .handler_fn = branchHandler(.equal),
    },
    .{
        .name = "@branchNotEqual",
        .param_count = 1,
        .handler_fn = branchHandler(.not_equal),
    },
    .{
        .name = "@branchPlus",
        .param_count = 1,
        .handler_fn = branchHandler(.plus),
    },
    .{
        .name = "@branchMinus",
        .param_count = 1,
        .handler_fn = branchHandler(.minus),
    },
    .{
        .name = "@pushValue",
        .param_count = 1,
        .handler_fn = pushValueHandler,
    },
    .{
        .name = "@pushA",
        .param_count = 0,
        .handler_fn = instructionHandler(.pha),
    },
    .{
        .name = "@pushX",
        .param_count = 0,
        .handler_fn = instructionHandler(.phx),
    },
    .{
        .name = "@pushY",
        .param_count = 0,
        .handler_fn = instructionHandler(.phy),
    },
    .{
        .name = "@pushDataBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.phb),
    },
    .{
        .name = "@pushDirectPage",
        .param_count = 0,
        .handler_fn = instructionHandler(.phd),
    },
    .{
        .name = "@pushProgramBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.phk),
    },
    .{
        .name = "@pushProcessorStatus",
        .param_count = 0,
        .handler_fn = instructionHandler(.php),
    },
    .{
        .name = "@pullA",
        .param_count = 0,
        .handler_fn = instructionHandler(.pla),
    },
    .{
        .name = "@pullX",
        .param_count = 0,
        .handler_fn = instructionHandler(.plx),
    },
    .{
        .name = "@pullY",
        .param_count = 0,
        .handler_fn = instructionHandler(.ply),
    },
    .{
        .name = "@pullDataBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.plb),
    },
    .{
        .name = "@pullDirectPage",
        .param_count = 0,
        .handler_fn = instructionHandler(.pld),
    },
    .{
        .name = "@pullProcessorStatus",
        .param_count = 0,
        .handler_fn = instructionHandler(.plp),
    },
};

fn sizeHandler(comptime target: Instruction.SizeType) HandlerFn {
    return struct {
        pub fn handler(ana: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
            const size_node = params[0];
            const size_literal = try ana.sema.expectToken(ana.ast, size_node, .int_literal);
            const size_value = try ana.sema.parseInt(u8, ana.ast, size_literal);

            const size_mode: Instruction.SizeMode = switch (size_value) {
                8 => .@"8bit",
                16 => .@"16bit",
                else => {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .invalid_size_mode,
                        .ast = ana.ast,
                        .token = size_literal,
                    });
                    return error.AnalyzeFailed;
                },
            };

            try ana.ir.append(ana.sema.allocator, .{
                .tag = .{ .change_size = .{
                    .target = target,
                    .mode = size_mode,
                } },
                .node = node_idx,
            });

            switch (target) {
                .mem => ana.mem_size = size_mode,
                .idx => ana.idx_size = size_mode,
                .none => unreachable,
            }
        }
    }.handler;
}

fn branchHandler(comptime branch_type: BranchRelocation.Type) HandlerFn {
    return struct {
        pub fn handler(ana: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
            const target_node = params[0];
            const target_ident = try ana.sema.expectToken(ana.ast, target_node, .ident);
            const target_name = ana.ast.tokenSource(target_ident);

            try ana.ir.append(ana.sema.allocator, .{
                .tag = .{ .branch = .{
                    .type = branch_type,
                    .target = target_name,
                } },
                .node = node_idx,
            });
        }
    }.handler;
}

fn instructionHandler(comptime instruction: Instruction) HandlerFn {
    return struct {
        pub fn handler(ana: *Analyzer, node_idx: NodeIndex, _: []const NodeIndex) Sema.AnalyzeError!void {
            try ana.ir.append(ana.sema.allocator, .{
                .tag = .{ .instruction = .{
                    .instr = instruction,
                    .reloc = null,
                } },
                .node = node_idx,
            });
        }
    }.handler;
}

fn pushValueHandler(ana: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
    const size_node = params[0];
    const size_expr = try ana.sema.resolveExprValue(ana.ast, size_node, .{ .raw = .{ .unsigned_int = 16 } }, ana.symbol_location.module);
    const size_value = size_expr.resolve(u16, ana.sema);

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .instruction = .{
            .instr = .{ .pea = size_value },
            .reloc = null,
        } },
        .node = node_idx,
    });
}

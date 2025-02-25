//! Provides built-in functions to call
const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const Instruction = @import("../instruction.zig").Instruction;
const BranchRelocation = @import("../CodeGen.zig").BranchRelocation;
const Analyzer = @import("FunctionAnalyzer.zig");
const BuiltinFn = @This();

const HandlerFn = fn (*Analyzer, NodeIndex, []const NodeIndex) Sema.AnalyzeError!void;

pub const Tag = enum {
    branch_always,
    branch_carry_set,
    branch_carry_clear,
    branch_overflow_set,
    branch_overflow_clear,
    branch_equal,
    branch_not_equal,
    branch_plus,
    branch_minus,
    jump,
    long_jump,
    push_value,
    push_a,
    push_x,
    push_y,
    push_data_bank,
    push_direct_page,
    push_program_bank,
    push_processor_status,
};

tag: Tag,
name: []const u8,
param_count: u8, // TODO: Provide types for parameters
handler_fn: *const HandlerFn,

/// Retrieves the built-in function with the specified name
pub fn get(name: []const u8) ?BuiltinFn {
    for (all.values) |builtin| {
        if (std.mem.eql(u8, name, builtin.name)) {
            return builtin;
        }
    }

    return null;
}

/// Retrieves the built-in function for the specific tag
pub fn getTag(tag: Tag) BuiltinFn {
    return all.get(tag);
}

const all = std.EnumArray(Tag, BuiltinFn).init(.{
    // .{
    //     .name = "@setMemorySize",
    //     .param_count = 1,
    //     .handler_fn = sizeHandler(.mem),
    // },
    // .{
    //     .name = "@setIndexSize",
    //     .param_count = 1,
    //     .handler_fn = sizeHandler(.idx),
    // },
    .branch_always = .{
        .tag = .branch_always,
        .name = "@branchAlways",
        .param_count = 1,
        .handler_fn = branchHandler(.always),
    },
    .branch_carry_set = .{
        .tag = .branch_carry_set,
        .name = "@branchCarrySet",
        .param_count = 1,
        .handler_fn = branchHandler(.carry_set),
    },
    .branch_carry_clear = .{
        .tag = .branch_carry_clear,
        .name = "@branchCarryClear",
        .param_count = 1,
        .handler_fn = branchHandler(.carry_clear),
    },
    .branch_overflow_set = .{
        .tag = .branch_overflow_set,
        .name = "@branchOverflowSet",
        .param_count = 1,
        .handler_fn = branchHandler(.overflow_set),
    },
    .branch_overflow_clear = .{
        .tag = .branch_overflow_clear,
        .name = "@branchOverflowClear",
        .param_count = 1,
        .handler_fn = branchHandler(.overflow_clear),
    },
    .branch_equal = .{
        .tag = .branch_equal,
        .name = "@branchEqual",
        .param_count = 1,
        .handler_fn = branchHandler(.equal),
    },
    .branch_not_equal = .{
        .tag = .branch_not_equal,
        .name = "@branchNotEqual",
        .param_count = 1,
        .handler_fn = branchHandler(.not_equal),
    },
    .branch_plus = .{
        .tag = .branch_plus,
        .name = "@branchPlus",
        .param_count = 1,
        .handler_fn = branchHandler(.plus),
    },
    .branch_minus = .{
        .tag = .branch_minus,
        .name = "@branchMinus",
        .param_count = 1,
        .handler_fn = branchHandler(.minus),
    },
    .jump = .{
        .tag = .jump,
        .name = "@jump",
        .param_count = 1,
        .handler_fn = branchHandler(.jump),
    },
    .long_jump = .{
        .tag = .long_jump,
        .name = "@longJump",
        .param_count = 1,
        .handler_fn = branchHandler(.jump_long),
    },
    .push_value = .{
        .tag = .push_value,
        .name = "@pushValue",
        .param_count = 1,
        .handler_fn = pushValueHandler,
    },
    .push_a = .{
        .tag = .push_a,
        .name = "@pushA",
        .param_count = 0,
        .handler_fn = instructionHandler(.pha),
    },
    .push_x = .{
        .tag = .push_x,
        .name = "@pushX",
        .param_count = 0,
        .handler_fn = instructionHandler(.phx),
    },
    .push_y = .{
        .tag = .push_y,
        .name = "@pushY",
        .param_count = 0,
        .handler_fn = instructionHandler(.phy),
    },
    .push_data_bank = .{
        .tag = .push_data_bank,
        .name = "@pushDataBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.phb),
    },
    .push_direct_page = .{
        .tag = .push_direct_page,
        .name = "@pushDirectPage",
        .param_count = 0,
        .handler_fn = instructionHandler(.phd),
    },
    .push_program_bank = .{
        .tag = .push_program_bank,
        .name = "@pushProgramBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.phk),
    },
    .push_processor_status = .{
        .tag = .push_processor_status,
        .name = "@pushProcessorStatus",
        .param_count = 0,
        .handler_fn = instructionHandler(.php),
    },
    .pull_a = .{
        .tag = .pull_a,
        .name = "@pullA",
        .param_count = 0,
        .handler_fn = instructionHandler(.pla),
    },
    .pull_x = .{
        .tag = .pull_x,
        .name = "@pullX",
        .param_count = 0,
        .handler_fn = instructionHandler(.plx),
    },
    .pull_y = .{
        .tag = .pull_y,
        .name = "@pullY",
        .param_count = 0,
        .handler_fn = instructionHandler(.ply),
    },
    .pull_data_bank = .{
        .tag = .pull_data_bank,
        .name = "@pullDataBank",
        .param_count = 0,
        .handler_fn = instructionHandler(.plb),
    },
    .pull_direct_page = .{
        .tag = .pull_direct_page,
        .name = "@pullDirectPage",
        .param_count = 0,
        .handler_fn = instructionHandler(.pld),
    },
    .pull_processor_status = .{
        .tag = .pull_processor_status,
        .name = "@pullProcessorStatus",
        .param_count = 0,
        .handler_fn = instructionHandler(.plp),
    },
});

// fn sizeHandler(comptime target: Instruction.SizeType) HandlerFn {
//     _ = target; // autofix
//     return struct {
//         pub fn handler(ana: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
//             const size_node = params[0];
//             const size_value = try ana.sema.parseInt(u8, ana.ast(), size_literal);

//             const size_mode: Instruction.SizeMode = switch (size_value) {
//                 8 => .@"8bit",
//                 16 => .@"16bit",
//                 else => {
//                     try ana.sema.errors.append(ana.sema.allocator, .{
//                         .tag = .invalid_size_mode,
//                         .ast = ana.ast(),
//                         .token = size_literal,
//                     });
//                     return error.AnalyzeFailed;
//                 },
//             };

//             try ana.ir.append(ana.sema.allocator, .{
//                 .tag = .{ .change_size = .{
//                     .target = target,
//                     .mode = size_mode,
//                 } },
//                 .node = node_idx,
//             });

//             switch (target) {
//                 .mem => ana.mem_size = size_mode,
//                 .idx => ana.idx_size = size_mode,
//                 .none => unreachable,
//             }
//         }
//     }.handler;
// }

fn branchHandler(comptime branch_type: BranchRelocation.Type) HandlerFn {
    return struct {
        pub fn handler(ana: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
            const target_node = params[0];
            const target_name = ana.ast().parseIdentifier(ana.ast().nodeToken(target_node));

            try ana.ir.append(ana.sema.allocator, .{
                .tag = .{ .branch = .{
                    .type = branch_type,
                    .target_label = target_name,
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
    const size_value = try ana.sema.parseInt(u16, ana.ast().nodeToken(size_node), ana.module_idx);

    try ana.ir.append(ana.sema.allocator, .{
        .tag = .{ .instruction = .{
            .instr = .{ .pea = size_value },
            .reloc = null,
        } },
        .node = node_idx,
    });
}

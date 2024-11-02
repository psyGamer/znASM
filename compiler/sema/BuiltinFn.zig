//! Provides built-in functions to call
const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
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
};

fn branchHandler(comptime branch_type: BranchRelocation.Type) HandlerFn {
    return struct {
        pub fn handler(anal: *Analyzer, node_idx: NodeIndex, params: []const NodeIndex) Sema.AnalyzeError!void {
            const target_node = params[0];
            const target_ident = try anal.sema.expectToken(anal.ast, target_node, .ident);
            const target_name = anal.ast.tokenSource(target_ident);

            try anal.ir.append(anal.sema.allocator, .{
                .tag = .{ .branch = .{
                    .type = branch_type,
                    .target = target_name,
                } },
                .node = node_idx,
            });
        }
    }.handler;
}

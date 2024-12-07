//! Provides built-in variables to map to hardware registers

const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const ExpressionValue = Sema.ExpressionValue;
const RegisterType = Sema.RegisterType;
const TypeSymbol = @import("../symbol.zig").TypeSymbol;
const Instruction = @import("../instruction.zig").Instruction;
const Analyzer = @import("FunctionAnalyzer.zig");
const BuiltinVar = @This();

const HandleWriteFn = fn (*Analyzer, NodeIndex, ExpressionValue, ?RegisterType) Sema.AnalyzeError!void;

name: []const u8,
type: TypeSymbol,
// read: ?HandleReadFn,
write: *const HandleWriteFn,
require_exact: bool,

/// Retrieves the built-in variable with the specified name
pub fn get(name: []const u8) ?BuiltinVar {
    for (&all) |builtin| {
        if (std.mem.eql(u8, name, builtin.name)) {
            return builtin;
        }
    }

    return null;
}

pub const all = [_]BuiltinVar{
    .{
        .name = "@stack_pointer",
        .type = .{ .raw = .{ .unsigned_int = 16 } },
        .write = handleStackPointerWrite,
        .require_exact = true,
    },
};

fn handleStackPointerWrite(ana: *Analyzer, node_idx: NodeIndex, expr: ExpressionValue, register: ?RegisterType) Sema.AnalyzeError!void {
    switch (expr) {
        .immediate, .symbol => {
            try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 3);
            ana.ir.appendAssumeCapacity(.{
                .tag = .{ .change_size = .{
                    .target = switch (register.?) {
                        .a8, .a16 => .mem,
                        .x8, .x16, .y8, .y16 => .idx,
                    },
                    .mode = switch (register.?) {
                        .a8, .x8, .y8 => .@"8bit",
                        .a16, .x16, .y16 => .@"16bit",
                    },
                } },
                .node = node_idx,
            });

            switch (register.?) {
                .a16 => {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load = .{
                            .target = register.?,
                            .value = expr,
                        } },
                        .node = node_idx,
                    });
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .instruction = .{ .instr = .tcs, .reloc = null } },
                        .node = node_idx,
                    });
                },
                .x8, .x16 => {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load = .{
                            .target = register.?,
                            .value = expr,
                        } },
                        .node = node_idx,
                    });
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .instruction = .{ .instr = .txs, .reloc = null } },
                        .node = node_idx,
                    });
                },
                else => {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .unsupported_register,
                        .ast = ana.ast,
                        .token = ana.ast.node_tokens[node_idx],
                        .extra = .{ .unsupported_register = .{
                            .register = register.?,
                            .message = "@stack_pointer",
                        } },
                    });
                    return error.AnalyzeFailed;
                },
            }
        },
        .register => |reg| {
            try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 1);

            switch (reg) {
                .a16 => {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .instruction = .{ .instr = .tcs, .reloc = null } },
                        .node = node_idx,
                    });
                },
                .x8, .x16 => {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .instruction = .{ .instr = .txs, .reloc = null } },
                        .node = node_idx,
                    });
                },
                else => {
                    try ana.sema.errors.append(ana.sema.allocator, .{
                        .tag = .unsupported_register,
                        .ast = ana.ast,
                        .token = ana.ast.node_tokens[node_idx],
                        .extra = .{ .unsupported_register = .{
                            .register = register.?,
                            .message = "@stack_pointer",
                        } },
                    });
                    return error.AnalyzeFailed;
                },
            }
        },
    }

    // ana.ir.appendAssumeCapacity(.{
    //     .tag = .{ .instruction = .{
    //         .instr = switch (register.?) {
    //             .a8, .a16 =>
    //         },
    //     } },
    //     .node = node_idx,
    // });
}

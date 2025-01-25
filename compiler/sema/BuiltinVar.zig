//! Provides built-in variables to map to hardware registers

const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const RegisterType = @import("../ir.zig").Ir.RegisterType;
const Analyzer = @import("FunctionAnalyzer.zig");
const TypeExpressionIndex = Sema.TypeExpressionIndex;
const TypeExpression = @import("type_expression.zig").TypeExpression;
const ExpressionIndex = Sema.ExpressionIndex;
const BuiltinVar = @This();

const HandleWriteFn = fn (*Analyzer, NodeIndex, ExpressionIndex, RegisterType) Sema.AnalyzeError!void;

name: []const u8,
type: TypeExpression,

/// List of allowed intermediate registers
allowed_registers: []const RegisterType,

// read: ?HandleReadFn,
write: *const HandleWriteFn,

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
        .type = .{ .integer = .{ .signedness = .unsigned, .bits = 16 } },
        .allowed_registers = &.{ .a16, .x8, .x16 },
        .write = handleStackPointerWrite,
    },
};

fn handleStackPointerWrite(ana: *Analyzer, node_idx: NodeIndex, expr_idx: ExpressionIndex, intermediate_register: RegisterType) Sema.AnalyzeError!void {
    const expr = ana.sema.getExpression(expr_idx).*;

    switch (expr.value) {
        .immediate, .symbol => {
            try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 3);
            try ana.setSizeMode(node_idx, switch (intermediate_register) {
                .a16 => .mem,
                .x8, .x16 => .idx,
                else => unreachable,
            }, switch (intermediate_register) {
                .x8 => .@"8bit",
                .a16, .x16 => .@"16bit",
                else => unreachable,
            });

            switch (expr.value) {
                .immediate => |value| {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load_value = .{
                            .register = intermediate_register,
                            .value = .{ .imm16 = value.to(u16) catch unreachable },
                        } },
                        .node = node_idx,
                    });
                },
                .symbol => |symbol_idx| {
                    ana.ir.appendAssumeCapacity(.{
                        .tag = .{ .load_variable = .{
                            .register = intermediate_register,
                            .symbol = symbol_idx,
                        } },
                        .node = node_idx,
                    });
                },
                else => unreachable,
            }

            switch (intermediate_register) {
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
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

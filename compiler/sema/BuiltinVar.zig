//! Provides built-in variables to map to hardware registers

const std = @import("std");
const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const RegisterType = @import("../codegen/AssemblyIr.zig").RegisterType;
const Analyzer = @import("FunctionAnalyzer.zig");
const TypeExpressionIndex = Sema.TypeExpressionIndex;
const TypeExpression = @import("type_expression.zig").TypeExpression;
const ExpressionIndex = Sema.ExpressionIndex;
const BuiltinVar = @This();

const HandleWriteFn = fn (*Analyzer, NodeIndex, ExpressionIndex) Sema.AnalyzeError!void;

pub const Tag = enum {
    stack_pointer,
};

tag: Tag,
name: []const u8,
type: TypeExpression,
type_idx: TypeExpression.Index = .none, // Will only be evaluated once used

/// List of allowed intermediate registers
allowed_registers: []const RegisterType,

// read: ?HandleReadFn,
write: *const HandleWriteFn,

/// Retrieves the built-in variable with the specified name
pub fn get(name: []const u8, sema: *Sema) !?BuiltinVar {
    for (&all.values) |*builtin| {
        if (std.mem.eql(u8, name, builtin.name)) {
            // Resolve type
            if (builtin.type_idx == .none) {
                builtin.type_idx = @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len)));
                try sema.type_expressions.append(sema.allocator, builtin.type);
            }

            return builtin.*;
        }
    }

    return null;
}

/// Retrieves the built-in variable for the specific tag
pub fn getTag(tag: Tag) BuiltinVar {
    const builtin = all.get(tag);
    std.debug.assert(builtin.type_idx != .none);
    return builtin;
}

var all = std.EnumArray(Tag, BuiltinVar).init(.{
    .stack_pointer = .{
        .tag = .stack_pointer,
        .name = "@stack_pointer",
        .type = .{ .integer = .{ .signedness = .unsigned, .bits = 16 } },
        .allowed_registers = &.{ .a16, .x8, .x16 },
        .write = handleStackPointerWrite,
    },
});

fn handleStackPointerWrite(ana: *Analyzer, node_idx: NodeIndex, expr_idx: ExpressionIndex) Sema.AnalyzeError!void {
    _ = ana; // autofix
    _ = node_idx; // autofix
    _ = expr_idx; // autofix
    // TODO:
    //     const expr = ana.sema.getExpression(expr_idx).*;

    //     switch (expr.value) {
    //         .immediate, .symbol => {
    //             try ana.ir.ensureUnusedCapacity(ana.sema.allocator, 3);
    //             try ana.setSizeMode(node_idx, switch (expr.intermediate_register) {
    //                 .a16 => .mem,
    //                 .x8, .x16 => .idx,
    //                 else => unreachable,
    //             }, switch (expr.intermediate_register) {
    //                 .x8 => .@"8bit",
    //                 .a16, .x16 => .@"16bit",
    //                 else => unreachable,
    //             });

    //             switch (expr.value) {
    //                 .immediate => |value| {
    //                     ana.ir.appendAssumeCapacity(.{
    //                         .tag = .{ .load_value = .{
    //                             .register = expr.intermediate_register,
    //                             .value = .{ .imm16 = value.to(u16) catch unreachable },
    //                         } },
    //                         .node = node_idx,
    //                     });
    //                 },
    //                 .symbol => |symbol_idx| {
    //                     ana.ir.appendAssumeCapacity(.{
    //                         .tag = .{ .load_variable = .{
    //                             .register = expr.intermediate_register,
    //                             .symbol = symbol_idx,
    //                         } },
    //                         .node = node_idx,
    //                     });
    //                 },
    //                 else => unreachable,
    //             }

    //             switch (expr.intermediate_register) {
    //                 .a16 => {
    //                     ana.ir.appendAssumeCapacity(.{
    //                         .tag = .{ .instruction = .{ .instr = .tcs, .reloc = null } },
    //                         .node = node_idx,
    //                     });
    //                 },
    //                 .x8, .x16 => {
    //                     ana.ir.appendAssumeCapacity(.{
    //                         .tag = .{ .instruction = .{ .instr = .txs, .reloc = null } },
    //                         .node = node_idx,
    //                     });
    //                 },
    //                 else => unreachable,
    //             }
    //         },
    //         else => unreachable,
    //     }
}

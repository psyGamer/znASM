//! Provides built-in variables to map to hardware registers
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const Expression = Sema.Expression;
const TypeExpression = Sema.TypeExpression;

const Analyzer = @import("FunctionAnalyzer.zig");
const BuiltinVar = @This();

const HandleWriteFn = fn (*Analyzer, NodeIndex, Expression.Index) Sema.AnalyzeError!void;

pub const Tag = enum {
    cpu_mode,
    stack_pointer,
};

tag: Tag,
name: []const u8,

/// List of allowed intermediate registers
allowed_registers: []const builtin_module.CpuRegister,

// read: ?HandleReadFn,
write: *const HandleWriteFn,

/// Retrieves the built-in variable with the specified name
pub fn getByName(name: []const u8, sema: *Sema) !?BuiltinVar {
    _ = sema; // autofix
    for (&all.values) |builtin| {
        if (std.mem.eql(u8, name, builtin.name)) {
            return builtin;
        }
    }

    return null;
}

/// Retrieves the built-in variable for the specific tag
pub fn getByTag(tag: Tag) BuiltinVar {
    const builtin = all.get(tag);
    std.debug.assert(builtin.type_idx != .none);
    return builtin;
}

pub fn getType(builtin: BuiltinVar, sema: *Sema) !TypeExpression.Index {
    return switch (builtin.tag) {
        .cpu_mode => try builtin_module.CpuMode.resolveType(sema),
        .stack_pointer => try sema.addTypeExpression(.{ .integer = .{ .signedness = .unsigned, .bits = 16 } }),
    };
}

var all = std.EnumArray(Tag, BuiltinVar).init(.{
    .cpu_mode = .{
        .tag = .cpu_mode,
        .name = "@cpu_mode",
        .allowed_registers = &.{},
        .write = handleCpuMode,
    },
    .stack_pointer = .{
        .tag = .stack_pointer,
        .name = "@stack_pointer",
        .allowed_registers = &.{ .a, .x },
        .write = handleStackPointerWrite,
    },
});

fn handleCpuMode(ana: *Analyzer, node: NodeIndex, value_expr: Expression.Index) Sema.AnalyzeError!void {
    _ = ana; // autofix
    _ = node; // autofix
    _ = value_expr; // autofix
    // try ana.emit(.{ .cpu_mode = try value_expr.toValue(builtin_module.CpuMode, ana.sema) }, node);
}
fn handleStackPointerWrite(ana: *Analyzer, node_idx: NodeIndex, expr_idx: Expression.Index) Sema.AnalyzeError!void {
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

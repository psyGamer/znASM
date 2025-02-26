//! Generates low-level Assembly IR from high-level Semantic IR of a function
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Module = @import("../Module.zig");
const Sema = @import("../Sema.zig");
const Symbol = Sema.Symbol;
const Expression = Sema.Expression;
const TypeExpression = Sema.TypeExpression;
const CodeGen = @import("../CodeGen.zig");

const SemanticIr = @import("../sema/SemanticIr.zig");
const AssemblyIr = @import("AssemblyIr.zig");
const Generator = @This();
const Error = CodeGen.GenerateError;

sema: *Sema,
symbol: Symbol.Index,

/// Interrupt vectors cannot rely on a calling convention
is_interrupt_vector: bool,

sir_index: usize = 0,
air: std.ArrayListUnmanaged(AssemblyIr) = .empty,

// /// Current size of the A-Register / Memory instructions
// mem_size: Instruction.SizeMode = .none,
// /// Current size of the X/Y-Registers
// idx_size: Instruction.SizeMode = .none,

pub fn process(gen: *Generator) Error!void {
    // TODO: Analyze livelieness

    const func = gen.getFunction();
    while (gen.sir_index < func.semantic_ir.len) : (gen.sir_index += 1) {
        const sir = func.semantic_ir[gen.sir_index];
        switch (sir.tag) {
            .begin_scope, .end_scope => {},

            .assign_global => try gen.handleAssignGlobal(sir),

            .cpu_mode => try gen.handleCpuMode(sir),

            .@"return" => try gen.emit(.@"return", sir.node),
            inline else => |_, t| @panic("TODO: " ++ @tagName(t)),
        }
    }
}
pub fn deinit(gen: *Generator) void {
    gen.air.deinit(gen.sema.allocator);
}

// Helper functions

pub inline fn getFunction(gen: Generator) *Symbol.Function {
    return &gen.symbol.get(gen.sema).function;
}

inline fn emit(gen: *Generator, tag: AssemblyIr.Tag, node: Node.Index) !void {
    try gen.air.append(gen.sema.allocator, .{ .tag = tag, .node = node });
}

// Semantic IR processing

fn handleAssignGlobal(gen: *Generator, sir: SemanticIr) Error!void {
    const assign_global = sir.tag.assign_global;
    //         var operations: std.ArrayListUnmanaged(struct { Ir.StoreOperation, Node.Index }) = .empty;
    // defer operations.deinit(ana.sema.allocator);

    // Resolve fields
    var target_type = switch (assign_global.symbol.get(gen.sema).*) {
        .constant => |const_sym| const_sym.type,
        .variable => |var_sym| var_sym.type,
        .register => |reg_sym| reg_sym.type,
        else => unreachable,
    };
    var bit_offset: u16 = 0;

    for (0..assign_global.field_target) |_| {
        gen.sir_index += 1;
        const field = gen.getFunction().semantic_ir[gen.sir_index].tag.field_reference;

        target_type = field.type;
        bit_offset += field.bit_offset;
    }

    const start_idx = gen.air.items.len;
    try gen.emit(.{ .store_symbol = .{
        .symbol = assign_global.symbol,
        .intermediate_register = assign_global.value.get(gen.sema).intermediate_register,
        .operations = undefined,
    } }, sir.node);

    try gen.generateStoreOperations(target_type, assign_global.value, bit_offset, sir.node);

    gen.air.items[start_idx].tag.store_symbol.operations = @intCast(gen.air.items.len - start_idx - 1);
}

fn generateStoreOperations(gen: *Generator, target_type: TypeExpression.Index, expr_value: Expression.Index, bit_offset: u16, node: Node.Index) Error!void {
    const expr = expr_value.get(gen.sema);
    switch (expr.value) {
        .immediate => |value| {
            try gen.emit(.{ .store_operation = .{
                .value = .{ .immediate = value },
                .bit_offset = bit_offset,
                .bit_size = target_type.bitSize(gen.sema),
            } }, node);
        },
        .symbol => |symbol| {
            try gen.emit(.{
                .store_operation = .{
                    .value = .{
                        .symbol = .{
                            .symbol = symbol,
                            .bit_offset = 0, // TODO: Field access for expressions
                        },
                    },
                    .bit_offset = bit_offset,
                    .bit_size = target_type.bitSize(gen.sema),
                },
            }, node);
        },
        .object_initializer => |init| {
            switch (target_type.get(gen.sema).*) {
                else => unreachable,
                inline .@"struct", .@"packed" => |symbol_idx, tag| {
                    const symbol = switch (tag) {
                        .@"struct" => symbol_idx.getStruct(gen.sema),
                        .@"packed" => symbol_idx.getPacked(gen.sema),
                        else => unreachable,
                    };
                    var curr_bit_offset = bit_offset;
                    for (@intFromEnum(init.fields_start)..@intFromEnum(init.fields_end), 0..) |field_expr, field_idx| {
                        const field_type = symbol.fields[field_idx].type;
                        try gen.generateStoreOperations(field_type, .cast(field_expr), curr_bit_offset, node);
                        curr_bit_offset += field_type.bitSize(gen.sema);
                    }
                },
            }
        },
    }
}

fn handleCpuMode(gen: *Generator, sir: SemanticIr) Error!void {
    switch (sir.tag.cpu_mode) {
        .native => try gen.emit(.{ .set_emulation_mode = false }, sir.node),
        .emulation => try gen.emit(.{ .set_emulation_mode = true }, sir.node),
    }
}

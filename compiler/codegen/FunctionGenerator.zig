//! Generates low-level Assembly IR from high-level Semantic IR of a function
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

const Ast = @import("../Ast.zig");
const Module = @import("../Module.zig");
const Sema = @import("../Sema.zig");
const Symbol = @import("../symbol.zig").Symbol;
const CodeGen = @import("../CodeGen.zig");
const NodeIndex = Ast.NodeIndex;

// const SemanticIr = @import("../sema/SemanticIr.zig");
const AssemblyIr = @import("AssemblyIr.zig");
const Generator = @This();
const Error = CodeGen.GenerateError;

sema: *Sema,
symbol: Symbol.Index,

/// Interrupt vectors cannot rely on a calling convention
is_interrupt_vector: bool,

ir: std.ArrayListUnmanaged(AssemblyIr) = .empty,

// /// Current size of the A-Register / Memory instructions
// mem_size: Instruction.SizeMode = .none,
// /// Current size of the X/Y-Registers
// idx_size: Instruction.SizeMode = .none,

pub fn process(gen: *Generator) Sema.AnalyzeError!void {
    try gen.emit(.{ .instruction = .{
        .instr = .rts,
        .reloc = null,
    } }, .none);
}
pub fn deinit(gen: *Generator) void {
    gen.ir.deinit(gen.sema.allocator);
}

// Helper functions

pub inline fn getFunction(gen: Generator) *Symbol.Function {
    return &gen.symbol.get(gen.sema).function;
}

inline fn emit(gen: *Generator, tag: AssemblyIr.Tag, node: NodeIndex) !void {
    try gen.ir.append(gen.sema.allocator, .{ .tag = tag, .node = node });
}

// Semantic IR processing

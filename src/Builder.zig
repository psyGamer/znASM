const std = @import("std");
const Symbol = @import("Function.zig").Symbol;
const BuildSystem = @import("BuildSystem.zig");
const Instruction = @import("instruction.zig").Instruction;

const Builder = @This();
const SymbolPtr = *const fn (b: *Builder) void;

build_system: *BuildSystem,

symbol: SymbolPtr,
instruction_data: std.ArrayListUnmanaged(u8),

pub fn init(sys: *BuildSystem, sym: Symbol) Builder {
    return .{
        .build_system = sys,

        .symbol = sym,
        .instruction_data = .{},
    };
}

pub fn build(b: *Builder) void {
    b.symbol(b);
}

// NOTE: This API intentionally doesn't expose OutOfMemory errors to keep the API simpler

pub fn emit(b: *Builder, instr: Instruction) void {
    instr.write_data(b.instruction_data.writer(b.build_system.allocator)) catch @panic("Out of memory");
    std.log.debug("    {}", .{b.instruction_data.items.len});
}

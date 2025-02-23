const std = @import("std");
const memory_map = @import("memory_map.zig");

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Symbol = @import("symbol.zig").Symbol;
const TypeSymbol = @import("symbol.zig").TypeSymbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const SymbolIndex = Sema.SymbolIndex;
const Sema = @import("Sema.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Ir = @import("codegen/AssemblyIr.zig");
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const FunctionGenerator = @import("codegen/FunctionGenerator.zig");
const FunctionBuilder = @import("codegen/FunctionBuilder.zig");

const CodeGen = @This();
pub const GenerateError = error{GenerateFailed} || std.mem.Allocator.Error;

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {
        /// Relative signed 8-bit offset to the symbol
        rel8,
        /// 16-bit Absolute address of the symbol
        addr16,
        /// 24-bit Long address of the symbol
        addr24,
    };

    type: Type,
    target_symbol: Sema.SymbolIndex,
    target_offset: u16 = 0,
};
/// Branches need to be relocated to point to their label and use the short / long form
pub const BranchRelocation = struct {
    pub const Type = enum {
        always,
        carry_set,
        carry_clear,
        overflow_set,
        overflow_clear,
        equal,
        not_equal,
        plus,
        minus,
        jump,
        jump_long,
    };

    type: Type,
    target_label: []const u8,
};
/// Metadata information about an instruction
pub const InstructionInfo = struct {
    instr: Instruction,
    /// Offset from the start of the function. Only defined once the assembly as been emitted
    offset: u16 = undefined,

    reloc: ?Relocation = null,
    branch_reloc: ?BranchRelocation = null,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode,

    /// Source-node for this instruction
    source: NodeIndex,
};

sema: *Sema,

/// Functions with genered Assembly IR in the order they were generated
/// Simple forward iteration ensures all dependency-functions have already been handled
generated_functions: std.ArrayListUnmanaged(Symbol.Index) = .empty,

pub fn process(codegen: *CodeGen) !void {
    inline for (std.meta.fields(@TypeOf(codegen.sema.interrupt_vectors))) |field| {
        if (@field(codegen.sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
            try codegen.generateFunction(vector_loc.symbol, true);
        }
    }

    for (codegen.generated_functions.items) |symbol| {
        try codegen.buildFunction(symbol);
    }
}

/// Generates Assembly IR for the target function
pub fn generateFunction(codegen: *CodeGen, symbol: Symbol.Index, is_interrupt_vector: bool) !void {
    var generator: FunctionGenerator = .{
        .sema = codegen.sema,
        .symbol = symbol,
        .is_interrupt_vector = is_interrupt_vector,
    };
    errdefer generator.deinit();

    try generator.process();

    const fn_sym = symbol.getFn(codegen.sema);
    fn_sym.assembly_ir = try generator.ir.toOwnedSlice(codegen.sema.allocator);

    try codegen.generated_functions.append(codegen.sema.allocator, symbol);
}

/// Builds assembly instructions for the target function
pub fn buildFunction(codegen: *CodeGen, symbol: Symbol.Index) !void {
    var builder: FunctionBuilder = .{
        .sema = codegen.sema,
        .symbol = symbol,

        // TODO
        .mem_size = .none,
        .idx_size = .none,

        // .mem_size = if (symbol.function.calling_convention.status_flags.mem_8bit) |mem_8bit|
        //     if (mem_8bit) .@"8bit" else .@"16bit"
        // else
        //     .none,
        // .idx_size = if (symbol.function.calling_convention.status_flags.idx_8bit) |idx_8bit|
        //     if (idx_8bit) .@"8bit" else .@"16bit"
        // else
        //     .none,
    };
    errdefer builder.instructions.deinit(codegen.sema.allocator);
    defer builder.labels.deinit(codegen.sema.allocator);

    try builder.build();
    // try builder.resolveBranchRelocs();

    const fn_sym = symbol.getFn(codegen.sema);
    fn_sym.assembly_data = try builder.genereateAssemblyData();
    // try builder.fixLabelRelocs();
    fn_sym.instructions = try builder.instructions.toOwnedSlice(codegen.sema.allocator);

    // const labels = try codegen.sema.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
    // for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
    //     label.* = .{ label_name, index };
    // }
    // symbol.function.labels = labels;
}

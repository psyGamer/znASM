const std = @import("std");
const builtin_module = @import("builtin_module.zig");
const memory_map = @import("memory_map.zig");

const Module = @import("Module.zig");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Sema = @import("Sema.zig");
const Symbol = Sema.Symbol;
const SymbolLocation = Sema.SymbolLocation;

const ErrorSystem = @import("error.zig").ErrorSystem;
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
        /// Relative signed 8-bit offset from the target
        rel8,
        /// Relative unsigned 8-bit offset to the Direct Page register
        rel8_dp,
        /// 16-bit Absolute address of the target
        addr16,
        /// 24-bit Long address of the target
        addr24,
    };

    type: Type,
    target_symbol: Symbol.Index,
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
    /// Offset of the Direct Page register
    direct_page: u16,

    /// Source-node for this instruction
    source: Node.Index,

    /// Writes the raw assembly bytes for this instruction into the writer
    pub fn writeAssembly(info: InstructionInfo, writer: anytype) !void {
        switch (info.instr) {
            inline else => |operand, opcode| {
                try writer.writeByte(@intFromEnum(opcode));

                const OperandType = @TypeOf(operand);
                const operand_info = @typeInfo(OperandType);

                if (OperandType == void) {
                    return; // No data associated
                }
                if (OperandType == Instruction.Imm816) {
                    const reg_size = switch (info.instr.immediateSizeType()) {
                        .mem => info.mem_size,
                        .idx => info.idx_size,
                        .none => unreachable,
                    };
                    switch (reg_size) {
                        .@"8bit" => try writer.writeInt(u8, operand.imm8, .little),
                        .@"16bit" => try writer.writeInt(u16, operand.imm16, .little),
                        .none => unreachable,
                    }
                    return;
                }
                if (operand_info == .int) {
                    try writer.writeInt(OperandType, operand, .little);
                    return;
                }
                if (operand_info == .@"struct" and operand_info.@"struct".layout == .@"packed" and operand_info.@"struct".backing_integer != null) {
                    try writer.writeInt(operand_info.@"struct".backing_integer.?, @bitCast(operand), .little);
                    return;
                }

                @compileError("Unsupported instruction operand type: " ++ @typeName(OperandType));
            },
        }
    }
};

sema: *Sema,

/// Functions with genered Assembly IR in the order they were generated
/// Simple forward iteration ensures all dependency-functions have already been handled
generated_functions: std.ArrayListUnmanaged(Symbol.Index) = .empty,

has_errors: bool = false,

pub fn process(codegen: *CodeGen) !void {
    std.log.info("== AIR GEN ==", .{});
    inline for (std.meta.fields(@TypeOf(codegen.sema.interrupt_vectors))) |field| {
        if (@field(codegen.sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
            try codegen.generateFunction(vector_loc.symbol, true);
        }
    }
    if (codegen.has_errors) {
        return error.GenerateFailed;
    }

    std.log.info("== ASM GEN ==", .{});
    for (codegen.generated_functions.items) |symbol| {
        try codegen.buildFunction(symbol);
    }
    if (codegen.has_errors) {
        return error.GenerateFailed;
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
    fn_sym.assembly_ir = try generator.air.toOwnedSlice(codegen.sema.allocator);

    std.log.debug("AIR for {}", .{codegen.sema.getSymbolLocation(symbol)});
    for (fn_sym.assembly_ir) |air| {
        std.log.debug(" - {}", .{air});
    }

    try codegen.generated_functions.append(codegen.sema.allocator, symbol);
}

/// Builds assembly instructions for the target function
pub fn buildFunction(codegen: *CodeGen, symbol: Symbol.Index) !void {
    var builder: FunctionBuilder = .{
        .codegen = codegen,
        .symbol = symbol,

        .callconv_frozen = is_vector: {
            inline for (std.meta.fields(@TypeOf(codegen.sema.interrupt_vectors))) |field| {
                if (@field(codegen.sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
                    if (vector_loc.symbol == symbol) {
                        break :is_vector true;
                    }
                }
            }

            break :is_vector false;
        },

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

    builder.build() catch |err| switch (err) {
        error.GenerateFailed => return,
        else => |e| return e,
    };
    // try builder.resolveBranchRelocs();

    const fn_sym = symbol.getFn(codegen.sema);
    fn_sym.assembly_data = try builder.genereateAssemblyData();
    // try builder.fixLabelRelocs();
    fn_sym.instructions = try builder.instructions.toOwnedSlice(codegen.sema.allocator);

    std.log.debug("ASM for {}", .{codegen.sema.getSymbolLocation(symbol)});
    for (fn_sym.instructions) |info| {
        std.log.debug(" - {}", .{info.instr});
    }

    // const labels = try codegen.sema.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
    // for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
    //     label.* = .{ label_name, index };
    // }
    // symbol.function.labels = labels;
}

// Error Handling

const Error = ErrorSystem(.{
    // .intermediate_register_too_large = "Intermediate register [!]{s}[] cannot write [!]single byte[] values",
    .idx_reg_long_read = "[!]X/Y Index Registers[] cannot read memory from other banks, because [!]long memory accesses aren't supported[]",
    .idx_reg_long_write = "[!]X/Y Index Registers[] cannot write memory to other banks, because [!]long memory accesses aren't supported[]",
});

fn beginError(codegen: *CodeGen, node: Node.Index, symbol: Symbol.Index, comptime msg_type: Error.MessageType) !Error.MessageContext {
    const common = symbol.getCommon(codegen.sema);
    const module = common.module_index.get(codegen.sema);
    return Error.begin(module, module.ast.nodeToken(node), msg_type);
}

pub fn emitError(codegen: *CodeGen, node: Node.Index, symbol: Symbol.Index, comptime tag: Error.Tag, args: anytype) !void {
    const err_ctx = try codegen.beginError(node, symbol, .err);
    try err_ctx.print(comptime Error.tagMessage(tag), args);
    try err_ctx.end();
}
pub fn emitNote(codegen: *CodeGen, node: Node.Index, symbol: Symbol.Index, comptime tag: Error.Tag, args: anytype) !void {
    const note_ctx = try codegen.beginError(node, symbol, .note);
    try note_ctx.print(comptime Error.tagMessage(tag), args);
    try note_ctx.end();
}

pub fn failUnsupportedIntermediateRegister(codegen: *CodeGen, node: Node.Index, symbol: Symbol.Index, used_register: builtin_module.CpuRegister, allowed_registers: []const builtin_module.CpuRegister, usage: []const u8) !void {
    const err_ctx = try codegen.beginError(node, symbol, .err);
    try err_ctx.print("Unsupported intermediate register [!]{s}[], for use with [!]{s}", .{ @tagName(used_register), usage });
    try err_ctx.end();

    try codegen.emitSupportedIntermediateRegisters(node, symbol, allowed_registers);

    return error.GenerateFailed;
}

pub fn emitSupportedIntermediateRegisters(codegen: *CodeGen, node: Node.Index, symbol: Symbol.Index, allowed_registers: []const builtin_module.CpuRegister) !void {
    const note_ctx = try codegen.beginError(node, symbol, .note);
    try note_ctx.print("Supported intermediate registers are: ", .{});
    for (allowed_registers, 0..) |register, i| {
        if (i > 0) {
            if (i == allowed_registers.len - 1) {
                try note_ctx.print(" and [!]{s}", .{@tagName(register)});
            } else {
                try note_ctx.print(", [!]{s}", .{@tagName(register)});
            }
        } else {
            try note_ctx.print("[!]{s}", .{@tagName(register)});
        }
    }
    try note_ctx.end();
}

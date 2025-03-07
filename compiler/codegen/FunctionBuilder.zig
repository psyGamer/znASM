//! Builds assembly instructions from low-level Assembly IR of a function
const std = @import("std");
const memory_map = @import("../memory_map.zig");

const Node = @import("../Ast.zig").Node;
const Sema = @import("../Sema.zig");
const Symbol = @import("../symbol.zig").Symbol;
const CodeGen = @import("../CodeGen.zig");
const InstructionInfo = CodeGen.InstructionInfo;
const Relocation = CodeGen.Relocation;
const BranchRelocation = CodeGen.BranchRelocation;
const AssemblyIr = @import("AssemblyIr.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Opcode = @import("../instruction.zig").Opcode;

const Builder = @This();
const Error = error{InstructionFailed} || CodeGen.GenerateError;

/// Index to a target instruction
pub const Label = u16;

/// Indicates how CPU status flags need to be updated, if at all
pub const ChangeStatusFlags = struct {
    carry: ?bool = null,
    zero: ?bool = null,
    irq_disable: ?bool = null,
    decimal: ?bool = null,
    idx_8bit: ?bool = null,
    mem_8bit: ?bool = null,
    overflow: ?bool = null,
    negative: ?bool = null,
};

codegen: *CodeGen,
symbol: Symbol.Index,

air_index: u32 = 0,

/// Assembly instructions for this function
instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,

/// CPU status flags which need to be set with the calling convention
calling_status: ChangeStatusFlags = .{},
/// Currently known state of the status flags
current_status: ChangeStatusFlags = .{},

/// Calling Convention cannot be changed anymore, due to this function being an interrupt vector / recursive
callconv_frozen: bool = false,

/// The direct page may temporarily be changed from the default $0000
/// Must be restored before returning
current_direct_page: u16 = 0x0000,

// BAD:

labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

/// Size of the A-Register / Memory instructions
mem_size: Instruction.SizeMode,
/// Size of the X/Y-Registers
idx_size: Instruction.SizeMode,

pub fn build(b: *Builder) CodeGen.GenerateError!void {
    // try b.emit(.none, .clc);
    // try b.emit(.none, .xce);

    const func = b.getFunction();
    var has_errors = false;

    while (b.air_index < func.assembly_ir.len) : (b.air_index += 1) {
        const air = func.assembly_ir[b.air_index];
        const result = switch (air.tag) {
            .instruction => b.handleInstruction(air),
            // .change_status_flags => try b.handleChangeStatusFlags(ir),
            .store_symbol => b.handleStore(air),
            .store_operation => {},
            // .load_value => try b.handleLoadValue(ir),
            // .load_variable => try b.handleLoadVariable(ir),
            // .store_variable => try b.handleStoreVariable(ir),
            // .zero_variable => try b.handleZeroVariable(ir),
            // .and_value => try b.handleAndValue(ir),
            // .and_variable => try b.handleAndVariable(ir),
            // .or_value => try b.handleOrValue(ir),
            // .or_variable => try b.handleOrVariable(ir),
            // .shift_accum_left => try b.handleShiftAccumLeft(ir),
            // .shift_accum_right => try b.handleShiftAccumRight(ir),
            // .rotate_accum_left => try b.handleRotateAccumLeft(ir),
            // .rotate_accum_right => try b.handleRotateAccumRight(ir),
            // .clear_bits => try b.handleClearBits(ir),
            // .call => try b.handleCall(ir),
            // .branch => try b.handleBranch(ir),
            // .label => try b.handleLabel(ir),
            .@"return" => b.emit(air.node, .rts), // TODO: Support long calling convention
            .set_emulation_mode => |flag| {
                if (flag) {
                    try b.emit(air.node, .sec);
                } else {
                    try b.emit(air.node, .clc);
                }
                try b.emit(air.node, .xce);
            },
            inline else => |_, tag| @panic("TODO: " ++ @tagName(tag)),
        };
        result catch |err| switch (err) {
            error.InstructionFailed => has_errors = true,
            else => |e| return e,
        };
    }

    if (has_errors) {
        return error.GenerateFailed;
    }
}

/// Generates the raw assembly bytes for all instructions
pub fn genereateAssemblyData(b: *Builder) ![]u8 {
    var data: std.ArrayListUnmanaged(u8) = .{};
    // TODO: Figure out good instruction/assembly bytes ratio
    try data.ensureTotalCapacity(b.allocator(), b.instructions.items.len * 2);

    const data_writer = data.writer(b.allocator());

    for (b.instructions.items) |*info| {
        info.offset = @intCast(data.items.len);
        try info.writeAssembly(data_writer);
    }

    return data.toOwnedSlice(b.allocator());
}

// Helper functions

inline fn sema(b: *Builder) *Sema {
    return b.codegen.sema;
}
inline fn allocator(b: *Builder) std.mem.Allocator {
    return b.sema().allocator;
}
inline fn getFunction(b: *Builder) *Symbol.Function {
    return b.symbol.getFn(b.sema());
}

fn emit(b: *Builder, node: Node.Index, instr: Instruction) !void {
    try b.instructions.append(b.allocator(), .{
        .instr = instr,

        .mem_size = if (b.current_status.mem_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .idx_size = if (b.current_status.idx_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .direct_page = b.current_direct_page,

        .source = node,
    });
}
fn emitReloc(b: *Builder, node: Node.Index, instr: Instruction, reloc: Relocation) !void {
    try b.instructions.append(b.allocator(), .{
        .instr = instr,
        .reloc = reloc,

        .mem_size = if (b.current_status.mem_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .idx_size = if (b.current_status.idx_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .direct_page = b.current_direct_page,

        .source = node,
    });
}

const MemoryAccessibility = enum { direct_page, absolute, long, stack };
fn determineMemoryAccesibilty(b: *Builder, symbol: Symbol.Index) MemoryAccessibility {
    const mapping_mode = b.sema().mapping_mode;
    const curr_bank = b.getFunction().bank;

    switch (symbol.get(b.sema()).*) {
        .variable => |var_sym| {
            const min_addr24 = memory_map.wramOffsetToAddr(var_sym.wram_offset_min);
            const max_addr24 = memory_map.wramOffsetToAddr(var_sym.wram_offset_max);
            const min_addr16: u16 = @truncate(min_addr24);
            const max_addr16: u16 = @truncate(max_addr24);

            if (memory_map.isAddressAccessibleInBank(mapping_mode, min_addr24, curr_bank) and
                memory_map.isAddressAccessibleInBank(mapping_mode, max_addr24, curr_bank))
            {
                if (min_addr16 >= b.current_direct_page and min_addr16 <= b.current_direct_page + std.math.maxInt(u8) and
                    max_addr16 >= b.current_direct_page and max_addr16 <= b.current_direct_page + std.math.maxInt(u8))
                {
                    return .direct_page;
                } else {
                    return .absolute;
                }
            } else {
                return .long;
            }
        },
        .register => |reg_sym| {
            if (memory_map.isAddressAccessibleInBank(mapping_mode, reg_sym.address, curr_bank)) {
                if (reg_sym.address >= b.current_direct_page and reg_sym.address <= b.current_direct_page + std.math.maxInt(u8)) {
                    return .direct_page;
                } else {
                    return .absolute;
                }
            } else {
                return .long;
            }
        },
        else => unreachable,
    }
}

/// Ensures the current CPU status flags are in a specific state
fn ensureStatusFlags(b: *Builder, node: Node.Index, target_status: ChangeStatusFlags) !void {
    var set: Instruction.StatusRegister = .{};
    var clear: Instruction.StatusRegister = .{};

    inline for (std.meta.fields(Instruction.StatusRegister)) |field| {
        if (@field(target_status, field.name)) |value| {
            if (!b.callconv_frozen and @field(b.calling_status, field.name) == null) {
                // Forward to calling convetion
                @field(b.calling_status, field.name) = @field(target_status, field.name);
            } else if (@field(b.current_status, field.name) == value) {
                // Already target value
            } else if (value) {
                @field(set, field.name) = true;
            } else {
                @field(clear, field.name) = true;
            }

            @field(b.current_status, field.name) = value;
        }
    }

    if (set.idx_8bit) {
        // TODO: Clobber 16-bit X/Y
    }

    if (set != @as(Instruction.StatusRegister, .{})) {
        try b.emit(node, .{ .sep = set });
    }
    if (clear != @as(Instruction.StatusRegister, .{})) {
        try b.emit(node, .{ .rep = clear });
    }
}

fn saveState(b: *Builder) Builder {
    return b.*;
}
fn restoreState(b: *Builder, state: Builder) void {
    const curr_instructions = b.instructions;
    b.* = state;
    b.instructions = curr_instructions;
    b.instructions.shrinkRetainingCapacity(state.instructions.items.len);
}

// Assembly IR processing

fn handleInstruction(b: *Builder, ir: AssemblyIr) !void {
    const instruction = ir.tag.instruction;
    try b.instructions.append(b.allocator(), .{
        .instr = instruction.instr,
        .reloc = instruction.reloc,

        .mem_size = if (b.current_status.mem_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .idx_size = if (b.current_status.idx_8bit) |flag|
            if (flag) .@"8bit" else .@"16bit"
        else
            .none,
        .direct_page = b.current_direct_page,

        .source = ir.node,
    });
}
fn handleChangeStatusFlags(b: *Builder, ir: AssemblyIr) !void {
    const change_flags = ir.tag.change_status_flags;

    var set: Instruction.StatusRegister = .{};
    var clear: Instruction.StatusRegister = .{};

    inline for (std.meta.fields(Instruction.StatusRegister)) |field| {
        if (@field(change_flags, field.name)) |value| {
            if (value) {
                @field(set, field.name) = true;
            } else {
                @field(clear, field.name) = true;
            }
        }
    }

    if (set != @as(Instruction.StatusRegister, .{})) {
        try b.emit(ir.node, .{ .sep = set });
    }
    if (clear != @as(Instruction.StatusRegister, .{})) {
        try b.emit(ir.node, .{ .rep = clear });
    }

    if (change_flags.mem_8bit) |mem_8bit| {
        b.mem_size = if (mem_8bit) .@"8bit" else .@"16bit";
    }
    if (change_flags.idx_8bit) |idx_8bit| {
        b.idx_size = if (idx_8bit) .@"8bit" else .@"16bit";
    }
}

const BuildConfiguration = enum { debug, release_safe, release_fast, release_small }; // Temporary location

/// Defines the result of a matrix with multiple combinations
const CodeMatrixResult = struct {
    pub const failure: @This() = .{ .bytes = undefined, .cycles = undefined, .uses_stz = undefined, .state = undefined, .instructions = &.{} };

    bytes: u16,
    cycles: u16,
    uses_stz: bool,

    state: Builder,
    instructions: []const InstructionInfo,

    pub fn fromState(b: *Builder, state: Builder, mem_size: Instruction.SizeMode, idx_size: Instruction.SizeMode) !CodeMatrixResult {
        const emitted_instructions = b.instructions.items[state.instructions.items.len..];

        var byte_count: u16 = 0;
        var cycle_count: u16 = 0;
        for (emitted_instructions) |instr| {
            const size_mode: Instruction.SizeMode = switch (instr.instr.immediateSizeType()) {
                .none => .none,
                .mem => mem_size,
                .idx => idx_size,
            };
            byte_count += instr.instr.getByteSize(size_mode);
            cycle_count += instr.instr.getCycleCount(mem_size == .@"16bit", idx_size == .@"16bit");
        }

        return .{
            .bytes = byte_count,
            .cycles = cycle_count,
            .uses_stz = mem_size != .none,

            .state = b.*,
            .instructions = try b.allocator().dupe(InstructionInfo, emitted_instructions),
        };
    }

    pub fn sortResults(results: []CodeMatrixResult, build_cfg: BuildConfiguration) void {
        switch (build_cfg) {
            .debug => std.mem.sortUnstable(CodeMatrixResult, results, {}, lessThanDebug),
            .release_safe, .release_fast => std.mem.sortUnstable(CodeMatrixResult, results, {}, lessThanReleaseFast),
            .release_small => std.mem.sortUnstable(CodeMatrixResult, results, {}, lessThanReleaseSmall),
        }
    }

    /// Prefer No STZs -> Least Cycles -> Least Bytes
    fn lessThanDebug(_: void, lhs: CodeMatrixResult, rhs: CodeMatrixResult) bool {
        // Check invalid
        if (lhs.instructions.len == 0) {
            return false;
        }
        if (rhs.instructions.len == 0) {
            return true;
        }

        // Check STZ
        if (!lhs.uses_stz and rhs.uses_stz) {
            return true;
        }
        if (lhs.uses_stz and !rhs.uses_stz) {
            return false;
        }

        // Check cycles
        if (lhs.cycles < rhs.cycles) {
            return true;
        }
        if (lhs.cycles > rhs.cycles) {
            return false;
        }

        // Check bytes
        if (lhs.bytes < rhs.bytes) {
            return true;
        }
        if (lhs.bytes > rhs.bytes) {
            return false;
        }

        return true; // Doesn't matter
    }

    /// Prefer Least Cycles -> Least Bytes -> No STZs
    fn lessThanReleaseFast(_: void, lhs: CodeMatrixResult, rhs: CodeMatrixResult) bool {
        // Check invalid
        if (lhs.instructions.len == 0) {
            return false;
        }
        if (rhs.instructions.len == 0) {
            return true;
        }

        // Check cycles
        if (lhs.cycles < rhs.cycles) {
            return true;
        }
        if (lhs.cycles > rhs.cycles) {
            return false;
        }

        // Check bytes
        if (lhs.bytes < rhs.bytes) {
            return true;
        }
        if (lhs.bytes > rhs.bytes) {
            return false;
        }

        // Check STZ
        if (!lhs.uses_stz and rhs.uses_stz) {
            return true;
        }
        if (lhs.uses_stz and !rhs.uses_stz) {
            return false;
        }

        return true; // Doesn't matter
    }

    /// Prefer Least Bytes -> Least Cycles -> No STZs
    fn lessThanReleaseSmall(_: void, lhs: CodeMatrixResult, rhs: CodeMatrixResult) bool {
        // Check invalid
        if (lhs.instructions.len == 0) {
            return false;
        }
        if (rhs.instructions.len == 0) {
            return true;
        }

        // Check bytes
        if (lhs.bytes < rhs.bytes) {
            return true;
        }
        if (lhs.bytes > rhs.bytes) {
            return false;
        }

        // Check cycles
        if (lhs.cycles < rhs.cycles) {
            return true;
        }
        if (lhs.cycles > rhs.cycles) {
            return false;
        }

        // Check STZ
        if (!lhs.uses_stz and rhs.uses_stz) {
            return true;
        }
        if (lhs.uses_stz and !rhs.uses_stz) {
            return false;
        }

        return true; // Doesn't matter
    }
};

fn handleStore(b: *Builder, air: AssemblyIr) Error!void {
    const operation_count = switch (air.tag) {
        inline .store_symbol => |info| info.operations,
        else => unreachable,
    };
    const int_reg = switch (air.tag) {
        inline .store_symbol => |info| info.intermediate_register,
        else => unreachable,
    };
    _ = int_reg; // autofix

    const operations = b.getFunction().assembly_ir[(b.air_index + 1)..(b.air_index + operation_count + 1)];

    var stack_fallback = std.heap.stackFallback(512, b.allocator());
    const stack_fallback_allocator = stack_fallback.get();

    const target_type = switch (air.tag) {
        .store_symbol => |info| switch (info.symbol.get(b.sema()).*) {
            .constant => |const_sym| const_sym.type,
            .variable => |var_sym| var_sym.type,
            .register => |reg_sym| reg_sym.type,
            else => unreachable,
        },
        else => unreachable,
    };
    const type_bytes = target_type.byteSize(b.sema());
    const type_bits = target_type.bitSize(b.sema());

    // Identify source operation of each assigned bit
    const OperationIndex = u16;

    var source_bits = try stack_fallback_allocator.alloc(OperationIndex, type_bytes * 8);
    defer stack_fallback_allocator.free(source_bits);

    @memset(source_bits, std.math.maxInt(OperationIndex));
    for (operations, 0..) |op_instr, op_idx| {
        const op = op_instr.tag.store_operation;
        @memset(source_bits[op.bit_offset..(op.bit_offset + op.bit_size)], @intCast(op_idx));
    }

    // Identify optimal sequence of 16-bit block writes
    const WordBlock = struct {
        bit_offset: u16,
        low_byte_defined: bool,
        high_byte_defined: bool,
    };

    var word_blocks: std.ArrayListUnmanaged(WordBlock) = try .initCapacity(stack_fallback_allocator, type_bytes - 1);
    defer word_blocks.deinit(stack_fallback_allocator);

    if (word_blocks.capacity > 0) {
        // Track which bytes are defined by which block
        const BlockIndex = enum(u8) { none = std.math.maxInt(u8), _ };
        const block_indices = try stack_fallback_allocator.alloc(BlockIndex, type_bytes);
        defer stack_fallback_allocator.free(block_indices);
        @memset(block_indices, .none);

        // 1st pass: Homogenous 16-bit writes

        var base_idx: u16 = 0;
        while (base_idx <= source_bits.len - 16) {
            const word_bits = source_bits[base_idx..][0..16];
            const valid_block = check: {
                const first_op = operations[word_bits[0]].tag.store_operation;
                switch (first_op.value) {
                    .immediate => {
                        // Different immedate values can still be merged
                        for (word_bits[1..]) |op_idx| {
                            if (operations[op_idx].tag.store_operation.value != .immediate) {
                                break :check false;
                            }
                        }
                        break :check true;
                    },
                    .symbol => |src_symbol| {
                        // Require sequential bit order in same symbol
                        const start_read_offset = base_idx - first_op.bit_offset + src_symbol.bit_offset;
                        for (word_bits[1..]) |op_idx| {
                            const op = operations[op_idx].tag.store_operation;
                            if (op.value != .symbol) {
                                break :check false;
                            }

                            const symbol = op.value.symbol;
                            if (symbol.symbol != src_symbol.symbol or base_idx + symbol.bit_offset < op.bit_offset or base_idx + symbol.bit_offset - op.bit_offset != start_read_offset) {
                                break :check false;
                            }
                        }
                        break :check true;
                    },
                }
            };

            if (valid_block) {
                const index: BlockIndex = @enumFromInt(@as(u8, @intCast(word_blocks.items.len)));
                block_indices[@divExact(base_idx, 8)..][0..2].* = @splat(index);

                word_blocks.appendAssumeCapacity(.{
                    .bit_offset = base_idx,
                    .low_byte_defined = true,
                    .high_byte_defined = true,
                });

                base_idx += 16;
            } else {
                base_idx += 8;
            }
        }

        // 2nd pass: Remaining 8-bit immediate values

        // Swap high/low byte for each store, so that 2x8-bit immediate values can be loaded at once later
        var is_high_byte = false;

        base_idx = 0;
        byte_iter: while (base_idx <= source_bits.len - 8) : (base_idx += 8) {
            if (block_indices[@divExact(base_idx, 8)] != .none) {
                continue;
            }

            for (source_bits[base_idx..][0..8]) |op_idx| {
                const op = operations[op_idx].tag.store_operation;
                if (op.value != .immediate) {
                    continue :byte_iter;
                }
            }

            // Last byte requires using high-byte
            if (base_idx == source_bits.len - 8) {
                is_high_byte = true;
            }

            const index: BlockIndex = @enumFromInt(@as(u8, @intCast(word_blocks.items.len)));
            const block: WordBlock = .{
                .bit_offset = base_idx - @as(u8, if (is_high_byte) 8 else 0),
                .low_byte_defined = !is_high_byte,
                .high_byte_defined = is_high_byte,
            };

            // Ensure undefined values are written before defined values
            const overwritten_idx = block_indices[if (is_high_byte) @divExact(base_idx, 8) - 1 else @divExact(base_idx, 8) + 1];
            if (overwritten_idx != .none) {
                std.log.err("{} <-> {} ({} | {})", .{ overwritten_idx, index, base_idx, word_blocks.items[@intFromEnum(overwritten_idx)] });
                //     word_blocks.insertAssumeCapacity(@intFromEnum(overwritten_idx), block);
                // word_blocks.appendAssumeCapacity(word_blocks.items[@intFromEnum(overwritten_idx)]);
                // word_blocks.items[@intFromEnum(overwritten_idx)] = block;

                const overwritten = word_blocks.orderedRemove(@intFromEnum(overwritten_idx));
                for (block_indices) |*idx| {
                    if (idx.* == .none) {
                        continue;
                    }

                    // Adjust indicies due to ordered remove
                    if (idx.* == overwritten_idx) {
                        idx.* = @enumFromInt(@as(u8, @intCast(word_blocks.items.len + 1)));
                    } else if (@intFromEnum(idx.*) > @intFromEnum(overwritten_idx)) {
                        idx.* = @enumFromInt(@intFromEnum(idx.*) - 1);
                    }
                }

                word_blocks.appendAssumeCapacity(block);
                word_blocks.appendAssumeCapacity(overwritten);

                block_indices[@divExact(base_idx, 8)] = @enumFromInt(@as(u8, @intCast(word_blocks.items.len - 2)));
            } else {
                word_blocks.appendAssumeCapacity(block);
                block_indices[@divExact(base_idx, 8)] = index;
            }

            is_high_byte = !is_high_byte;
        }

        // 3rd pass: Remaining 8-bit symbols

        // Last two bytes need to be both defined in last block
        if (block_indices[type_bytes - 1] == .none and block_indices[type_bytes - 2] == .none) {
            const index: BlockIndex = @enumFromInt(@as(u8, @intCast(word_blocks.items.len)));
            block_indices[(type_bytes - 2)..][0..2].* = @splat(index);

            word_blocks.appendAssumeCapacity(.{
                .bit_offset = type_bits - 16,
                .low_byte_defined = true,
                .high_byte_defined = true,
            });
        }

        base_idx = 0;
        while (base_idx <= source_bits.len - 8) : (base_idx += 8) {
            if (block_indices[@divExact(base_idx, 8)] != .none) {
                continue;
            }

            if (base_idx == source_bits.len - 8) {
                // Last byte requires using high-byte
                is_high_byte = true;
            } else {
                // Otherwise prefer low-byte, to avoid shifting
                is_high_byte = false;
            }

            const index: BlockIndex = @enumFromInt(@as(u8, @intCast(word_blocks.items.len)));
            const block: WordBlock = .{
                .bit_offset = base_idx - @as(u8, if (is_high_byte) 8 else 0),
                .low_byte_defined = !is_high_byte,
                .high_byte_defined = is_high_byte,
            };

            // Ensure undefined values are written before defined values
            const overwritten_idx = block_indices[if (is_high_byte) @divExact(base_idx, 8) - 1 else @divExact(base_idx, 8) + 1];
            if (overwritten_idx != .none) {
                std.log.err("{} <-> {} ({} / {})", .{ overwritten_idx, if (is_high_byte) @divExact(base_idx, 8) - 1 else @divExact(base_idx, 8) + 1, base_idx, word_blocks.items[@intFromEnum(overwritten_idx)] });
                // word_blocks.insertAssumeCapacity(@intFromEnum(overwritten_idx), block);
                // word_blocks.appendAssumeCapacity(word_blocks.items[@intFromEnum(overwritten_idx)]);
                // word_blocks.items[@intFromEnum(overwritten_idx)] = block;

                // for (block_indices) |*idx| {
                //     if (idx.* == overwritten_idx) {
                //         idx.* = index;
                //     }
                // }
                // block_indices[@divExact(base_idx, 8)] = overwritten_idx;

                const overwritten = word_blocks.orderedRemove(@intFromEnum(overwritten_idx));
                for (block_indices) |*idx| {
                    if (idx.* == .none) {
                        continue;
                    }

                    // Adjust indicies due to ordered remove
                    if (idx.* == overwritten_idx) {
                        idx.* = @enumFromInt(@as(u8, @intCast(word_blocks.items.len + 1)));
                    } else if (@intFromEnum(idx.*) > @intFromEnum(overwritten_idx)) {
                        idx.* = @enumFromInt(@intFromEnum(idx.*) - 1);
                    }
                }

                word_blocks.appendAssumeCapacity(block);
                word_blocks.appendAssumeCapacity(overwritten);

                block_indices[@divExact(base_idx, 8)] = @enumFromInt(@as(u8, @intCast(word_blocks.items.len - 2)));
            } else {
                word_blocks.appendAssumeCapacity(block);
                block_indices[@divExact(base_idx, 8)] = index;
            }
        }

        // Emit partial writes before full writes
        // Helps with merging 2x8-bit immedate value loads into a single instruction
        std.mem.sort(WordBlock, word_blocks.items, {}, struct {
            fn lessThan(_: void, lhs: WordBlock, rhs: WordBlock) bool {
                return @as(u2, @intFromBool(lhs.low_byte_defined)) + @intFromBool(lhs.high_byte_defined) < @as(u2, @intFromBool(rhs.low_byte_defined)) + @intFromBool(rhs.high_byte_defined);
            }
        }.lessThan);
    }

    std.log.info(" ~~~~~ WORD BLOCKS ~~~~~", .{});
    for (word_blocks.items) |block| {
        std.log.debug(" - {}", .{block});
    }
    std.log.info(" ~~~~~~~~~~~~~~~~~~~~~~~", .{});

    // TODO: Useful later

    // // Extract relevant bit
    // const read_offset = base_idx + bit_idx - op.bit_offset;
    // const bit: u1 = @truncate(op.value.immediate.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));

    // value |= @as(u8, bit) << @intCast(bit_idx);

    const target_access = switch (air.tag) {
        .store_symbol => |info| b.determineMemoryAccesibilty(info.symbol),
        else => unreachable,
    };
    _ = target_access; // autofix

    // switch (int_reg) {
    //     .none => {
    //         // Target needs to be accessible
    //         if (target_access == .long) {
    //             try b.codegen.emitError(air.node, b.symbol, .no_reg_long_write, .{});
    //             try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{.a});
    //             return error.InstructionFailed;
    //         }

    //         const mem_bit_sizes: [2]Instruction.SizeMode = .{ .@"8bit", .@"16bit" };
    //         var matrix_results: [mem_bit_sizes.len]CodeMatrixResult = undefined;

    //         matrix_loop: for (mem_bit_sizes, 0..) |mem_size, mem_i| {
    //             const register_bits: u8 = switch (mem_size) {
    //                 .@"8bit" => 8,
    //                 .@"16bit" => 16,
    //                 else => unreachable,
    //             };
    //             if (source_bits.len < register_bits) {
    //                 matrix_results[mem_i] = .failure;
    //                 continue;
    //             }

    //             const state = b.saveState();
    //             defer b.restoreState(state);

    //             try b.ensureStatusFlags(air.node, .{
    //                 .mem_8bit = switch (mem_size) {
    //                     .none => unreachable,
    //                     .@"8bit" => true,
    //                     .@"16bit" => false,
    //                 },
    //             });

    //             var write_offset: u16 = 0;
    //             while (write_offset <= source_bits.len - register_bits) : (write_offset += register_bits) {
    //                 if (std.mem.allEqual(OperationIndex, source_bits[write_offset..(write_offset + register_bits)], std.math.maxInt(OperationIndex))) {
    //                     continue; // Entire byte isn't written
    //                 }

    //                 // Require entire byte to be set to zero
    //                 for (source_bits[write_offset..(write_offset + register_bits)], 0..) |op_idx, bit_idx| {
    //                     const op = operations[op_idx].tag.store_operation;
    //                     if (op.value != .immediate) {
    //                         matrix_results[mem_i] = .failure;
    //                         continue :matrix_loop;
    //                     }

    //                     const read_offset = write_offset + bit_idx - op.bit_offset;
    //                     const bit: u1 = @truncate(op.value.immediate.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));

    //                     if (bit != 0) {
    //                         matrix_results[mem_i] = .failure;
    //                         continue :matrix_loop;
    //                     }
    //                 }

    //                 try b.emitReloc(air.node, switch (target_access) {
    //                     .direct_page => .{ .stz_dp = undefined },
    //                     .absolute => .{ .stz_addr16 = undefined },
    //                     .long, .stack => unreachable,
    //                 }, .{
    //                     .type = switch (target_access) {
    //                         .direct_page => .rel8_dp,
    //                         .absolute => .addr16,
    //                         .long, .stack => unreachable,
    //                     },
    //                     .target_symbol = air.tag.store_symbol.symbol,
    //                     .target_offset = @divExact(write_offset, 8),
    //                 });
    //             }

    //             matrix_results[mem_i] = try .fromState(b, state, mem_size, .none);
    //         }

    //         // Determine best result
    //         CodeMatrixResult.sortResults(&matrix_results, .debug);
    //         if (matrix_results[0].instructions.len == 0) {
    //             try b.codegen.emitError(air.node, b.symbol, .no_reg_non_zero, .{});
    //             try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{ .a, .x, .y });
    //             return error.InstructionFailed;
    //         }

    //         try b.instructions.appendSlice(b.allocator(), matrix_results[0].instructions);
    //         b.restoreState(matrix_results[0].state);
    //     },

    //     .a => @panic("TODO"),

    //     .x, .y => {
    //         // Target needs to be accessible
    //         if (target_access == .long) {
    //             try b.codegen.emitError(air.node, b.symbol, .idx_reg_long_write, .{});
    //             try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{.a});
    //             return error.InstructionFailed;
    //         }

    //         // Variables have to be byte-aligned
    //         for (operations) |op_instr| {
    //             const op = op_instr.tag.store_operation;
    //             if (op.value == .symbol and op.bit_offset % 8 != 0) {
    //                 return b.codegen.failUnsupportedIntermediateRegister(air.node, b.symbol, int_reg, &.{.a}, "non-byte-aligned assignment target with variable");
    //             }
    //             if (op.value == .symbol and op.value.symbol.bit_offset % 8 != 0) {
    //                 return b.codegen.failUnsupportedIntermediateRegister(air.node, b.symbol, int_reg, &.{.a}, "non-byte-aligned assignment source with variable");
    //             }
    //         }

    //         // Determine fastest instructions for none/mem8/mem16 | idx8/idx16 matix
    //         const mem_bit_sizes: [3]Instruction.SizeMode = .{ .none, .@"8bit", .@"16bit" };
    //         const idx_bit_sizes: [2]Instruction.SizeMode = .{ .@"8bit", .@"16bit" };

    //         var matrix_results: [mem_bit_sizes.len * idx_bit_sizes.len]CodeMatrixResult = undefined;

    //         for (mem_bit_sizes, 0..) |mem_size, mem_i| {
    //             for (idx_bit_sizes, 0..) |idx_size, idx_i| {
    //                 const result_idx = mem_i + idx_i * mem_bit_sizes.len;

    //                 // Last two bytes may not be mixed in 16-bit mode, unless a STZ is applicable
    //                 if (idx_size == .@"16bit") {
    //                     const is_invalid = check: {
    //                         const last_idx = source_bits[(source_bits.len - 8)];
    //                         const last_op = operations[last_idx].tag.store_operation;

    //                         if (last_op.value == .immediate) {
    //                             const last_read_offset = source_bits.len - 8 - last_op.bit_offset;
    //                             const last_byte: u8 = @truncate(last_op.value.immediate.limbs[@divFloor(last_read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(last_read_offset % @bitSizeOf(std.math.big.Limb)));
    //                             if (last_byte == 0 and mem_size == .@"8bit") {
    //                                 break :check false; // Can use STZ
    //                             }
    //                         }

    //                         if (source_bits.len < 16) {
    //                             break :check true; // Register too big
    //                         }

    //                         const second_last_idx = source_bits[source_bits.len - 16];
    //                         const second_last_op = operations[second_last_idx].tag.store_operation;

    //                         if (last_idx == second_last_idx) {
    //                             break :check false; // Not mixed
    //                         }
    //                         if (last_op.value == .immediate and second_last_op.value == .immediate) {
    //                             break :check false; // Can be combined
    //                         }

    //                         break :check true; // Not possible
    //                     };

    //                     if (is_invalid) {
    //                         matrix_results[result_idx] = .failure;
    //                         continue;
    //                     }
    //                 }

    //                 const state = b.saveState();
    //                 defer b.restoreState(state);

    //                 try b.ensureStatusFlags(air.node, .{
    //                     .mem_8bit = switch (mem_size) {
    //                         .none => null,
    //                         .@"8bit" => true,
    //                         .@"16bit" => false,
    //                     },
    //                     .idx_8bit = switch (idx_size) {
    //                         .none => unreachable,
    //                         .@"8bit" => true,
    //                         .@"16bit" => false,
    //                     },
    //                 });

    //                 const register_bits: u8 = switch (idx_size) {
    //                     .@"8bit" => 8,
    //                     .@"16bit" => 16,
    //                     else => unreachable,
    //                 };

    //                 var write_offset: u16 = 0;
    //                 while (write_offset <= source_bits.len - register_bits) {
    //                     if (idx_size == .@"8bit") {
    //                         // Always perform full 8-bit write
    //                         const op = operations[source_bits[write_offset]].tag.store_operation;
    //                         const read_offset = write_offset - op.bit_offset;

    //                         // Special case: Use 16-bit STZ if available
    //                         if (mem_size == .@"16bit" and source_bits.len >= 16 and write_offset <= source_bits.len - 16) {
    //                             const second_op = operations[source_bits[write_offset + 8]].tag.store_operation;
    //                             const second_read_offset = write_offset + 8 - second_op.bit_offset;

    //                             if (op.value == .immediate and second_op.value == .immediate) {

    //                                 // Extract relevant bytes from full value
    //                                 const first_byte: u8 = @truncate(op.value.immediate.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));
    //                                 const second_byte: u8 = @truncate(second_op.value.immediate.limbs[@divFloor(second_read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(second_read_offset % @bitSizeOf(std.math.big.Limb)));

    //                                 if (first_byte == 0 and second_byte == 0) {
    //                                     try b.emitReloc(air.node, switch (target_access) {
    //                                         .direct_page => .{ .stz_dp = undefined },
    //                                         .absolute => .{ .stz_addr16 = undefined },
    //                                         .long, .stack => unreachable,
    //                                     }, .{
    //                                         .type = switch (target_access) {
    //                                             .direct_page => .rel8_dp,
    //                                             .absolute => .addr16,
    //                                             .long, .stack => unreachable,
    //                                         },
    //                                         .target_symbol = air.tag.store_symbol.symbol,
    //                                         .target_offset = @divExact(write_offset, 8),
    //                                     });

    //                                     write_offset += 16;
    //                                     continue;
    //                                 }
    //                             }
    //                         }

    //                         switch (op.value) {
    //                             .immediate => |value| {
    //                                 // Extract relevant byte from full value
    //                                 const byte: u8 = @truncate(value.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));

    //                                 if (byte == 0 and mem_size != .none) {
    //                                     // Optimize for zero-values
    //                                     try b.emitReloc(air.node, switch (target_access) {
    //                                         .direct_page => .{ .stz_dp = undefined },
    //                                         .absolute => .{ .stz_addr16 = undefined },
    //                                         .long, .stack => unreachable,
    //                                     }, .{
    //                                         .type = switch (target_access) {
    //                                             .direct_page => .rel8_dp,
    //                                             .absolute => .addr16,
    //                                             .long, .stack => unreachable,
    //                                         },
    //                                         .target_symbol = air.tag.store_symbol.symbol,
    //                                         .target_offset = @divExact(write_offset, 8),
    //                                     });

    //                                     write_offset += 8;
    //                                     continue;
    //                                 }

    //                                 try b.emit(air.node, switch (int_reg) {
    //                                     .x => .{ .ldx_imm = .{ .imm8 = byte } },
    //                                     .y => .{ .ldy_imm = .{ .imm8 = byte } },
    //                                     else => unreachable,
    //                                 });
    //                             },
    //                             .symbol => |source| {
    //                                 const source_access = b.determineMemoryAccesibilty(source.symbol);
    //                                 if (source_access == .long) {
    //                                     try b.codegen.emitError(air.node, b.symbol, .idx_reg_long_read, .{});
    //                                     try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{.a});
    //                                     return error.InstructionFailed;
    //                                 }

    //                                 try b.emitReloc(air.node, switch (source_access) {
    //                                     .direct_page => switch (int_reg) {
    //                                         .x => .{ .ldx_dp = undefined },
    //                                         .y => .{ .ldy_dp = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .absolute => switch (int_reg) {
    //                                         .x => .{ .ldx_addr16 = undefined },
    //                                         .y => .{ .ldy_addr16 = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .long, .stack => unreachable,
    //                                 }, .{
    //                                     .type = switch (source_access) {
    //                                         .direct_page => .rel8_dp,
    //                                         .absolute => .addr16,
    //                                         .long, .stack => unreachable,
    //                                     },
    //                                     .target_symbol = source.symbol,
    //                                     .target_offset = @divExact(source.bit_offset + read_offset, 8),
    //                                 });
    //                             },
    //                         }

    //                         try b.emitReloc(air.node, switch (target_access) {
    //                             .direct_page => switch (int_reg) {
    //                                 .x => .{ .stx_dp = undefined },
    //                                 .y => .{ .sty_dp = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .absolute => switch (int_reg) {
    //                                 .x => .{ .stx_addr16 = undefined },
    //                                 .y => .{ .sty_addr16 = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .long, .stack => unreachable,
    //                         }, .{
    //                             .type = switch (target_access) {
    //                                 .direct_page => .rel8_dp,
    //                                 .absolute => .addr16,
    //                                 .long, .stack => unreachable,
    //                             },
    //                             .target_symbol = air.tag.store_symbol.symbol,
    //                             .target_offset = @divExact(write_offset, 8),
    //                         });

    //                         write_offset += 8;
    //                         continue;
    //                     }

    //                     // 16-bit writes
    //                     const first_idx = source_bits[write_offset];
    //                     const second_idx = source_bits[write_offset + 8];
    //                     const first_op = operations[first_idx].tag.store_operation;
    //                     const second_op = operations[second_idx].tag.store_operation;

    //                     if (first_idx == second_idx) {
    //                         // Operations are the same -> full 16-bit write
    //                         const read_offset = write_offset - first_op.bit_offset;
    //                         switch (first_op.value) {
    //                             .immediate => |value| {
    //                                 // Extract relevant bytes from full value
    //                                 const first_byte: u8 = @truncate(value.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));
    //                                 const second_byte: u8 = @truncate(value.limbs[@divFloor(read_offset + 8, @bitSizeOf(std.math.big.Limb))] >> @intCast((read_offset + 8) % @bitSizeOf(std.math.big.Limb)));
    //                                 const word: u16 = first_byte | @as(u16, second_byte) << 8;

    //                                 if (word == 0 and mem_size != .none) {
    //                                     // Optimize for zero-values
    //                                     // Perform 2 writes in 8-bit mode and 1 write in 16-bit mode
    //                                     for (0..(1 + @intFromBool(mem_size == .@"8bit"))) |offset| {
    //                                         try b.emitReloc(air.node, switch (target_access) {
    //                                             .direct_page => .{ .stz_dp = undefined },
    //                                             .absolute => .{ .stz_addr16 = undefined },
    //                                             .long, .stack => unreachable,
    //                                         }, .{
    //                                             .type = switch (target_access) {
    //                                                 .direct_page => .rel8_dp,
    //                                                 .absolute => .addr16,
    //                                                 .long, .stack => unreachable,
    //                                             },
    //                                             .target_symbol = air.tag.store_symbol.symbol,
    //                                             .target_offset = @intCast(@divExact(write_offset, 8) + offset),
    //                                         });
    //                                     }

    //                                     write_offset += 16;
    //                                     continue;
    //                                 }

    //                                 try b.emit(air.node, switch (int_reg) {
    //                                     .x => .{ .ldx_imm = .{ .imm16 = word } },
    //                                     .y => .{ .ldy_imm = .{ .imm16 = word } },
    //                                     else => unreachable,
    //                                 });
    //                             },
    //                             .symbol => |source| {
    //                                 const source_access = b.determineMemoryAccesibilty(source.symbol);
    //                                 if (source_access == .long) {
    //                                     try b.codegen.emitError(air.node, b.symbol, .idx_reg_long_read, .{});
    //                                     try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{.a});
    //                                     return error.InstructionFailed;
    //                                 }

    //                                 try b.emitReloc(air.node, switch (source_access) {
    //                                     .direct_page => switch (int_reg) {
    //                                         .x => .{ .ldx_dp = undefined },
    //                                         .y => .{ .ldy_dp = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .absolute => switch (int_reg) {
    //                                         .x => .{ .ldx_addr16 = undefined },
    //                                         .y => .{ .ldy_addr16 = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .long, .stack => unreachable,
    //                                 }, .{
    //                                     .type = switch (source_access) {
    //                                         .direct_page => .rel8_dp,
    //                                         .absolute => .addr16,
    //                                         .long, .stack => unreachable,
    //                                     },
    //                                     .target_symbol = source.symbol,
    //                                     .target_offset = @divExact(source.bit_offset + read_offset, 8),
    //                                 });
    //                             },
    //                         }

    //                         try b.emitReloc(air.node, switch (target_access) {
    //                             .direct_page => switch (int_reg) {
    //                                 .x => .{ .stx_dp = undefined },
    //                                 .y => .{ .sty_dp = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .absolute => switch (int_reg) {
    //                                 .x => .{ .stx_addr16 = undefined },
    //                                 .y => .{ .sty_addr16 = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .long, .stack => unreachable,
    //                         }, .{
    //                             .type = switch (target_access) {
    //                                 .direct_page => .rel8_dp,
    //                                 .absolute => .addr16,
    //                                 .long, .stack => unreachable,
    //                             },
    //                             .target_symbol = air.tag.store_symbol.symbol,
    //                             .target_offset = @divExact(write_offset, 8),
    //                         });
    //                         write_offset += 16;
    //                     } else if (first_op.value == .immediate and second_op.value == .immediate) {
    //                         // Operations can be combined -> full 16-bit write
    //                         const first_read_offset = write_offset - first_op.bit_offset;
    //                         const second_read_offset = write_offset + 8 - second_op.bit_offset;

    //                         // Extract relevant bytes from full value
    //                         const first_byte: u8 = @truncate(first_op.value.immediate.limbs[@divFloor(first_read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(first_read_offset % @bitSizeOf(std.math.big.Limb)));
    //                         const second_byte: u8 = @truncate(second_op.value.immediate.limbs[@divFloor(second_read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(second_read_offset % @bitSizeOf(std.math.big.Limb)));
    //                         const word: u16 = first_byte | @as(u16, second_byte) << 8;

    //                         if (word == 0 and mem_size != .none) {
    //                             // Optimize for zero-values
    //                             // Perform 2 writes in 8-bit mode and 1 write in 16-bit mode
    //                             for (0..(1 + @intFromBool(mem_size == .@"8bit"))) |offset| {
    //                                 try b.emitReloc(air.node, switch (target_access) {
    //                                     .direct_page => .{ .stz_dp = undefined },
    //                                     .absolute => .{ .stz_addr16 = undefined },
    //                                     .long, .stack => unreachable,
    //                                 }, .{
    //                                     .type = switch (target_access) {
    //                                         .direct_page => .rel8_dp,
    //                                         .absolute => .addr16,
    //                                         .long, .stack => unreachable,
    //                                     },
    //                                     .target_symbol = air.tag.store_symbol.symbol,
    //                                     .target_offset = @intCast(@divExact(write_offset, 8) + offset),
    //                                 });
    //                             }

    //                             write_offset += 16;
    //                             continue;
    //                         }

    //                         try b.emit(air.node, switch (int_reg) {
    //                             .x => .{ .ldx_imm = .{ .imm16 = word } },
    //                             .y => .{ .ldy_imm = .{ .imm16 = word } },
    //                             else => unreachable,
    //                         });

    //                         try b.emitReloc(air.node, switch (target_access) {
    //                             .direct_page => switch (int_reg) {
    //                                 .x => .{ .stx_dp = undefined },
    //                                 .y => .{ .sty_dp = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .absolute => switch (int_reg) {
    //                                 .x => .{ .stx_addr16 = undefined },
    //                                 .y => .{ .sty_addr16 = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .long, .stack => unreachable,
    //                         }, .{
    //                             .type = switch (target_access) {
    //                                 .direct_page => .rel8_dp,
    //                                 .absolute => .addr16,
    //                                 .long, .stack => unreachable,
    //                             },
    //                             .target_symbol = air.tag.store_symbol.symbol,
    //                             .target_offset = @divExact(write_offset, 8),
    //                         });
    //                         write_offset += 16;
    //                     } else {
    //                         // Operations cannot be compiled -> half 8-bit write
    //                         // 2nd byte is undefined and will be overwritten with next operation
    //                         const read_offset = write_offset - first_op.bit_offset;
    //                         switch (first_op.value) {
    //                             .immediate => |value| {
    //                                 // Extract relevant byte from full value
    //                                 const first_byte: u8 = @truncate(value.limbs[@divFloor(read_offset, @bitSizeOf(std.math.big.Limb))] >> @intCast(read_offset % @bitSizeOf(std.math.big.Limb)));

    //                                 if (first_byte == 0 and mem_size != .none) {
    //                                     // Optimize for zero-values
    //                                     try b.emitReloc(air.node, switch (target_access) {
    //                                         .direct_page => .{ .stz_dp = undefined },
    //                                         .absolute => .{ .stz_addr16 = undefined },
    //                                         .long, .stack => unreachable,
    //                                     }, .{
    //                                         .type = switch (target_access) {
    //                                             .direct_page => .rel8_dp,
    //                                             .absolute => .addr16,
    //                                             .long, .stack => unreachable,
    //                                         },
    //                                         .target_symbol = air.tag.store_symbol.symbol,
    //                                         .target_offset = @divExact(write_offset, 8),
    //                                     });

    //                                     write_offset += 8;
    //                                     continue;
    //                                 }

    //                                 try b.emit(air.node, switch (int_reg) {
    //                                     .x => .{ .ldx_imm = .{ .imm16 = first_byte } },
    //                                     .y => .{ .ldy_imm = .{ .imm16 = first_byte } },
    //                                     else => unreachable,
    //                                 });
    //                             },
    //                             .symbol => |source| {
    //                                 const source_access = b.determineMemoryAccesibilty(source.symbol);
    //                                 if (source_access == .long) {
    //                                     try b.codegen.emitError(air.node, b.symbol, .idx_reg_long_read, .{});
    //                                     try b.codegen.emitSupportedIntermediateRegisters(air.node, b.symbol, &.{.a});
    //                                     return error.InstructionFailed;
    //                                 }

    //                                 try b.emitReloc(air.node, switch (source_access) {
    //                                     .direct_page => switch (int_reg) {
    //                                         .x => .{ .ldx_dp = undefined },
    //                                         .y => .{ .ldy_dp = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .absolute => switch (int_reg) {
    //                                         .x => .{ .ldx_addr16 = undefined },
    //                                         .y => .{ .ldy_addr16 = undefined },
    //                                         else => unreachable,
    //                                     },
    //                                     .long, .stack => unreachable,
    //                                 }, .{
    //                                     .type = switch (source_access) {
    //                                         .direct_page => .rel8_dp,
    //                                         .absolute => .addr16,
    //                                         .long, .stack => unreachable,
    //                                     },
    //                                     .target_symbol = source.symbol,
    //                                     .target_offset = @divExact(source.bit_offset + read_offset, 8),
    //                                 });
    //                             },
    //                         }

    //                         try b.emitReloc(air.node, switch (target_access) {
    //                             .direct_page => switch (int_reg) {
    //                                 .x => .{ .stx_dp = undefined },
    //                                 .y => .{ .sty_dp = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .absolute => switch (int_reg) {
    //                                 .x => .{ .stx_addr16 = undefined },
    //                                 .y => .{ .sty_addr16 = undefined },
    //                                 else => unreachable,
    //                             },
    //                             .long, .stack => unreachable,
    //                         }, .{
    //                             .type = switch (target_access) {
    //                                 .direct_page => .rel8_dp,
    //                                 .absolute => .addr16,
    //                                 .long, .stack => unreachable,
    //                             },
    //                             .target_symbol = air.tag.store_symbol.symbol,
    //                             .target_offset = @divExact(write_offset, 8),
    //                         });
    //                         write_offset += 8;
    //                     }
    //                 }

    //                 matrix_results[result_idx] = try .fromState(b, state, mem_size, idx_size);
    //             }
    //         }

    //         // Determine best result
    //         CodeMatrixResult.sortResults(&matrix_results, .debug);
    //         std.debug.assert(matrix_results[0].instructions.len != 0);
    //         try b.instructions.appendSlice(b.allocator(), matrix_results[0].instructions);
    //         b.restoreState(matrix_results[0].state);
    //     },
    // }
}

fn handleStoreOLD(b: *Builder, ir: AssemblyIr) !void {
    const operation_count = switch (ir.tag) {
        inline .store_global, .store_local, .store_push => |info| info.operations,
        else => unreachable,
    };
    const int_reg = switch (ir.tag) {
        inline .store_global, .store_local, .store_push => |info| info.intermediate_register,
        else => unreachable,
    };

    const operations = b.getFunction().semantic_ir[(b.air_index + 1)..(b.air_index + operation_count + 1)];
    std.debug.assert(operations.len != 0);

    b.air_index += operation_count;

    var start_idx: usize = 0;
    var curr_offset = operations[start_idx].tag.store_operation.write_offset;
    for (operations, 0..) |op_ir, op_idx| {
        const op = op_ir.tag.store_operation;
        if (op_idx != operations.len - 1 and operations[op_idx + 1].tag.store_operation.write_offset == curr_offset) {
            continue;
        }
        defer if (op_idx != operations.len - 1) {
            start_idx = op_idx + 1;
            curr_offset = operations[start_idx].tag.store_operation.write_offset;
        };

        // Boundry of current write
        var immediate_value: u16 = 0;
        var mask_value: u16 = 0;

        const Variable = packed struct {
            source_type: enum(u2) { none, global, local },
            source: packed union {
                symbol: Symbol.Index,
                index: u16,
            },

            bit_size: u16,
        };
        var variables = [_]Variable{.{ .source_type = .none, .source = undefined, .bit_size = undefined }} ** 16;

        // Iterate in reverse, since most-significant bits need to come first
        var iter = std.mem.reverseIterator(operations[start_idx..(op_idx + 1)]);
        while (iter.next()) |curr_op_ir| {
            const curr_op = curr_op_ir.tag.store_operation;
            switch (curr_op.value) {
                .immediate => |value| {
                    immediate_value |= (value.to(u16) catch unreachable) << @intCast(curr_op.bit_offset % int_reg.bitSize());
                },
                .global => |global| {
                    // TODO: Support packed field loads
                    std.debug.assert(global.bit_offset % 8 == 0);

                    variables[curr_op.bit_offset % int_reg.bitSize()] = .{
                        .source_type = .global,
                        .source = .{ .symbol = global.symbol },
                        .bit_size = curr_op.bit_size,
                    };
                },
                .local => |local| {
                    // TODO: Support packed field loads
                    std.debug.assert(local.bit_offset % 8 == 0);

                    variables[curr_op.bit_offset % int_reg.bitSize()] = .{
                        .source_type = .local,
                        .source = .{ .index = local.index },
                        .bit_size = curr_op.bit_size,
                    };
                },
            }

            // Mark written bits
            mask_value |= @as(u16, @intCast((@as(u17, 1) << @intCast(curr_op.bit_size)) - 1)) << @intCast(curr_op.bit_offset % int_reg.bitSize());
        }

        // Next byte will be overwritten anyway
        if (op.write_offset == curr_offset + 1) {
            mask_value |= std.math.maxInt(u8) << 8;
        }

        // Flush current write
        const full_write = switch (int_reg) {
            .a8, .x8, .y8 => mask_value & std.math.maxInt(u8) == std.math.maxInt(u8),
            .a16, .x16, .y16 => mask_value & std.math.maxInt(u16) == std.math.maxInt(u16),
            .none => true,
        };
        const imm_value: Instruction.Imm816 = switch (int_reg) {
            .a8, .x8, .y8 => .{ .imm8 = @intCast(immediate_value) },
            .a16, .x16, .y16 => .{ .imm16 = @intCast(immediate_value) },
            else => undefined,
        };
        std.log.info("FLUSH {} @ {} || {} {b} {}", .{ immediate_value, curr_offset, full_write, mask_value, imm_value });

        if (full_write) {
            // Full register write
            var shift_offset: u8 = std.math.maxInt(u8);
            for (variables, 0..) |variable, bit_offset| {
                if (variable.source_type == .none) {
                    continue;
                }

                if (shift_offset == std.math.maxInt(u8)) {
                    switch (variable.source_type) {
                        .global => try b.emitReloc(ir.node, switch (int_reg) {
                            .a8, .a16 => .{ .lda_addr16 = undefined },
                            .x8, .x16 => .{ .ldx_addr16 = undefined },
                            .y8, .y16 => .{ .ldy_addr16 = undefined },
                            else => unreachable,
                        }, .{
                            .type = .addr16,
                            .target_symbol = variable.source.symbol,
                            .target_offset = 0,
                        }),
                        .local => switch (b.getFunction().local_variables[variable.source.index].location) {
                            .scratch => @panic("TODO"),
                            .stack => |offset| try b.emit(ir.node, .{ .lda_sr = offset }),
                        },
                        .none => unreachable,
                    }
                } else {
                    std.debug.assert(int_reg == .a8 or int_reg == .a16);

                    for (0..(shift_offset - bit_offset)) |_| {
                        try b.emit(ir.node, .asl_accum);
                    }

                    switch (variable.source_type) {
                        .global => try b.emitReloc(ir.node, .{ .ora_addr16 = undefined }, .{
                            .type = .addr16,
                            .target_symbol = variable.source.symbol,
                            .target_offset = 0,
                        }),
                        .local => switch (b.getFunction().local_variables[variable.source.index].location) {
                            .scratch => @panic("TODO"),
                            .stack => |offset| try b.emit(ir.node, .{ .ora_sr = offset }),
                        },
                        .none => unreachable,
                    }
                }

                shift_offset = @intCast(bit_offset);
            }
            if (shift_offset == std.math.maxInt(u8)) {
                try b.emit(op_ir.source, switch (int_reg) {
                    .a8, .a16 => .{ .lda_imm = imm_value },
                    .x8, .x16 => .{ .ldx_imm = imm_value },
                    .y8, .y16 => .{ .ldy_imm = imm_value },
                    else => unreachable,
                });
            } else {
                std.debug.assert(int_reg == .a8 or int_reg == .a16);

                for (0..shift_offset) |_| {
                    try b.emit(op_ir.source, .asl_accum);
                }
                try b.emit(op_ir.source, .{ .ora_imm = imm_value });
            }

            switch (ir.tag) {
                .store_global => |global| {
                    const instr: Instruction = if (int_reg == .none or (immediate_value == 0 and shift_offset == 0))
                        .{ .stz_addr16 = undefined }
                    else switch (int_reg) {
                        .a8, .a16 => .{ .sta_addr16 = undefined },
                        .x8, .x16 => .{ .stx_addr16 = undefined },
                        .y8, .y16 => .{ .sty_addr16 = undefined },
                        else => unreachable,
                    };

                    try b.emitReloc(op_ir.source, instr, .{
                        .type = .addr16,
                        .target_symbol = global.symbol,
                        .target_offset = curr_offset,
                    });
                },
                .store_local => |local| switch (b.getFunction().local_variables[local.index].location) {
                    .scratch => @panic("TODO"),
                    .stack => |offset| try b.emit(ir.node, .{ .sta_sr = offset }),
                },
                .store_push => try b.emit(ir.node, .pha),
                else => unreachable,
            }
        } else {
            // Partial register write
            std.debug.assert(int_reg == .a8 or int_reg == .a16);

            var min_left_shifts: u8 = std.math.maxInt(u8);
            var min_left_offset: u8 = undefined;
            var min_right_shifts: u8 = std.math.maxInt(u8);
            var min_right_offset: u8 = undefined;

            var bit_offset: u8 = 0;
            var shift_offset: u8 = 0;
            var has_variables = false;

            const rotate_size: u8 = switch (int_reg) {
                // +1 since the carry bit is included when rotating
                .a8 => 8 + 1,
                .a16 => 16 + 1,
                else => unreachable,
            };

            while (true) : (bit_offset += 1) {
                if (bit_offset == variables.len) {
                    if (min_left_shifts == std.math.maxInt(u8) or min_right_shifts == std.math.maxInt(u8)) {
                        break;
                    }

                    std.log.warn("MinLeft {} @ {}", .{ min_left_shifts, min_left_offset });
                    std.log.warn("MinRight {} @ {}", .{ min_left_shifts, min_left_offset });
                    if (min_left_shifts <= min_right_shifts) {
                        shift_offset = min_left_offset;
                    } else {
                        shift_offset = min_right_offset;
                    }

                    if (has_variables) {} else {}

                    variables[shift_offset].source_type = .none; // Mark variable as used
                    has_variables = true;

                    // Repeat for next variable
                    bit_offset = 0;
                    continue;
                }

                if (variables[bit_offset].source_type == .none) {
                    continue;
                }

                const left_shifts: u8 = @intCast(@mod(@as(i8, @intCast(bit_offset)) - @as(i8, @intCast(shift_offset)), @as(i8, @intCast(rotate_size))));
                const right_shifts: u8 = @intCast(@mod(rotate_size - bit_offset + shift_offset, rotate_size));
                std.log.info("SHIFT at {d} from {d} << {d} | >> {d}", .{ bit_offset, shift_offset, left_shifts, right_shifts });

                if (left_shifts < min_left_shifts) {
                    min_left_shifts = left_shifts;
                    min_left_offset = bit_offset;
                }
                if (right_shifts < min_right_shifts) {
                    min_right_shifts = right_shifts;
                    min_right_offset = bit_offset;
                }
            }

            if (has_variables) {
                @panic("TODO");
            } else {
                switch (ir.tag) {
                    .store_global => |global| try b.emitReloc(op_ir.source, .{ .lda_addr16 = undefined }, .{
                        .type = .addr16,
                        .target_symbol = global.symbol,
                        .target_offset = curr_offset,
                    }),
                    .store_local => |local| switch (b.getFunction().local_variables[local.index].location) {
                        .scratch => @panic("TODO"),
                        .stack => |offset| try b.emit(ir.node, .{ .lda_sr = offset }),
                    },
                    // Value doesn't exist yet
                    .store_push => {},
                    else => unreachable,
                }

                switch (ir.tag) {
                    .store_global, .store_local => {
                        try b.emit(op_ir.source, .{ .and_imm = switch (int_reg) {
                            .a8 => .{ .imm8 = @truncate(~mask_value) },
                            .a16 => .{ .imm16 = @truncate(~mask_value) },
                            else => undefined,
                        } });
                        try b.emit(op_ir.source, .{ .ora_imm = imm_value });
                    },
                    // Value doesn't exist yet
                    .store_push => try b.emit(op_ir.source, .{ .lda_imm = imm_value }),
                    else => unreachable,
                }

                switch (ir.tag) {
                    .store_global => |global| try b.emitReloc(op_ir.source, .{ .sta_addr16 = undefined }, .{
                        .type = .addr16,
                        .target_symbol = global.symbol,
                        .target_offset = curr_offset,
                    }),
                    .store_local => |local| switch (b.getFunction().local_variables[local.index].location) {
                        .scratch => @panic("TODO"),
                        .stack => |offset| try b.emit(ir.node, .{ .sta_sr = offset }),
                    },
                    .store_push => try b.emit(op_ir.source, .pha),
                    else => unreachable,
                }
            }
        }
    }
    // const operations

}

fn handleLoadValue(b: *Builder, ir: AssemblyIr) !void {
    const load = ir.tag.load_value;

    try b.instructions.append(b.allocator(), .{
        .instr = switch (load.register) {
            .a8, .a16 => .{ .lda_imm = load.value },
            .x8, .x16 => .{ .ldx_imm = load.value },
            .y8, .y16 => .{ .ldy_imm = load.value },
            .none => unreachable,
        },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleLoadVariable(b: *Builder, ir: AssemblyIr) !void {
    const load = ir.tag.load_variable;
    const target_symbol = b.sema.getSymbol(load.symbol).*;

    const instr: Instruction, const reloc: ?Relocation = switch (load.register) {
        .a8, .a16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .lda_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = load.symbol,
                .target_offset = load.offset,
            } }
        else
            .{ .{ .lda_addr24 = undefined }, .{
                .type = .addr24,
                .target_symbol = load.symbol,
                .target_offset = load.offset,
            } },

        .x8, .x16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .ldx_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = load.symbol,
                .target_offset = load.offset,
            } }
        else
            unreachable,

        .y8, .y16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .ldx_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = load.symbol,
                .target_offset = load.offset,
            } }
        else
            unreachable,

        .none => unreachable,
    };

    try b.instructions.append(b.allocator(), .{
        .instr = instr,
        .reloc = reloc,

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleStoreVariable(b: *Builder, ir: AssemblyIr) !void {
    const store = ir.tag.store_variable;
    const target_symbol = b.sema.getSymbol(store.symbol).*;

    const instr: Instruction, const reloc: Relocation = switch (store.register) {
        .a8, .a16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .sta_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = store.symbol,
                .target_offset = store.offset,
            } }
        else
            .{ .{ .sta_addr24 = undefined }, .{
                .type = .addr24,
                .target_symbol = store.symbol,
                .target_offset = store.offset,
            } },

        .x8, .x16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .stx_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = store.symbol,
                .target_offset = store.offset,
            } }
        else
            unreachable,

        .y8, .y16 => if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
            .{ .{ .sty_addr16 = undefined }, .{
                .type = .addr16,
                .target_symbol = store.symbol,
                .target_offset = store.offset,
            } }
        else
            unreachable,
    };

    try b.instructions.append(b.allocator(), .{
        .instr = instr,
        .reloc = reloc,

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleZeroVariable(b: *Builder, ir: AssemblyIr) !void {
    const store = ir.tag.zero_variable;

    // STZ does not support long addresses
    std.debug.assert(b.sema.isSymbolAccessibleInBank(b.sema.getSymbol(store.symbol).*, b.getFunction().bank));

    try b.instructions.append(b.allocator(), .{
        .instr = .{ .stz_addr16 = undefined },
        .reloc = .{
            .type = .addr16,
            .target_symbol = store.symbol,
            .target_offset = store.offset,
        },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleOrValue(b: *Builder, ir: AssemblyIr) !void {
    const value = ir.tag.or_value;

    try b.instructions.append(b.allocator(), .{
        .instr = .{ .ora_imm = value },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleOrVariable(b: *Builder, ir: AssemblyIr) !void {
    const variable = ir.tag.or_variable;
    const target_symbol = b.sema.getSymbol(variable.symbol).*;

    const instr: Instruction, const reloc: Relocation = if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
        .{ .{ .ora_addr16 = undefined }, .{
            .type = .addr16,
            .target_symbol = variable.symbol,
            .target_offset = variable.offset,
        } }
    else
        .{ .{ .ora_addr24 = undefined }, .{
            .type = .addr24,
            .target_symbol = variable.symbol,
            .target_offset = variable.offset,
        } };

    try b.instructions.append(b.allocator(), .{
        .instr = instr,
        .reloc = reloc,

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleAndValue(b: *Builder, ir: AssemblyIr) !void {
    const value = ir.tag.and_value;

    try b.instructions.append(b.allocator(), .{
        .instr = .{ .and_imm = value },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleAndVariable(b: *Builder, ir: AssemblyIr) !void {
    const variable = ir.tag.and_variable;
    const target_symbol = b.sema.getSymbol(variable.symbol).*;

    const instr: Instruction, const reloc: Relocation = if (b.sema.isSymbolAccessibleInBank(target_symbol, b.getFunction().bank))
        .{ .{ .and_addr16 = undefined }, .{
            .type = .addr16,
            .target_symbol = variable.symbol,
            .target_offset = variable.offset,
        } }
    else
        .{ .{ .and_addr24 = undefined }, .{
            .type = .addr24,
            .target_symbol = variable.symbol,
            .target_offset = variable.offset,
        } };

    try b.instructions.append(b.allocator(), .{
        .instr = instr,
        .reloc = reloc,

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleShiftAccumLeft(b: *Builder, ir: AssemblyIr) !void {
    const shift = ir.tag.shift_accum_left;

    for (0..shift) |_| {
        try b.instructions.append(b.allocator(), .{
            .instr = .asl_accum,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
}
fn handleShiftAccumRight(b: *Builder, ir: AssemblyIr) !void {
    const shift = ir.tag.shift_accum_right;

    for (0..shift) |_| {
        try b.instructions.append(b.allocator(), .{
            .instr = .lsr_accum,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
}
fn handleRotateAccumLeft(b: *Builder, ir: AssemblyIr) !void {
    const rotate = ir.tag.rotate_accum_left;

    for (0..rotate) |_| {
        try b.instructions.append(b.allocator(), .{
            .instr = .rol_accum,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
}
fn handleRotateAccumRight(b: *Builder, ir: AssemblyIr) !void {
    const rotate = ir.tag.rotate_accum_right;

    for (0..rotate) |_| {
        try b.instructions.append(b.allocator(), .{
            .instr = .ror_accum,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
}
fn handleClearBits(b: *Builder, ir: AssemblyIr) !void {
    const clear_bits = ir.tag.clear_bits;

    try b.instructions.append(b.allocator(), .{
        .instr = .{ .lda_imm = clear_bits.mask },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
    try b.instructions.append(b.allocator(), .{
        .instr = .{ .trb_addr16 = undefined },
        .reloc = .{
            .type = .addr16,
            .target_symbol = clear_bits.symbol,
            .target_offset = clear_bits.offset,
        },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleLabel(b: *Builder, ir: AssemblyIr) !void {
    try b.labels.put(b.allocator(), ir.tag.label, @intCast(b.instructions.items.len));
}
fn handleCall(b: *Builder, ir: AssemblyIr) !void {
    const call = ir.tag.call;
    const target_symbol = b.sema.getSymbol(call.target);

    // TODO: Support long jumps
    std.debug.assert(target_symbol.function.bank == b.getFunction().bank);

    try b.instructions.append(b.allocator(), .{
        .instr = .{ .jsr = undefined },
        .reloc = .{
            .type = .addr16,
            .target_symbol = call.target,
            .target_offset = call.target_offset,
        },

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}
fn handleBranch(b: *Builder, ir: AssemblyIr) !void {
    try b.instructions.append(b.allocator(), .{
        .instr = undefined,
        .branch_reloc = ir.tag.branch,

        .mem_size = b.mem_size,
        .idx_size = b.idx_size,

        .source = ir.node,
    });
}

/// By default labels point to a symbol in the current module, instead of the current one with an offset
pub fn fixLabelRelocs(b: *Builder) !void {
    for (b.instructions.items) |*info| {
        const reloc = &(info.reloc orelse continue);
        if (!std.mem.eql(u8, reloc.target_sym.module, b.symbol_location.module)) continue;

        for (b.labels.keys(), b.labels.values()) |label, instr_index| {
            if (std.mem.eql(u8, label, reloc.target_sym.name)) {
                reloc.target_symbol = b.symbol_location;
                reloc.target_offset = if (instr_index == b.instructions.items.len)
                    @intCast(b.getFunction().assembly_data.len)
                else
                    b.instructions.items[instr_index].offset;
            }
        }
    }
}

/// Resolves branch locations into a short / long form
pub fn resolveBranchRelocs(b: *Builder) !void {
    // Relative offsets to the target instruction to determine short- / long-form
    var reloc_offsets: std.AutoArrayHashMapUnmanaged(usize, i32) = .{};
    defer reloc_offsets.deinit(b.allocator());

    for (b.instructions.items, 0..) |info, i| {
        if (info.branch_reloc != null) {
            // Default to long-form, lower to short-form later
            try reloc_offsets.put(b.allocator(), i, std.math.maxInt(i32));
        }
    }

    const short_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
        .always = comptime Instruction.bra.size(.none),
        .carry_set = comptime Instruction.bcs.size(.none),
        .carry_clear = comptime Instruction.bcc.size(.none),
        .overflow_set = comptime Instruction.bvs.size(.none),
        .overflow_clear = comptime Instruction.bvc.size(.none),
        .equal = comptime Instruction.beq.size(.none),
        .not_equal = comptime Instruction.bne.size(.none),
        .plus = comptime Instruction.bpl.size(.none),
        .minus = comptime Instruction.bmi.size(.none),
        .jump = comptime Instruction.jmp.size(.none),
        .jump_long = comptime Instruction.jml.size(.none),
    });
    const long_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
        .always = comptime Instruction.jmp.size(.none),
        .carry_set = comptime Instruction.bcc.size(.none) + Instruction.jmp.size(.none),
        .carry_clear = comptime Instruction.bcs.size(.none) + Instruction.jmp.size(.none),
        .overflow_set = comptime Instruction.bvc.size(.none) + Instruction.jmp.size(.none),
        .overflow_clear = comptime Instruction.bvs.size(.none) + Instruction.jmp.size(.none),
        .equal = comptime Instruction.bne.size(.none) + Instruction.jmp.size(.none),
        .not_equal = comptime Instruction.beq.size(.none) + Instruction.jmp.size(.none),
        .plus = comptime Instruction.bmi.size(.none) + Instruction.jmp.size(.none),
        .minus = comptime Instruction.bpl.size(.none) + Instruction.jmp.size(.none),
        .jump = comptime Instruction.jmp.size(.none),
        .jump_long = comptime Instruction.jml.size(.none),
    });

    // Interativly lower to short-form
    var changed = true;
    while (changed) {
        changed = false;

        for (reloc_offsets.keys(), reloc_offsets.values()) |source_idx, *relative_offset| {
            // If its's already short, don't mark this as a change, but still recalculate the offset
            const already_short = relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8);

            const reloc = b.instructions.items[source_idx].branch_reloc.?;
            const target_idx = b.labels.get(reloc.target_label).?;

            // Calculate offset to target
            const min = @min(source_idx + 1, target_idx);
            const max = @max(source_idx + 1, target_idx);

            relative_offset.* = 0;
            for (b.instructions.items[min..max], min..max) |info, i| {
                if (info.branch_reloc) |other_reloc| {
                    const other_offset = reloc_offsets.get(i).?;

                    if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                        relative_offset.* += short_sizes.get(other_reloc.type);
                    } else {
                        relative_offset.* += long_sizes.get(other_reloc.type);
                    }
                } else {
                    const size_type = info.instr.immediateSizeType();
                    const size_mode = switch (size_type) {
                        .none => .none,
                        .mem => info.mem_size,
                        .idx => info.idx_size,
                    };
                    if (size_type != .none) {
                        std.debug.assert(size_mode != .none);
                    }

                    relative_offset.* += info.instr.size(size_mode);
                }
            }
            if (target_idx <= source_idx) {
                relative_offset.* = -relative_offset.*;
            }

            if (!already_short and relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8)) {
                changed = true;
            }
        }
    }

    // Calculate target offsets (for jumps)
    var target_offsets: std.AutoArrayHashMapUnmanaged(usize, u16) = .{};
    defer target_offsets.deinit(b.allocator());

    for (reloc_offsets.keys()) |source_idx| {
        const reloc = b.instructions.items[source_idx].branch_reloc.?;
        const target_idx = b.labels.get(reloc.target_label).?;

        var offset: usize = 0;
        for (b.instructions.items[0..target_idx], 0..) |info, i| {
            if (info.branch_reloc) |other_reloc| {
                const other_offset = reloc_offsets.get(i).?;

                if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                    offset += short_sizes.get(other_reloc.type);
                } else {
                    offset += long_sizes.get(other_reloc.type);
                }
            } else {
                const size_type = info.instr.immediateSizeType();
                const size_mode = switch (size_type) {
                    .none => .none,
                    .mem => info.mem_size,
                    .idx => info.idx_size,
                };
                if (size_type != .none) {
                    std.debug.assert(size_mode != .none);
                }

                offset += info.instr.size(size_mode);
            }
        }

        try target_offsets.put(b.allocator(), source_idx, @intCast(offset));
    }

    // Insert instructions (reversed to avoid shifting following indices)
    var it = std.mem.reverseIterator(reloc_offsets.keys());

    while (it.next()) |source_idx| {
        const info = &b.instructions.items[source_idx];
        const reloc = info.branch_reloc.?;

        const relative_offset = reloc_offsets.get(source_idx).?;
        const target_offset = target_offsets.get(source_idx).?;

        const use_short = relative_offset >= std.math.minInt(i8) and relative_offset <= std.math.maxInt(i8);

        if (use_short) {
            info.instr = switch (reloc.type) {
                .always => .{ .bra = @intCast(relative_offset) },
                .carry_set => .{ .bcs = @intCast(relative_offset) },
                .carry_clear => .{ .bcc = @intCast(relative_offset) },
                .overflow_set => .{ .bvs = @intCast(relative_offset) },
                .overflow_clear => .{ .bvc = @intCast(relative_offset) },
                .equal => .{ .beq = @intCast(relative_offset) },
                .not_equal => .{ .bne = @intCast(relative_offset) },
                .plus => .{ .bpl = @intCast(relative_offset) },
                .minus => .{ .bmi = @intCast(relative_offset) },
                .jump => .{ .jmp = undefined },
                .jump_long => .{ .jml = undefined },
            };

            if (reloc.type == .jump) {
                info.reloc = .{
                    .type = .addr16,
                    .target_symbol = b.symbol_idx,
                    .target_offset = target_offset,
                };
            } else if (reloc.type == .jump_long) {
                info.reloc = .{
                    .type = .addr24,
                    .target_symbol = b.symbol_idx,
                    .target_offset = target_offset,
                };
            }
        } else {
            const jmp_size = comptime Opcode.jmp.size(.none);
            info.instr = switch (reloc.type) {
                .always, .jump => .{ .jmp = undefined },
                .carry_set => .{ .bcc = jmp_size },
                .carry_clear => .{ .bcs = jmp_size },
                .overflow_set => .{ .bvc = jmp_size },
                .overflow_clear => .{ .bvs = jmp_size },
                .equal => .{ .bne = jmp_size },
                .not_equal => .{ .beq = jmp_size },
                .plus => .{ .bmi = jmp_size },
                .minus => .{ .bpl = jmp_size },
                .jump_long => .{ .jml = undefined },
            };

            const jmp_reloc: Relocation = .{
                .type = .addr16,
                .target_symbol = b.symbol_idx,
                .target_offset = target_offset,
            };

            if (reloc.type == .always or reloc.type == .jump) {
                info.reloc = jmp_reloc;
            } else if (reloc.type == .jump_long) {
                info.reloc = .{
                    .type = .addr24,
                    .target_symbol = b.symbol_idx,
                    .target_offset = target_offset,
                };
            } else {
                try b.instructions.insert(b.allocator(), source_idx + 1, .{
                    .instr = .{ .jmp = undefined },
                    .reloc = jmp_reloc,

                    .mem_size = info.mem_size,
                    .idx_size = info.idx_size,

                    .source = info.source,
                });
            }
        }
    }
}

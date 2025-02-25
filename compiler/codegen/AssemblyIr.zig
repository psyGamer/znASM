//! Low-level Assembly Intermediate Representation
const std = @import("std");

const Instruction = @import("../instruction.zig").Instruction;
const Sema = @import("../Sema.zig");
const Symbol = Sema.Symbol;
const RegisterType = Sema.RegisterType;
const Relocation = @import("../CodeGen.zig").Relocation;
const BranchRelocation = @import("../CodeGen.zig").BranchRelocation;
const NodeIndex = @import("../Ast.zig").NodeIndex;

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

/// Micro-Operation which compose `store_...` instructions
pub const StoreOperation = struct {
    value: union(enum) {
        immediate: std.math.big.int.Const,
        symbol: struct {
            symbol: Symbol.Index,
            bit_offset: u16,
        },
        // local: struct {
        //     index: u16,
        //     bit_offset: u16,
        // },
    },

    bit_offset: u16,
    bit_size: u16,

    /// Byte-offset for the register write for this operation
    write_offset: u16 = undefined,
};

pub const Tag = union(enum) {
    instruction: struct {
        instr: Instruction,
        reloc: ?Relocation,
    },
    change_status_flags: ChangeStatusFlags,

    /// Stores the value of the following `store_operation`s into the target symbol
    store_symbol: struct {
        symbol: Symbol.Index,
        intermediate_register: RegisterType,
        operations: u16,
    },
    /// Stores the value of the following `store_operation`s into the target local variable
    // store_local: struct {
    //     intermediate_register: RegisterType,
    //     index: u16,
    //     operations: u16,
    // },
    /// Pushes the value of the following `store_operation`s onto the stack
    // store_push: struct {
    //     intermediate_register: RegisterType,
    //     operations: u16,
    // },

    /// Immediatly followed `store.operations`-times after a `store_*` instruction
    store_operation: StoreOperation,

    /// Loads the immedate value into the register
    load_value: struct {
        register: RegisterType,
        value: Instruction.Imm816,
    },
    /// Loads the variable at the offset into the register
    load_variable: struct {
        register: RegisterType,
        symbol: Symbol.Index,
        offset: u16 = 0,
    },

    /// Stores the value of the register into the variable
    store_variable: struct {
        register: RegisterType,
        symbol: Symbol.Index,
        offset: u16 = 0,
    },
    /// Stores zero into the variable
    zero_variable: struct {
        symbol: Symbol.Index,
        offset: u16 = 0,
    },

    /// ANDs the accumulator with the value
    and_value: Instruction.Imm816,
    /// ANDs the accumulator with the variable
    and_variable: struct {
        symbol: Symbol.Index,
        offset: u16 = 0,
    },

    /// ORs the accumulator with the value
    or_value: Instruction.Imm816,
    /// ORs the accumulator with the variable
    or_variable: struct {
        symbol: Symbol.Index,
        offset: u16 = 0,
    },

    /// Bit-Shift the value in the accumulator to the left
    shift_accum_left: u16,
    /// Bit-Shift the value in the accumulator to the right
    shift_accum_right: u16,

    /// Bit-Rotate the value in the accumulator to the left
    rotate_accum_left: u16,
    /// Bit-Rotate the value in the accumulator to the right
    rotate_accum_right: u16,

    /// Sets all bits in the target to 0 where the mask is 1
    clear_bits: struct {
        symbol: Symbol.Index,
        offset: u16 = 0,
        mask: Instruction.Imm816,
    },

    /// Invokes the target method as a subroutine
    call: struct {
        target: Symbol.Index,
        target_offset: u16 = 0,
    },

    /// Return from the current subroutine
    @"return": void,

    branch: BranchRelocation,
    label: []const u8,
};

tag: Tag,
node: NodeIndex,

//! Intermediate representation for program logic
const std = @import("std");
const Instruction = @import("instruction.zig").Instruction;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const RegisterType = @import("Sema.zig").RegisterType;
const ExpressionValue = @import("Sema.zig").ExpressionValue;
const Relocation = @import("CodeGen.zig").Relocation;
const BranchRelocation = @import("CodeGen.zig").BranchRelocation;
const NodeIndex = @import("Ast.zig").NodeIndex;

pub const Ir = struct {
    const Tag = union(enum) {
        instruction: struct {
            instr: Instruction,
            reloc: ?Relocation,
        },
        change_size: struct {
            target: Instruction.SizeType,
            mode: Instruction.SizeMode,
        },

        /// Loads the immedate value into the register
        load_value: struct {
            register: RegisterType,
            value: Instruction.Imm816,
        },
        /// Loads the variable at the offset into the register
        load_variable: struct {
            register: RegisterType,
            symbol: SymbolLocation,
            offset: u16 = 0,
        },

        load: struct {
            target: RegisterType,
            value: ExpressionValue,
            source_offset: u16 = 0,
        },
        /// Stores the value of the register into the variable
        store_variable: struct {
            register: RegisterType,
            symbol: SymbolLocation,
            offset: u16 = 0,
        },

        call: struct {
            target: SymbolLocation,
            target_offset: u16 = 0,
        },
        branch: BranchRelocation,

        label: []const u8,
    };

    tag: Tag,
    node: NodeIndex,

    pub fn deinit(ir: Ir, allocator: std.mem.Allocator) void {
        switch (ir.tag) {
            .label => |name| allocator.free(name),
            else => {},
        }
    }
};

//! Intermediate representation for program logic
const Instruction = @import("instruction.zig").Instruction;
const Relocation = @import("CodeGen.zig").Relocation;
const BranchRelocation = @import("CodeGen.zig").BranchRelocation;
const NodeIndex = @import("Ast.zig").NodeIndex;

pub const Ir = struct {
    const Tag = union(enum) {
        instruction: struct {
            instr: Instruction,
            reloc: ?Relocation,
        },
        label: []const u8,
        branch: BranchRelocation,
    };

    tag: Tag,
    node: NodeIndex,
};

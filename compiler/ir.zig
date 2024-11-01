//! Intermediate representation for program logic
const Instruction = @import("instruction.zig").Instruction;
const NodeIndex = @import("Ast.zig").NodeIndex;
const Relocation = @import("CodeGen.zig").Relocation;

pub const Ir = struct {
    const Tag = union(enum) {
        instruction: struct {
            instr: Instruction,
            reloc: ?Relocation,
        },
        label: []const u8,
    };

    tag: Tag,
    node: NodeIndex,
};

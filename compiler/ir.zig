//! Intermediate representation for program logic
const Instruction = @import("instruction.zig").Instruction;
const Relocation = @import("CodeGen.zig").Relocation;
const NodeIndex = @import("Ast.zig").NodeIndex;

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

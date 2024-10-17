const std = @import("std");

pub const Instruction = union(InstructionType) {
    /// Return from Interrupt
    rti: void,

    /// Coverts the instruction into the assembly bytes for it
    pub fn write_data(instr: Instruction, writer: anytype) !void {
        // Opcode
        try writer.writeByte(@intFromEnum(instr));

        // Operands
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(instr))) {
                if (@bitSizeOf(field.type) == 0) {
                    // No other data associated
                    return;
                }

                const operand_data: [@bitSizeOf(field.type) / 8]u8 = @bitCast(@field(instr, field.name));
                try writer.writeAll(&operand_data);

                return;
            }
        }

        unreachable;
    }
};

pub const InstructionType = enum(u8) {
    rti = 0x40,
};

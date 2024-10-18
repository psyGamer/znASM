const std = @import("std");

pub const Instruction = union(InstructionType) {
    // TODO
    ora: void,
    @"and": void,
    eor: void,
    adc: void,
    bit: void,
    lda: void,
    cmp: void,
    sbc: void,

    /// Return from Interrupt
    rti: void,
    /// Branch Always
    bra: i8,
    /// Load Index Y with Memory
    ldy: packed union { imm8: u8, imm16: u16 },
    /// Load Index X with Memory
    ldx: packed union { imm8: u8, imm16: u16 },
    /// Compare Memory and Index Y
    cpy: packed union { imm8: u8, imm16: u16 },
    /// Compare Memory and Index X
    cpx: packed union { imm8: u8, imm16: u16 },
    /// No Operation
    nop: void,

    /// Coverts the instruction into the assembly bytes for it
    pub fn write_data(instr: Instruction, writer: anytype) !void {
        // TODO: Handle 8/16bit instructions
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

    /// Computes the size of opcode + operands
    pub inline fn size(instr: Instruction) u8 {
        return @as(InstructionType, instr).size();
    }
};

pub const InstructionType = enum(u8) {
    ora = 0x09,
    @"and" = 0x29,
    eor = 0x49,
    adc = 0x69,
    bit = 0x89,
    lda = 0xA9,
    cmp = 0xC9,
    sbc = 0xE9,

    rti = 0x40,
    bra = 0x80,
    ldy = 0xA0,
    ldx = 0xA2,
    cpy = 0xC0,
    cpx = 0xE0,
    nop = 0xEA,

    /// Computes the size of opcode + operands
    pub fn size(instr: InstructionType) u8 {
        // TODO: Handle 8/16bit instructions
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(instr))) {
                const opcode_size = 1; // Always 1 byte
                const operand_size = @bitSizeOf(field.type) / 8;

                return opcode_size + operand_size;
            }
        }

        unreachable;
    }
};

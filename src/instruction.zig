const std = @import("std");

pub const Instruction = union(InstructionType) {
    /// Some instructions support either 8bit or 16bit immediate values,
    /// depending on the current register mode
    const Imm816 = packed union { imm8: u8, imm16: u16 };

    // TODO
    ora: void,
    @"and": void,
    eor: void,
    adc: void,
    bit: void,
    sbc: void,

    /// Branch Always
    bra: i8,

    /// Jump
    jmp: u16,
    /// Jump Long
    jml: u24,
    /// Jump to Subroutine
    jsr: u16,
    /// Jump Long to Subroutine
    jsl: u24,

    /// Return from Interrupt
    rti: void,
    /// Return from Subroutine
    rts: void,

    /// Load Accumulator from Memory
    lda: Imm816,
    /// Load Index Register X from Memory
    ldx: Imm816,
    /// Load Index Register Y from Memory
    ldy: Imm816,

    /// Compare Accumulator with Memory
    cmp: Imm816,
    /// Compare Index Register X with Memory
    cpx: Imm816,
    /// Compare Index Register Y with Memory
    cpy: Imm816,

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

    pub fn to_data(instrs: []const Instruction, allocator: std.mem.Allocator) ![]u8 {
        var data_size: usize = 0;
        for (instrs) |instr| {
            data_size += instr.size();
        }

        const data = try allocator.alloc(u8, data_size);
        var offset: usize = 0;

        outer: for (instrs) |instr| {
            // TODO: Handle 8/16bit instructions

            // Opcode
            data[offset] = @intFromEnum(instr);
            offset += 1;

            // Operands
            inline for (@typeInfo(Instruction).@"union".fields) |field| {
                if (std.mem.eql(u8, field.name, @tagName(instr))) {
                    if (@bitSizeOf(field.type) == 0) {
                        // No other data associated
                        continue :outer;
                    }

                    const operand_data: [@bitSizeOf(field.type) / 8]u8 = @bitCast(@field(instr, field.name));
                    @memcpy(data[offset..(offset + operand_data.len)], &operand_data);
                    offset += operand_data.len;
                }
            }
        }

        return data;
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
    sbc = 0xE9,

    bra = 0x80,

    jmp = 0x4C,
    jml = 0x5C,
    jsr = 0x20,
    jsl = 0x22,

    rti = 0x40,
    rts = 0x60,

    lda = 0xA9,
    ldx = 0xA2,
    ldy = 0xA0,

    cmp = 0xC9,
    cpx = 0xE0,
    cpy = 0xC0,

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

const std = @import("std");

pub const Instruction = union(InstructionType) {
    /// Some instructions support either 8bit or 16bit immediate values,
    /// depending on the current register mode
    const Imm816 = packed union { imm8: u8, imm16: u16 };

    /// State of the Status Flags Register
    pub const StatusRegister = packed struct(u8) {
        carry: bool = false,
        zero: bool = false,
        irq_disable: bool = false,
        decimal: bool = false,
        xy_8bit: bool = false,
        a_8bit: bool = false,
        overflow: bool = false,
        negative: bool = false,
    };

    /// Defines which size to use for Imm816 instructions
    /// Should always be none for non-Imm816 instructions
    pub const SizeMode = enum { none, @"8bit", @"16bit" };

    /// Represents the register used by an instruction
    pub const RegisterType = enum { none, a, x, y };

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
    /// Return Long from Subroutine
    rtl: void,

    /// Load Accumulator from Memory
    lda: Imm816,
    /// Load Index Register X from Memory
    ldx: Imm816,
    /// Load Index Register Y from Memory
    ldy: Imm816,

    /// Store Zero to Memory (Direct Page)
    stz_dp: u8,
    /// Store Zero to Memory (Absolute)
    stz_addr16: u16,
    /// Store Zero to Memory (Direct Page, X Indexed)
    stz_dp_idx_x: u8,
    /// Store Zero to Memory (Absolute, X Indexed)
    stz_addr16_idx_x: u16,

    /// Store Accumulator to Memory (Direct Page)
    sta_addr8: u8,
    /// Store Accumulator to Memory (Absolute)
    sta_addr16: u16,
    /// Store Accumulator to Memory (Long)
    sta_addr24: u24,

    /// Store Index Register X to Memory (Direct Page)
    stx_addr8: u8,
    /// Store Index Register X to Memory (Absolute)
    stx_addr16: u16,
    /// Store Index Register X to Memory (Direct Page, Y Indexed)
    stx_addr8_idx_y: u8,

    /// Store Index Register Y to Memory (Direct Page)
    sty_addr8: u8,
    /// Store Index Register Y to Memory (Absolute)
    sty_addr16: u16,
    /// Store Index Register Y to Memory (Direct Page, X Indexed)
    sty_addr8_idx_x: u8,

    /// Compare Accumulator with Memory
    cmp: Imm816,
    /// Compare Index Register X with Memory
    cpx: Imm816,
    /// Compare Index Register Y with Memory
    cpy: Imm816,

    /// Set Carry Flag
    sec: void,
    /// Set Interrupt Disable Flag
    sei: void,
    /// Set Decimal Flag
    sed: void,

    /// Clear Carry Flag
    clc: void,
    /// Clear Interrupt Disable Flag
    cli: void,
    /// Clear Decimal Flag
    cld: void,
    /// Clear Overflow Flag
    clv: void,

    /// Set Status Bits
    sep: StatusRegister,
    /// Reset Status Bits
    rep: StatusRegister,

    /// Transfer A to X
    tax: void,
    /// Transfer A to Y
    tay: void,
    /// Transfer X to A
    txa: void,
    /// Transfer Y to A
    tya: void,
    /// Transfer X to Y
    txy: void,
    /// Transfer Y to X
    tyx: void,
    /// Transfer 16-bit A to Direct Page
    tcd: void,
    /// Transfer 16-bit A to Stack Pointer
    tcs: void,
    /// Transfer Direct Page to 16-bit A
    tdc: void,
    /// Transfer Stack Pointer to 16-bit A
    tsc: void,
    /// Transfer 16-bit X to Stack Pointer
    txs: void,
    /// Transfer Stack Pointer to X
    tsx: void,

    /// Exchange Carry and Emulation Bits
    xce: void,

    /// Push Accumulator to Stack
    pha: void,
    /// Push Index Register X to Stack
    phx: void,
    /// Push Index Register Y to Stack
    phy: void,
    /// Push Data Bank to Stack
    phb: void,
    /// Push Direct Page to Stack
    phd: void,
    /// Push Program Bank to Stack
    phk: void,
    /// Push Processor Status to Stack
    php: void,
    /// Push Effective Absolute Address to Stack
    pea: u16,
    /// Push Effective Indirect Address to Stack
    pei: u8,
    /// Push PC Relative Address to Stack
    per: i16,

    /// Pull Accumulator from Stack
    pla: void,
    /// Pull Index Register X from Stack
    plx: void,
    /// Pull Index Register Y from Stack
    ply: void,
    /// Pull Data Bank from Stack
    plb: void,
    /// Pull Direct Page from Stack
    pld: void,
    /// Pull Processor Status from Stack
    plp: void,

    /// No Operation
    nop: void,

    /// Coverts the instruction into the assembly bytes for it
    pub fn write_data(instr: Instruction, writer: anytype, register_size: SizeMode) !void {
        // Opcode
        try writer.writeByte(@intFromEnum(instr));

        // Operands
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(instr))) {
                if (@bitSizeOf(field.type) == 0) {
                    // No other data associated
                    return;
                }

                // Handle 8-bit / 16-bit instructions
                if (field.type == Imm816) {
                    std.debug.assert(register_size != .none);

                    if (register_size == .@"8bit") {
                        const value = @field(instr, field.name).imm8;
                        try writer.writeInt(u8, value, .little);
                    } else {
                        const value = @field(instr, field.name).imm16;
                        try writer.writeInt(u16, value, .little);
                    }
                } else {
                    std.debug.assert(register_size == .none);

                    const operand_data: [@bitSizeOf(field.type) / 8]u8 = @bitCast(@field(instr, field.name));
                    try writer.writeAll(&operand_data);
                }

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

                    // Handle 8-bit / 16-bit instructions
                    if (field.type == Imm816) {
                        @panic("8-bit / 16-bit instructions aren't supported");
                    } else {
                        const operand_data: [@bitSizeOf(field.type) / 8]u8 = @bitCast(@field(instr, field.name));
                        @memcpy(data[offset..(offset + operand_data.len)], &operand_data);
                        offset += operand_data.len;
                    }

                    continue :outer;
                }
            }
        }

        return data;
    }

    /// Computes the size of opcode + operands
    pub inline fn size(instr: Instruction) u8 {
        return @as(InstructionType, instr).size();
    }

    /// Checks which register is used for this instruction
    pub fn target_register(instr: Instruction) RegisterType {
        return @as(InstructionType, instr).target_register();
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
    rtl = 0x6B,

    lda = 0xA9,
    ldx = 0xA2,
    ldy = 0xA0,

    stz_dp = 0x64,
    stz_addr16 = 0x9C,
    stz_dp_idx_x = 0x74,
    stz_addr16_idx_x = 0x9E,

    sta_addr8 = 0x85,
    sta_addr16 = 0x8D,
    sta_addr24 = 0x8F,
    // sta_dpind_addr16 = 0x92,
    // sta_dpind_addr24 = 0x87,
    // sta_addr8_idx_x = 0x95,
    // sta_addr16_idx_x = 0x9D,
    // sta_addr24_idx_X = 0x9F,
    // sta_addr16_idx_y = 0x99,
    // sta_dpind_idx_x_addr16 = 0x81,
    // sta_dpind_addr16_idx_y = 0x91,
    // sta_dpind_addr24_idx_y = 0x97,
    // sta_stack = 0x83,
    // sta_stackind_idx_y = 0x93,

    stx_addr8 = 0x86,
    stx_addr16 = 0x8E,
    stx_addr8_idx_y = 0x96,

    sty_addr8 = 0x84,
    sty_addr16 = 0x8C,
    sty_addr8_idx_x = 0x94,

    cmp = 0xC9,
    cpx = 0xE0,
    cpy = 0xC0,

    sec = 0x38,
    sei = 0x78,
    sed = 0xF8,

    clc = 0x18,
    cli = 0x58,
    cld = 0xD8,
    clv = 0xB8,

    sep = 0xE2,
    rep = 0xC2,

    tax = 0xAA,
    tay = 0xA8,
    txa = 0x8A,
    tya = 0x98,
    txy = 0x9B,
    tyx = 0xBB,
    tcd = 0x5B,
    tcs = 0x1B,
    tdc = 0x7B,
    tsc = 0x3B,
    txs = 0x9A,
    tsx = 0xBA,

    xce = 0xFB,

    pha = 0x48,
    phx = 0xDA,
    phy = 0x5A,
    phb = 0x8B,
    phd = 0x0B,
    phk = 0x4B,
    php = 0x08,
    pea = 0xF4,
    pei = 0xD4,
    per = 0x62,

    pla = 0x68,
    plx = 0xFA,
    ply = 0x7A,
    plb = 0xAB,
    pld = 0x2B,
    plp = 0x28,

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

    /// Checks which register is used for this instruction
    pub fn target_register(instr: InstructionType) Instruction.RegisterType {
        return switch (instr) {
            .lda, .cmp => .a,
            .ldx, .cpx => .x,
            .ldy, .cpy => .y,
            else => .none,
        };
    }
};

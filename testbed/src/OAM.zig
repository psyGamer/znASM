pub var index: znasm.ZeroPage(u8) = undefined;

pub var memory: znasm.Globa([512]u8) = undefined;
pub var memory_hi: znasm.Global([128]u8) = undefined;

pub fn clear_lo(b: *znasm.Builder) void {
    const a = b.regA(.@"16bit");
    const y = b.regY(.@"16bit");
    a.write_imm(0x0000);
    y.write_imm(0x0000);
    // b.emitLdy(0x0000);
    // b.emitLda(0x0000);

    // WHILE Y < 512
    const next = b.defineLabel();
    {
        memory.write_indexed(b, .a, .y);
        // b.emitStaIndexed(memory, .y);
        b.add_y(2);
        // b.emitIny();
        // b.emitIny();
        memory.write_indexed(b, .a, .y);
        // b.emitStaIndexed(memory, .y);
        // b.emitIny();
        // b.emitIny();
        b.add_y(2);

        // b.emitCpy(512);
        // b.emitBne(next);
        b.jumpYLessThan(512);
    }
}

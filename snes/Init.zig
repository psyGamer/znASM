const znasm = @import("znasm");

const reg = @import("reg.zig");

const MEMSEL: znasm.Register = .init(0x420D);

pub fn Reset(b: *znasm.Builder) void {
    b.setup_debug(@src(), @This(), null);

    b.emit(.sei); // Disable interrupts

    // Enter native mode
    b.emit(.clc);
    b.emit(.xce);

    // Jump to fast mirror in bank 0x80
    const fast = b.create_label();
    b.jump_long(fast);

    fast.define(b);

    // Initialize system
    b.call(CPU);

    // Main loop
    const loop = b.define_label();
    b.branch_always(loop);
}

pub fn CPU(b: *znasm.Builder) void {
    b.setup_debug(@src(), @This(), null);

    var a = b.reg_a8();
    a = .load_store(b, MEMSEL, 0x01);

    b.emit(.rts);
}

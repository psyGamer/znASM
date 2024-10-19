const znasm = @import("znasm");

const reg = @import("reg.zig");

const MEMSEL: znasm.FixedAddress = .init(0x420D, null);

pub fn reset(b: *znasm.Builder) void {
    // Register sizes a 8-bits on reset
    b.start_a_size = .@"8bit";
    b.start_xy_size = .@"8bit";

    b.setup_debug(@src(), @This(), null);

    // Jump to fast mirror in bank 0x80
    const fast = b.create_label();
    b.jump_long(fast);
    fast.define(b);

    // Enter native mode
    b.emit(.clc);
    b.emit(.xce);

    // Setup stauts flags
    b.change_status_flags(.{
        .carry = false,
        .zero = false,
        .irq_disable = true,
        .decimal = false,
        .xy_8bit = false,
        .a_8bit = true,
        .overflow = false,
        .negative = false,
    });

    // Initialize system
    init_cpu(b);

    var a = b.reg_a8();
    a = .load_store(b, MEMSEL, 0x12);

    // Main loop
    const loop = b.define_label();
    b.branch_always(loop);
}

fn init_cpu(b: *znasm.Builder) void {
    b.emit(.nop);
    // hi
    b.emit(.nop);
}

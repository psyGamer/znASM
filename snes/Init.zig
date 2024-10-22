const znasm = @import("znasm");

const cpu = @import("cpu.zig");
const ppu = @import("ppu.zig");
const dma = @import("dma.zig");
const hdma = @import("hdma.zig");

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

    // Main loop
    const loop = b.define_label();
    b.branch_always(loop);
}

fn init_cpu(b: *znasm.Builder) void {
    var x = b.reg_x16();

    // Set 3.58MHz access cycle
    cpu.set_rom_access_speed(b, .a, .fast);

    // Setup stack pointer
    x = .load(b, 0x1fff);
    x.transfer_to(.stack_ptr);

    // Reset Direct Page to 0x0000
    b.push_stack(.{ .addr16 = 0x0000 });
    b.pull_stack(.direct_page);

    // Set Data Bank to Program Bank
    b.push_stack(.program_bank);
    b.pull_stack(.data_bank);

    // Disable interrupts
    cpu.set_interrupt_config(b, .a, .{
        .joypad_autoread = false,
        .screen_interrupt = .disabled,
        .vblank_interrupt = false,
    });
    // Disable HDMA
    hdma.set_enabled(b, .a, .disable_all);

    // Disable screen
    ppu.set_screen(b, .a, .force_blank);

    // Fill Work-RAM with zeros using 2 DMAs
    dma.workram_memset(b, .a, 0, 0x00000, 0x10000, 0x00);
    dma.workram_memset(b, .a, 0, 0x10000, 0x10000, 0x00);
}

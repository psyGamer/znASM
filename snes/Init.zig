const znasm = @import("znasm");

const cpu = @import("cpu.zig");
const ppu = @import("ppu.zig");
const dma = @import("dma.zig");
const hdma = @import("hdma.zig");
const mmio = @import("reg.zig");

/// Starting location for execution
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

    // Setup CPU
    init_cpu(b);

    // Setup PPU
    b.call(init_registers);

    // Main loop
    const loop = b.define_label();
    b.branch_always(loop);
}

/// Initialize CPU state
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

    // Fill CGRAM with zeros
    // Fill VRAM with zeros
    dma.vram_memset(b, .a, 0, 0x0000, 0x10000, 0x00);
    // Fill Work-RAM with zeros using 2 DMAs
    dma.wram_memset(b, .a, 0, 0x00000, 0x10000, 0x00);
    dma.wram_memset(b, .a, 0, 0x10000, 0x10000, 0x00);
}

/// Initialize PPU & CPU MMIO registers
fn init_registers(b: *znasm.Builder) void {
    b.setup_debug(@src(), @This(), null);

    var a = b.reg_a8();
    var x = b.reg_x8();

    // Change Direct Page since we're accessing a lot of registers in that region
    b.set_direct_page(0x2100);

    // Enable F-Blank
    ppu.set_screen(b, .a, .force_blank);

    // Reset OAM registers
    b.store_zero(.@"8bit", mmio.OBJSEL);
    b.store_zero(.@"8bit", mmio.OAMADDL);
    b.store_zero(.@"8bit", mmio.OAMADDH);

    // Reset Background settings
    b.store_zero(.@"8bit", mmio.BGMODE);
    b.store_zero(.@"8bit", mmio.MOSAIC);
    b.store_zero(.@"8bit", mmio.BG1SC);
    b.store_zero(.@"8bit", mmio.BG2SC);
    b.store_zero(.@"8bit", mmio.BG3SC);
    b.store_zero(.@"8bit", mmio.BG4SC);
    b.store_zero(.@"8bit", mmio.BG12NBA);
    b.store_zero(.@"8bit", mmio.BG34NBA);
    // Set H-scrolling to 0x000
    b.store_zero(.@"8bit", mmio.BG1HOFS);
    b.store_zero(.@"8bit", mmio.BG1HOFS);
    b.store_zero(.@"8bit", mmio.BG2HOFS);
    b.store_zero(.@"8bit", mmio.BG2HOFS);
    b.store_zero(.@"8bit", mmio.BG3HOFS);
    b.store_zero(.@"8bit", mmio.BG3HOFS);
    b.store_zero(.@"8bit", mmio.BG4HOFS);
    b.store_zero(.@"8bit", mmio.BG4HOFS);
    // Set H-scrolling to 0x7FF
    a = .load(b, 0xFF);
    x = .load(b, 0x07);
    a.store(mmio.BG1VOFS);
    x.store(mmio.BG1VOFS);
    a.store(mmio.BG2VOFS);
    x.store(mmio.BG2VOFS);
    a.store(mmio.BG3VOFS);
    x.store(mmio.BG3VOFS);
    a.store(mmio.BG4VOFS);
    x.store(mmio.BG4VOFS);

    // Reset MODE1 VRAM increment
    a = .load(b, 0x80);
    a.store(mmio.VMAIN);

    // Reset VRAM address
    b.store_zero(.@"8bit", mmio.VMADDL);
    b.store_zero(.@"8bit", mmio.VMADDH);

    // Reset MODE7 settings
    b.store_zero(.@"8bit", mmio.M7SEL);
    b.store_zero(.@"8bit", mmio.M7A);
    a.store(mmio.M7A);
    b.store_zero(.@"8bit", mmio.M7B);
    b.store_zero(.@"8bit", mmio.M7B);
    b.store_zero(.@"8bit", mmio.M7C);
    b.store_zero(.@"8bit", mmio.M7C);
    b.store_zero(.@"8bit", mmio.M7D);
    a.store(mmio.M7D);
    b.store_zero(.@"8bit", mmio.M7X);
    b.store_zero(.@"8bit", mmio.M7X);
    b.store_zero(.@"8bit", mmio.M7Y);
    b.store_zero(.@"8bit", mmio.M7Y);

    // Reset CGRAM address
    b.store_zero(.@"8bit", mmio.CGADD);

    // Reset windows
    b.store_zero(.@"8bit", mmio.W12SEL);
    b.store_zero(.@"8bit", mmio.W34SEL);
    b.store_zero(.@"8bit", mmio.WOBJSEL);
    b.store_zero(.@"8bit", mmio.WH0);
    b.store_zero(.@"8bit", mmio.WH1);
    b.store_zero(.@"8bit", mmio.WH2);
    b.store_zero(.@"8bit", mmio.WH3);
    b.store_zero(.@"8bit", mmio.WBGLOG);
    b.store_zero(.@"8bit", mmio.WOBJLOG);
    b.store_zero(.@"8bit", mmio.TM);
    b.store_zero(.@"8bit", mmio.TS);
    b.store_zero(.@"8bit", mmio.TMW);
    b.store_zero(.@"8bit", mmio.TSW);

    // Reset color-math settings
    a = .load(b, 0x30);
    a.store(mmio.CGWSEL);
    b.store_zero(.@"8bit", mmio.CGADSUB);
    a = .load(b, 0xe0);
    a.store(mmio.COLDATA);
    b.store_zero(.@"8bit", mmio.SETINI);

    // Change Direct Page again
    b.set_direct_page(0x4200);

    b.store_zero(.@"8bit", mmio.NMITIMEN); // Disable NMI + joypad-autoread
    b.store_value(.@"8bit", .a, mmio.WRIO, 0xFF); // Put 0xFF on I/O port

    // Reset hardware multiplication / division
    b.store_zero(.@"8bit", mmio.WRMPYA);
    b.store_zero(.@"8bit", mmio.WRMPYB);
    b.store_zero(.@"8bit", mmio.WRDIVL);
    b.store_zero(.@"8bit", mmio.WRDIVH);
    b.store_zero(.@"8bit", mmio.WRDIVB);

    // Reset H/V IRQ setting
    b.store_zero(.@"8bit", mmio.HTIMEL);
    b.store_zero(.@"8bit", mmio.HTIMEH);
    b.store_zero(.@"8bit", mmio.VTIMEL);
    b.store_zero(.@"8bit", mmio.VTIMEH);

    b.store_zero(.@"8bit", mmio.MDMAEN); // Reset DMA designation
    b.store_zero(.@"8bit", mmio.HDMAEN); // Reset H-DMA designation

    // Reset Direct Page to 0x0000
    b.set_direct_page(0x0000);
    b.emit(.rts);
}

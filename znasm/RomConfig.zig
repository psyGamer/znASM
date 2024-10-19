const rom = @import("Rom.zig");
const Builder = @import("Builder.zig");
const FunctionSym = @import("symbol.zig").FunctionSym;

title: []const u8,
mode: rom.Header.Mode,
chipset: rom.Header.Chipset,
country: rom.Header.Country,
rom_size: u24,
ram_size: u24,
version: u8,

vectors: struct {
    native: struct {
        cop: FunctionSym = empty_vector,
        brk: FunctionSym = empty_vector,
        nmi: FunctionSym = empty_vector,
        irq: FunctionSym = empty_vector,
    },
    emulation: struct {
        cop: FunctionSym = empty_vector,
        nmi: FunctionSym = empty_vector,
        reset: FunctionSym = empty_vector,
        irqbrk: FunctionSym = empty_vector,
    },
},

// segments: []const rom.Segment,

// Vectors must have a target function, so default to this stub
pub fn empty_vector(b: *Builder) void {
    b.setup_debug(@src(), @This(), "empty_vector");
    b.emit(.rti);
}

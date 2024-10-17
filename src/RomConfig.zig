const rom = @import("Rom.zig");
const Builder = @import("Builder.zig");
const Symbol = @import("Function.zig").Symbol;

title: []const u8,
mode: rom.Header.Mode,
chipset: rom.Header.Chipset,
country: rom.Header.Country,
rom_size: u24,
ram_size: u24,
version: u8,

vectors: struct {
    native: struct {
        cop: Symbol = EmptyVector,
        brk: Symbol = EmptyVector,
        nmi: Symbol = EmptyVector,
        irq: Symbol = EmptyVector,
    },
    emulation: struct {
        cop: Symbol = EmptyVector,
        nmi: Symbol = EmptyVector,
        reset: Symbol = EmptyVector,
        irqbrk: Symbol = EmptyVector,
    },
},

// segments: []const rom.Segment,

// Vectors must have a target function, so default to this stub
pub fn EmptyVector(b: *Builder) void {
    b.emit(.rti);
}

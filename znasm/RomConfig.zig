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
        cop: FunctionSym = EmptyVector,
        brk: FunctionSym = EmptyVector,
        nmi: FunctionSym = EmptyVector,
        irq: FunctionSym = EmptyVector,
    },
    emulation: struct {
        cop: FunctionSym = EmptyVector,
        nmi: FunctionSym = EmptyVector,
        reset: FunctionSym = EmptyVector,
        irqbrk: FunctionSym = EmptyVector,
    },
},

// segments: []const rom.Segment,

// Vectors must have a target function, so default to this stub
pub fn EmptyVector(b: *Builder) void {
    b.setup_debug(@src(), @This(), "EmptyVector");
    b.emit(.rti);
}

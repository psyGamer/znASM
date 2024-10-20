const znasm = @import("znasm");
const Builder = znasm.Builder;

// General Registers

pub const NMITIMEN: znasm.Address = 0x4200;
pub const MEMSEL: znasm.Address = 0x420D;

// PPU Registers

pub const INIDISP: znasm.Address = 0x2100;

// DMA Registers

pub const HDMAEN: znasm.Address = 0x420C;

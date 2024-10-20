const znasm = @import("znasm");
const Builder = znasm.Builder;

// General Registers

pub const MEMSEL: znasm.Address = 0x420D;

// Interrupt Registers

pub const NMITIMEN: znasm.Address = 0x4200;
pub const HVBJOY: znasm.Address = 0x4212;

pub const HTIMEL: znasm.Address = 0x4207;
pub const HTIMEH: znasm.Address = 0x4208;

pub const VTIMEL: znasm.Address = 0x4209;
pub const VTIMEH: znasm.Address = 0x420A;

pub const RDNMI: znasm.Address = 0x4210;

pub const TIMEUP: znasm.Address = 0x4211;

// ALU Registers

pub const MPYL: znasm.Address = 0x2134;
pub const MPYM: znasm.Address = 0x2135;
pub const MPYH: znasm.Address = 0x2136;

pub const WRMPYA: znasm.Address = 0x4202;
pub const WRMPYB: znasm.Address = 0x4203;

pub const WRDIVL: znasm.Address = 0x4204;
pub const WRDIVH: znasm.Address = 0x4205;
pub const WRDIVB: znasm.Address = 0x4206;

pub const RDDIVL: znasm.Address = 0x4214;
pub const RDDIVH: znasm.Address = 0x4215;

pub const RDMPYL: znasm.Address = 0x4216;
pub const RDMPYH: znasm.Address = 0x4217;

// PPU Registers

pub const INIDISP: znasm.Address = 0x2100;

pub const OBJSEL: znasm.Address = 0x2101;

pub const OAMADDL: znasm.Address = 0x2102;
pub const OAMADDH: znasm.Address = 0x2103;
pub const OAMDATA: znasm.Address = 0x2104;

pub const BGMODE: znasm.Address = 0x2105;

pub const MOSAIC: znasm.Address = 0x2106;

pub const BG1SC: znasm.Address = 0x2107;
pub const BG2CC: znasm.Address = 0x2108;
pub const BG3SC: znasm.Address = 0x2109;
pub const BG4SC: znasm.Address = 0x210A;

pub const BG12NBA: znasm.Address = 0x210B;
pub const BG34NBA: znasm.Address = 0x210C;

pub const BG1HOFS: znasm.Address = 0x210D;
pub const BG1VOFS: znasm.Address = 0x210E;
pub const BG2HOFS: znasm.Address = 0x210F;
pub const BG2VOFS: znasm.Address = 0x2110;
pub const BG3HOFS: znasm.Address = 0x2111;
pub const BG3VOFS: znasm.Address = 0x2112;
pub const BG4HOFS: znasm.Address = 0x2113;
pub const BG4VOFS: znasm.Address = 0x2114;

pub const M7HOFS: znasm.Address = 0x210D;
pub const M7VOFS: znasm.Address = 0x210E;

pub const VMAIN: znasm.Address = 0x2115;
pub const VMADDL: znasm.Address = 0x2116;
pub const VMADDH: znasm.Address = 0x2117;
pub const VMDATAL: znasm.Address = 0x2118;
pub const VMDATAH: znasm.Address = 0x2119;

pub const M7SEL: znasm.Address = 0x211A;
pub const M7A: znasm.Address = 0x211B;
pub const M7B: znasm.Address = 0x211C;
pub const M7C: znasm.Address = 0x211D;
pub const M7D: znasm.Address = 0x211E;
pub const M7X: znasm.Address = 0x211F;
pub const M7Y: znasm.Address = 0x2120;

pub const CGADD: znasm.Address = 0x2121;
pub const CGDATA: znasm.Address = 0x2122;

pub const W12SEL: znasm.Address = 0x2123;
pub const W34SEL: znasm.Address = 0x2124;
pub const WOBJSEL: znasm.Address = 0x2125;

pub const WH0: znasm.Address = 0x2126;
pub const WH1: znasm.Address = 0x2127;
pub const WH2: znasm.Address = 0x2128;
pub const WH3: znasm.Address = 0x2129;

pub const WBGLOG: znasm.Address = 0x212A;
pub const WOBJLOG: znasm.Address = 0x212B;

pub const TM: znasm.Address = 0x212C;
pub const TS: znasm.Address = 0x212D;

pub const TMW: znasm.Address = 0x212E;
pub const TSW: znasm.Address = 0x212F;

pub const CGWSEL: znasm.Address = 0x2130;
pub const CGADSUB: znasm.Address = 0x2131;

pub const SETINI: znasm.Address = 0x2133;

pub const SLHV: znasm.Address = 0x2137;

pub const OAMDATAREAD: znasm.Address = 0x2138;

pub const VMDATALREAD: znasm.Address = 0x2139;
pub const VMDATAHREAD: znasm.Address = 0x213A;

pub const CGMDATAREAD: znasm.Address = 0x213B;

pub const OPHCT: znasm.Address = 0x213C;
pub const OPVCT: znasm.Address = 0x213D;

pub const STAT77: znasm.Address = 0x213E;
pub const STAT78: znasm.Address = 0x213F;

// DMA Registers

pub const WMDATA: znasm.Address = 0x2180;

pub const WMADDL: znasm.Address = 0x2181;
pub const WMADDM: znasm.Address = 0x2182;
pub const WMADDH: znasm.Address = 0x2183;

pub const MDMAEN: znasm.Address = 0x420B;
pub const HDMAEN: znasm.Address = 0x420C;

// TODO: Channel specific registers

// Joypad Registers

pub const JOYOUT: znasm.Address = 0x4016;

pub const JOYSER0: znasm.Address = 0x4016;
pub const JOYSER1: znasm.Address = 0x4017;

pub const WRIO: znasm.Address = 0x4201;
pub const RDIO: znasm.Address = 0x4213;

pub const JOY1L: znasm.Address = 0x4218;
pub const JOY1H: znasm.Address = 0x4219;

pub const JOY2L: znasm.Address = 0x421A;
pub const JOY2H: znasm.Address = 0x421B;

pub const JOY3L: znasm.Address = 0x421C;
pub const JOY3H: znasm.Address = 0x421D;

pub const JOY4L: znasm.Address = 0x421E;
pub const JOY4H: znasm.Address = 0x421F;

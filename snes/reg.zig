//! Definitions for all Memory-Mapped I/O regsiters, using the official names
const Addr = @import("znasm").Address;

// General Registers

pub const MEMSEL: Addr = 0x420D;

// Interrupt Registers

pub const NMITIMEN: Addr = 0x4200;
pub const HVBJOY: Addr = 0x4212;

pub const HTIMEL: Addr = 0x4207;
pub const HTIMEH: Addr = 0x4208;

pub const VTIMEL: Addr = 0x4209;
pub const VTIMEH: Addr = 0x420A;

pub const RDNMI: Addr = 0x4210;

pub const TIMEUP: Addr = 0x4211;

// ALU Registers

pub const MPYL: Addr = 0x2134;
pub const MPYM: Addr = 0x2135;
pub const MPYH: Addr = 0x2136;

pub const WRMPYA: Addr = 0x4202;
pub const WRMPYB: Addr = 0x4203;

pub const WRDIVL: Addr = 0x4204;
pub const WRDIVH: Addr = 0x4205;
pub const WRDIVB: Addr = 0x4206;

pub const RDDIVL: Addr = 0x4214;
pub const RDDIVH: Addr = 0x4215;

pub const RDMPYL: Addr = 0x4216;
pub const RDMPYH: Addr = 0x4217;

// PPU Registers

pub const INIDISP: Addr = 0x2100;

pub const OBJSEL: Addr = 0x2101;

pub const OAMADDL: Addr = 0x2102;
pub const OAMADDH: Addr = 0x2103;
pub const OAMDATA: Addr = 0x2104;

pub const BGMODE: Addr = 0x2105;

pub const MOSAIC: Addr = 0x2106;

pub const BG1SC: Addr = 0x2107;
pub const BG2SC: Addr = 0x2108;
pub const BG3SC: Addr = 0x2109;
pub const BG4SC: Addr = 0x210A;

pub const BG12NBA: Addr = 0x210B;
pub const BG34NBA: Addr = 0x210C;

pub const BG1HOFS: Addr = 0x210D;
pub const BG1VOFS: Addr = 0x210E;
pub const BG2HOFS: Addr = 0x210F;
pub const BG2VOFS: Addr = 0x2110;
pub const BG3HOFS: Addr = 0x2111;
pub const BG3VOFS: Addr = 0x2112;
pub const BG4HOFS: Addr = 0x2113;
pub const BG4VOFS: Addr = 0x2114;

pub const M7HOFS: Addr = 0x210D;
pub const M7VOFS: Addr = 0x210E;

pub const VMAIN: Addr = 0x2115;
pub const VMADDL: Addr = 0x2116;
pub const VMADDH: Addr = 0x2117;
pub const VMDATAL: Addr = 0x2118;
pub const VMDATAH: Addr = 0x2119;

pub const M7SEL: Addr = 0x211A;
pub const M7A: Addr = 0x211B;
pub const M7B: Addr = 0x211C;
pub const M7C: Addr = 0x211D;
pub const M7D: Addr = 0x211E;
pub const M7X: Addr = 0x211F;
pub const M7Y: Addr = 0x2120;

pub const CGADD: Addr = 0x2121;
pub const CGDATA: Addr = 0x2122;

pub const W12SEL: Addr = 0x2123;
pub const W34SEL: Addr = 0x2124;
pub const WOBJSEL: Addr = 0x2125;

pub const WH0: Addr = 0x2126;
pub const WH1: Addr = 0x2127;
pub const WH2: Addr = 0x2128;
pub const WH3: Addr = 0x2129;

pub const WBGLOG: Addr = 0x212A;
pub const WOBJLOG: Addr = 0x212B;

pub const TM: Addr = 0x212C;
pub const TS: Addr = 0x212D;

pub const TMW: Addr = 0x212E;
pub const TSW: Addr = 0x212F;

pub const CGWSEL: Addr = 0x2130;
pub const CGADSUB: Addr = 0x2131;
pub const COLDATA: Addr = 0x2132;

pub const SETINI: Addr = 0x2133;

pub const SLHV: Addr = 0x2137;

pub const OAMDATAREAD: Addr = 0x2138;

pub const VMDATALREAD: Addr = 0x2139;
pub const VMDATAHREAD: Addr = 0x213A;

pub const CGMDATAREAD: Addr = 0x213B;

pub const OPHCT: Addr = 0x213C;
pub const OPVCT: Addr = 0x213D;

pub const STAT77: Addr = 0x213E;
pub const STAT78: Addr = 0x213F;

// DMA Registers

pub const WMDATA: Addr = 0x2180;

pub const WMADDL: Addr = 0x2181;
pub const WMADDM: Addr = 0x2182;
pub const WMADDH: Addr = 0x2183;

pub const MDMAEN: Addr = 0x420B;
pub const HDMAEN: Addr = 0x420C;

pub const channel_0 = 0x00;
pub const channel_1 = 0x10;
pub const channel_2 = 0x20;
pub const channel_3 = 0x30;
pub const channel_4 = 0x40;
pub const channel_5 = 0x50;
pub const channel_6 = 0x60;
pub const channel_7 = 0x70;

pub const DMAPn: Addr = 0x4300;

pub const BBADn: Addr = 0x4301;

pub const A1TnL: Addr = 0x4302;
pub const A1TnH: Addr = 0x4303;
pub const A1Bn: Addr = 0x4304;

pub const DASnL: Addr = 0x4305;
pub const DASnH: Addr = 0x4306;
pub const DASBn: Addr = 0x4307;

pub const A2AnL: Addr = 0x4308;
pub const A2AnH: Addr = 0x4309;

pub const NLTRn: Addr = 0x430A;

// Joypad Registers

pub const JOYOUT: Addr = 0x4016;

pub const JOYSER0: Addr = 0x4016;
pub const JOYSER1: Addr = 0x4017;

pub const WRIO: Addr = 0x4201;
pub const RDIO: Addr = 0x4213;

pub const JOY1L: Addr = 0x4218;
pub const JOY1H: Addr = 0x4219;

pub const JOY2L: Addr = 0x421A;
pub const JOY2H: Addr = 0x421B;

pub const JOY3L: Addr = 0x421C;
pub const JOY3H: Addr = 0x421D;

pub const JOY4L: Addr = 0x421E;
pub const JOY4H: Addr = 0x421F;

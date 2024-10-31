const Symbol = @import("symbol.zig").Symbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const InstructionType = @import("instruction.zig").InstructionType;

pub const module = "builtin";

pub const empty_vector_loc: SymbolLocation = .{
    .module = module,
    .name = "empty_vector",
};

var empty_vector_asm: [1]u8 = .{@intFromEnum(InstructionType.rti)};
pub const empty_vector_sym: Symbol = .{ .function = .{
    .is_pub = false,
    .node = undefined,
    .assembly_data = &empty_vector_asm,
} };

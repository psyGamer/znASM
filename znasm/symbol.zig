const Builder = @import("Builder.zig");

/// Label to a location in ROM / RAM, being functions, global variables, etc.
pub const Symbol = union(enum) {
    pub const Address = u24;
    pub const Data = *const DataSymbol(void);
    pub const Function = *const fn (b: *Builder) void;

    /// Fixed memory-mapped target address
    address: Address,
    /// Read-Only target data in ROM
    data: Data,
    /// Executable target function in ROM
    function: Function,
};

// Referencing Symbol.Function would cause a dependency loop...
// See https://github.com/ziglang/zig/issues/12325
pub const FunctionSym = *const fn (b: *Builder) void;

pub fn DataSymbol(comptime T: type) type {
    return struct {
        const Data = @This();

        value: T,
        data_size: u16,

        /// Optionally constrains the target memory-mapped bank
        bank: ?u8,
        /// Constrains the minimum absolute address for the first byte
        addr_min: u16,
        /// Constrains the maximum absolute address for the last byte
        addr_max: u16,

        pub fn init(value: T) Data {
            return .{
                .value = value,
                .data_size = @sizeOf(T),

                .bank = null,
                .addr_min = 0x0000,
                .addr_max = 0xFFFF,
            };
        }
        pub fn init_bank(value: T, bank: u8) Data {
            return .{
                .value = value,
                .data_size = @sizeOf(T),

                .bank = bank,
                .addr_min = 0x0000,
                .addr_max = 0xFFFF,
            };
        }
        pub fn init_bank_range(value: T, bank: u8, addr_min: u16, addr_max: u16) Data {
            return .{
                .value = value,
                .data_size = @sizeOf(T),

                .bank = bank,
                .addr_min = addr_min,
                .addr_max = addr_max,
            };
        }

        pub fn reloc_addr16(data: *const Data) Builder.InstructionInfo.Relocation {
            return .{
                .type = .addr16,
                .target_sym = .{ .data = @ptrCast(data) },
                .target_offset = 0,
            };
        }
        pub fn reloc_bank(data: *const Data) Builder.InstructionInfo.Relocation {
            return .{
                .type = .addr_bank,
                .target_sym = .{ .data = @ptrCast(data) },
                .target_offset = 0,
            };
        }
    };
}

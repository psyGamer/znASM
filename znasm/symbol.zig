const std = @import("std");
const Builder = @import("Builder.zig");

/// Label to a location in ROM / RAM, being functions, global variables, etc.
pub const Symbol = union(enum) {
    pub const Address = u24;
    pub const Function = *const fn (b: *Builder) void;

    pub const Data = DataSymbol(void);

    /// Fixed memory-mapped target address
    address: Address,
    /// Executable target function in ROM
    function: Function,

    /// Read-Only target data in ROM
    data: Data,
};

// Referencing Symbol.Function would cause a dependency loop...
// See https://github.com/ziglang/zig/issues/12325
pub const FunctionSym = *const fn (b: *Builder) void;

var symbol_id_prng: std.Random.DefaultPrng = .init(0x00);

pub fn DataSymbol(comptime T: type) type {
    return *const struct {
        const Data = @This();

        /// Actual byte data of this symbol
        data: []const u8,

        /// Name of this symbol
        name: []const u8,

        /// Optionally constrains the target memory-mapped bank
        bank: ?u8,
        /// Constrains the minimum absolute address for the first byte
        addr_min: u16,
        /// Constrains the maximum absolute address for the last byte
        addr_max: u16,

        pub fn init(comptime value: T, comptime name: []const u8, declaring_type: type) *const Data {
            // Statically store data
            const S = struct {
                const value_storage = value;
                const value_ptr: [*]const u8 = @ptrCast(&value_storage);

                const symbol_storage: Data = .{
                    .data = value_ptr[0..@sizeOf(T)],
                    .name = @typeName(declaring_type) ++ "@" ++ name,

                    .bank = 0x80,
                    .addr_min = 0x0000,
                    .addr_max = 0xFFFF,
                };
            };

            return &S.symbol_storage;
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

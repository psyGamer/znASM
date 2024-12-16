const std = @import("std");
const Ast = @import("Ast.zig");
const Ir = @import("ir.zig").Ir;
const Sema = @import("Sema.zig");
const ExpressionValue = Sema.ExpressionValue;
const SymbolIndex = Sema.SymbolIndex;
const InstructionInfo = @import("CodeGen.zig").InstructionInfo;

pub const SymbolLocation = struct {
    pub const separator = "::";

    module: []const u8,
    name: []const u8,

    pub fn parse(sym_loc: []const u8, current_module: []const u8) SymbolLocation {
        const separator_idx = std.mem.indexOf(u8, sym_loc, separator) orelse return .{
            .module = current_module,
            .name = sym_loc,
        };
        return .{
            .module = sym_loc[0..separator_idx],
            .name = sym_loc[(separator_idx + separator.len)..],
        };
    }

    pub fn format(value: SymbolLocation, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(value.module);
        try writer.writeAll("::");
        try writer.writeAll(value.name);
    }
};

const A = enum(u2) {};

pub const Symbol = union(enum) {
    pub const Common = struct {
        /// Parent node
        node: Ast.NodeIndex,
        /// Accessibilty for other modules
        is_pub: bool,

        /// Current status for analyzation
        analyze_status: enum { pending, active, done } = .pending,
    };

    pub const Function = struct {
        /// Commonly shared data between symbols
        common: Common,

        /// Memory-mapped target bank
        bank: u8,
        /// Offset into the target bank
        bank_offset: u16 = undefined,

        /// Intermediate representation for instructions
        ir: []const Ir,
        /// Named indices to target instructions
        labels: []const struct { []const u8, u16 },
        /// Higher-level instruction data
        instructions: []InstructionInfo,
        /// Raw assembly data
        assembly_data: []u8,
    };
    pub const Constant = struct {
        /// Commonly shared data between symbols
        common: Common,

        /// Memory-mapped target bank
        bank: u8,
        /// Offset into the target bank
        bank_offset: u16 = undefined,

        /// Type of this constant
        type: TypeSymbol,
        /// Value of this constant
        value: ExpressionValue,
    };
    pub const Variable = struct {
        /// Commonly shared data between symbols
        common: Common,

        /// Minimum offset into Work-RAM
        wram_offset_min: u17,
        /// Maximum offset into Work-RAM
        wram_offset_max: u17,

        /// Offset into Work-RAM
        wram_offset: u17 = undefined,

        /// Type of this variable
        type: TypeSymbol,
    };
    pub const Register = struct {
        pub const AccessType = enum {
            /// Write-Only 8-bit
            w8,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// How this register can be accessed
        access: AccessType,

        /// Bank-independant address of this register
        address: u16,
        /// Type of this register
        type: TypeSymbol,
    };

    pub const Packed = struct {
        pub const Field = struct {
            name: []const u8,
            type: TypeSymbol,
            default_value: ?ExpressionValue,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeSymbol,

        /// Fields representable by this enum
        fields: []const Field,
    };
    pub const Enum = struct {
        pub const Field = struct {
            name: []const u8,
            value: ExpressionValue,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeSymbol,

        /// Fields representable by this enum
        fields: []const Field,
    };

    function: Function,
    constant: Constant,
    variable: Variable,
    register: Register,

    @"packed": Packed,
    @"enum": Enum,

    pub fn common(sym: *Symbol) *Common {
        return switch (sym.*) {
            .function => |*fn_sym| &fn_sym.common,
            .constant => |*const_sym| &const_sym.common,
            .variable => |*var_sym| &var_sym.common,
            .register => |*reg_sym| &reg_sym.common,
            .@"packed" => |*packed_sym| &packed_sym.common,
            .@"enum" => |*enum_sym| &enum_sym.common,
        };
    }

    pub fn deinit(sym: *Symbol, allocator: std.mem.Allocator) void {
        switch (sym.*) {
            .function => |fn_sym| {
                for (fn_sym.ir) |ir| {
                    ir.deinit(allocator);
                }
                allocator.free(fn_sym.ir);
                allocator.free(fn_sym.labels);
                allocator.free(fn_sym.instructions);
                allocator.free(fn_sym.assembly_data);
            },
            .constant => |const_sym| {
                const_sym.value.deinit(allocator);
            },
            .variable => {},
            .register => {},
            .@"packed" => |packed_sym| {
                for (packed_sym.fields) |field| {
                    if (field.default_value) |default| {
                        default.deinit(allocator);
                    }
                }
                allocator.free(packed_sym.fields);
            },
            .@"enum" => |enum_sym| {
                for (enum_sym.fields) |field| {
                    field.value.deinit(allocator);
                }
                allocator.free(enum_sym.fields);
            },
        }
    }
};

pub const TypeSymbol = union(enum) {
    // Backing types for comptime variables
    // Allows representing up both u64/i64
    pub const ComptimeIntValue = i65;
    /// Useful when working with just the bytes
    pub const UnsignedComptimeIntValue = u65;

    const Payload = union(enum) {
        // Payload indicates bit-size
        signed_int: u16,
        unsigned_int: u16,
        // Payload indicates value
        comptime_int: ComptimeIntValue,

        @"packed": struct {
            is_signed: bool,
            bits: u16,
            symbol_index: SymbolIndex,
        },
        @"enum": struct {
            is_signed: bool,
            bits: u16,
            symbol_index: SymbolIndex,
        },
    };

    /// Simple type with no extra annotations
    raw: Payload,

    /// Checks if `type_sym` is assigable to `other_sym`
    pub fn isAssignableTo(type_sym: TypeSymbol, other_sym: TypeSymbol) bool {
        if (std.meta.activeTag(type_sym) != std.meta.activeTag(other_sym)) {
            return false;
        }

        switch (type_sym) {
            .raw => |payload| {
                const other_payload = other_sym.raw;

                switch (payload) {
                    .signed_int => |bits| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits >= bits,
                        .unsigned_int => return false,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                        .@"enum" => |other_backing_type| return other_backing_type.is_signed and other_backing_type.bits >= bits,
                        .@"packed" => |other_backing_type| return other_backing_type.is_signed and other_backing_type.bits >= bits,
                    },
                    .unsigned_int => |bits| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits > bits,
                        .unsigned_int => |other_bits| return other_bits >= bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                        .@"enum" => |other_backing_type| return other_backing_type.bits + @intFromBool(other_backing_type.is_signed) >= bits,
                        .@"packed" => |other_backing_type| return other_backing_type.bits + @intFromBool(other_backing_type.is_signed) >= bits,
                    },
                    .comptime_int => return true, // Depends on the size of the value
                    .@"packed" => |backing_type| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                        .unsigned_int => |other_bits| return backing_type.is_signed and other_bits >= backing_type.bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                        .@"packed" => |other_backing_type| return if (backing_type.is_signed)
                            other_backing_type.is_signed and other_backing_type.bits >= backing_type.bits
                        else
                            other_backing_type.bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                        .@"enum" => |other_backing_type| return if (backing_type.is_signed)
                            other_backing_type.is_signed and other_backing_type.bits >= backing_type.bits
                        else
                            other_backing_type.bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                    },
                    .@"enum" => |backing_type| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                        .unsigned_int => |other_bits| return backing_type.is_signed and other_bits >= backing_type.bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                        .@"packed" => |other_backing_type| return if (backing_type.is_signed)
                            other_backing_type.is_signed and other_backing_type.bits >= backing_type.bits
                        else
                            other_backing_type.bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                        .@"enum" => |other_backing_type| return if (backing_type.is_signed)
                            other_backing_type.is_signed and other_backing_type.bits >= backing_type.bits
                        else
                            other_backing_type.bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                    },
                }
            },
        }
    }

    /// Calculates the size of this type in bytes
    pub fn size(type_sym: TypeSymbol) u16 {
        switch (type_sym) {
            .raw => |payload| return payloadSize(payload),
        }
    }
    fn payloadSize(payload: Payload) u16 {
        return switch (payload) {
            .signed_int => |bits| std.mem.alignForward(u16, bits, 8) / 8,
            .unsigned_int => |bits| std.mem.alignForward(u16, bits, 8) / 8,
            .comptime_int => unreachable,
            .@"packed" => |backing_type| std.mem.alignForward(u16, backing_type.bits, 8) / 8,
            .@"enum" => |backing_type| std.mem.alignForward(u16, backing_type.bits, 8) / 8,
        };
    }

    /// Calculates the size of this type in bits
    pub fn bitSize(type_sym: TypeSymbol) u16 {
        switch (type_sym) {
            .raw => |payload| return payloadBitSize(payload),
        }
    }
    fn payloadBitSize(payload: Payload) u16 {
        return switch (payload) {
            .signed_int => |bits| bits,
            .unsigned_int => |bits| bits,
            .comptime_int => unreachable,
            .@"packed" => |backing_type| backing_type.bits,
            .@"enum" => |backing_type| backing_type.bits,
        };
    }

    pub fn format(type_sym: TypeSymbol, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (type_sym) {
            .raw => |payload| return formatPayload(payload, writer),
        }
    }
    fn formatPayload(payload: Payload, writer: anytype) !void {
        return switch (payload) {
            .signed_int => |bits| writer.print("i{d}", .{bits}),
            .unsigned_int => |bits| writer.print("u{d}", .{bits}),
            .comptime_int => writer.writeAll("comptime_int"),
            .@"packed" => |backing_type| if (backing_type.is_signed)
                writer.print("packed(i{d})", .{backing_type.bits})
            else
                writer.print("packed(u{d})", .{backing_type.bits}),
            .@"enum" => |backing_type| if (backing_type.is_signed)
                writer.print("enum(i{d})", .{backing_type.bits})
            else
                writer.print("enum(u{d})", .{backing_type.bits}),
        };
    }

    pub fn formatDetailed(type_sym: *const TypeSymbol, sema: *const Sema, writer: anytype) !void {
        switch (type_sym.*) {
            .raw => |payload| return formatPayloadDetailed(payload, sema, writer),
        }
    }
    fn formatPayloadDetailed(payload: Payload, sema: *const Sema, writer: anytype) !void {
        return switch (payload) {
            .signed_int => |bits| writer.print("i{d}", .{bits}),
            .unsigned_int => |bits| writer.print("u{d}", .{bits}),
            .comptime_int => writer.writeAll("comptime_int"),
            .@"packed" => |packed_type_sym| {
                const packed_sym_loc = sema.symbols.items(.loc)[packed_type_sym.symbol_index];

                if (packed_type_sym.is_signed)
                    try writer.print("packed {}(i{d})", .{ packed_sym_loc, packed_type_sym.bits })
                else
                    try writer.print("packed {}(u{d})", .{ packed_sym_loc, packed_type_sym.bits });
            },
            .@"enum" => |enum_type_sym| {
                const enum_sym_loc = sema.symbols.items(.loc)[enum_type_sym.symbol_index];

                if (enum_type_sym.is_signed)
                    try writer.print("enum {}(i{d})", .{ enum_sym_loc, enum_type_sym.bits })
                else
                    try writer.print("enum {}(u{d})", .{ enum_sym_loc, enum_type_sym.bits });
            },
        };
    }
};

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
        /// Current status for analyzation
        analyze_status: enum { pending, active, done } = .pending,
        /// Accessibilty for other modules
        is_pub: bool,
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

        /// Type if this variable
        type: TypeSymbol,
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
    @"enum": Enum,

    pub fn common(sym: *Symbol) *Common {
        return switch (sym.*) {
            .function => |*fn_sym| &fn_sym.common,
            .constant => |*const_sym| &const_sym.common,
            .variable => |*var_sym| &var_sym.common,
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
            .constant => {},
            .variable => {},
            .@"enum" => {
                allocator.free(sym.@"enum".fields);
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
                    },
                    .unsigned_int => |bits| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits > bits,
                        .unsigned_int => |other_bits| return other_bits >= bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                        .@"enum" => |other_backing_type| return other_backing_type.bits + @intFromBool(other_backing_type.is_signed) >= bits,
                    },
                    .comptime_int => return true, // Depends on the size of the value
                    .@"enum" => |backing_type| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits >= backing_type.bits + @intFromBool(!backing_type.is_signed),
                        .unsigned_int => |other_bits| return backing_type.is_signed and other_bits >= backing_type.bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
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
            .@"enum" => |backing_type| std.mem.alignForward(u16, backing_type.bits, 8) / 8,
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
            .@"enum" => |backing_type| if (backing_type.is_signed)
                writer.print("enum(i{d})", .{backing_type.bits})
            else
                writer.print("enum(u{d})", .{backing_type.bits}),
        };
    }
};

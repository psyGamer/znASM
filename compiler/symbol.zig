const std = @import("std");
const Ast = @import("Ast.zig");
const Ir = @import("ir.zig").Ir;
const ExpressionValue = @import("Sema.zig").ExpressionValue;
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

pub const Symbol = union(enum) {
    pub const Function = struct {
        is_pub: bool,

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

        /// `fn_def` node of this symbol
        node: Ast.NodeIndex,
    };
    pub const Constant = struct {
        is_pub: bool,

        /// Memory-mapped target bank
        bank: u8,
        /// Offset into the target bank
        bank_offset: u16 = undefined,

        /// Type of this constant
        type: TypeSymbol,
        /// Value of this constant
        value: ExpressionValue,

        /// `const_def` node of this symbol
        node: Ast.NodeIndex,
    };
    pub const Variable = struct {
        is_pub: bool,

        /// Minimum offset into Work-RAM
        wram_offset_min: u17,
        /// Maximum offset into Work-RAM
        wram_offset_max: u17,

        /// Offset into Work-RAM
        wram_offset: u17 = undefined,

        /// Type if this variable
        type: TypeSymbol,

        /// `const_def` node of this symbol
        node: Ast.NodeIndex,
    };

    function: Function,
    constant: Constant,
    variable: Variable,

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
                    },
                    .unsigned_int => |bits| switch (other_payload) {
                        .signed_int => |other_bits| return other_bits > bits,
                        .unsigned_int => |other_bits| return other_bits >= bits,
                        .comptime_int => unreachable, // Cannot assign to a comptime_int
                    },
                    .comptime_int => return true, // Depends on the size of the value
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
        };
    }
};

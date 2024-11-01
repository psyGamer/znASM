const std = @import("std");
const Ast = @import("Ast.zig");
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
};

pub const Symbol = union(enum) {
    pub const Variable = struct {
        type: SymbolLocation,
        is_const: bool,
        is_pub: bool,

        bank: ?u8,
        addr_min: ?u16,
        addr_max: ?u16,

        node: Ast.NodeIndex,
    };
    pub const Function = struct {
        is_pub: bool,
        // return_type: SymbolLocation,

        /// Named indices to target instructions
        labels: []const struct { []const u8, u16 } = &.{},
        /// Higher-level instruction data
        instructions: []InstructionInfo = &.{},
        /// Raw assembly data
        assembly_data: []u8 = &.{},

        /// Memory-mapped target bank
        bank: u8 = undefined,
        /// Offset into the target bank
        bank_offset: u16 = undefined,

        /// `fn_def` node of this symbol
        node: Ast.NodeIndex,
    };

    variable: Variable,
    function: Function,

    pub fn deinit(sym: *Symbol, allocator: std.mem.Allocator) void {
        switch (sym.*) {
            .variable => {},
            .function => |*fn_sym| {
                allocator.free(fn_sym.assembly_data);
                // fn_sym.ir.deinit(allocator);
                // fn_sym.code.deinit(allocator);
                // fn_sym.relocs.deinit(allocator);
            },
        }
    }
};

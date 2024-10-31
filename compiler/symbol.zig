const std = @import("std");
const Ast = @import("Ast.zig");

pub const SymbolLocation = struct {
    pub const separator = "::";

    module: ?[]const u8,
    name: []const u8,

    pub fn parse(sym_loc: []const u8) SymbolLocation {
        const separator_idx = std.mem.indexOf(u8, sym_loc, separator) orelse return .{
            .module = null,
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
        pub const Reloc = struct {
            offset: u24,
            sym: SymbolLocation,
            type: enum {
                imm8,
            },
        };
        // TODO: Parameters
        // pub const Parameter = struct {
        //     name: []const u8,
        //     type: Symbol,
        // };

        // return_type: SymbolLocation,
        // segment: []const u8,
        // is_inline: bool,
        is_pub: bool,
        // parameters: []const Parameter

        // TODO: Is this an appropriate location?
        node: Ast.NodeIndex,

        assembly_data: []u8 = &.{},
        // ir: std.ArrayListUnmanaged(IR) = .{},
        // code: std.ArrayListUnmanaged(u8) = .{},
        // relocs: std.ArrayListUnmanaged(Reloc) = .{},
        // segment_offset: u16 = 0,
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

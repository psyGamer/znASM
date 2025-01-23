const std = @import("std");
const Ast = @import("Ast.zig");
const Ir = @import("ir.zig").Ir;
const Sema = @import("Sema.zig");
const FunctionAnalyzer = @import("sema/FunctionAnalyzer.zig");
const SymbolIndex = Sema.SymbolIndex;
const ExpressionIndex = Sema.ExpressionIndex;
const TypeExpressionIndex = Sema.TypeExpressionIndex;
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
        try writer.writeAll(separator);
        try writer.writeAll(value.name);
    }
};

pub const Symbol = union(enum) {
    pub const Common = struct {
        /// Containing node for this symbol
        node: Ast.NodeIndex,
        /// Accessibilty for other modules
        is_pub: bool,

        /// Module which contains this symbol
        module_index: Sema.ModuleIndex = undefined,
        /// Index of this symbol into the modules's symbol-map
        /// Useful for retrieving the name of this symbol
        module_symbol_index: Sema.SymbolIndex = undefined,

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

        calling_convention: FunctionAnalyzer.CallingConvention,

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
        type: TypeExpressionIndex,
        /// Value of this constant
        value: ExpressionIndex,
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
        type: TypeExpressionIndex,
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
        type: TypeExpressionIndex,
    };

    pub const Struct = struct {
        pub const Field = struct {
            name: []const u8,
            type: TypeExpressionIndex,
            default_value: ExpressionIndex,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Fields of this struct
        fields: []const Field,
    };
    pub const Packed = struct {
        pub const Field = struct {
            name: []const u8,
            type: TypeExpressionIndex,
            default_value: ExpressionIndex,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeExpressionIndex,

        /// Fields of this packed
        fields: []const Field,
    };
    pub const Enum = struct {
        pub const Field = struct {
            name: []const u8,
            value: ExpressionIndex,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeExpressionIndex,

        /// Fields representable by this enum
        fields: []const Field,
    };

    function: Function,
    constant: Constant,
    variable: Variable,
    register: Register,

    @"struct": Struct,
    @"packed": Packed,
    @"enum": Enum,

    pub fn common(sym: *Symbol) *Common {
        return switch (sym.*) {
            .function => |*fn_sym| &fn_sym.common,
            .constant => |*const_sym| &const_sym.common,
            .variable => |*var_sym| &var_sym.common,
            .register => |*reg_sym| &reg_sym.common,
            .@"struct" => |*struct_sym| &struct_sym.common,
            .@"packed" => |*packed_sym| &packed_sym.common,
            .@"enum" => |*enum_sym| &enum_sym.common,
        };
    }
    pub fn commonConst(sym: *const Symbol) *const Common {
        return @constCast(sym).common();
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
            .@"struct" => |struct_sym| {
                allocator.free(struct_sym.fields);
            },
            .@"packed" => |packed_sym| {
                allocator.free(packed_sym.fields);
            },
            .@"enum" => |enum_sym| {
                allocator.free(enum_sym.fields);
            },
            else => {},
        }
    }
};

const std = @import("std");
const builtin_module = @import("builtin_module.zig");

const Module = @import("Module.zig");
const Ast = @import("Ast.zig");
const Sema = @import("Sema.zig");
const Expression = Sema.Expression;
const TypeExpression = Sema.TypeExpression;
const SemanticIr = @import("sema/SemanticIr.zig");
const AssemblyIr = @import("codegen/AssemblyIr.zig");
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
        if (value.module.len > 0) {
            try writer.writeAll(value.module);
            try writer.writeAll(separator);
        }
        try writer.writeAll(value.name);
    }
};

pub const Symbol = union(enum) {
    pub const Index = enum(u32) {
        none = std.math.maxInt(u32),
        _,

        /// Helper function to cast a generic number to a `Index`
        pub inline fn cast(x: anytype) Index {
            return switch (@typeInfo(@TypeOf(x))) {
                .null => .none,
                .int, .comptime_int => @enumFromInt(@as(u32, @intCast(x))),
                .optional => if (x) |value| @enumFromInt(@as(u32, @intCast(value))) else .none,
                else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a Index"),
            };
        }

        /// Fetches the underlying value
        pub fn get(index: Index, sema: *Sema) *Symbol {
            return &sema.symbols.items[@intFromEnum(index)];
        }

        pub fn getCommon(index: Index, sema: *Sema) *Symbol.Common {
            return sema.symbols.items[@intFromEnum(index)].common();
        }

        pub fn getFn(index: Index, sema: *Sema) *Symbol.Function {
            return &sema.symbols.items[@intFromEnum(index)].function;
        }
        pub fn getConst(index: Index, sema: *Sema) *Symbol.Constant {
            return &sema.symbols.items[@intFromEnum(index)].constant;
        }
        pub fn getVar(index: Index, sema: *Sema) *Symbol.Variable {
            return &sema.symbols.items[@intFromEnum(index)].variable;
        }
        pub fn getReg(index: Index, sema: *Sema) *Symbol.Register {
            return &sema.symbols.items[@intFromEnum(index)].register;
        }
        pub fn getStruct(index: Index, sema: *Sema) *Symbol.Struct {
            return &sema.symbols.items[@intFromEnum(index)].@"struct";
        }
        pub fn getPacked(index: Index, sema: *Sema) *Symbol.Packed {
            return &sema.symbols.items[@intFromEnum(index)].@"packed";
        }
        pub fn getEnum(index: Index, sema: *Sema) *Symbol.Enum {
            return &sema.symbols.items[@intFromEnum(index)].@"enum";
        }
    };

    pub const Common = struct {
        /// Containing node for this symbol
        node: Ast.NodeIndex,
        /// Accessibilty for other modules
        is_pub: bool,

        /// Module which contains this symbol
        module_index: Module.Index = undefined,
        /// Index of this symbol into the modules's symbol-map
        /// Useful for retrieving the name of this symbol
        module_symbol_index: Symbol.Index = undefined,

        /// Current status for analyzation
        analyze_status: enum { pending, active, done } = .pending,
    };

    pub const Function = struct {
        pub const LocalVariable = struct {
            location: union(builtin_module.VariableLocation) {
                scratch: void,

                /// Offset from the stack pointer
                stack: u8,
            },

            type: TypeExpression.Index,
            node: Ast.NodeIndex,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Memory-mapped target bank
        bank: u8,
        /// Offset into the target bank
        bank_offset: u16 = undefined,

        // calling_convention: FunctionAnalyzer.CallingConvention,

        /// Local function variables. Includes function parameters
        // local_variables: []const LocalVariable,

        /// Named indices to target instructions
        labels: []const struct { []const u8, u16 },

        /// High-level Semantic IR graph
        semantic_ir: SemanticIr.Graph,
        /// Low-level Assembly IR instructions
        assembly_ir: []const AssemblyIr,

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
        type: TypeExpression.Index,
        /// Root SIR node for the value
        value: SemanticIr.Index,

        /// High-level Semantic IR graph
        sir_graph: SemanticIr.Graph,
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
        type: TypeExpression.Index,
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
        type: TypeExpression.Index,
    };

    pub const Struct = struct {
        pub const Field = struct {
            name: []const u8,
            type: TypeExpression.Index,
            default_value: Expression.Index,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Fields of this struct
        fields: []const Field,
    };
    pub const Packed = struct {
        pub const Field = struct {
            name: []const u8,
            type: TypeExpression.Index,
            default_value: Expression.Index,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeExpression.Index,

        /// Fields of this packed
        fields: []const Field,
    };
    pub const Enum = struct {
        pub const Field = struct {
            name: []const u8,
            value: Expression.Index,
        };

        /// Commonly shared data between symbols
        common: Common,

        /// Backing type of this enum
        backing_type: TypeExpression.Index,

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
            .function => |*fn_sym| {
                // allocator.free(fn_sym.local_variables);
                allocator.free(fn_sym.labels);

                fn_sym.semantic_ir.deinit(allocator);
                allocator.free(fn_sym.assembly_ir);
                allocator.free(fn_sym.instructions);
                allocator.free(fn_sym.assembly_data);
            },
            // TODO:
            // .@"struct" => |struct_sym| {
            //     allocator.free(struct_sym.fields);
            // },
            // .@"packed" => |packed_sym| {
            //     allocator.free(packed_sym.fields);
            // },
            // .@"enum" => |enum_sym| {
            //     allocator.free(enum_sym.fields);
            // },
            else => {},
        }
    }
};

//! High-level Semantic Intermediate Representation
const builtin_module = @import("../builtin_module.zig");

const Ast = @import("../Ast.zig");
const Sema = @import("../Sema.zig");
const NodeIndex = Ast.NodeIndex;
const Symbol = @import("../symbol.zig").Symbol;
const Expression = @import("expression.zig").Expression;
const TypeExpression = @import("type_expression.zig").TypeExpression;
const BuiltinVar = @import("BuiltinVar.zig");
const BuiltinFn = @import("BuiltinFn.zig");

pub const Index = enum(u32) { _ };

pub const Tag = union(enum) {
    /// Indicates n `field_reference` instructions following the current instruction
    pub const FieldTarget = u16;

    /// Starts a new nested scope, containing local variables inside it
    begin_scope: void,
    /// Ends the most recently stated scope
    end_scope: void,

    /// Declares a new local variable for the current scope
    /// `node` is a `local_var_decl`
    declare_variable: struct {
        location: builtin_module.VariableLocation,
        type: TypeExpression.Index,
    },

    /// Declares a new label for the function
    /// `node` is a `label`
    declare_label: void,

    /// References a field on the current target
    field_reference: struct {
        bit_offset: u16,
        type: TypeExpression.Index,
    },

    /// Assigns the expression to the target local variable
    assign_local: struct {
        local_index: Index,
        field_target: FieldTarget,
        value: Expression.Index,
    },
    /// Assigns the expression to the target global symbol
    assign_global: struct {
        symbol: Symbol.Index,
        field_target: FieldTarget,
        value: Expression.Index,
    },
    /// Assigns the expression to the target built-in
    assign_builtin: struct {
        builtin: BuiltinVar.Tag,
        value: Expression.Index,
    },

    /// Sets the current CPU mode
    cpu_mode: builtin_module.CpuMode,

    /// Calls the target function
    call_function: struct {
        symbol: Symbol.Index,
    },
    /// Calls the target built-in
    call_builtin: struct {
        builtin: BuiltinFn.Tag,
    },

    /// Returns from the current function
    @"return": void,
};

tag: Tag,
node: NodeIndex,

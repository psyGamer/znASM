//! Node in the Abstract-Syntax-Tree of a .znasm source file
const std = @import("std");

const Ast = @import("../Ast.zig");
const Token = Ast.Token;
const Node = @This();
const ExtraIndex = Ast.ExtraIndex;

pub const Index = enum(u32) {
    // Root and null share the same index, since no Node can have a root node as a child.
    pub const root: Index = @enumFromInt(0);

    none = 0,
    _,

    pub inline fn getTag(index: Index, ast: *const Ast) Tag {
        return ast.node_tags[@intFromEnum(index)];
    }
    pub inline fn getData(index: Index, ast: *const Ast) Data {
        return ast.node_data[@intFromEnum(index)];
    }
    pub inline fn getToken(index: Index, ast: *const Ast) Token.Index {
        return ast.node_tokens[@intFromEnum(index)];
    }

    /// Helper function to cast a generic number to a Node.Index
    pub inline fn cast(x: anytype) Index {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(Index), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(Index), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a Node.Index"),
        };
    }
};

/// Documents the valid data for each tag. If something is undocumented, the data is undefined to use
pub const Tag = enum {
    /// `data` is `sub_range`
    root,
    /// `main_token` is `identifier` of module name
    module,

    /// `main_token` is the `doc_comment`
    doc_comment,

    /// `data` is `fn_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    fn_def,

    /// `data` is `const_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    const_def,

    /// `data` is `var_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    var_def,

    /// `data` is `var_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    reg_def,

    /// `data` is `struct_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    struct_def,

    /// `data` is `packed_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    packed_def,

    /// `data` is `enum_def`
    /// `main_token - 1` is the optional `keyword_pub`
    /// `main_token + 1` is the `identifier` of the name
    enum_def,

    /// `data` is `sub_range`
    /// `main_token` is the `lbrace`
    struct_block,

    /// `data` is `struct_field`
    /// `main_token` is the `identifier` of the name
    struct_field,

    /// `data` is `sub_range`
    /// `main_token` is the `lbrace`
    enum_block,

    /// `data` is `enum_field`
    /// `main_token` is the `identifier` of the name
    enum_field,

    /// `main_token` is the `int_literal` of bank
    bank_attr,

    /// `main_token` is the `identifier` of access type
    access_attr,

    /// `data` is `attr_two`
    /// `main_token` is the `identifier` of the attribute
    attr_two,

    /// `data` is `sub_range` of expressions
    /// `main_token` is the `identifier` of the attribute
    attr_multi,

    /// `data` is `sub_range`
    /// `main_token` is the `lbrace`
    block,

    /// `data` is `assign_statement`
    /// `main_token` is the `equal`
    assign_statement,

    /// `main_token` is the `identifier` of the target
    call_statement,

    /// `data` is `while_statement`
    /// `main_token` is the `keyword_while`
    while_statement,

    /// `data` is `local_var_decl`
    /// `main_token` is the `identifier` of the name
    local_var_decl,

    /// `main_token` is the `identifier` of the name
    label,

    /// `data` is `sub_range`
    /// `main_token` is the first `identifier` of the field chain
    field_access,

    /// `data` is `expr`
    /// `main_token` is the `identifier` of the expression
    expr_ident,
    /// `data` is `expr`
    /// `main_token` is the `int_literal` value of the expression
    expr_int_value,
    /// `data` is `expr`
    /// `main_token` is the `identifier` value of the expression
    expr_enum_value,
    /// `data` is `expr_init`
    /// `main_token` is the `period` value of the init-expression
    expr_init,
    /// `data` is `expr_init_field`
    /// `main_token` is the `identifier` of the target field
    expr_init_field,

    /// `main_token` is the `identifier` of the type
    type_ident,
};

tag: Tag,
data: Data,
main_token: Token.Index,

/// Data associated with each node. Usage is documented for each tag
pub const Data = union {
    comptime {
        // Allow a bigger size in safe builds, because of the hidden active flag
        const max_size = switch (@import("builtin").mode) {
            .Debug, .ReleaseSafe => 12,
            .ReleaseFast, .ReleaseSmall => 8,
        };
        // Keep this data-type small
        std.debug.assert(@sizeOf(Data) <= max_size);
    }

    sub_range: SubRange,

    fn_def: struct {
        block: Node.Index,
        extra: ExtraIndex,
    },
    const_def: struct {
        value: Node.Index,
        extra: ExtraIndex,
    },
    var_def: struct {
        extra: ExtraIndex,
    },
    reg_def: struct {
        extra: ExtraIndex,
    },
    struct_def: struct {
        block: Node.Index,
        doc_comments: ExtraIndex,
    },
    packed_def: struct {
        block: Node.Index,
        extra: ExtraIndex,
    },
    enum_def: struct {
        block: Node.Index,
        extra: ExtraIndex,
    },
    enum_field: struct {
        value: Node.Index,
        doc_comments: ExtraIndex,
    },
    struct_field: struct {
        extra: ExtraIndex,
        doc_comments: ExtraIndex,
    },
    attr_two: struct {
        expr_one: Node.Index,
        expr_two: Node.Index,
    },
    assign_statement: struct {
        target: Node.Index,
        value: Node.Index,
    },
    while_statement: struct {
        condition: Node.Index,
        block: Node.Index,
    },
    local_var_decl: struct {
        value: Node.Index,
        extra: ExtraIndex,
    },
    expr: struct {
        intermediate_register: Token.Index,
    },
    expr_init: struct {
        extra: ExtraIndex,
        intermediate_register: Token.Index,
    },
    expr_init_field: struct {
        value: Node.Index,
    },
};

pub const SubRange = struct {
    pub const empty: SubRange = .{ .extra_start = @enumFromInt(0), .extra_end = @enumFromInt(0) };

    extra_start: ExtraIndex,
    extra_end: ExtraIndex,
};

pub const FnDefData = struct {
    bank_attr: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const ConstDefData = struct {
    type: Node.Index,
    bank_attr: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const VarDefData = struct {
    type: Node.Index,
    bank_attr: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const RegDefData = struct {
    type: Node.Index,
    address: Token.Index,
    access_attr: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const PackedDefData = struct {
    backing_type: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const EnumDefData = struct {
    backing_type: Node.Index,
    doc_comment_start: ExtraIndex,
    doc_comment_end: ExtraIndex,
};
pub const StructFieldData = struct {
    type: Node.Index,
    value: Node.Index,
};
pub const LocalVarDeclData = struct {
    type: Node.Index,
    location_attr: Node.Index,
};

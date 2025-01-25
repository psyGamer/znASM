const std = @import("std");
const rich = @import("util/rich.zig");
const SymbolLocation = @import("symbol.zig").SymbolLocation;

pub const Node = struct {
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

        /// `main_token` is the `dot_literal` of access type
        access_attr,

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

        /// `main_token` is the `identifier` of the name
        label,

        /// `data` is `sub_range`
        /// `main_token` is the first `identifier` of the field chain
        field_access,

        /// `main_token` is the `identifier` of the expression
        expr_ident,
        /// `main_token` is the `int_literal` value of the expression
        expr_int_value,
        /// `main_token` is the `dot_literal` value of the expression
        expr_enum_value,
        /// `data` is `sub_range`
        /// `main_token` is the `period` value of the init-expression
        expr_init,
        /// `data` is `expr_init_field`
        /// `main_token` is the `dot_literal` of the target field
        expr_init_field,

        /// `main_token` is the `identifier` of the type
        type_ident,
    };

    tag: Tag,
    main_token: TokenIndex,
    data: Data,

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
            block: NodeIndex,
            extra: ExtraIndex,
        },
        const_def: struct {
            value: NodeIndex,
            extra: ExtraIndex,
        },
        var_def: struct {
            extra: ExtraIndex,
        },
        reg_def: struct {
            extra: ExtraIndex,
        },
        struct_def: struct {
            block: NodeIndex,
            doc_comments: ExtraIndex,
        },
        packed_def: struct {
            block: NodeIndex,
            extra: ExtraIndex,
        },
        enum_def: struct {
            block: NodeIndex,
            extra: ExtraIndex,
        },
        enum_field: struct {
            value: NodeIndex,
            doc_comments: ExtraIndex,
        },
        struct_field: struct {
            extra: ExtraIndex,
            doc_comments: ExtraIndex,
        },
        assign_statement: struct {
            target: NodeIndex,
            extra: ExtraIndex,
        },
        while_statement: struct {
            condition: NodeIndex,
            block: NodeIndex,
        },
        expr_init_field: struct {
            value: NodeIndex,
        },
    };

    pub const SubRange = struct {
        pub const empty: SubRange = .{ .extra_start = @enumFromInt(0), .extra_end = @enumFromInt(0) };

        extra_start: ExtraIndex,
        extra_end: ExtraIndex,
    };

    pub const FnDefData = struct {
        bank_attr: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const ConstDefData = struct {
        type: NodeIndex,
        bank_attr: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const VarDefData = struct {
        type: NodeIndex,
        bank_attr: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const RegDefData = struct {
        type: NodeIndex,
        address: TokenIndex,
        access_attr: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const PackedDefData = struct {
        backing_type: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const EnumDefData = struct {
        backing_type: NodeIndex,
        doc_comment_start: ExtraIndex,
        doc_comment_end: ExtraIndex,
    };
    pub const StructFieldData = struct {
        type: NodeIndex,
        value: NodeIndex,
    };
    pub const AssignStatementData = struct {
        value: NodeIndex,
        intermediate_register: TokenIndex,
    };
};

pub const TokenIndex = enum(u32) {
    // Token 0 is valid, so use maxInt as null
    none = std.math.maxInt(u32),
    _,

    pub inline fn next(index: TokenIndex) TokenIndex {
        return @enumFromInt(@intFromEnum(index) + 1);
    }
    pub inline fn prev(index: TokenIndex) TokenIndex {
        return @enumFromInt(@intFromEnum(index) - 1);
    }
    pub inline fn offset(index: TokenIndex, off: i32) TokenIndex {
        return @enumFromInt(@intFromEnum(index) + off);
    }

    /// Cast a generic number to a TokenIndex
    pub inline fn cast(x: anytype) TokenIndex {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(TokenIndex), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(TokenIndex), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a TokenIndex"),
        };
    }
};
pub const NodeIndex = enum(u32) {
    // Root and null share the same index, since no Node can have a root node as a child.
    pub const root: NodeIndex = @enumFromInt(0);

    none = 0,
    _,

    /// Helper function to cast a generic number to a NodeIndex
    pub inline fn cast(x: anytype) NodeIndex {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(NodeIndex), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(NodeIndex), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a NodeIndex"),
        };
    }
};
pub const ExtraIndex = enum(u32) {
    _,

    /// Helper function to cast a generic number to a ExtraIndex
    pub inline fn cast(x: anytype) ExtraIndex {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(ExtraIndex), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(ExtraIndex), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a ExtraIndex"),
        };
    }
};

/// Non-typesafe type which can represent either a token, node, or extra index
pub const CommonIndex = @TypeOf(
    @as(std.meta.Tag(TokenIndex), undefined),
    @as(std.meta.Tag(NodeIndex), undefined),
    @as(std.meta.Tag(ExtraIndex), undefined),
);

const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Token = Tokenizer.Token;

const Ast = @This();

/// Reference to externally-owned data.
source: [:0]const u8,
source_path: []const u8,

token_slice: std.MultiArrayList(Token).Slice,
token_tags: []const Token.Tag,
token_locs: []const Token.Loc,

node_slice: std.MultiArrayList(Node).Slice,
node_tags: []const Node.Tag,
node_tokens: []const TokenIndex,
node_data: []const Node.Data,

extra_data: []const CommonIndex,

errors: []const Error,

pub inline fn tokenTag(tree: Ast, index: TokenIndex) Token.Tag {
    return tree.token_tags[@intFromEnum(index)];
}
pub inline fn tokenLoc(tree: Ast, index: TokenIndex) Token.Loc {
    return tree.token_locs[@intFromEnum(index)];
}
pub inline fn nodeTag(tree: Ast, index: NodeIndex) Node.Tag {
    return tree.node_tags[@intFromEnum(index)];
}
pub inline fn nodeToken(tree: Ast, index: NodeIndex) TokenIndex {
    return tree.node_tokens[@intFromEnum(index)];
}
pub inline fn nodeData(tree: Ast, index: NodeIndex) Node.Data {
    return tree.node_data[@intFromEnum(index)];
}
pub inline fn extraData(tree: Ast, index: ExtraIndex) CommonIndex {
    return tree.extra_data[@intFromEnum(index)];
}

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8, source_path: []const u8) !Ast {
    var tokens: std.MultiArrayList(Token) = .empty;

    var lexer: Tokenizer = .{ .buffer = source };
    while (true) {
        const token = lexer.next();
        try tokens.append(allocator, token);
        if (token.tag == .eof) break;
    }

    var token_slice = tokens.toOwnedSlice();
    errdefer token_slice.deinit(allocator);

    const token_tags = token_slice.items(.tag);
    const token_locs = token_slice.items(.loc);

    for (token_slice.items(.tag)) |tag| {
        std.log.warn(" - {}", .{tag});
    }

    var parser: Parser = .{
        .source = source,
        .token_tags = token_tags,
        .token_locs = token_locs,
        .allocator = allocator,
    };
    defer parser.scratch.deinit(allocator);

    errdefer parser.nodes.deinit(allocator);
    errdefer parser.errors.deinit(allocator);

    parser.parseRoot() catch |err| switch (err) {
        error.ParseFailed => {
            std.debug.assert(parser.errors.items.len > 0);
        },
        else => |e| return e,
    };
    std.log.debug("err {any}", .{parser.errors.items});

    var node_slice = parser.nodes.toOwnedSlice();
    errdefer node_slice.deinit(allocator);

    return .{
        .source = source,
        .source_path = source_path,
        .token_slice = token_slice,
        .token_tags = token_tags,
        .token_locs = token_locs,
        .node_slice = node_slice,
        .node_tags = node_slice.items(.tag),
        .node_tokens = node_slice.items(.main_token),
        .node_data = node_slice.items(.data),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub fn deinit(tree: *Ast, allocator: std.mem.Allocator) void {
    tree.token_slice.deinit(allocator);
    tree.node_slice.deinit(allocator);
    allocator.free(tree.extra_data);
    allocator.free(tree.errors);
}

/// Parses the extra data for the specified type
pub fn readExtraData(tree: Ast, comptime T: type, extra_idx: ExtraIndex) T {
    var result: T = undefined;
    inline for (std.meta.fields(T), 0..) |field, i| {
        @field(result, field.name) = .cast(tree.extra_data[@intFromEnum(extra_idx) + i]);
    }
    return result;
}

/// Resolves the source data of a token
pub fn tokenSource(tree: Ast, token_idx: TokenIndex) []const u8 {
    return tree.token_locs[@intFromEnum(token_idx)].source(tree.source);
}
/// Parses the string value of a token
pub fn parseIdentifier(tree: Ast, token_idx: TokenIndex) []const u8 {
    std.debug.assert(tree.tokenTag(token_idx) == .ident or tree.tokenTag(token_idx) == .builtin_ident);
    return tree.tokenSource(token_idx);
}
/// Parses the int value of an `int_literal`
pub fn parseIntLiteral(tree: Ast, comptime T: type, token_idx: TokenIndex) !T {
    std.debug.assert(tree.tokenTag(token_idx) == .int_literal);
    const int_str = tree.tokenSource(token_idx);
    const number_base: u8 = switch (int_str[0]) {
        '$' => 16,
        '%' => 2,
        '0'...'9' => 10,
        else => unreachable,
    };

    return try std.fmt.parseInt(T, int_str[(if (number_base == 10) 0 else 1)..], number_base);
}
/// Parses the big-int value of an `int_literal`
pub fn parseBigIntLiteral(tree: Ast, comptime T: type, token_idx: TokenIndex, allocator: std.mem.Allocator) !T {
    std.debug.assert(tree.tokenTag(token_idx) == .int_literal);
    const int_str = tree.tokenSource(token_idx);
    const number_base: u8 = switch (int_str[0]) {
        '$' => 16,
        '%' => 2,
        '0'...'9' => 10,
        else => unreachable,
    };

    var int: std.math.big.int.Managed = try .init(allocator);
    errdefer int.deinit();

    try int.setString(number_base, int_str[(if (number_base == 10) 0 else 1)..]);

    switch (T) {
        std.math.big.int.Const => {
            // Shrink allocation to avoid leaking excess capacity
            if (allocator.resize(int.limbs, int.len())) {
                return int.toConst();
            } else {
                const limbs = try allocator.alloc(std.math.big.Limb, int.len());
                @memcpy(limbs, int.limbs[0..int.len()]);
                defer int.deinit();

                return .{
                    .limbs = limbs,
                    .positive = int.isPositive(),
                };
            }
        },
        std.math.big.int.Mutable => return int.toMutable(),
        std.math.big.int.Managed => return int,
        else => @compileError(@typeName(T) ++ " is not supported for big integers"),
    }
}
/// Parses the enum value of an enum literal
pub fn parseEnumLiteral(tree: Ast, comptime T: type, token_idx: TokenIndex) !T {
    std.debug.assert(tree.tokenTag(token_idx) == .ident and tree.tokenTag(token_idx.prev()) == .period);
    const enum_str = tree.tokenSource(token_idx);
    return std.meta.stringToEnum(T, enum_str) orelse error.UnknownField;
}
/// Parse a `name` or `module::name` symbol location
pub fn parseSymbolLocation(tree: *const Ast, token_idx: Ast.TokenIndex) SymbolLocation {
    if (tree.tokenTag(token_idx.next()) == .double_colon) {
        return .{
            .module = tree.parseIdentifier(token_idx),
            .name = tree.parseIdentifier(token_idx.offset(2)),
        };
    } else {
        return .{
            .module = "",
            .name = tree.parseIdentifier(token_idx),
        };
    }
}

/// Checks if the index is of the specified token tag
pub fn isToken(tree: Ast, token_idx: TokenIndex, tag: Token.Tag) bool {
    return if (token_idx >= tree.token_tags.len)
        false
    else
        tree.tokenTag(token_idx) == tag;
}
/// Checks if the index are of the specified token tags
pub fn areTokens(tree: Ast, token_idx: TokenIndex, tags: []const Token.Tag) bool {
    return if (token_idx + tags.len - 1 >= tree.token_tags.len)
        false
    else
        std.mem.eql(Token.Tag, tree.token_tags[@intFromEnum(token_idx)..(@intFromEnum(token_idx) + tags.len)], tags);
}

// Error handling

pub const Error = struct {
    pub const Tag = enum {
        // Extra: none
        expected_toplevel,
        expected_statement,
        expected_expr,
        expected_comma_after_arg,
        expected_enum_member,
        // Extra: expected_tag
        expected_token,
    };

    tag: Tag,
    token: TokenIndex,

    /// Notes are associated with the previous error
    is_note: bool = false,

    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },
};

pub fn detectErrors(tree: Ast, writer: anytype, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (tree.errors) |err| {
        const token_loc = tree.tokenLoc(err.token);
        const src_loc = std.zig.findLineColumn(tree.source, token_loc.start);

        const args = .{ tree.source_path, src_loc.line + 1, src_loc.column + 1 };
        if (err.is_note) {
            try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", args);
        } else {
            try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", args);
        }

        try tree.renderError(writer, tty_config, err);
        try writer.writeByte('\n');

        try writer.writeAll(src_loc.source_line);
        try writer.writeByte('\n');

        try tty_config.setColor(writer, .green);
        try writer.writeByteNTimes(' ', src_loc.column);
        try writer.writeByte('^');
        try writer.writeByteNTimes('~', token_loc.end -| token_loc.start -| 1);
        try tty_config.setColor(writer, .reset);

        try writer.writeByte('\n');
        try writer.writeByte('\n');
    }

    return tree.errors.len != 0;
}

pub fn renderError(tree: Ast, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    const highlight = "bold bright_magenta";

    switch (err.tag) {
        .expected_toplevel => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]a top-level definition[reset], found [" ++ highlight ++ "]{s}", .{
            tree.tokenTag(err.token).symbol(),
        }),
        .expected_statement => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]a statement[reset], found [" ++ highlight ++ "]{s}", .{
            tree.tokenTag(err.token).symbol(),
        }),
        .expected_expr => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]an expression[reset], found [" ++ highlight ++ "]{s}", .{
            tree.tokenTag(err.token).symbol(),
        }),
        .expected_comma_after_arg => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]a comma[reset], after an argument, found [" ++ highlight ++ "]{s}", .{
            tree.tokenTag(err.token).symbol(),
        }),
        .expected_enum_member => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]an enum member[reset], inside [" ++ highlight ++ "]enum block[reset], found [" ++ highlight ++ "]{s}", .{
            tree.tokenTag(err.token).symbol(),
        }),

        .expected_token => try rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]{s}[reset], found [" ++ highlight ++ "]{s}", .{
            err.extra.expected_tag.symbol(),
            tree.tokenTag(err.token).symbol(),
        }),
    }
}

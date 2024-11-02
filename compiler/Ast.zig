const std = @import("std");
const rich = @import("util/rich.zig");
const InstructionType = @import("instruction.zig").InstructionType;

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

        /// `main_token` is the `int_literal` of bank
        bank_attr,

        /// `data` is `sub_range`
        /// `main_token` is the `lbrace`
        block,

        /// `main_token` is the `identifier` of the opcode, followed by operands until a `new_line`
        instruction,

        /// `main_token` is the `identifier` of the name
        label,
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
    };

    pub const SubRange = struct {
        extra_start: ExtraIndex,
        extra_end: ExtraIndex,
    };

    pub const FnDefData = struct {
        bank_attr: NodeIndex,
        doc_comment_start: NodeIndex,
        doc_comment_end: NodeIndex,
    };
};

pub const Error = struct {
    pub const Tag = enum {
        // Extra: none
        expected_toplevel,
        expected_expr_instr_label,
        // Extra: expected_tag
        expected_token,
    };

    tag: Tag,
    type: enum { err, warn, note },

    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: TokenIndex,

    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },
};

pub const TokenIndex = u32;
pub const NodeIndex = u32;
pub const ExtraIndex = u32;

// Token 0 is valid, so use ~0 as null
pub const null_token: TokenIndex = std.math.maxInt(TokenIndex);

// Root and null share the same index, since no Node can have a root node as a child.
pub const root_node: NodeIndex = 0;
pub const null_node: NodeIndex = 0;

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Token = Lexer.Token;

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

extra_data: []const NodeIndex,

errors: []const Error,

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8, source_path: []const u8) !Ast {
    var tokens: std.MultiArrayList(Token) = .empty;

    var lexer: Lexer = .{ .buffer = source };
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
    allocator.free(tree.errors);
}

/// Parses the extra data for the specified type
pub fn extraData(tree: Ast, comptime T: type, extra_idx: usize) T {
    var result: T = undefined;
    inline for (std.meta.fields(T), 0..) |field, i| {
        comptime std.debug.assert(field.type == NodeIndex);
        @field(result, field.name) = tree.extra_data[extra_idx + i];
    }
    return result;
}

/// Resolves the source data of a token
pub fn tokenSource(tree: Ast, token_idx: TokenIndex) []const u8 {
    return tree.token_locs[token_idx].source(tree.source);
}
/// Parses the string value of a token
pub fn parseIdentifier(tree: Ast, token_idx: TokenIndex) []const u8 {
    std.debug.assert(tree.token_tags[token_idx] == .ident);
    return tree.tokenSource(token_idx);
}
/// Parses the int value of an `int_literal`
pub fn parseIntLiteral(tree: Ast, comptime T: type, token_idx: TokenIndex) !T {
    std.debug.assert(tree.token_tags[token_idx] == .int_literal);
    const int_str = tree.tokenSource(token_idx);
    const number_base: u8 = switch (int_str[0]) {
        '$' => 16,
        '%' => 2,
        '0'...'9' => 10,
        else => unreachable,
    };
    return try std.fmt.parseInt(T, int_str[(if (number_base == 10) 0 else 1)..], number_base);
}

/// Checks if the index is of the specified token tag
pub fn isToken(tree: Ast, token_idx: TokenIndex, tag: Token.Tag) bool {
    return if (token_idx >= tree.token_tags.len)
        false
    else
        tree.token_tags[token_idx] == tag;
}
/// Checks if the index are of the specified token tags
pub fn areTokens(tree: Ast, token_idx: TokenIndex, tags: []const Token.Tag) bool {
    return if (token_idx + tags.len - 1 >= tree.token_tags.len)
        false
    else
        std.mem.eql(Token.Tag, tree.token_tags[token_idx..(token_idx + tags.len)], tags);
}

pub fn detectErrors(tree: Ast, writer: anytype, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (tree.errors) |err| {
        const token_loc = tree.token_locs[err.token];
        const src_loc = std.zig.findLineColumn(tree.source, token_loc.start);

        const args = .{ tree.source_path, src_loc.line + 1, src_loc.column + 1 };
        switch (err.type) {
            .err => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", args),
            .warn => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [yellow]warning: ", args),
            .note => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", args),
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
        .expected_toplevel => try rich.print(
            writer,
            tty_config,
            "Expected [" ++ highlight ++ "]a top-level definition[reset], found [" ++ highlight ++ "]{s}",
            .{
                tree.token_tags[err.token - @intFromBool(err.token_is_prev)].symbol(),
            },
        ),
        .expected_expr_instr_label => try rich.print(
            writer,
            tty_config,
            "Expected [" ++ highlight ++ "]an expression[reset], [" ++ highlight ++ "]an instruction[reset], or [" ++ highlight ++ "]a label[reset], found [" ++ highlight ++ "]{s}",
            .{
                tree.token_tags[err.token - @intFromBool(err.token_is_prev)].symbol(),
            },
        ),

        .expected_token => try rich.print(
            writer,
            tty_config,
            "Expected [" ++ highlight ++ "]{s}[reset], found [" ++ highlight ++ "]{s}",
            .{
                err.extra.expected_tag.symbol(),
                tree.token_tags[err.token - @intFromBool(err.token_is_prev)].symbol(),
            },
        ),
    }
}

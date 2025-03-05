const std = @import("std");
const rich = @import("util/rich.zig");

const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Tokenizer = @import("ast/Tokenizer.zig");
const Parser = @import("ast/Parser.zig");

const Ast = @This();

pub const Token = Tokenizer.Token;
pub const Node = @import("ast/Node.zig");

pub const TokenIndex = Token.Index;
pub const NodeIndex = Node.Index;
pub const ExtraIndex = enum(u32) {
    _,

    pub inline fn get(index: ExtraIndex, ast: *const Ast) CommonIndex {
        return ast.extra_data[@intFromEnum(index)];
    }

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

pub const ParseError = error{ParseFailed} || std.mem.Allocator.Error || std.fs.File.Writer.Error;

/// Source code for this file
source: [:0]const u8,
/// File path relative to the project root
source_path: []const u8,

token_slice: std.MultiArrayList(Token).Slice = .empty,
token_tags: []const Token.Tag = &.{},
token_locs: []const Token.Loc = &.{},

node_slice: std.MultiArrayList(Node).Slice = .empty,
node_tags: []const Node.Tag = &.{},
node_tokens: []const TokenIndex = &.{},
node_data: []const Node.Data = &.{},

extra_data: []const CommonIndex = &.{},

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

pub fn parse(ast: *Ast, data_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator) ParseError!void {
    // Generate tokens
    var tokens: std.MultiArrayList(Token) = .empty;
    var tokenizer: Tokenizer = .{ .buffer = ast.source };
    while (true) {
        const token = tokenizer.next();
        try tokens.append(data_allocator, token);
        if (token.tag == .eof) break;
    }

    ast.token_slice = tokens.toOwnedSlice();
    ast.token_tags = ast.token_slice.items(.tag);
    ast.token_locs = ast.token_slice.items(.loc);

    // Generate nodes
    var parser: Parser = .{
        .ast = ast,
        .data_allocator = data_allocator,
        .temp_allocator = temp_allocator,
    };
    try parser.parseRoot();

    ast.node_slice = parser.nodes.toOwnedSlice();
    ast.node_tags = ast.node_slice.items(.tag);
    ast.node_tokens = ast.node_slice.items(.main_token);
    ast.node_data = ast.node_slice.items(.data);
    ast.extra_data = try parser.extra_data.toOwnedSlice(data_allocator);
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
pub fn parseBigIntLiteral(tree: Ast, comptime T: type, token_idx: TokenIndex, arena: std.mem.Allocator) !T {
    std.debug.assert(tree.tokenTag(token_idx) == .int_literal);
    const int_str = tree.tokenSource(token_idx);
    const number_base: u8 = switch (int_str[0]) {
        '$' => 16,
        '%' => 2,
        '0'...'9' => 10,
        else => unreachable,
    };

    var int: std.math.big.int.Managed = try .init(arena);
    try int.setString(number_base, int_str[(if (number_base == 10) 0 else 1)..]);

    return switch (T) {
        std.math.big.int.Const => int.toConst(),
        std.math.big.int.Mutable => int.toMutable(),
        std.math.big.int.Managed => int,
        else => @compileError(@typeName(T) ++ " is not supported for big integers"),
    };
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

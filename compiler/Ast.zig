const std = @import("std");
const log = @import("logging.zig");
const InstructionType = @import("instruction.zig").InstructionType;

pub const Node = struct {
    pub const Tag = union(enum) {
        /// main_token is invalid
        root: void,
        module: []const u8,
        /// main_token is the `keyword_var` or `keyword_const`
        global_var_decl: struct {
            name: []const u8,
            type: []const u8,
            is_const: bool,
            is_pub: bool,
        },
        /// main_token is the `keyword_fn`
        fn_def: struct {
            name: []const u8,
            return_type: ?[]const u8,
            is_pub: bool,
        },

        /// main_token is the `identifier` of "bank"
        bank_attr: u8,

        /// main_token is the `lbrace`
        block_scope: void,

        /// main_token is the `identifier` of the opcode
        instruction: struct {
            opcode: InstructionType,
            operand: union(enum) {
                none: void,
                number: u16,
                identifier: []const u8,
            },
        },

        /// main_token is the `identifier` of the name
        label: []const u8,
    };

    tag: Tag,
    main_token: TokenIndex,
    /// The parent node of this node. Only the root-node doesn't have a parent
    parent: TokenIndex = undefined,
};

pub const Error = struct {
    pub const Tag = enum {
        // Extra: none
        unexpected_eof,
        unexpected_token,
        expected_var_decl_or_fn,
        expected_expression,
        invalid_opcode,
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

/// Root and null share the same index, since no Node can have a root node as a child.
pub const null_node: NodeIndex = 0;

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Token = Lexer.Token;

const Self = @This();

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: []const Token,
nodes: []const Node,
errors: []const Error,

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Self {
    var tokens: std.ArrayListUnmanaged(Token) = .{};

    var lexer: Lexer = .{ .buffer = source };
    while (true) {
        const token = lexer.next();
        try tokens.append(allocator, token);
        if (token.tag == .eof) break;
    }

    std.log.warn("Tokens:", .{});
    for (tokens.items) |tok| {
        std.log.warn(" - {}", .{tok});
    }

    var parser: Parser = .{
        .source = source,
        .tokens = tokens.items,
        .allocator = allocator,
    };
    defer parser.nodes.deinit(allocator);
    defer parser.errors.deinit(allocator);

    // Root is always index 0
    try parser.nodes.append(allocator, .{ .tag = .root, .main_token = undefined });
    parser.parseRoot() catch |err| switch (err) {
        error.ParseFailed => {
            std.debug.assert(parser.errors.items.len > 0);
        },
        else => |e| return e,
    };
    std.log.debug("err {any}", .{parser.errors.items});

    return .{
        .source = source,
        .tokens = try tokens.toOwnedSlice(allocator),
        .nodes = try parser.nodes.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub fn deinit(tree: Self, allocator: std.mem.Allocator) void {
    allocator.free(tree.tokens);
    allocator.free(tree.nodes);
    allocator.free(tree.errors);
}

const ChildIterator = struct {
    index: NodeIndex,
    parent: NodeIndex,
    nodes: []const Node,

    pub fn next(iter: *ChildIterator) ?Node {
        while (iter.index < iter.nodes.len) {
            defer iter.index += 1;
            if (iter.nodes[iter.index].parent == iter.parent) {
                return iter.nodes[iter.index];
            }
        }

        return null;
    }
    pub fn nextIndex(iter: *ChildIterator) ?NodeIndex {
        while (iter.index < iter.nodes.len) {
            defer iter.index += 1;
            if (iter.nodes[iter.index].parent == iter.parent) {
                return iter.index;
            }
        }

        return null;
    }
};
/// Iterates the children of the specified node
pub fn iterChildren(tree: Self, parent: NodeIndex) ChildIterator {
    return .{
        .index = parent + 1,
        .parent = parent,
        .nodes = tree.nodes,
    };
}

pub fn detectErrors(tree: Self, writer: std.fs.File.Writer, tty_config: std.io.tty.Config, src_file_path: []const u8, source_data: []const u8) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (tree.errors) |err| {
        const token = tree.tokens[err.token];
        const src_loc = std.zig.findLineColumn(source_data, token.loc.start);

        try tty_config.setColor(writer, .bold);
        try writer.print("{s}:{}:{}: ", .{ src_file_path, src_loc.line + 1, src_loc.column + 1 });

        switch (err.type) {
            .err => {
                try tty_config.setColor(writer, .red);
                try writer.writeAll("error: ");
            },
            .warn => {
                try tty_config.setColor(writer, .yellow);
                try writer.writeAll("warning: ");
            },
            .note => {
                try tty_config.setColor(writer, .cyan);
                try writer.writeAll("note: ");
            },
        }
        try tty_config.setColor(writer, .reset);
        try tree.renderError(writer, tty_config, err);
        try writer.writeByte('\n');

        try writer.writeAll(src_loc.source_line);
        try writer.writeByte('\n');

        try tty_config.setColor(writer, .green);
        try writer.writeByteNTimes(' ', src_loc.column);
        try writer.writeByte('^');
        try writer.writeByteNTimes('~', token.loc.end -| token.loc.start -| 1);
        try tty_config.setColor(writer, .reset);

        try writer.writeByte('\n');
        try writer.writeByte('\n');
    }

    return tree.errors.len != 0;
}

pub fn renderError(tree: Self, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    switch (err.tag) {
        .unexpected_eof => return writer.writeAll("Unexpected end-of-file"),
        .unexpected_token => {
            try writer.writeAll("Unexpected token: ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(tree.tokens[err.token - @intFromBool(err.token_is_prev)].tag.symbol());
            try tty_config.setColor(writer, .reset);
        },
        .expected_var_decl_or_fn => return writer.print("Expected variable declaration or function definition, found {s}", .{
            tree.tokens[err.token - @intFromBool(err.token_is_prev)].tag.symbol(),
        }),
        .expected_expression => {
            try writer.writeAll("Expected ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("an expression");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(", found ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(tree.tokens[err.token - @intFromBool(err.token_is_prev)].tag.symbol());
            try tty_config.setColor(writer, .reset);
        },
        .invalid_opcode => {
            try writer.writeAll("Expected ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("opcode");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(", found ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(tree.tokens[err.token - @intFromBool(err.token_is_prev)].tag.symbol());
            try tty_config.setColor(writer, .reset);
        },
        .expected_expr_instr_label => {
            try writer.writeAll("Expected ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("expression");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(", ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("instruction");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(", or ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("label");
            try tty_config.setColor(writer, .reset);
        },

        .expected_token => {
            try writer.writeAll("Expected ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(err.extra.expected_tag.symbol());
            try tty_config.setColor(writer, .reset);

            try writer.writeAll(", found ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(tree.tokens[err.token - @intFromBool(err.token_is_prev)].tag.symbol());
            try tty_config.setColor(writer, .reset);
        },
    }
}

const TestNode = struct {
    tag: Node.Tag,
    children: []const @This() = &.{},
};
fn testAst(source: [:0]const u8, expected_tree: TestNode) !void {
    const ast = try parse(std.testing.allocator, source);
    defer ast.deinit(std.testing.allocator);

    // Should not contain any errors
    if (ast.errors.len != 0) {
        for (ast.errors) |err| {
            const writer = log.startLog(.err, .default);
            try ast.renderError(err, writer);
            log.endLog();
        }
        return error.TestExpectedEqual;
    }

    return expectNodeEquals(ast, expected_tree, ast.nodes[0]);
}
fn expectNodeEquals(ast: Self, expected: TestNode, actual: Node) !void {
    if (expected.children.len != actual.children.items.len) {
        return error.TestExpectedEqual;
    }

    try std.testing.expectEqualDeep(expected.tag, actual.tag);

    for (expected.children, actual.children.items) |expected_child, actual_child| {
        try expectNodeEquals(ast, expected_child, ast.nodes[actual_child]);
    }
}

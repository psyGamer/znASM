const std = @import("std");
const log = @import("logging.zig");
const InstructionType = @import("instruction.zig").InstructionType;

pub const Node = struct {
    pub const Tag = union(enum) {
        /// main_token is invalid
        root: void,
        // namespace: []const u8,
        // segment: []const u8,
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
        /// main_token is the `lbrace`
        block_expr: void,

        /// main_token is the `identifier` of the opcode
        instruction: struct {
            opcode: InstructionType,
        },
    };

    tag: Tag,
    main_token: TokenIndex,
    children: std.ArrayListUnmanaged(NodeIndex) = .{},
};

pub const Error = struct {
    pub const Tag = enum {
        // Extra: none
        unexpected_eof,
        unexpected_token,
        expected_var_decl_or_fn,
        expected_expression,
        invalid_opcode,
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
    for (tree.nodes) |node| {
        var children = node.children;
        children.deinit(allocator);
    }

    allocator.free(tree.tokens);
    allocator.free(tree.nodes);
    allocator.free(tree.errors);
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
        try writer.writeByteNTimes('~', token.loc.end - token.loc.start - 1);
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

test "segments in namespace" {
    try testAst(
        \\namespace Level;
        \\segment ROM0;
        \\segment ROM1;
    , .{
        .tag = .root,
        .children = &.{
            .{ .tag = .{ .namespace = "Level" } },
            .{ .tag = .{ .segment = "ROM0" } },
            .{ .tag = .{ .segment = "ROM1" } },
        },
    });

    try testAst(
        \\namespace Level;
        \\segment ROM0;
        \\segment ROM1;
        \\namespace Level2;
        \\segment ROM0;
        \\segment ROM1;
    , .{
        .tag = .root,
        .children = &.{
            .{ .tag = .{ .namespace = "Level" } },
            .{ .tag = .{ .segment = "ROM0" } },
            .{ .tag = .{ .segment = "ROM1" } },
            .{ .tag = .{ .namespace = "Level2" } },
            .{ .tag = .{ .segment = "ROM0" } },
            .{ .tag = .{ .segment = "ROM1" } },
        },
    });
}

test "global variables" {
    try testAst(
        \\namespace Level;
        \\
        \\segment ROM0;
        \\
        \\var my_counter1: u8;
        \\const my_counter2: u16;
        \\pub var my_counter3: u32;
        \\pub const my_counter4: u64;
    , .{
        .tag = .root,
        .children = &.{
            .{ .tag = .{ .namespace = "Level" } },
            .{ .tag = .{ .segment = "ROM0" } },
            .{
                .tag = .{
                    .global_var_decl = .{
                        .name = "my_counter1",
                        .type = "u8",
                        .is_const = false,
                        .is_pub = false,
                    },
                },
            },
            .{
                .tag = .{
                    .global_var_decl = .{
                        .name = "my_counter2",
                        .type = "u16",
                        .is_const = true,
                        .is_pub = false,
                    },
                },
            },
            .{
                .tag = .{
                    .global_var_decl = .{
                        .name = "my_counter3",
                        .type = "u32",
                        .is_const = false,
                        .is_pub = true,
                    },
                },
            },
            .{
                .tag = .{
                    .global_var_decl = .{
                        .name = "my_counter4",
                        .type = "u64",
                        .is_const = true,
                        .is_pub = true,
                    },
                },
            },
        },
    });
}

test "function definitions" {
    try testAst(
        \\namespace Level;
        \\
        \\segment ROM0;
        \\
        \\fn test1() void { }
        \\pub inline fn test2() u32 { }
    , .{
        .tag = .root,
        .children = &.{
            .{ .tag = .{ .namespace = "Level" } },
            .{ .tag = .{ .segment = "ROM0" } },
            .{
                .tag = .{
                    .fn_def = .{
                        .name = "test1",
                        .return_type = "void",
                        .is_inline = false,
                        .is_pub = false,
                    },
                },
                .children = &.{
                    .{ .tag = .block_expr },
                },
            },
            .{
                .tag = .{
                    .fn_def = .{
                        .name = "test2",
                        .return_type = "u32",
                        .is_inline = true,
                        .is_pub = true,
                    },
                },
                .children = &.{
                    .{ .tag = .block_expr },
                },
            },
        },
    });
}

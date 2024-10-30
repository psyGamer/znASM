const std = @import("std");

const Lexer = @This();

pub const Token = struct {
    pub const Tag = enum {
        invalid,
        eof,

        colon,
        semicolon,
        lparen,
        rparen,
        lbracket,
        rbracket,
        lbrace,
        rbrace,

        ident,
        builtin_ident,
        int_literal,

        // keyword_namespace,
        // keyword_segment,
        keyword_pub,
        // keyword_var,
        // keyword_const,
        // keyword_inline,
        keyword_fn,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .ident,
                .builtin_ident
                    .int_literal,
                => null,

                .colon => ":",
                .semicolon => ";",
                .lparen => "(",
                .rparen => ")",
                .lbracket => "[",
                .rbracket => "]",
                .lbrace => "{",
                .rbrace => "}",

                .keyword_namespace => "namespace",
                .keyword_segment => "segment",
                .keyword_pub => "pub",
                .keyword_var => "var",
                .keyword_const => "const",
                .keyword_inline => "inline",
                .keyword_fn => "fn",
            };
        }
        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid bytes",
                .int_literal => "a number literal",
                .ident => "an identifier",
                .builtin_ident => "a builtin identifier",
                else => unreachable,
            };
        }
    };

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const keywords: std.StaticStringMap(Tag) = .initComptime(.{
        // .{ "namespace", .keyword_namespace },
        // .{ "segment", .keyword_segment },
        .{ "pub", .keyword_pub },
        // .{ "var", .keyword_var },
        // .{ "const", .keyword_const },
        // .{ "inline", .keyword_inline },
        .{ "fn", .keyword_fn },
    });

    tag: Tag,
    loc: Loc,
};

buffer: [:0]const u8,
index: usize = 0,

pub fn next(lexer: *Lexer) Token {
    const State = enum {
        start,
        ident,
        dec_int,
        hex_int,
        bin_int,
    };

    var state: State = .start;
    var token: Token = .{
        .tag = .eof,
        .loc = .{
            .start = lexer.index,
            .end = undefined,
        },
    };

    while (true) : (lexer.index += 1) {
        const c = lexer.buffer[lexer.index];

        switch (state) {
            .start => switch (c) {
                0 => {
                    if (lexer.index != lexer.buffer.len) {
                        token.tag = .invalid;
                        token.loc.start = lexer.index;
                        lexer.index += 1;
                        token.loc.end = lexer.index;
                        return token;
                    }
                    break;
                },

                ' ', '\n', '\t', '\r' => {
                    token.loc.start = lexer.index + 1;
                },

                'a'...'z', 'A'...'Z', '_' => {
                    token.tag = .ident;
                    state = .ident;
                },
                '@' => {
                    token.tag = .builtin_ident;
                    state = .ident;
                },

                '0'...'9' => {
                    token.tag = .int_literal;
                    state = .dec_int;
                },
                '$' => {
                    token.tag = .int_literal;
                    state = .hex_int;
                },
                '%' => {
                    token.tag = .int_literal;
                    state = .bin_int;
                },
                ':' => {
                    token.tag = .colon;
                    lexer.index += 1;
                    break;
                },
                ';' => {
                    token.tag = .semicolon;
                    lexer.index += 1;
                    break;
                },
                '(' => {
                    token.tag = .lparen;
                    lexer.index += 1;
                    break;
                },
                ')' => {
                    token.tag = .rparen;
                    lexer.index += 1;
                    break;
                },
                '[' => {
                    token.tag = .lbracket;
                    lexer.index += 1;
                    break;
                },
                ']' => {
                    token.tag = .rbracket;
                    lexer.index += 1;
                    break;
                },
                '{' => {
                    token.tag = .lbrace;
                    lexer.index += 1;
                    break;
                },
                '}' => {
                    token.tag = .rbrace;
                    lexer.index += 1;
                    break;
                },
                else => {
                    token.tag = .invalid;
                    token.loc.end = lexer.index;
                    lexer.index += 1;
                    return token;
                },
            },

            .ident => switch (c) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                else => {
                    if (Token.getKeyword(lexer.buffer[token.loc.start..lexer.index])) |tag| {
                        token.tag = tag;
                    }
                    break;
                },
            },

            .dec_int => switch (c) {
                '0'...'9' => {},
                else => break,
            },

            .hex_int => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {},
                else => break,
            },

            .bin_int => switch (c) {
                '0'...'1' => {},
                else => break,
            },
        }
    }

    if (token.tag == .eof) {
        token.loc.start = lexer.index;
    }

    token.loc.end = lexer.index;
    return token;
}

fn testLexer(src: [:0]const u8, expected_tokens: []const Token.Tag) !void {
    var lexer: Lexer = .{ .buffer = src };
    for (expected_tokens) |expected| {
        const token = lexer.next();
        try std.testing.expectEqualDeep(expected, token.tag);
    }
    const last_token = lexer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(src.len, last_token.loc.start);
    try std.testing.expectEqual(src.len, last_token.loc.end);
}

test "empty" {
    try testLexer("", &.{.eof});
}

test "decimal literal" {
    try testLexer("1", &.{.int_literal});
    try testLexer("12", &.{.int_literal});
    try testLexer("123", &.{.int_literal});
}

test "hex literal" {
    try testLexer("$1", &.{.int_literal});
    try testLexer("$12", &.{.int_literal});
    try testLexer("$123", &.{.int_literal});
}

test "bin literal" {
    try testLexer("%1", &.{.int_literal});
    try testLexer("%10", &.{.int_literal});
    try testLexer("%101", &.{.int_literal});
}

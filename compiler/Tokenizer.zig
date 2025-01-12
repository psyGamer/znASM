const std = @import("std");

const Tokenizer = @This();

pub const Token = struct {
    pub const Tag = enum {
        invalid,
        new_line,
        eof,

        period,
        comma,
        colon,
        double_colon,
        semicolon,
        equal,
        lparen,
        rparen,
        lbracket,
        rbracket,
        lbrace,
        rbrace,

        ident,
        builtin_ident,
        int_literal,
        doc_comment,

        keyword_module,
        keyword_pub,
        keyword_fn,
        keyword_var,
        keyword_const,
        keyword_reg,
        keyword_struct,
        keyword_packed,
        keyword_enum,
        keyword_true,
        keyword_false,
        keyword_if,
        keyword_else,
        keyword_do,
        keyword_while,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .new_line,
                .eof,
                .ident,
                .builtin_ident,
                .int_literal,
                .doc_comment,
                => null,

                .period => ".",
                .comma => ",",
                .colon => ":",
                .double_colon => "::",
                .semicolon => ";",
                .equal => "=",
                .lparen => "(",
                .rparen => ")",
                .lbracket => "[",
                .rbracket => "]",
                .lbrace => "{",
                .rbrace => "}",

                .keyword_module => "module",
                .keyword_pub => "pub",
                .keyword_fn => "fn",
                .keyword_var => "var",
                .keyword_const => "const",
                .keyword_reg => "reg",
                .keyword_struct => "struct",
                .keyword_packed => "packed",
                .keyword_enum => "enum",
                .keyword_true => "true",
                .keyword_false => "false",
                .keyword_if => "if",
                .keyword_else => "else",
                .keyword_do => "do",
                .keyword_while => "while",
            };
        }
        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid bytes",
                .new_line => "a new-line",
                .ident => "an identifier",
                .builtin_ident => "a built-in identifier",
                .int_literal => "a number literal",
                .doc_comment => "a document comment",
                else => unreachable,
            };
        }
    };

    pub const Loc = struct {
        start: usize,
        end: usize,

        /// Returns the source data of the token location
        pub fn source(loc: Loc, src: []const u8) []const u8 {
            return src[loc.start..loc.end];
        }
    };

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const keywords: std.StaticStringMap(Tag) = .initComptime(.{
        .{ "module", .keyword_module },
        .{ "pub", .keyword_pub },
        .{ "fn", .keyword_fn },
        .{ "var", .keyword_var },
        .{ "const", .keyword_const },
        .{ "reg", .keyword_reg },
        .{ "struct", .keyword_struct },
        .{ "packed", .keyword_packed },
        .{ "enum", .keyword_enum },
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "do", .keyword_do },
        .{ "while", .keyword_while },
    });

    tag: Tag,
    loc: Loc,
};

buffer: [:0]const u8,
index: usize = 0,

pub fn next(lexer: *Tokenizer) Token {
    const State = enum {
        start,
        ident,
        slash,
        colon,
        comment,
        doc_comment,
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
        if (c == 0) {
            if (lexer.index != lexer.buffer.len) {
                token.tag = .invalid;
                token.loc.start = lexer.index;
                lexer.index += 1;
                token.loc.end = lexer.index;
                return token;
            }
            break;
        }

        switch (state) {
            .start => switch (c) {
                ' ', '\t' => {
                    token.loc.start = lexer.index + 1;
                },
                '\n', '\r' => {
                    token.tag = .new_line;
                    lexer.index += 1;
                    break;
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

                '/' => {
                    state = .slash;
                },
                '.' => {
                    token.tag = .period;
                    lexer.index += 1;
                    break;
                },
                ',' => {
                    token.tag = .comma;
                    lexer.index += 1;
                    break;
                },
                ':' => {
                    state = .colon;
                },
                ';' => {
                    token.tag = .semicolon;
                    lexer.index += 1;
                    break;
                },
                '=' => {
                    token.tag = .equal;
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
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    if (Token.getKeyword(lexer.buffer[token.loc.start..lexer.index])) |tag| {
                        token.tag = tag;
                    }
                    break;
                },
            },

            .slash => switch (c) {
                '/' => {
                    state = .comment;
                },
                else => break,
            },

            .colon => switch (c) {
                ':' => {
                    token.tag = .double_colon;
                    lexer.index += 1;
                    break;
                },
                else => {
                    token.tag = .colon;
                    break;
                },
            },

            .comment => switch (c) {
                '/' => {
                    state = .doc_comment;
                },
                '\n', '\r' => {
                    token.tag = .new_line;
                    lexer.index += 1;
                    break;
                },
                else => {},
            },

            .doc_comment => switch (c) {
                '\n', '\r' => {
                    token.tag = .doc_comment;
                    lexer.index += 1;
                    break;
                },
                else => {},
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

fn testTokenizer(src: [:0]const u8, expected_tokens: []const Token.Tag) !void {
    var lexer: Tokenizer = .{ .buffer = src };
    for (expected_tokens) |expected| {
        const token = lexer.next();
        try std.testing.expectEqualDeep(expected, token.tag);
    }
    const last_token = lexer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(src.len, last_token.loc.start);
    try std.testing.expectEqual(src.len, last_token.loc.end);
}

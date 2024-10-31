const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Error = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const NodeIndex = Ast.NodeIndex;

const Self = @This();

source: [:0]const u8,

index: TokenIndex = 0,
tokens: []const Token,

allocator: std.mem.Allocator,
nodes: std.ArrayListUnmanaged(Node) = .{},
errors: std.ArrayListUnmanaged(Error) = .{},

// Root and null share the same index, since no Node can have a root node as a child.
const null_node: NodeIndex = 0;

// Grammer parsing

/// Root <- (NamespaceExpr / SegmentExpr / GlobalVarDecl / FnDef)*
pub fn parseRoot(self: *Self) !void {
    var root = self.nodes.items[0];

    while (true) {
        const t = self.tokens[self.index];
        std.log.debug("tok {}", .{t});

        switch (t.tag) {
            .eof => break,
            // .keyword_namespace => try root.children.append(self.allocator, try self.parseNamespaceExpr()),
            // .keyword_segment => try root.children.append(self.allocator, try self.parseSegmentExpr()),
            .keyword_pub => {
                // Skip KEYWORD_pub
                self.index += 1;

                // const global_var_decl = try self.parseGlobalVarDecl(true);
                // if (global_var_decl != null_node) {
                //     try root.children.append(self.allocator, global_var_decl);
                //     continue;
                // }

                const fn_def = try self.parseFnDef(true);
                std.log.debug("Fn {}", .{fn_def});
                if (fn_def != null_node) {
                    try root.children.append(self.allocator, fn_def);
                    continue;
                }

                unreachable;
            },
            // .keyword_var,
            // .keyword_const,
            .keyword_fn,
            // .keyword_inline,
            => {
                // const global_var_decl = try self.parseGlobalVarDecl(false);
                // if (global_var_decl != null_node) {
                //     try root.children.append(self.allocator, global_var_decl);
                //     continue;
                // }

                const fn_def = try self.parseFnDef(false);
                if (fn_def != null_node) {
                    try root.children.append(self.allocator, fn_def);
                    continue;
                }

                unreachable;
            },

            else => {
                try self.errors.append(self.allocator, .{
                    .tag = .unexpected_token,
                    .type = .err,
                    .token = self.index,
                });
                return error.ParseFailed;
            },
        }
    }

    self.nodes.items[0] = root;
}

/// NamespaceExpr <- KEYWORD_namespace IDENTIFIER SEMICOLON
fn parseNamespaceExpr(self: *Self) !NodeIndex {
    _ = try self.expectToken(.keyword_namespace);
    const ident = try self.expectToken(.ident);
    _ = try self.expectToken(.semicolon);

    return try self.addNode(.{ .tag = .{ .namespace = self.source[ident.loc.start..ident.loc.end] } });
}

/// SegmentExpr <- KEYWORD_segment IDENTIFIER SEMICOLON
fn parseSegmentExpr(self: *Self) !NodeIndex {
    _ = try self.expectToken(.keyword_segment);
    const ident = try self.expectToken(.ident);
    _ = try self.expectToken(.semicolon);

    return try self.addNode(.{ .tag = .{ .segment = self.source[ident.loc.start..ident.loc.end] } });
}

/// GlobalVarDecl <- (KEYWORD_pub)? (KEYWORD_const / KEYWORD_var) IDENTIFIER COLON IDENTIFIER SEMICOLON
fn parseGlobalVarDecl(self: *Self, is_pub: bool) !NodeIndex {
    const mut =
        self.eatToken(.keyword_var) orelse
        self.eatToken(.keyword_const) orelse
        return null_node;

    const ident_name = try self.expectToken(.ident);
    _ = try self.expectToken(.colon);
    const ident_type = try self.expectToken(.ident);
    _ = try self.expectToken(.semicolon);

    return try self.addNode(.{
        .tag = .{
            .global_var_decl = .{
                .name = self.source[ident_name.loc.start..ident_name.loc.end],
                .type = self.source[ident_type.loc.start..ident_type.loc.end],
                .is_const = mut.tag == .keyword_const,
                .is_pub = is_pub,
            },
        },
    });
}

/// FnDef <- (KEYWORD_pub)? KEYWORD_fn (IDENTIFIER BUILTIN_IDENTIFIER) LPAREN RPAREN IDENTIFIER BlockExpr
fn parseFnDef(self: *Self, is_pub: bool) !NodeIndex {
    const main_token = self.index;
    _ = try self.expectToken(.keyword_fn);
    const ident_name = try self.expectToken(.ident);
    _ = try self.expectToken(.lparen);
    _ = try self.expectToken(.rparen);
    const opt_ident_return = self.eatToken(.ident);

    const node = try self.addNode(.{
        .tag = .{
            .fn_def = .{
                .name = self.source[ident_name.loc.start..ident_name.loc.end],
                .return_type = if (opt_ident_return) |ident_return|
                    self.source[ident_return.loc.start..ident_return.loc.end]
                else
                    null,
                .is_pub = is_pub,
            },
        },
        .main_token = main_token,
    });
    try self.nodes.items[node].children.append(self.allocator, try self.parseBlockExpr());
    return node;
}

/// BlockExpr <- LBRACE RBRACE
fn parseBlockExpr(self: *Self) !NodeIndex {
    const main_token = self.index;
    _ = try self.expectToken(.lbrace);
    while (true) {
        const t = self.tokens[self.index];
        if (t.tag == .rbrace) {
            break;
        }

        // Parse expression
        self.index += 1;
    }
    _ = try self.expectToken(.rbrace);

    return try self.addNode(.{ .tag = .block_expr, .main_token = main_token });
}

// Helper functions

fn nextToken(self: *Self) Token {
    const result = self.tokens[self.index];
    self.index += 1;
    return result;
}

fn eatToken(self: *Self, tag: Token.Tag) ?Token {
    return if (self.tokens[self.index].tag == tag)
        self.nextToken()
    else
        null;
}
fn expectToken(self: *Self, tag: Token.Tag) !Token {
    if (self.tokens[self.index].tag != tag) {
        return self.fail(.{
            .tag = .expected_token,
            .type = .err,
            .token = self.index,
            .extra = .{ .expected_tag = tag },
        });
    }
    return self.nextToken();
}

fn addNode(self: *Self, node: Node) !NodeIndex {
    try self.nodes.append(self.allocator, node);
    return @intCast(self.nodes.items.len - 1);
}

fn fail(self: *Self, err: Error) error{ ParseFailed, OutOfMemory } {
    try self.errors.append(self.allocator, err);
    return error.ParseFailed;
}

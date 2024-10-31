const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Ast = @import("Ast.zig");
const InstructionType = @import("instruction.zig").InstructionType;
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

const null_node = Ast.null_node;

// Grammer parsing

/// Root <- (ModuleExpr / GlobalVarDecl / FnDef)*
pub fn parseRoot(self: *Self) !void {
    const root = 0;

    while (true) {
        const t = self.tokens[self.index];
        std.log.debug("tok {}", .{t});

        sw: switch (t.tag) {
            .eof => break,
            .keyword_module => try self.addChild(root, try self.parseModuleExpr()),
            .keyword_pub => {
                // Skip KEYWORD_pub
                self.index += 1;
                continue :sw .keyword_fn;
            },
            // .keyword_var,
            // .keyword_const,
            .keyword_fn,
            => {
                // const global_var_decl = try self.parseGlobalVarDecl(false);
                // if (global_var_decl != null_node) {
                //     try root.children.append(self.allocator, global_var_decl);
                //     continue;
                // }

                const fn_def = try self.parseFnDef(false);
                if (fn_def != null_node) {
                    try self.addChild(root, fn_def);
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
}

/// ModuleExpr <- KEYWORD_module IDENTIFIER
fn parseModuleExpr(self: *Self) !NodeIndex {
    const main_token = self.index;

    _ = try self.expectToken(.keyword_module);
    const ident = try self.expectToken(.ident);

    return try self.addNode(.{
        .tag = .{ .module = self.source[ident.loc.start..ident.loc.end] },
        .main_token = main_token,
    });
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
    try self.addChild(node, try self.parseBlockExpr());
    return node;
}

/// BlockExpr <- LBRACE Expr* RBRACE
fn parseBlockExpr(self: *Self) !NodeIndex {
    const node = try self.addNode(.{ .tag = .block_expr, .main_token = self.index });

    _ = try self.expectToken(.lbrace);
    while (true) {
        const t = self.tokens[self.index];
        if (t.tag == .rbrace) {
            break;
        }

        try self.addChild(node, try self.parseExpr());
    }
    _ = try self.expectToken(.rbrace);

    return node;
}

/// Expr <- Instrucrtion
fn parseExpr(self: *Self) !NodeIndex {
    const start_index = self.index;

    const instr = self.parseInstruction() catch |err| switch (err) {
        error.ParseFailed => null_node,
        else => |e| return e,
    };
    if (instr != null_node) {
        return instr;
    }

    self.index = start_index;

    return self.fail(.{
        .tag = .expected_expression,
        .type = .err,
        .token = self.index,
    });
}

/// Instruction <- IDENTIFIER
fn parseInstruction(self: *Self) !NodeIndex {
    const ident_opcode, const ident_opcode_idx = try self.expectTokenIdx(.ident);

    const opcode_name = self.source[ident_opcode.loc.start..ident_opcode.loc.end];
    const opcode: InstructionType = find_opcode: {
        inline for (std.meta.fields(InstructionType)) |field| {
            if (std.mem.eql(u8, field.name, opcode_name)) {
                break :find_opcode @enumFromInt(field.value);
            }
        }

        return error.ParseFailed;
        // return self.fail(.{
        //     .tag = .invalid_opcode,
        //     .type = .err,
        //     .token = ident_opcode_idx,
        // });
    };

    return self.addNode(.{
        .tag = .{ .instruction = .{
            .opcode = opcode,
        } },
        .main_token = ident_opcode_idx,
    });
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
fn expectTokenIdx(self: *Self, tag: Token.Tag) !struct { Token, TokenIndex } {
    if (self.tokens[self.index].tag != tag) {
        return self.fail(.{
            .tag = .expected_token,
            .type = .err,
            .token = self.index,
            .extra = .{ .expected_tag = tag },
        });
    }
    return .{ self.nextToken(), self.index - 1 };
}

fn addNode(self: *Self, node: Node) !NodeIndex {
    try self.nodes.append(self.allocator, node);
    return @intCast(self.nodes.items.len - 1);
}
fn addChild(self: *Self, parent: NodeIndex, child: NodeIndex) !void {
    self.nodes.items[child].parent = parent;
}

fn fail(self: *Self, err: Error) error{ ParseFailed, OutOfMemory } {
    try self.errors.append(self.allocator, err);
    return error.ParseFailed;
}

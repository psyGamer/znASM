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

state_stack: std.ArrayListUnmanaged(struct {
    index: u32,
    nodes: usize,
    errors: usize,
}) = .empty,

const null_node = Ast.null_node;

/// Saves the current parser state, for use when trying multiple sub-items
fn saveState(self: *Self) !void {
    try self.state_stack.append(self.allocator, .{
        .index = self.index,
        .nodes = self.nodes.items.len,
        .errors = self.errors.items.len,
    });
}
/// Restores a previously saved parser state
fn restoreState(self: *Self) void {
    const state = self.state_stack.pop();
    self.index = state.index;
    self.nodes.items.len = state.nodes;
    self.errors.items.len = state.errors;
}

// Grammer parsing

const ParseError = error{ParseFailed} || std.mem.Allocator.Error;

/// Root <- (ModuleExpr / GlobalVarDecl / FnDef)*
pub fn parseRoot(self: *Self) ParseError!void {
    const root = 0;

    while (true) {
        const t = self.tokens[self.index];
        std.log.debug("tok {}", .{t});

        sw: switch (t.tag) {
            .new_line => {
                self.index += 1;
                continue;
            },
            .eof => break,
            .keyword_module => self.addChild(root, try self.parseModuleExpr()),
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
                    self.addChild(root, fn_def);
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
fn parseModuleExpr(self: *Self) ParseError!NodeIndex {
    const main_token = self.index;

    _ = try self.expectToken(.keyword_module);
    const ident = try self.expectToken(.ident);

    return try self.addNode(.{
        .tag = .{ .module = self.source[ident.loc.start..ident.loc.end] },
        .main_token = main_token,
    });
}

/// GlobalVarDecl <- (KEYWORD_pub)? (KEYWORD_const / KEYWORD_var) IDENTIFIER COLON IDENTIFIER SEMICOLON
fn parseGlobalVarDecl(self: *Self, is_pub: bool) ParseError!NodeIndex {
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

/// FnDef <- (KEYWORD_pub)? KEYWORD_fn (IDENTIFIER BUILTIN_IDENTIFIER) LPAREN RPAREN IDENTIFIER FnBlock
fn parseFnDef(self: *Self, is_pub: bool) ParseError!NodeIndex {
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
    self.addChild(node, try self.parseFnBlock());

    return node;
}

/// FnBlock <- LBRACE NEW_LINE (NEW_NEWLINE | Expr | Instruction | Label)* RBRACE NEW_LINE
fn parseFnBlock(self: *Self) ParseError!NodeIndex {
    const node = try self.addNode(.{ .tag = .block_expr, .main_token = self.index });

    _ = try self.expectToken(.lbrace);
    _ = try self.expectToken(.new_line);
    while (true) {
        const t = self.tokens[self.index];
        if (t.tag == .rbrace) {
            break;
        }

        if (t.tag == .new_line) {
            self.index += 1;
            continue;
        }

        // Expression
        const expr = self.parseExpr() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (expr != null_node) {
            self.addChild(node, expr);
            continue;
        }

        // Instruction
        const instr = self.parseInstruction() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (instr != null_node) {
            self.addChild(node, instr);
            continue;
        }

        // Label
        const label = self.parseLabel() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (label != null_node) {
            self.addChild(node, label);
            continue;
        }

        return self.fail(.{
            .tag = .expected_expr_instr_label,
            .type = .err,
            .token = self.index,
        });
    }
    _ = try self.expectToken(.rbrace);
    _ = try self.expectToken(.new_line);

    return node;
}

/// Expr <- BlockExpr
fn parseExpr(self: *Self) ParseError!NodeIndex {
    try self.saveState();
    errdefer self.restoreState();

    const start_index = self.index;

    const block = self.parseBlockExpr() catch |err| switch (err) {
        error.ParseFailed => null_node,
        else => |e| return e,
    };
    if (block != null_node) {
        return block;
    }

    self.index = start_index;

    return self.fail(.{
        .tag = .expected_expression,
        .type = .err,
        .token = self.index,
    });
}

/// BlockExpr <- LBRACE NEW_LINE Expr* RBRACE NEW_LINE
fn parseBlockExpr(self: *Self) ParseError!NodeIndex {
    const node = try self.addNode(.{ .tag = .block_expr, .main_token = self.index });

    _ = try self.expectToken(.lbrace);
    _ = try self.expectToken(.new_line);
    while (true) {
        const t = self.tokens[self.index];
        if (t.tag == .rbrace) {
            break;
        }

        self.addChild(node, try self.parseExpr());
    }
    _ = try self.expectToken(.rbrace);
    _ = try self.expectToken(.new_line);

    return node;
}

/// Instruction <- IDENTIFIER (INT_LITERAL | IDENTIFIER)? NEW_LINE
fn parseInstruction(self: *Self) ParseError!NodeIndex {
    try self.saveState();
    errdefer self.restoreState();

    const ident_opcode, const ident_opcode_idx = try self.expectTokenIdx(.ident);

    const opcode_name = self.source[ident_opcode.loc.start..ident_opcode.loc.end];
    const opcode: InstructionType = find_opcode: {
        inline for (std.meta.fields(InstructionType)) |field| {
            if (std.mem.eql(u8, field.name, opcode_name)) {
                break :find_opcode @enumFromInt(field.value);
            }
        }

        return error.ParseFailed;
    };

    const node = try self.addNode(.{
        .tag = .{ .instruction = .{
            .opcode = opcode,
            .operand = .none,
        } },
        .main_token = ident_opcode_idx,
    });

    if (self.eatToken(.int_literal)) |ident_operand| {
        const operand_str = self.source[ident_operand.loc.start..ident_operand.loc.end];
        const number_base: u8 = switch (operand_str[0]) {
            '$' => 16,
            '%' => 2,
            '0'...'9' => 10,
            else => unreachable,
        };
        const operand = std.fmt.parseInt(u16, operand_str[(if (number_base == 10) 0 else 1)..], number_base) catch unreachable;
        self.nodes.items[node].tag.instruction.operand = .{ .number = operand };
    }
    if (self.eatToken(.ident)) |ident_operand| {
        const operand_str = self.source[ident_operand.loc.start..ident_operand.loc.end];
        self.nodes.items[node].tag.instruction.operand = .{ .identifier = operand_str };
    }

    _ = try self.expectToken(.new_line);

    return node;
}

/// Label <- IDENTIFIER COLON NEW_LINE
fn parseLabel(self: *Self) ParseError!NodeIndex {
    try self.saveState();
    errdefer self.restoreState();

    const ident_name, const ident_name_idx = try self.expectTokenIdx(.ident);
    _ = try self.expectToken(.colon);
    _ = try self.expectToken(.new_line);

    return try self.addNode(.{
        .tag = .{ .label = self.source[ident_name.loc.start..ident_name.loc.end] },
        .main_token = ident_name_idx,
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
fn eatTokenIdx(self: *Self, tag: Token.Tag) ?struct { Token, TokenIndex } {
    return if (self.tokens[self.index].tag == tag)
        .{ self.nextToken(), self.index - 1 }
    else
        null;
}

fn expectToken(self: *Self, tag: Token.Tag) ParseError!Token {
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
fn expectTokenIdx(self: *Self, tag: Token.Tag) ParseError!struct { Token, TokenIndex } {
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
fn addChild(self: *Self, parent: NodeIndex, child: NodeIndex) void {
    self.nodes.items[child].parent = parent;
}

fn fail(self: *Self, err: Error) ParseError {
    try self.errors.append(self.allocator, err);
    return error.ParseFailed;
}

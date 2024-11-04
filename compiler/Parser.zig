const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Error = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const NodeIndex = Ast.NodeIndex;
const ExtraIndex = Ast.ExtraIndex;

const Parser = @This();

source: [:0]const u8,
index: TokenIndex = 0,

token_tags: []const Token.Tag,
token_locs: []const Token.Loc,

nodes: std.MultiArrayList(Node) = .empty,
/// Additional data for specific nodes
extra_data: std.ArrayListUnmanaged(NodeIndex) = .empty,

errors: std.ArrayListUnmanaged(Error) = .{},

/// Temporarily stores children of nodes
scratch: std.ArrayListUnmanaged(NodeIndex) = .empty,

state_stack: std.ArrayListUnmanaged(struct {
    index: u32,
    nodes: usize,
    errors: usize,
}) = .empty,

allocator: std.mem.Allocator,

const null_node = Ast.null_node;

// Grammer parsing

const ParseError = error{ParseFailed} || std.mem.Allocator.Error;

/// Root <- ModuleDef (FnDef | ConstDef | VarDef)*
pub fn parseRoot(p: *Parser) ParseError!void {
    const tld_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(tld_start);

    const root = try p.addNode(.{
        .tag = .root,
        .main_token = undefined,
        .data = undefined,
    });

    p.skipNewLines();
    try p.scratch.append(p.allocator, try p.parseModuleDef());

    var doc_start: ?usize = null;
    while (true) {
        const t = p.token_tags[p.index];
        std.log.info("t {}", .{t});
        if (t == .new_line) {
            p.index += 1;
            continue;
        }
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.allocator, try p.addNode(.{
                .tag = .doc_comment,
                .main_token = p.index,
                .data = undefined,
            }));
            p.index += 1;
            continue;
        }
        if (t == .eof) {
            break;
        }

        const doc_comments: Node.SubRange = get_comments: {
            if (doc_start) |start| {
                const comments = try p.writeExtraSubRange(p.scratch.items[start..]);
                p.scratch.shrinkRetainingCapacity(start);
                doc_start = null;
                break :get_comments comments;
            } else {
                break :get_comments .{ .extra_start = null_node, .extra_end = null_node };
            }
        };

        // Function Definition
        const fn_def = try p.parseFnDef(doc_comments);
        if (fn_def != null_node) {
            try p.scratch.append(p.allocator, fn_def);
            continue;
        }

        // Constant Definition
        const const_def = try p.parseConstDef(doc_comments);
        if (const_def != null_node) {
            try p.scratch.append(p.allocator, const_def);
            continue;
        }

        // Variable Definition
        const var_def = try p.parseVarDef(doc_comments);
        if (var_def != null_node) {
            try p.scratch.append(p.allocator, var_def);
            continue;
        }

        return p.fail(.expected_toplevel);
    }

    p.nodes.items(.data)[root] = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[tld_start..]) };
}

/// ModuleDef <- KEYWORD_module IDENTIFIER NEW_LINE
fn parseModuleDef(p: *Parser) ParseError!NodeIndex {
    _ = try p.expectToken(.keyword_module);
    const ident = try p.expectToken(.ident);
    _ = try p.expectToken(.new_line);

    // TODO: Verify module name is legal
    return try p.addNode(.{
        .tag = .module,
        .main_token = ident,
        .data = undefined,
    });
}

/// FnDef <- (KEYWORD_pub)? KEYWORD_fn (IDENTIFIER | BUILTIN_IDENTIFIER) LPAREN RPAREN (BankAttr)? (IDENTIFIER)? Block
fn parseFnDef(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    _ = p.eatToken(.keyword_pub);
    const keyword_fn = p.eatToken(.keyword_fn) orelse return null_node;
    _ = p.eatToken(.builtin_ident) orelse try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.rparen);
    const bank_attr = try p.parseBankAttr();
    _ = p.eatToken(.ident);

    const block = try p.parseBlock();

    return try p.addNode(.{
        .tag = .fn_def,
        .main_token = keyword_fn,
        .data = .{ .fn_def = .{
            .block = block,
            .extra = try p.writeExtraData(Ast.Node.FnDefData, .{
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

/// ConstDef <- (KEYWORD_pub)? KEYWORD_const IDENTIFIER COLON TypeExpr (BankAttr)? EQUAL Expr NEW_LINE
fn parseConstDef(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    _ = p.eatToken(.keyword_pub);
    const keyword_const = p.eatToken(.keyword_const) orelse return null_node;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const bank_attr = try p.parseBankAttr();
    _ = try p.expectToken(.equal);
    const value_expr = try p.parseExpr();
    if (value_expr == null_node) {
        return p.fail(.expected_expr);
    }
    _ = try p.expectToken(.new_line);

    return try p.addNode(.{
        .tag = .const_def,
        .main_token = keyword_const,
        .data = .{ .const_def = .{
            .value = value_expr,
            .extra = try p.writeExtraData(Ast.Node.ConstDefData, .{
                .type = type_expr,
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

/// VarDef <- (KEYWORD_pub)? KEYWORD_var IDENTIFIER COLON TypeExpr (BankAttr)? NEW_LINE
fn parseVarDef(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    _ = p.eatToken(.keyword_pub);
    const keyword_var = p.eatToken(.keyword_var) orelse return null_node;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const bank_attr = try p.parseBankAttr();
    _ = try p.expectToken(.new_line);

    return try p.addNode(.{
        .tag = .var_def,
        .main_token = keyword_var,
        .data = .{ .var_def = .{
            .extra = try p.writeExtraData(Ast.Node.VarDefData, .{
                .type = type_expr,
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

/// BankAttr <- IDENTIFIER LPAREN (INT_LITERAL) RPAREN
fn parseBankAttr(p: *Parser) ParseError!NodeIndex {
    const ident_bank = p.eatToken(.ident) orelse return null_node;
    if (!std.mem.eql(u8, "bank", p.source[p.token_locs[ident_bank].start..p.token_locs[ident_bank].end]))
        return null_node;

    _ = try p.expectToken(.lparen);
    const int_literal = try p.expectToken(.int_literal);
    _ = try p.expectToken(.rparen);

    return p.addNode(.{
        .tag = .bank_attr,
        .main_token = int_literal,
        .data = undefined,
    });
}

/// Block <- LBRACE NEW_LINE (NEW_NEWLINE | Expr | Instruction | Label)* RBRACE NEW_LINE
fn parseBlock(p: *Parser) ParseError!NodeIndex {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const lbrace = try p.expectToken(.lbrace);
    _ = try p.expectToken(.new_line);
    while (true) {
        const t = p.token_tags[p.index];
        if (t == .new_line) {
            p.index += 1;
            continue;
        }
        if (t == .rbrace) {
            break;
        }

        const start_idx = p.index;

        // Call
        const call = try p.parseCallStatement();
        if (call != null_node) {
            try p.scratch.append(p.allocator, call);
            continue;
        }
        p.index = start_idx;

        // While
        const @"while" = try p.parseWhileStatement();
        if (@"while" != null_node) {
            try p.scratch.append(p.allocator, @"while");
            continue;
        }
        p.index = start_idx;

        // Instruction
        const instr = try p.parseInstruction();
        if (instr != null_node) {
            try p.scratch.append(p.allocator, instr);
            continue;
        }
        p.index = start_idx;

        // Label
        const label = try p.parseLabel();
        if (label != null_node) {
            try p.scratch.append(p.allocator, label);
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_statement);
    }
    _ = try p.expectToken(.rbrace);
    _ = try p.expectToken(.new_line);

    return p.addNode(.{
        .tag = .block,
        .main_token = lbrace,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
    });
}

/// Instruction <- IDENTIFIER (INT_LITERAL | IDENTIFIER)? NEW_LINE
fn parseInstruction(p: *Parser) ParseError!NodeIndex {
    const ident_opcode = p.eatToken(.ident) orelse return null_node;
    _ = p.eatToken(.ident) orelse p.eatToken(.int_literal);
    _ = p.eatToken(.new_line) orelse return null_node;

    return try p.addNode(.{
        .tag = .instruction,
        .main_token = ident_opcode,
        .data = undefined,
    });
}

/// Label <- IDENTIFIER COLON NEW_LINE
fn parseLabel(p: *Parser) ParseError!NodeIndex {
    const ident_name = p.eatToken(.ident) orelse return null_node;
    _ = p.eatToken(.colon) orelse return null_node;
    _ = p.eatToken(.new_line) orelse return null_node;

    return try p.addNode(.{
        .tag = .label,
        .main_token = ident_name,
        .data = undefined,
    });
}

/// CallStatement <- (IDENTIFIER | BUILTIN_IDENTIFIER) LPAREN ExprList RPAREN NEW_LINE
///
/// ExprList <- (Expr COMMA)* Expr?
fn parseCallStatement(p: *Parser) ParseError!NodeIndex {
    const expr_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(expr_start);

    const ident_name = p.eatToken(.builtin_ident) orelse p.eatToken(.ident) orelse return null_node;
    _ = p.eatToken(.lparen) orelse return null_node;

    while (true) {
        if (p.eatToken(.rparen)) |_| break;

        const param = try p.parseExpr();
        if (param == null_node) {
            return p.fail(.expected_expr);
        }

        try p.scratch.append(p.allocator, param);

        switch (p.token_tags[p.index]) {
            .comma => p.index += 1,
            .rparen => {
                p.index += 1;
                break;
            },
            else => {
                // Likely just a missing comma
                try p.warn(.expected_comma_after_arg);
            },
        }
    }
    _ = try p.expectToken(.new_line);

    return p.addNode(.{
        .tag = .call_statement,
        .main_token = ident_name,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[expr_start..]) },
    });
}

/// WhiteStatement
///     <- KEYWORD_while LPAREN KEYWORD_true RPAREN Block
fn parseWhileStatement(p: *Parser) ParseError!NodeIndex {
    const keyword_while = p.eatToken(.keyword_while) orelse return null_node;
    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.keyword_true);
    _ = try p.expectToken(.rparen);

    const block = try p.parseBlock();

    return try p.addNode(.{
        .tag = .while_statement,
        .main_token = keyword_while,
        .data = .{ .while_statement = .{
            .condition = null_node,
            .block = block,
        } },
    });
}

/// Expr <- (IDENTIFIER | INT_LITERAL)
fn parseExpr(p: *Parser) ParseError!NodeIndex {
    if (p.eatToken(.ident)) |ident| {
        return try p.addNode(.{
            .tag = .expr_ident,
            .main_token = ident,
            .data = undefined,
        });
    }
    if (p.eatToken(.int_literal)) |literal| {
        return try p.addNode(.{
            .tag = .expr_value,
            .main_token = literal,
            .data = undefined,
        });
    }

    return null_node;
}

/// TypeExpr <- (IDENTIFIER)
fn parseTypeExpr(p: *Parser) ParseError!NodeIndex {
    if (p.eatToken(.ident)) |ident| {
        return try p.addNode(.{
            .tag = .type_ident,
            .main_token = ident,
            .data = undefined,
        });
    }

    return null_node;
}

// Helper functions

fn addNode(p: *Parser, node: Node) ParseError!NodeIndex {
    try p.nodes.append(p.allocator, node);
    return @intCast(p.nodes.len - 1);
}

/// Stores a list of sub-nodes into `extra`
fn writeExtraSubRange(p: *Parser, list: []const NodeIndex) ParseError!Node.SubRange {
    const start: ExtraIndex = @intCast(p.extra_data.items.len);
    try p.extra_data.appendSlice(p.allocator, list);
    const end: ExtraIndex = @intCast(p.extra_data.items.len);

    return .{ .extra_start = start, .extra_end = end };
}
/// Stores a custom data-type into `extra`
fn writeExtraData(p: *Parser, comptime T: type, value: T) !ExtraIndex {
    const fields = std.meta.fields(T);

    const extra_idx = p.extra_data.items.len;
    try p.extra_data.ensureUnusedCapacity(p.allocator, fields.len);
    p.extra_data.items.len += fields.len;

    inline for (fields, 0..) |field, i| {
        comptime std.debug.assert(field.type == NodeIndex);
        p.extra_data.items[extra_idx + i] = @field(value, field.name);
    }

    return @intCast(extra_idx);
}

fn skipNewLines(p: *Parser) void {
    while (p.token_tags[p.index] == .new_line) {
        p.index += 1;
    }
}

fn nextToken(p: *Parser) TokenIndex {
    const result = p.index;
    p.index += 1;
    return result;
}
fn checkToken(p: *Parser, offset: TokenIndex, tag: Token.Tag) bool {
    return if (p.index + offset >= p.token_tags.len)
        false
    else
        p.token_tags[p.index + offset] == tag;
}
fn eatToken(p: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (p.token_tags[p.index] == tag)
        p.nextToken()
    else
        null;
}
fn expectToken(p: *Parser, tag: Token.Tag) ParseError!TokenIndex {
    if (p.token_tags[p.index] != tag) {
        try p.errors.append(p.allocator, .{
            .tag = .expected_token,
            .token = p.index,
            .extra = .{ .expected_tag = tag },
        });
        return error.ParseFailed;
    }
    return p.nextToken();
}

/// Reports a fatal error, which doesn't allow for further parsing
fn fail(p: *Parser, tag: Error.Tag) ParseError {
    try p.errors.append(p.allocator, .{
        .tag = tag,
        .token = p.index,
    });
    return error.ParseFailed;
}

/// Reports an error, while continuing with parsing
fn warn(p: *Parser, tag: Error.Tag) !void {
    try p.errors.append(p.allocator, .{
        .tag = tag,
        .token = p.index,
    });
}

/// Attaches a note to the previous error
fn note(p: *Parser, tag: Error.Tag) !void {
    return p.errMsg(.{
        .tag = tag,
        .token = p.index,
        .is_note = true,
    });
}

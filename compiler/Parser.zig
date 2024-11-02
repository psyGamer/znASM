const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Ast = @import("Ast.zig");
const InstructionType = @import("instruction.zig").InstructionType;
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

/// Root <- ModuleDef (FnDef)*
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

        return p.fail(.{
            .tag = .expected_toplevel,
            .type = .err,
            .token = p.index,
        });
    }

    p.nodes.items(.data)[root] = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[tld_start..]) };
}

/// ModuleDef <- KEYWORD_module IDENTIFIER NEW_LINE
fn parseModuleDef(self: *Parser) ParseError!NodeIndex {
    _ = try self.expectToken(.keyword_module);
    const ident = try self.expectToken(.ident);
    _ = try self.expectToken(.new_line);

    // TODO: Verify module name is legal
    return try self.addNode(.{
        .tag = .module,
        .main_token = ident,
        .data = undefined,
    });
}

/// FnDef <- (KEYWORD_pub)? KEYWORD_fn IDENTIFIER LPAREN RPAREN (BankAttr)? (IDENTIFIER)? Block
fn parseFnDef(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    _ = p.eatToken(.keyword_pub);
    const keyword_fn = p.eatToken(.keyword_fn) orelse return null_node;
    _ = try p.expectToken(.ident);
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

        // Expression
        const expr = p.parseExpr() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (expr != null_node) {
            try p.scratch.append(p.allocator, expr);
            continue;
        }

        // Instruction
        const instr = p.parseInstruction() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (instr != null_node) {
            try p.scratch.append(p.allocator, instr);
            continue;
        }

        // Label
        const label = p.parseLabel() catch |err| switch (err) {
            error.ParseFailed => null_node,
            else => |e| return e,
        };
        if (label != null_node) {
            try p.scratch.append(p.allocator, label);
            continue;
        }

        return p.fail(.{
            .tag = .expected_expr_instr_label,
            .type = .err,
            .token = p.index,
        });
    }
    _ = try p.expectToken(.rbrace);
    _ = try p.expectToken(.new_line);

    return p.addNode(.{
        .tag = .block,
        .main_token = lbrace,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
    });
}

/// Expr <-
fn parseExpr(self: *Parser) ParseError!NodeIndex {
    _ = self; // autofix
    return null_node;
}

/// Instruction <- IDENTIFIER (INT_LITERAL | IDENTIFIER)? NEW_LINE
fn parseInstruction(self: *Parser) ParseError!NodeIndex {
    const start_idx = self.index;
    errdefer self.index = start_idx;

    const ident_opcode = self.eatToken(.ident) orelse return null_node;
    _ = self.eatToken(.ident) orelse self.eatToken(.int_literal);
    _ = self.eatToken(.new_line) orelse return error.ParseFailed;

    return try self.addNode(.{
        .tag = .instruction,
        .main_token = ident_opcode,
        .data = undefined,
    });
}

/// Label <- IDENTIFIER COLON NEW_LINE
fn parseLabel(self: *Parser) ParseError!NodeIndex {
    const start_idx = self.index;
    errdefer self.index = start_idx;

    const ident_name = self.eatToken(.ident) orelse return null_node;
    _ = self.eatToken(.colon) orelse return error.ParseFailed;
    _ = self.eatToken(.new_line) orelse return error.ParseFailed;

    return try self.addNode(.{
        .tag = .label,
        .main_token = ident_name,
        .data = undefined,
    });
}

// Helper functions

fn addNode(self: *Parser, node: Node) ParseError!NodeIndex {
    try self.nodes.append(self.allocator, node);
    return @intCast(self.nodes.len - 1);
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
        return p.fail(.{
            .tag = .expected_token,
            .type = .err,
            .token = p.index,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn fail(p: *Parser, err: Error) ParseError {
    try p.errors.append(p.allocator, err);
    return error.ParseFailed;
}

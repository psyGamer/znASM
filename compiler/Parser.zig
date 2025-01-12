const std = @import("std");

const Token = @import("Tokenizer.zig").Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Error = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const NodeIndex = Ast.NodeIndex;
const ExtraIndex = Ast.ExtraIndex;

const Parser = @This();

source: [:0]const u8,
index: std.meta.Tag(TokenIndex) = 0,

token_tags: []const Token.Tag,
token_locs: []const Token.Loc,

nodes: std.MultiArrayList(Node) = .empty,
/// Additional data for specific nodes
extra_data: std.ArrayListUnmanaged(Ast.CommonIndex) = .empty,

errors: std.ArrayListUnmanaged(Error) = .{},

/// Temporarily store range data for `extra_data`
scratch: std.ArrayListUnmanaged(Ast.CommonIndex) = .empty,

state_stack: std.ArrayListUnmanaged(struct {
    index: std.meta.Tag(TokenIndex),
    nodes: usize,
    errors: usize,
}) = .empty,

allocator: std.mem.Allocator,

pub inline fn tokenTag(p: Parser, index: TokenIndex) Token.Tag {
    return p.token_tags[@intFromEnum(index)];
}
pub inline fn tokenLoc(p: Parser, index: TokenIndex) Token.Loc {
    return p.token_locs[@intFromEnum(index)];
}

// Grammer parsing

const ParseError = error{ParseFailed} || std.mem.Allocator.Error;

/// Root <- ModuleDef (FnDef | ConstDef | VarDef | EnumDef)*
pub fn parseRoot(p: *Parser) ParseError!void {
    const tld_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(tld_start);

    const root = try p.addNode(.{
        .tag = .root,
        .main_token = undefined,
        .data = undefined,
    });
    _ = root; // autofix

    p.skipNewLines();
    try p.scratch.append(p.allocator, @intFromEnum(try p.parseModuleDef()));

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
            try p.scratch.append(p.allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .main_token = @enumFromInt(p.index),
                .data = undefined,
            })));
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
                break :get_comments .empty;
            }
        };

        // Function Definition
        const fn_def = try p.parseFnDef(doc_comments);
        if (fn_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(fn_def));
            continue;
        }

        // Constant Definition
        const const_def = try p.parseConstDef(doc_comments);
        if (const_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(const_def));
            continue;
        }

        // Variable Definition
        const var_def = try p.parseVarDef(doc_comments);
        if (var_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(var_def));
            continue;
        }

        // Register Definition
        const reg_def = try p.parseRegDef(doc_comments);
        if (reg_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(reg_def));
            continue;
        }

        // Packed Definition
        const packed_def = try p.parsePackedDef(doc_comments, .top_level);
        if (packed_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(packed_def));
            continue;
        }

        // Enum Definition
        const enum_def = try p.parseEnumDef(doc_comments, .top_level);
        if (enum_def != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(enum_def));
            continue;
        }

        return p.fail(.expected_toplevel);
    }

    p.nodes.items(.data)[@intFromEnum(NodeIndex.root)] = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[tld_start..]) };
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
    const keyword_fn = p.eatToken(.keyword_fn) orelse return .none;
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
    const keyword_const = p.eatToken(.keyword_const) orelse return .none;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const bank_attr = try p.parseBankAttr();
    _ = try p.expectToken(.equal);
    const value_expr = try p.parseExpr();
    if (value_expr == .none) {
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
    const keyword_var = p.eatToken(.keyword_var) orelse return .none;
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

/// RegDef <- (KEYWORD_pub)? KEYWORD_reg IDENTIFIER COLON TypeExpr AccessAttr EQUAL INT_LITERAL NEW_LINE
fn parseRegDef(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    _ = p.eatToken(.keyword_pub);
    const keyword_reg = p.eatToken(.keyword_reg) orelse return .none;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const access_attr = try p.parseAccessAttr();
    _ = try p.expectToken(.equal);
    const ident_address = try p.expectToken(.int_literal);
    _ = try p.expectToken(.new_line);

    return try p.addNode(.{
        .tag = .reg_def,
        .main_token = keyword_reg,
        .data = .{ .reg_def = .{
            .extra = try p.writeExtraData(Ast.Node.RegDefData, .{
                .type = type_expr,
                .address = ident_address,
                .access_attr = access_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

const DeclType = enum { top_level, anonymous };

///     PackedDef <- (KEYWORD_pub)? KEYWORD_packed IDENTIFIER LPAREN TypeExpr RPAREN StructBlock
/// AnonPackedDef <- KEYWORD_packed LPAREN TypeExpr RPAREN StructBlock
fn parsePackedDef(p: *Parser, doc_comments: Node.SubRange, decl_type: DeclType) ParseError!NodeIndex {
    if (decl_type == .top_level) _ = p.eatToken(.keyword_pub);
    const keyword_packed = p.eatToken(.keyword_packed) orelse return .none;
    if (decl_type == .top_level) _ = try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    const type_expr = try p.parseTypeExpr();
    _ = try p.expectToken(.rparen);
    const struct_block = try p.parseStructBlock();

    return try p.addNode(.{
        .tag = .packed_def,
        .main_token = keyword_packed,
        .data = .{ .packed_def = .{
            .block = struct_block,
            .extra = try p.writeExtraData(Ast.Node.PackedDefData, .{
                .backing_type = type_expr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

/// EmumDef     <- (KEYWORD_pub)? KEYWORD_enum IDENTIFIER LPAREN TypeExpr RPAREN EnumBlock
/// AnonEmumDef <- KEYWORD_enum LPAREN TypeExpr RPAREN EnumBlock
fn parseEnumDef(p: *Parser, doc_comments: Node.SubRange, decl_type: DeclType) ParseError!NodeIndex {
    if (decl_type == .top_level) _ = p.eatToken(.keyword_pub);
    const keyword_enum = p.eatToken(.keyword_enum) orelse return .none;
    if (decl_type == .top_level) _ = try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    const type_expr = try p.parseTypeExpr();
    _ = try p.expectToken(.rparen);
    const enum_block = try p.parseEnumBlock();

    return try p.addNode(.{
        .tag = .enum_def,
        .main_token = keyword_enum,
        .data = .{ .enum_def = .{
            .block = enum_block,
            .extra = try p.writeExtraData(Ast.Node.EnumDefData, .{
                .backing_type = type_expr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
    });
}

/// StructBlock <- LBRACE NEW_LINE (StructField)* RBRACE
fn parseStructBlock(p: *Parser) ParseError!NodeIndex {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const lbrace = try p.expectToken(.lbrace);
    _ = try p.expectToken(.new_line);

    var doc_start: ?usize = null;
    while (true) {
        const t = p.token_tags[p.index];
        if (t == .new_line) {
            p.index += 1;
            continue;
        }
        if (t == .rbrace) {
            break;
        }
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .main_token = @enumFromInt(p.index),
                .data = undefined,
            })));
            p.index += 1;
            continue;
        }

        const doc_comments: Node.SubRange = get_comments: {
            if (doc_start) |start| {
                const comments = try p.writeExtraSubRange(p.scratch.items[start..]);
                p.scratch.shrinkRetainingCapacity(start);
                doc_start = null;
                break :get_comments comments;
            } else {
                break :get_comments .empty;
            }
        };

        const start_idx = p.index;

        // Struct Field
        const field = try p.parseStructField(doc_comments);
        if (field != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(field));
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_enum_member);
    }
    _ = try p.expectToken(.rbrace);
    _ = try p.expectToken(.new_line);

    return p.addNode(.{
        .tag = .struct_block,
        .main_token = lbrace,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
    });
}

/// StructField <- IDENTIFIER COLON TypeExpr (EQUAL Expr)? COMMA NEW_LINE
fn parseStructField(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();

    const value_expr = if (p.eatToken(.equal)) |_|
        try p.parseExpr()
    else
        .none;

    _ = try p.expectToken(.comma);
    _ = try p.expectToken(.new_line);

    return try p.addNode(.{
        .tag = .struct_field,
        .main_token = ident_name,
        .data = .{ .struct_field = .{
            .extra = try p.writeExtraData(Node.StructFieldData, .{
                .type = type_expr,
                .value = value_expr,
            }),
            .doc_comments = try p.writeExtraData(Node.SubRange, doc_comments),
        } },
    });
}

/// EnumBlock <- LBRACE EnumField? (COMMA EnumField)* RBRACE
fn parseEnumBlock(p: *Parser) ParseError!NodeIndex {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const lbrace = try p.expectToken(.lbrace);
    _ = try p.expectToken(.new_line);

    var doc_start: ?usize = null;
    var has_comma = true; // First argument doesn't need a comma before it
    while (true) {
        const t = p.token_tags[p.index];
        if (t == .new_line) {
            p.index += 1;
            continue;
        }
        if (t == .comma) {
            p.index += 1;
            has_comma = true;
            continue;
        }
        if (t == .rbrace) {
            break;
        }
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .main_token = @enumFromInt(p.index),
                .data = undefined,
            })));
            p.index += 1;
            continue;
        }

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.errors.append(p.allocator, .{
                .tag = .expected_comma_after_arg,
                .token = @enumFromInt(p.index),
            });
        }
        has_comma = false;

        const doc_comments: Node.SubRange = get_comments: {
            if (doc_start) |start| {
                const comments = try p.writeExtraSubRange(p.scratch.items[start..]);
                p.scratch.shrinkRetainingCapacity(start);
                doc_start = null;
                break :get_comments comments;
            } else {
                break :get_comments .empty;
            }
        };

        const start_idx = p.index;

        // Enum Field
        const field = try p.parseEnumField(doc_comments);
        if (field != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(field));
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_enum_member);
    }

    _ = p.eatToken(.comma); // Optional trailing comma
    _ = try p.expectToken(.rbrace);

    return p.addNode(.{
        .tag = .enum_block,
        .main_token = lbrace,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
    });
}

/// EnumField <- IDENTIFIER EQUAL Expr
fn parseEnumField(p: *Parser, doc_comments: Node.SubRange) ParseError!NodeIndex {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = try p.expectToken(.equal);
    const expr_value = try p.parseExpr();

    return try p.addNode(.{
        .tag = .enum_field,
        .main_token = ident_name,
        .data = .{ .enum_field = .{
            .value = expr_value,
            .doc_comments = try p.writeExtraData(Node.SubRange, doc_comments),
        } },
    });
}

/// BankAttr <- IDENTIFIER LPAREN (INT_LITERAL) RPAREN
fn parseBankAttr(p: *Parser) ParseError!NodeIndex {
    const ident_bank = p.eatToken(.ident) orelse return .none;
    if (!std.mem.eql(u8, "bank", p.source[p.tokenLoc(ident_bank).start..p.tokenLoc(ident_bank).end]))
        return .none;

    _ = try p.expectToken(.lparen);
    const int_literal = try p.expectToken(.int_literal);
    _ = try p.expectToken(.rparen);

    return p.addNode(.{
        .tag = .bank_attr,
        .main_token = int_literal,
        .data = undefined,
    });
}
/// AccessAttr <- IDENTIFIER LPAREN (DOT_LITERAL) RPAREN
fn parseAccessAttr(p: *Parser) ParseError!NodeIndex {
    const ident_access = p.eatToken(.ident) orelse return .none;
    if (!std.mem.eql(u8, "access", p.source[p.tokenLoc(ident_access).start..p.tokenLoc(ident_access).end]))
        return .none;

    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.period);
    const int_literal = try p.expectToken(.ident);
    _ = try p.expectToken(.rparen);

    return p.addNode(.{
        .tag = .access_attr,
        .main_token = int_literal,
        .data = undefined,
    });
}

/// Block <- LBRACE NEW_LINE (NEW_LINE | Expr | Label)* RBRACE NEW_LINE
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

        // Asignment
        const assign = try p.parseAssignStatement();
        if (assign != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(assign));
            continue;
        }
        p.index = start_idx;

        // Call
        const call = try p.parseCallStatement();
        if (call != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(call));
            continue;
        }
        p.index = start_idx;

        // While
        const @"while" = try p.parseWhileStatement();
        if (@"while" != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(@"while"));
            continue;
        }
        p.index = start_idx;

        // Label
        const label = try p.parseLabel();
        if (label != .none) {
            try p.scratch.append(p.allocator, @intFromEnum(label));
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

/// Label <- IDENTIFIER COLON NEW_LINE
fn parseLabel(p: *Parser) ParseError!NodeIndex {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = p.eatToken(.colon) orelse return .none;
    _ = p.eatToken(.new_line) orelse return .none;

    return try p.addNode(.{
        .tag = .label,
        .main_token = ident_name,
        .data = undefined,
    });
}

/// FieldAccess <- ((IDENTIFIER | BUILTIN_IDENTIFIER) | (IDENTIFIER DOUBLE_COLON IDENTIFIER)) (PERIOD IDENTIFIER)*
fn parseFieldAccess(p: *Parser) ParseError!NodeIndex {
    const token_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(token_start);

    const ident_main = p.eatToken(.ident) orelse p.eatToken(.builtin_ident) orelse return .none;
    try p.scratch.append(p.allocator, @intFromEnum(ident_main));

    if (p.tokenTag(ident_main) == .ident and p.eatToken(.double_colon) != null) {
        _ = try p.expectToken(.ident);
    }

    while (p.eatToken(.period)) |_| {
        try p.scratch.append(p.allocator, @intFromEnum(try p.expectToken(.ident)));
    }

    return p.addNode(.{
        .tag = .field_access,
        .main_token = ident_main,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[token_start..]) },
    });
}

/// AssignStatement <- FieldAccess EQUAL Expr (COLON IDENTIFIER)? NEW_LINE
fn parseAssignStatement(p: *Parser) ParseError!NodeIndex {
    const field_access = try p.parseFieldAccess();
    if (field_access == .none) return .none;

    const equal = p.eatToken(.equal) orelse return .none;

    const expr_value = try p.parseExpr();
    if (expr_value == .none) {
        return p.fail(.expected_expr);
    }

    const ident_register = if (p.eatToken(.colon)) |_|
        try p.expectToken(.ident)
    else
        .none;

    _ = try p.expectToken(.new_line);

    return try p.addNode(.{
        .tag = .assign_statement,
        .main_token = equal,
        .data = .{ .assign_statement = .{
            .target = field_access,
            .extra = try p.writeExtraData(Node.AssignStatementData, .{
                .value = expr_value,
                .intermediate_register = ident_register,
            }),
        } },
    });
}

/// CallStatement <- (IDENTIFIER | BUILTIN_IDENTIFIER) LPAREN ExprList RPAREN NEW_LINE
///
/// ExprList <- (Expr COMMA)* Expr?
fn parseCallStatement(p: *Parser) ParseError!NodeIndex {
    const expr_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(expr_start);

    const ident_name = p.eatToken(.builtin_ident) orelse p.eatToken(.ident) orelse return .none;
    _ = p.eatToken(.lparen) orelse return .none;

    while (true) {
        if (p.eatToken(.rparen)) |_| break;

        const expr_param = try p.parseExpr();
        if (expr_param == .none) {
            return p.fail(.expected_expr);
        }

        try p.scratch.append(p.allocator, @intFromEnum(expr_param));

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
    const keyword_while = p.eatToken(.keyword_while) orelse return .none;
    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.keyword_true);
    _ = try p.expectToken(.rparen);

    const block = try p.parseBlock();

    return try p.addNode(.{
        .tag = .while_statement,
        .main_token = keyword_while,
        .data = .{ .while_statement = .{
            .condition = .none,
            .block = block,
        } },
    });
}

/// Expr <- (IDENTIFIER | INT_LITERAL | DOT_LITERAL | InitExpr)
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
            .tag = .expr_int_value,
            .main_token = literal,
            .data = undefined,
        });
    }
    if (p.eatTokens(&.{ .period, .ident })) |literal| {
        return try p.addNode(.{
            .tag = .expr_enum_value,
            .main_token = literal.next(),
            .data = undefined,
        });
    }

    const init_expr = try p.parseInitExpr();
    if (init_expr != .none) {
        return init_expr;
    }

    return .none;
}
/// InitExpr  <- PERIOD LBRACE NEW_LINE (InitField)* RBRACE
/// InitField <- PERIOD IDENTIFIER EQUAL Expr COMMA NEW_LINE
fn parseInitExpr(p: *Parser) ParseError!NodeIndex {
    const period = p.eatToken(.period) orelse return .none;
    _ = try p.expectToken(.lbrace);
    _ = try p.expectToken(.new_line);

    const fields_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(fields_start);

    while (true) {
        if (p.eatToken(.rbrace)) |_| break;

        _ = try p.expectToken(.period);
        const field_ident = try p.expectToken(.ident);

        _ = try p.expectToken(.equal);
        const value_expr = try p.parseExpr();
        if (value_expr == .none) {
            return p.fail(.expected_expr);
        }
        _ = p.eatToken(.comma) orelse {
            // Likely just a missing comma
            try p.warn(.expected_comma_after_arg);
        };
        _ = try p.expectToken(.new_line);

        try p.scratch.append(p.allocator, @intFromEnum(try p.addNode(.{
            .tag = .expr_init_field,
            .main_token = field_ident,
            .data = .{ .expr_init_field = .{
                .value = value_expr,
            } },
        })));
    }

    return p.addNode(.{
        .tag = .expr_init,
        .main_token = period,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[fields_start..]) },
    });
}

/// TypeExpr <- AnonPackedDef | AnonEnumDef | IDENTIFIER
fn parseTypeExpr(p: *Parser) ParseError!NodeIndex {
    if (p.token_tags[p.index] == .keyword_packed) {
        return p.parsePackedDef(.empty, .anonymous);
    }
    if (p.token_tags[p.index] == .keyword_enum) {
        return p.parseEnumDef(.empty, .anonymous);
    }

    if (p.eatToken(.ident)) |ident| {
        return try p.addNode(.{
            .tag = .type_ident,
            .main_token = ident,
            .data = undefined,
        });
    }

    return .none;
}

// Helper functions

fn addNode(p: *Parser, node: Node) ParseError!NodeIndex {
    try p.nodes.append(p.allocator, node);
    return @enumFromInt(p.nodes.len - 1);
}

/// Stores a list of sub-nodes into `extra_data`
fn writeExtraSubRange(p: *Parser, list: []const Ast.CommonIndex) ParseError!Node.SubRange {
    const start: ExtraIndex = @enumFromInt(p.extra_data.items.len);
    try p.extra_data.appendSlice(p.allocator, list);
    const end: ExtraIndex = @enumFromInt(p.extra_data.items.len);

    return .{ .extra_start = start, .extra_end = end };
}
/// Stores a custom data-type into `extra_data`
fn writeExtraData(p: *Parser, comptime T: type, value: T) !ExtraIndex {
    const fields = std.meta.fields(T);

    const extra_idx = p.extra_data.items.len;
    try p.extra_data.ensureUnusedCapacity(p.allocator, fields.len);
    p.extra_data.items.len += fields.len;

    inline for (fields, 0..) |field, i| {
        p.extra_data.items[extra_idx + i] = @intFromEnum(@field(value, field.name));
    }

    return @enumFromInt(@as(std.meta.Tag(ExtraIndex), @intCast(extra_idx)));
}

fn skipNewLines(p: *Parser) void {
    while (p.token_tags[p.index] == .new_line) {
        p.index += 1;
    }
}

fn nextToken(p: *Parser) TokenIndex {
    const result = p.index;
    p.index += 1;
    return @enumFromInt(result);
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
fn eatTokens(p: *Parser, tags: []const Token.Tag) ?TokenIndex {
    if (p.index + tags.len <= p.token_tags.len and std.mem.eql(Token.Tag, p.token_tags[p.index .. p.index + tags.len], tags)) {
        const result = p.index;
        p.index += @intCast(tags.len);
        return @enumFromInt(result);
    }

    return null;
}
fn expectToken(p: *Parser, tag: Token.Tag) ParseError!TokenIndex {
    if (p.token_tags[p.index] != tag) {
        try p.errors.append(p.allocator, .{
            .tag = .expected_token,
            .token = @enumFromInt(p.index),
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
        .token = @enumFromInt(p.index),
    });
    return error.ParseFailed;
}

/// Reports an error, while continuing with parsing
fn warn(p: *Parser, tag: Error.Tag) !void {
    try p.errors.append(p.allocator, .{
        .tag = tag,
        .token = @enumFromInt(p.index),
    });
}

/// Attaches a note to the previous error
fn note(p: *Parser, tag: Error.Tag) !void {
    return p.errMsg(.{
        .tag = tag,
        .token = @enumFromInt(p.index),
        .is_note = true,
    });
}

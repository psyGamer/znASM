const std = @import("std");

const Ast = @import("../Ast.zig");
const Token = Ast.Token;
const Node = Ast.Node;
const ExtraIndex = Ast.ExtraIndex;
const ParseError = Ast.ParseError;

const ErrorSystem = @import("../error.zig").ErrorSystem;

const Parser = @This();

ast: *const Ast,
index: Token.Index = @enumFromInt(0),

/// Generated nodes
nodes: std.MultiArrayList(Node) = .empty,
/// Additional data for specific nodes
extra_data: std.ArrayListUnmanaged(Ast.CommonIndex) = .empty,

/// Temporarily store range data for `extra_data`
scratch: std.ArrayListUnmanaged(Ast.CommonIndex) = .empty,

state_stack: std.ArrayListUnmanaged(struct {
    index: std.meta.Tag(Token.Index),
    nodes: usize,
}) = .empty,

/// Whether any errors were encountered during parsing
has_errors: bool = false,

data_allocator: std.mem.Allocator,
temp_allocator: std.mem.Allocator,

inline fn advanceIndex(p: *Parser, offset: std.meta.Tag(Token.Index)) void {
    p.index = @enumFromInt(@intFromEnum(p.index) + offset);
}

pub inline fn tokenTag(p: Parser, index: Token.Index) Token.Tag {
    return index.getTag(p.ast);
}
pub inline fn tokenLoc(p: Parser, index: Token.Index) Token.Loc {
    return index.getLocation(p.ast);
}

// Grammar parsing

/// Root <- ModuleDef (FnDef | ConstDef | VarDef | EnumDef)*
pub fn parseRoot(p: *Parser) ParseError!void {
    const tld_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(tld_start);

    const root = try p.addNode(.{
        .tag = .root,
        .data = undefined,
        .main_token = undefined,
    });

    try p.scratch.append(p.temp_allocator, @intFromEnum(try p.parseModuleDef()));

    var doc_start: ?usize = null;
    while (true) {
        const t = p.index.getTag(p.ast);
        std.log.info("t {}", .{t});
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.temp_allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .data = undefined,
                .main_token = p.index,
            })));
            p.advanceIndex(1);
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
            try p.scratch.append(p.temp_allocator, @intFromEnum(fn_def));
            continue;
        }

        // Constant Definition
        const const_def = try p.parseConstDef(doc_comments);
        if (const_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(const_def));
            continue;
        }

        // Variable Definition
        const var_def = try p.parseVarDef(doc_comments);
        if (var_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(var_def));
            continue;
        }

        // Register Definition
        const reg_def = try p.parseRegDef(doc_comments);
        if (reg_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(reg_def));
            continue;
        }

        // Struct Definition
        const struct_def = try p.parseStructDef(doc_comments, .top_level);
        if (struct_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(struct_def));
            continue;
        }

        // Packed Definition
        const packed_def = try p.parsePackedDef(doc_comments, .top_level);
        if (packed_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(packed_def));
            continue;
        }

        // Enum Definition
        const enum_def = try p.parseEnumDef(doc_comments, .top_level);
        if (enum_def != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(enum_def));
            continue;
        }

        return p.fail(.expected_toplevel);
    }

    p.nodes.items(.data)[@intFromEnum(root)] = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[tld_start..]) };
}

/// ModuleDef <- KEYWORD_module IDENTIFIER SEMICOLON
fn parseModuleDef(p: *Parser) ParseError!Node.Index {
    _ = try p.expectToken(.keyword_module);
    const ident = try p.expectToken(.ident);
    _ = try p.expectToken(.semicolon);

    // TODO: Verify module name is legal
    return try p.addNode(.{
        .tag = .module,
        .data = undefined,
        .main_token = ident,
    });
}

/// FnDef <- (KEYWORD_pub)? KEYWORD_fn (IDENTIFIER | BUILTIN_IDENTIFIER) LPAREN RPAREN (BankAttr)? (IDENTIFIER)? Block
fn parseFnDef(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    _ = p.eatToken(.keyword_pub);
    const keyword_fn = p.eatToken(.keyword_fn) orelse return .none;
    _ = p.eatToken(.builtin_ident) orelse try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.rparen);
    const bank_attr = try p.parseBankAttr();
    _ = p.eatToken(.ident);

    const block = try p.parseBlock();
    if (block == .none) {
        return p.fail(.expected_fn_block);
    }

    return try p.addNode(.{
        .tag = .fn_def,
        .data = .{ .fn_def = .{
            .block = block,
            .extra = try p.writeExtraData(Ast.Node.FnDefData, .{
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_fn,
    });
}

/// ConstDef <- (KEYWORD_pub)? KEYWORD_const IDENTIFIER COLON TypeExpr (BankAttr)? EQUAL Expr SEMICOLON
fn parseConstDef(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    _ = p.eatToken(.keyword_pub);
    const keyword_const = p.eatToken(.keyword_const) orelse return .none;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const bank_attr = try p.parseBankAttr();
    _ = try p.expectToken(.equal);
    const value_expr = try p.parseExpr(false);
    if (value_expr == .none) {
        return p.fail(.expected_expr);
    }
    _ = try p.expectToken(.semicolon);

    return try p.addNode(.{
        .tag = .const_def,
        .data = .{ .const_def = .{
            .value = value_expr,
            .extra = try p.writeExtraData(Ast.Node.ConstDefData, .{
                .type = type_expr,
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_const,
    });
}

/// VarDef <- (KEYWORD_pub)? KEYWORD_var IDENTIFIER COLON TypeExpr (BankAttr)? SEMICOLON
fn parseVarDef(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    _ = p.eatToken(.keyword_pub);
    const keyword_var = p.eatToken(.keyword_var) orelse return .none;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const bank_attr = try p.parseBankAttr();
    _ = try p.expectToken(.semicolon);

    return try p.addNode(.{
        .tag = .var_def,
        .data = .{ .var_def = .{
            .extra = try p.writeExtraData(Ast.Node.VarDefData, .{
                .type = type_expr,
                .bank_attr = bank_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_var,
    });
}

/// RegDef <- (KEYWORD_pub)? KEYWORD_reg IDENTIFIER COLON TypeExpr AccessAttr EQUAL INT_LITERAL SEMICOLON
fn parseRegDef(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    _ = p.eatToken(.keyword_pub);
    const keyword_reg = p.eatToken(.keyword_reg) orelse return .none;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const access_attr = try p.parseAccessAttr();
    _ = try p.expectToken(.equal);
    const ident_address = try p.expectToken(.int_literal);
    _ = try p.expectToken(.semicolon);

    return try p.addNode(.{
        .tag = .reg_def,
        .data = .{ .reg_def = .{
            .extra = try p.writeExtraData(Ast.Node.RegDefData, .{
                .type = type_expr,
                .address = ident_address,
                .access_attr = access_attr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_reg,
    });
}

const DeclType = enum { top_level, anonymous };

///     StructDef <- (KEYWORD_pub)? KEYWORD_struct IDENTIFIER StructBlock
/// AnonStructDef <- KEYWORD_struct StructBlock
fn parseStructDef(p: *Parser, doc_comments: Node.SubRange, decl_type: DeclType) ParseError!Node.Index {
    if (decl_type == .top_level) _ = p.eatToken(.keyword_pub);
    const keyword_struct = p.eatToken(.keyword_struct) orelse return .none;
    if (decl_type == .top_level) _ = try p.expectToken(.ident);
    const struct_block = try p.parseStructBlock();

    return try p.addNode(.{
        .tag = .struct_def,
        .data = .{ .struct_def = .{
            .block = struct_block,
            .doc_comments = try p.writeExtraData(Node.SubRange, doc_comments),
        } },
        .main_token = keyword_struct,
    });
}

///     PackedDef <- (KEYWORD_pub)? KEYWORD_packed IDENTIFIER LPAREN TypeExpr RPAREN StructBlock
/// AnonPackedDef <- KEYWORD_packed LPAREN TypeExpr RPAREN StructBlock
fn parsePackedDef(p: *Parser, doc_comments: Node.SubRange, decl_type: DeclType) ParseError!Node.Index {
    if (decl_type == .top_level) _ = p.eatToken(.keyword_pub);
    const keyword_packed = p.eatToken(.keyword_packed) orelse return .none;
    if (decl_type == .top_level) _ = try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    const type_expr = try p.parseTypeExpr();
    _ = try p.expectToken(.rparen);
    const struct_block = try p.parseStructBlock();

    return try p.addNode(.{
        .tag = .packed_def,
        .data = .{ .packed_def = .{
            .block = struct_block,
            .extra = try p.writeExtraData(Ast.Node.PackedDefData, .{
                .backing_type = type_expr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_packed,
    });
}

/// EmumDef     <- (KEYWORD_pub)? KEYWORD_enum IDENTIFIER LPAREN TypeExpr RPAREN EnumBlock
/// AnonEmumDef <- KEYWORD_enum LPAREN TypeExpr RPAREN EnumBlock
fn parseEnumDef(p: *Parser, doc_comments: Node.SubRange, decl_type: DeclType) ParseError!Node.Index {
    if (decl_type == .top_level) _ = p.eatToken(.keyword_pub);
    const keyword_enum = p.eatToken(.keyword_enum) orelse return .none;
    if (decl_type == .top_level) _ = try p.expectToken(.ident);
    _ = try p.expectToken(.lparen);
    const type_expr = try p.parseTypeExpr();
    _ = try p.expectToken(.rparen);
    const enum_block = try p.parseEnumBlock();

    return try p.addNode(.{
        .tag = .enum_def,
        .data = .{ .enum_def = .{
            .block = enum_block,
            .extra = try p.writeExtraData(Ast.Node.EnumDefData, .{
                .backing_type = type_expr,
                .doc_comment_start = doc_comments.extra_start,
                .doc_comment_end = doc_comments.extra_end,
            }),
        } },
        .main_token = keyword_enum,
    });
}

/// StructBlock <- LBRACE StructField? (COMMA StructField)* RBRACE
fn parseStructBlock(p: *Parser) ParseError!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const lbrace = try p.expectToken(.lbrace);

    var doc_start: ?usize = null;
    var has_comma = true; // First argument doesn't need a comma before it
    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .comma and !has_comma) {
            p.advanceIndex(1);
            has_comma = true;
            continue;
        }
        if (t == .rbrace) {
            break;
        }
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.temp_allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .data = undefined,
                .main_token = p.index,
            })));
            p.advanceIndex(1);
            continue;
        }

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.warn(.expected_comma_after_arg);
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

        // Struct Field
        const field = try p.parseStructField(doc_comments);
        if (field != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(field));
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_enum_member);
    }
    _ = try p.expectToken(.rbrace);

    return p.addNode(.{
        .tag = .struct_block,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
        .main_token = lbrace,
    });
}

/// StructField <- IDENTIFIER COLON TypeExpr (EQUAL Expr)?
fn parseStructField(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();

    const value_expr = if (p.eatToken(.equal)) |_|
        try p.parseExpr(false)
    else
        .none;

    return try p.addNode(.{
        .tag = .struct_field,
        .data = .{ .struct_field = .{
            .extra = try p.writeExtraData(Node.StructFieldData, .{
                .type = type_expr,
                .value = value_expr,
            }),
            .doc_comments = try p.writeExtraData(Node.SubRange, doc_comments),
        } },
        .main_token = ident_name,
    });
}

/// EnumBlock <- LBRACE EnumField? (COMMA EnumField)* RBRACE
fn parseEnumBlock(p: *Parser) ParseError!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const lbrace = try p.expectToken(.lbrace);

    var doc_start: ?usize = null;
    var has_comma = true; // First argument doesn't need a comma before it
    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .comma and !has_comma) {
            p.advanceIndex(1);
            has_comma = true;
            continue;
        }
        if (t == .rbrace) {
            break;
        }
        if (t == .doc_comment) {
            doc_start = doc_start orelse p.scratch.items.len;
            try p.scratch.append(p.temp_allocator, @intFromEnum(try p.addNode(.{
                .tag = .doc_comment,
                .data = undefined,
                .main_token = p.index,
            })));
            p.advanceIndex(1);
            continue;
        }

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.warn(.expected_comma_after_arg);
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
            try p.scratch.append(p.temp_allocator, @intFromEnum(field));
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_enum_member);
    }

    _ = p.eatToken(.comma); // Optional trailing comma
    _ = try p.expectToken(.rbrace);

    return p.addNode(.{
        .tag = .enum_block,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
        .main_token = lbrace,
    });
}

/// EnumField <- IDENTIFIER EQUAL Expr
fn parseEnumField(p: *Parser, doc_comments: Node.SubRange) ParseError!Node.Index {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = try p.expectToken(.equal);
    const expr_value = try p.parseExpr(false);

    return try p.addNode(.{
        .tag = .enum_field,
        .data = .{ .enum_field = .{
            .value = expr_value,
            .doc_comments = try p.writeExtraData(Node.SubRange, doc_comments),
        } },
        .main_token = ident_name,
    });
}

/// BankAttr <- IDENTIFIER LPAREN (INT_LITERAL) RPAREN
fn parseBankAttr(p: *Parser) ParseError!Node.Index {
    const ident_bank = p.eatToken(.ident) orelse return .none;
    if (!std.mem.eql(u8, "bank", p.ast.source[p.tokenLoc(ident_bank).start..p.tokenLoc(ident_bank).end]))
        return .none;

    _ = try p.expectToken(.lparen);
    const int_literal = try p.expectToken(.int_literal);
    _ = try p.expectToken(.rparen);

    return p.addNode(.{
        .tag = .bank_attr,
        .data = undefined,
        .main_token = int_literal,
    });
}
/// AccessAttr <- IDENTIFIER LPAREN (DOT_LITERAL) RPAREN
fn parseAccessAttr(p: *Parser) ParseError!Node.Index {
    const ident_access = p.eatToken(.ident) orelse return .none;
    if (!std.mem.eql(u8, "access", p.ast.source[p.tokenLoc(ident_access).start..p.tokenLoc(ident_access).end]))
        return .none;

    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.period);
    const int_literal = try p.expectToken(.ident);
    _ = try p.expectToken(.rparen);

    return p.addNode(.{
        .tag = .access_attr,
        .data = undefined,
        .main_token = int_literal,
    });
}

/// Block <- LBRACE (Block | AssignStatement | CallStatement | WhileStatement | LocalVarDecl | Label)* RBRACE
fn parseBlock(p: *Parser) ParseError!Node.Index {
    const lbrace = p.eatToken(.lbrace) orelse return .none;

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .rbrace) {
            break;
        }

        const start_idx = p.index;

        // Block
        const block = try p.parseBlock();
        if (block != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(block));
            continue;
        }
        p.index = start_idx;

        // Asignment
        const assign = try p.parseAssignStatement();
        if (assign != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(assign));
            continue;
        }
        p.index = start_idx;

        // Call
        const call = try p.parseCallStatement();
        if (call != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(call));
            continue;
        }
        p.index = start_idx;

        // While
        const @"while" = try p.parseWhileStatement();
        if (@"while" != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(@"while"));
            continue;
        }
        p.index = start_idx;

        // Local Variable
        const local_var = try p.parseLocalVarDecl();
        if (local_var != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(local_var));
            continue;
        }
        p.index = start_idx;

        // Label
        const label = try p.parseLabel();
        if (label != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(label));
            continue;
        }
        p.index = start_idx;

        return p.fail(.expected_statement);
    }
    _ = try p.expectToken(.rbrace);

    return p.addNode(.{
        .tag = .block,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[scratch_top..]) },
        .main_token = lbrace,
    });
}

/// Label <- IDENTIFIER COLON
fn parseLabel(p: *Parser) ParseError!Node.Index {
    const ident_name = p.eatToken(.ident) orelse return .none;
    _ = p.eatToken(.colon) orelse return .none;

    return try p.addNode(.{
        .tag = .label,
        .data = undefined,
        .main_token = ident_name,
    });
}

/// FieldAccess <- ((IDENTIFIER | BUILTIN_IDENTIFIER) | (IDENTIFIER DOUBLE_COLON IDENTIFIER)) (PERIOD IDENTIFIER)*
fn parseFieldAccess(p: *Parser) ParseError!Node.Index {
    const token_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(token_start);

    const ident_main = p.eatToken(.ident) orelse p.eatToken(.builtin_ident) orelse return .none;
    try p.scratch.append(p.temp_allocator, @intFromEnum(ident_main));

    if (p.tokenTag(ident_main) == .ident and p.eatToken(.double_colon) != null) {
        _ = try p.expectToken(.ident);
    }

    while (p.eatToken(.period)) |_| {
        try p.scratch.append(p.temp_allocator, @intFromEnum(try p.expectToken(.ident)));
    }

    return p.addNode(.{
        .tag = .field_access,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[token_start..]) },
        .main_token = ident_main,
    });
}

/// AssignStatement <- FieldAccess EQUAL Expr SEMICOLON
fn parseAssignStatement(p: *Parser) ParseError!Node.Index {
    const field_access = try p.parseFieldAccess();
    if (field_access == .none) return .none;

    const equal = p.eatToken(.equal) orelse return .none;

    const expr_value = try p.parseExpr(true);
    if (expr_value == .none) {
        return p.fail(.expected_expr);
    }

    _ = try p.expectToken(.semicolon);

    return try p.addNode(.{
        .tag = .assign_statement,
        .data = .{ .assign_statement = .{
            .target = field_access,
            .value = expr_value,
        } },
        .main_token = equal,
    });
}

/// CallStatement <- (IDENTIFIER | BUILTIN_IDENTIFIER) LPAREN ExprList RPAREN SEMICOLON
///
/// ExprList <- Expr? (COMMA Expr)*
fn parseCallStatement(p: *Parser) ParseError!Node.Index {
    const expr_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(expr_start);

    const ident_name = p.eatToken(.builtin_ident) orelse p.eatToken(.ident) orelse return .none;
    _ = p.eatToken(.lparen) orelse return .none;

    var has_comma = true; // First argument doesn't need a comma before it
    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .comma and !has_comma) {
            p.advanceIndex(1);
            has_comma = true;
            continue;
        }
        if (t == .rbrace) {
            break;
        }

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.warn(.expected_comma_after_arg);
        }
        has_comma = false;

        const expr_param = try p.parseExpr(true);
        if (expr_param != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(expr_param));
            continue;
        }

        return p.fail(.expected_expr);
    }
    _ = try p.expectToken(.semicolon);

    return p.addNode(.{
        .tag = .call_statement,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[expr_start..]) },
        .main_token = ident_name,
    });
}

/// WhiteStatement <- KEYWORD_while LPAREN KEYWORD_true RPAREN Block
fn parseWhileStatement(p: *Parser) ParseError!Node.Index {
    const keyword_while = p.eatToken(.keyword_while) orelse return .none;
    _ = try p.expectToken(.lparen);
    _ = try p.expectToken(.keyword_true);
    _ = try p.expectToken(.rparen);

    const block = try p.parseBlock();

    return try p.addNode(.{
        .tag = .while_statement,
        .data = .{ .while_statement = .{
            .condition = .none,
            .block = block,
        } },
        .main_token = keyword_while,
    });
}

/// LocalVarDecl <- (KEYWORD_var | KEYWORD_const) IDENTIFIER COLON TypeExpr Attr("location") EQUAL Expr SEMICOLON
fn parseLocalVarDecl(p: *Parser) ParseError!Node.Index {
    _ = p.eatToken(.keyword_var) orelse p.eatToken(.keyword_const) orelse return .none;
    const ident_name = try p.expectToken(.ident);
    _ = try p.expectToken(.colon);
    const type_expr = try p.parseTypeExpr();
    const location_attr = try p.parseAttr("location", 1);
    _ = try p.expectToken(.equal);
    const value_expr = try p.parseExpr(true);
    _ = try p.expectToken(.semicolon);

    return try p.addNode(.{
        .tag = .local_var_decl,
        .data = .{ .local_var_decl = .{
            .value = value_expr,
            .extra = try p.writeExtraData(Node.LocalVarDeclData, .{
                .type = type_expr,
                .location_attr = location_attr,
            }),
        } },
        .main_token = ident_name,
    });
}

/// Expr <- (IDENTIFIER | INT_LITERAL | (PERIOD IDENTIFIER) | InitExpr) (COLON IDENTIFIER)?
fn parseExpr(p: *Parser, allow_register: bool) ParseError!Node.Index {
    if (p.eatToken(.ident)) |ident| {
        const ident_register = if (allow_register and p.eatToken(.colon) != null)
            try p.expectToken(.ident)
        else
            .none;

        return try p.addNode(.{
            .tag = .expr_ident,
            .data = .{ .expr = .{ .intermediate_register = ident_register } },
            .main_token = ident,
        });
    }
    if (p.eatToken(.int_literal)) |literal| {
        const ident_register = if (allow_register and p.eatToken(.colon) != null)
            try p.expectToken(.ident)
        else
            .none;

        return try p.addNode(.{
            .tag = .expr_int_value,
            .data = .{ .expr = .{ .intermediate_register = ident_register } },
            .main_token = literal,
        });
    }
    if (p.eatTokens(&.{ .period, .ident })) |literal| {
        const ident_register = if (allow_register and p.eatToken(.colon) != null)
            try p.expectToken(.ident)
        else
            .none;

        return try p.addNode(.{
            .tag = .expr_enum_value,
            .data = .{ .expr = .{ .intermediate_register = ident_register } },
            .main_token = literal.next(),
        });
    }

    const init_expr = try p.parseInitExpr(allow_register);
    if (init_expr != .none) {
        return init_expr;
    }

    return .none;
}
/// InitExpr  <- PERIOD LBRACE InitField? (COMMA InitField)* RBRACE
/// InitField <- PERIOD IDENTIFIER EQUAL Expr
fn parseInitExpr(p: *Parser, allow_register: bool) ParseError!Node.Index {
    const period = p.eatToken(.period) orelse return .none;
    _ = try p.expectToken(.lbrace);

    const fields_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(fields_start);

    var has_comma = true; // First argument doesn't need a comma before it
    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .comma and !has_comma) {
            p.advanceIndex(1);
            has_comma = true;
            continue;
        }
        if (t == .rbrace) {
            break;
        }

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.warn(.expected_comma_after_arg);
        }
        has_comma = false;

        _ = try p.expectToken(.period);
        const field_ident = try p.expectToken(.ident);

        _ = try p.expectToken(.equal);

        const value_expr = try p.parseExpr(false);
        if (value_expr != .none) {
            try p.scratch.append(p.temp_allocator, @intFromEnum(try p.addNode(.{
                .tag = .expr_init_field,
                .data = .{ .expr_init_field = .{
                    .value = value_expr,
                } },
                .main_token = field_ident,
            })));
            continue;
        }

        return p.fail(.expected_expr);
    }
    _ = try p.expectToken(.rbrace);

    const ident_register = if (allow_register and p.eatToken(.colon) != null)
        try p.expectToken(.ident)
    else
        .none;

    return p.addNode(.{
        .tag = .expr_init,
        .data = .{ .expr_init = .{
            .extra = try p.writeExtraData(Node.SubRange, try p.writeExtraSubRange(p.scratch.items[fields_start..])),
            .intermediate_register = ident_register,
        } },
        .main_token = period,
    });
}

/// TypeExpr <- AnonPackedDef | AnonEnumDef | IDENTIFIER
fn parseTypeExpr(p: *Parser) ParseError!Node.Index {
    if (p.index.getTag(p.ast) == .keyword_struct) {
        return p.parseStructDef(.empty, .anonymous);
    }
    if (p.index.getTag(p.ast) == .keyword_packed) {
        return p.parsePackedDef(.empty, .anonymous);
    }
    if (p.index.getTag(p.ast) == .keyword_enum) {
        return p.parseEnumDef(.empty, .anonymous);
    }

    if (p.eatToken(.ident)) |ident| {
        return try p.addNode(.{
            .tag = .type_ident,
            .data = undefined,
            .main_token = ident,
        });
    }

    return .none;
}

/// Attr(name) <- IDENTIFIER LPAREN ExprList RPAREN
///
/// ExprList <- Expr? (COMMA Expr)*
fn parseAttr(p: *Parser, attr_name: []const u8, argument_count: ?u32) ParseError!Node.Index {
    const ident_attr = p.eatToken(.ident) orelse return .none;
    if (!std.mem.eql(u8, attr_name, p.ast.source[p.tokenLoc(ident_attr).start..p.tokenLoc(ident_attr).end]))
        return .none;

    _ = try p.expectToken(.lparen);

    const args_start = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(args_start);

    var has_comma = true; // First argument doesn't need a comma before it
    var arg_idx: u32 = 0;

    while (true) {
        const t = p.index.getTag(p.ast);
        if (t == .comma and !has_comma) {
            p.advanceIndex(1);
            has_comma = true;
            continue;
        }
        if (t == .rparen) {
            break;
        }

        arg_idx += 1;

        if (!has_comma) {
            // Most likely just missing. Can continue parsing
            try p.warn(.expected_comma_after_arg);
        }
        has_comma = false;

        const expr = try p.parseExpr(false);
        if (expr == .none) {
            return p.fail(.expected_expr);
        }

        try p.scratch.append(p.temp_allocator, @intFromEnum(expr));
    }
    _ = try p.expectToken(.rparen);

    if (argument_count) |arg_count| {
        if (arg_idx > arg_count) {
            const err_ctx = try Error.begin(p.ast, p.index, .err);
            try err_ctx.print("Expected [!]{d} arguments[], found [!]{d}", .{ arg_count, arg_idx });
            try err_ctx.end();

            return error.ParseFailed;
        }

        // Avoid `sub_range` with `attr_two` node if possible
        return try p.addNode(switch (arg_count) {
            0 => .{
                .tag = .attr_two,
                .data = .{ .attr_two = .{
                    .expr_one = undefined,
                    .expr_two = undefined,
                } },
                .main_token = ident_attr,
            },
            1 => .{
                .tag = .attr_two,
                .data = .{ .attr_two = .{
                    .expr_one = .cast(p.scratch.items[args_start]),
                    .expr_two = undefined,
                } },
                .main_token = ident_attr,
            },
            2 => .{
                .tag = .attr_two,
                .data = .{ .attr_two = .{
                    .expr_one = .cast(p.scratch.items[args_start]),
                    .expr_two = .cast(p.scratch.items[args_start + 1]),
                } },
                .main_token = ident_attr,
            },
            else => .{
                .tag = .attr_multi,
                .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[args_start..]) },
                .main_token = ident_attr,
            },
        });
    }

    return p.addNode(.{
        .tag = .attr_multi,
        .data = .{ .sub_range = try p.writeExtraSubRange(p.scratch.items[args_start..]) },
        .main_token = ident_attr,
    });
}

// Helper functions

fn addNode(p: *Parser, node: Node) ParseError!Node.Index {
    const index: Node.Index = .cast(p.nodes.len);
    try p.nodes.append(p.data_allocator, node);
    return index;
}

/// Stores a list of sub-nodes into `extra_data`
fn writeExtraSubRange(p: *Parser, list: []const Ast.CommonIndex) ParseError!Node.SubRange {
    const start: ExtraIndex = @enumFromInt(p.extra_data.items.len);
    try p.extra_data.appendSlice(p.data_allocator, list);
    const end: ExtraIndex = @enumFromInt(p.extra_data.items.len);

    return .{ .extra_start = start, .extra_end = end };
}
/// Stores a custom data-type into `extra_data`
fn writeExtraData(p: *Parser, comptime T: type, value: T) !ExtraIndex {
    const fields = std.meta.fields(T);

    const extra_idx = p.extra_data.items.len;
    try p.extra_data.ensureUnusedCapacity(p.data_allocator, fields.len);
    p.extra_data.items.len += fields.len;

    inline for (fields, 0..) |field, i| {
        p.extra_data.items[extra_idx + i] = @intFromEnum(@field(value, field.name));
    }

    return @enumFromInt(@as(std.meta.Tag(ExtraIndex), @intCast(extra_idx)));
}

fn nextToken(p: *Parser) Token.Index {
    const index = p.index;
    p.advanceIndex(1);
    return index;
}
fn eatToken(p: *Parser, tag: Token.Tag) ?Token.Index {
    return if (p.index.getTag(p.ast) == tag)
        p.nextToken()
    else
        null;
}
fn eatTokens(p: *Parser, tags: []const Token.Tag) ?Token.Index {
    if (@intFromEnum(p.index) + tags.len <= p.ast.token_tags.len and std.mem.eql(Token.Tag, p.ast.token_tags[@intFromEnum(p.index)..(@intFromEnum(p.index) + tags.len)], tags)) {
        const index = p.index;
        p.advanceIndex(@intCast(tags.len));
        return index;
    }

    return null;
}
fn expectToken(p: *Parser, tag: Token.Tag) ParseError!Token.Index {
    if (p.index.getTag(p.ast) != tag) {
        const err_ctx = try Error.begin(p.ast, p.index, .err);
        try err_ctx.print("Expected [!]{s}[], found [!]{s}", .{ tag.symbol(), p.index.getTag(p.ast).symbol() });
        try err_ctx.end();

        return error.ParseFailed;
    }
    return p.nextToken();
}

// Error handling

const Error = ErrorSystem(.{
    .expected_toplevel = "Expected [!]a top-level definition[reset], found [!]{s}",
    .expected_statement = "Expected [!]a statement[reset], found [!]{s}",
    .expected_expr = "Expected [!]an expression[reset], found [!]{s}",
    .expected_comma_after_arg = "Expected [!]a comma[reset], after an argument, found [!]{s}",
    .expected_enum_member = "Expected [!]an enum member[reset], inside [!]enum block[reset], found [!]{s}",
    .expected_fn_block = "Expected [!]a function block[reset], found [!]{s}",
});

/// Reports a fatal error, which doesn't allow for further parsing
fn fail(p: *Parser, comptime tag: Error.Tag) ParseError {
    const err_ctx = try Error.begin(p.ast, p.index, .err);
    try err_ctx.print(comptime Error.tagMessage(tag), .{p.index.getTag(p.ast).symbol()});
    try err_ctx.end();

    return error.ParseFailed;
}

/// Reports an error, while continuing with parsing
fn warn(p: *Parser, comptime tag: Error.Tag) !void {
    const err_ctx = try Error.begin(p.ast, p.index, .err);
    try err_ctx.print(comptime Error.tagMessage(tag), .{p.index.getTag(p.ast).symbol()});
    try err_ctx.end();
}

/// Attaches a note to the previous error
fn note(p: *Parser, comptime tag: Error.Tag) !void {
    const err_ctx = try Error.begin(p.ast, p.index, .note);
    try err_ctx.print(comptime Error.tagMessage(tag), .{p.index.getTag(p.ast).symbol()});
    try err_ctx.end();
}

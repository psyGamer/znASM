//! Analyzes AST nodes into a flat Semantic IR for a function
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

const Module = @import("../Module.zig");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Sema = @import("../Sema.zig");
const Error = Sema.AnalyzeError;
const Symbol = Sema.Symbol;
const Expression = Sema.Expression;
const TypeExpression = Sema.TypeExpression;
const BuiltinVar = @import("BuiltinVar.zig");
const BuiltinFn = @import("BuiltinFn.zig");

const SemanticIr = @import("SemanticIr.zig");
const Analyzer = @This();

sema: *Sema,

symbol: Symbol.Index,
module: Module.Index,

graph: SemanticIr.NodeGraph = .empty,

// Helper functions

pub inline fn getAst(ana: Analyzer) *Ast {
    return &ana.module.get(ana.sema).ast;
}

pub inline fn create(ana: *Analyzer, node: SemanticIr) !SemanticIr.Index {
    return ana.graph.create(ana.sema.allocator, node);
}
pub inline fn emit(ana: *Analyzer, tag: SemanticIr.Data, node: Node.Index) !void {
    try ana.graph.append(ana.sema.allocator, .{ .tag = tag, .node = node });
}

pub fn deinit(ana: *Analyzer) void {
    ana.graph.deinit(ana.sema.allocator);
}

pub fn process(ana: *Analyzer) Error!void {
    const node = ana.symbol.getCommon(ana.sema).node;
    const fn_def = ana.getAst().nodeData(node).fn_def;

    try ana.handleBlock(fn_def.block);

    // if (ana.ir.items.len != 0 and ana.ir.getLast().tag == .@"return") {
    //     return; // Already explicitly returned
    // }

    var queue: std.ArrayListUnmanaged(SemanticIr.Index) = .empty;
    defer queue.deinit(ana.sema.allocator);

    // TODO: Handle return values
    const ret_node = try ana.create(.{ .data = .@"return", .source = node });

    ana.graph.refresh();
    for (ana.graph.slice.tags, 0..) |tag, idx| {
        if (tag == .store_symbol) {
            try ret_node.addDependency(ana.sema.allocator, &ana.graph, @enumFromInt(@as(std.meta.Tag(SemanticIr.Index), @intCast(idx))), .both);
        }
    }
}

/// Searches for the Semantic IR index of the `declare_variable` for the target local variable
fn findLocalVariable(ana: *const Analyzer, name: []const u8) ?SemanticIr.Index {
    const ast = ana.getAst();

    // Find IR index
    var iter: ScopeIterator = .init(ana);
    while (iter.next()) |ir| {
        if (ir.tag == .declare_variable) {
            const var_name = ast.parseIdentifier(ast.nodeToken(ir.source));
            if (std.mem.eql(u8, var_name, name)) {
                return @enumFromInt(iter.index);
            }
        }
    }

    // Not found
    return null;
}

/// Parses field accessors of a target and emits appropriate `field_reference` instructions
fn parseFieldTarget(ana: *Analyzer, fields: []const Ast.CommonIndex, root_type: TypeExpression.Index, node: Node.Index) Error!void {
    const ast = ana.getAst();

    var current_type: TypeExpression.Index = root_type;

    for (fields) |field_idx| {
        const field_ident: Ast.TokenIndex = .cast(ast.extra_data[field_idx]);
        const field_name = ast.parseIdentifier(field_ident);

        var bit_offset: u16 = 0;
        current_type = switch (current_type.get(ana.sema).*) {
            .@"struct" => |symbol| b: {
                const struct_sym = symbol.getStruct(ana.sema);
                for (struct_sym.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        break :b field.type;
                    }

                    bit_offset += field.type.byteSize(ana.sema) * 8;
                }

                try ana.sema.emitError(field_ident, ana.module, .unknown_field, .{ field_name, current_type.fmt(ana.sema) });
                return error.AnalyzeFailed;
            },
            .@"packed" => |symbol| b: {
                const packed_sym = symbol.getPacked(ana.sema);
                for (packed_sym.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        break :b field.type;
                    }

                    bit_offset += field.type.bitSize(ana.sema);
                }

                try ana.sema.emitError(field_ident, ana.module, .unknown_field, .{ field_name, current_type.fmt(ana.sema) });
                return error.AnalyzeFailed;
            },
            else => {
                try ana.sema.emitError(field_ident, ana.module, .no_members, .{current_type.fmt(ana.sema)});
                return error.AnalyzeFailed;
            },
        };

        try ana.emit(.{ .field_reference = .{ .bit_offset = bit_offset, .type = current_type } }, node);
    }
}

/// Iterates the SemanticIR instructions of the current and all parent scopes
const ScopeIterator = struct {
    analyzer: *const Analyzer,

    index: std.meta.Tag(SemanticIr.Index),
    curr_depth: i16 = 0,
    min_depth: i16 = 0,

    pub fn init(ana: *const Analyzer) ScopeIterator {
        return .{
            .analyzer = ana,
            .index = @intCast(ana.graph.items.len),
        };
    }

    pub fn next(iter: *ScopeIterator) ?SemanticIr {
        while (iter.index > 0) : (iter.index -= 1) {
            iter.min_depth = @min(iter.min_depth, iter.curr_depth);

            const ir = iter.analyzer.graph.items[iter.index - 1];
            switch (ir.tag) {
                .begin_scope => iter.curr_depth -= 1,
                .end_scope => iter.curr_depth += 1,
                else => {
                    if (iter.min_depth == iter.curr_depth) {
                        iter.index -= 1;
                        return ir;
                    }
                },
            }
        }

        return null;
    }
};

// AST node handling

pub fn handleBlock(ana: *Analyzer, node: Node.Index) Error!void {
    // try ana.emit(.begin_scope, node);

    const range = ana.getAst().nodeData(node).sub_range;
    for (@intFromEnum(range.extra_start)..@intFromEnum(range.extra_end)) |extra| {
        const child: Node.Index = @enumFromInt(ana.getAst().extra_data[extra]);

        switch (ana.getAst().nodeTag(child)) {
            .block => try ana.handleBlock(child),
            // .label => try ana.handleLabel(child),
            // .local_var_decl => try ana.handleLocalVarDecl(child),
            .assign_statement => try ana.handleAssignStatement(child),
            // .call_statement => try ana.handleCallStatement(child),
            // .while_statement => try ana.handleWhileStatement(child),
            else => {},
        }
    }

    // try ana.emit(.end_scope, node);
}

fn handleLabel(ana: *Analyzer, node: Node.Index) Error!void {
    const ast = ana.getAst();
    const name_ident = ast.nodeToken(node);
    const name = ast.parseIdentifier(name_ident);

    // Avoid shadowing any global symbols
    if (ana.module.get(ana.sema).symbol_map.get(name)) |existing_symbol_idx| {
        try ana.sema.emitError(name_ident, ana.module, .local_shadow_global, .{name});
        try ana.sema.emitNote(ast.nodeToken(existing_symbol_idx.getCommon(ana.sema).node), ana.module, .existing_symbol, .{});
        return error.AnalyzeFailed;
    }

    // Avoid shadowing variables / labels
    for (ana.graph.items) |ir| {
        if (ir.tag != .declare_variable and ir.tag != .declare_label) {
            continue;
        }

        const other_name_ident = ast.nodeToken(ir.node);
        const other_name = ast.parseIdentifier(other_name_ident);
        if (std.mem.eql(u8, other_name, name)) {
            switch (ir.tag) {
                .declare_variable => try ana.sema.emitError(name_ident, ana.module, .label_shadow_local, .{name}),
                .declare_label => try ana.sema.emitError(name_ident, ana.module, .duplicate_label, .{name}),
                else => unreachable,
            }
            try ana.sema.emitNote(other_name_ident, ana.module, .existing_local, .{});
            return error.AnalyzeFailed;
        }
    }

    try ana.emit(.declare_label, node);
}

fn handleLocalVarDecl(ana: *Analyzer, node: Node.Index) Error!void {
    const ast = ana.getAst();
    const name_ident = ast.nodeToken(node);
    const name = ast.parseIdentifier(name_ident);

    // Avoid shadowing any global symbols
    if (ana.module.get(ana.sema).symbol_map.get(name)) |existing_symbol_idx| {
        try ana.sema.emitError(name_ident, ana.module, .local_shadow_global, .{name});
        try ana.sema.emitNote(ast.nodeToken(existing_symbol_idx.getCommon(ana.sema).node), ana.module, .existing_symbol, .{});
        return error.AnalyzeFailed;
    }

    // Avoid shadowing variables / labels in parent scopes
    var idx: std.meta.Tag(SemanticIr.Index) = @intCast(ana.graph.items.len);
    var curr_depth: i16 = 0;
    var min_depth: i16 = 0;
    while (idx > 0) : (idx -= 1) {
        const ir = ana.graph.items[idx - 1];

        min_depth = @min(min_depth, curr_depth);
        switch (ir.tag) {
            .begin_scope => curr_depth -= 1,
            .end_scope => curr_depth += 1,
            .declare_variable => {
                if (curr_depth != min_depth) {
                    continue;
                }

                const other_name_ident = ast.nodeToken(ir.node);
                const other_name = ast.parseIdentifier(other_name_ident);
                if (std.mem.eql(u8, other_name, name)) {
                    try ana.sema.emitError(name_ident, ana.module, .duplicate_local, .{name});
                    try ana.sema.emitNote(other_name_ident, ana.module, .existing_local, .{});
                    return error.AnalyzeFailed;
                }
            },
            .declare_label => {
                const other_name_ident = ast.nodeToken(ir.node);
                const other_name = ast.parseIdentifier(other_name_ident);
                if (std.mem.eql(u8, other_name, name)) {
                    try ana.sema.emitError(name_ident, ana.module, .local_shadow_label, .{name});
                    try ana.sema.emitNote(other_name_ident, ana.module, .existing_local, .{});
                    return error.AnalyzeFailed;
                }
            },
            else => {},
        }
    }

    const decl = ast.nodeData(node).local_var_decl;
    const data = ast.readExtraData(Ast.Node.LocalVarDeclData, decl.extra);

    const location_node: Node.Index = ast.nodeData(data.location_attr).attr_two.expr_one;
    const location_expr: Expression.Index = try .resolve(ana.sema, try builtin_module.VariableLocation.resolveType(ana.sema), location_node, ana.module);
    const location = try location_expr.toValue(builtin_module.VariableLocation, ana.sema);

    const var_type: TypeExpression.Index = try .resolve(ana.sema, data.type, ana.module, ana.symbol);
    const var_initial_value: Expression.Index = try .resolve(ana.sema, var_type, decl.value, ana.module);
    _ = var_initial_value; // autofix

    // TODO: Assign initial value
    try ana.emit(.{ .declare_variable = .{
        .location = location,
        .type = var_type,
    } }, node);
}

fn handleAssignStatement(ana: *Analyzer, node: Node.Index) Error!void {
    const ast = ana.getAst();
    const assign = ast.nodeData(node).assign_statement;

    const field_range = ast.nodeData(assign.target).sub_range;
    const fields = ast.extra_data[@intFromEnum(field_range.extra_start)..@intFromEnum(field_range.extra_end)];
    std.debug.assert(fields.len != 0);

    // Resolve root target
    const root_ident: Ast.TokenIndex = .cast(fields[0]);
    const root_name = ast.parseIdentifier(root_ident);

    // Built-in variable
    if (ast.tokenTag(root_ident) == .builtin_ident) {
        if (try BuiltinVar.getByName(root_name, ana.sema)) |builtin| {
            try builtin.write(ana, assign.value, try .resolve(ana.sema, try builtin.getType(ana.sema), assign.value, ana.module));
            return;
        }

        try ana.sema.emitError(root_ident, ana.module, .invalid_builtin_var, .{root_name});
        return error.AnalyzeFailed;
    }

    // Local variable
    // if (ana.findLocalVariable(root_name)) |ir_idx| {
    //     const root_type = ana.graph.items[@intFromEnum(ir_idx)].tag.declare_variable.type;

    //     const assign_idx = ana.graph.items.len;
    //     try ana.emit(.{ .assign_local = undefined }, node);
    //     try ana.parseFieldTarget(fields[1..], root_type, node);

    //     const target_type = if (fields.len > 1)
    //         ana.graph.getLast().tag.field_reference.type
    //     else
    //         root_type;
    //     const value_expr: Expression.Index = try .resolve(ana.sema, target_type, assign.value, ana.module);

    //     ana.graph.items[assign_idx].tag.assign_local = .{
    //         .local_index = ir_idx,
    //         .field_target = @intCast(ana.graph.items.len - assign_idx - 1),
    //         .value = value_expr,
    //     };
    //     return;
    // }

    // Global variable
    const symbol = try ana.sema.resolveSymbol(root_ident, ana.module);
    const root_type = switch (symbol.get(ana.sema).*) {
        .constant => |const_sym| const_sym.type,
        .variable => |var_sym| var_sym.type,
        .register => |reg_sym| reg_sym.type,
        else => {
            try ana.sema.emitError(root_ident, ana.module, .expected_const_var_reg_symbol, .{@tagName(symbol.get(ana.sema).*)});
            return error.AnalyzeFailed;
        },
    };
    _ = root_type; // autofix

    var int: std.math.big.int.Managed = try .init(ana.sema.allocator);
    errdefer int.deinit();
    int.setString(10, "12345") catch unreachable;

    const val_node = try ana.create(.{ .data = .{ .value = int.toConst() }, .source = node });
    _ = try ana.create(.{ .data = .{ .store_symbol = symbol }, .parents = .{ .left = val_node, .right = .none }, .source = node });

    // const assign_idx = ana.graph.items.len;
    // try ana.emit(.{ .assign_global = undefined }, node);
    // try ana.parseFieldTarget(fields[1..], root_type, node);

    // const target_type = if (fields.len > 1)
    //     ana.graph.getLast().tag.field_reference.type
    // else
    //     root_type;
    // const value_expr: Expression.Index = try .resolve(ana.sema, target_type, assign.value, ana.module);

    // ana.graph.items[assign_idx].tag.assign_global = .{
    //     .symbol = symbol,
    //     .field_target = @intCast(ana.graph.items.len - assign_idx - 1),
    //     .value = value_expr,
    // };
}

fn handleCallStatement(ana: *Analyzer, node: Node.Index) Error!void {
    const ast = ana.getAst();
    const target_ident = ast.nodeToken(node);
    const target_name = ast.parseIdentifier(target_ident);

    // Built-in function
    if (ast.tokenTag(target_ident) == .builtin_ident) {
        // TODO
        try ana.sema.emitError(target_ident, ana.module, .invalid_builtin_fn, .{target_name});
        return error.AnalyzeFailed;
    }

    // Global function
    const symbol = try ana.sema.resolveSymbol(target_ident, ana.module);
    switch (symbol.get(ana.sema).*) {
        .function => {
            try ana.emit(.{ .call_function = .{
                .symbol = symbol,
            } }, node);
        },
        else => {
            try ana.sema.emitError(target_ident, ana.module, .expected_fn_symbol, .{@tagName(symbol.get(ana.sema).*)});
            return error.AnalyzeFailed;
        },
    }
}

fn handleWhileStatement(ana: *Analyzer, node: Node.Index) Error!void {
    // TODO:
    try ana.emit(.@"return", node);
}

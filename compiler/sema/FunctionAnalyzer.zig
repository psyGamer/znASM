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

const Sir = Sema.Sir;
const sir_gen = Sema.sir_gen;

const Analyzer = @This();

/// Scope for the currently active Static-Single-Assignment (SSA)
const Scope = struct {
    /// `block_start` node of this scope
    start: Sir.Index,
    /// `block_end` node of this scope
    end: Sir.Index,
};

sema: *Sema,

symbol: Symbol.Index,
module: Module.Index,

graph: Sir.Graph = .empty,

scopes: std.ArrayListUnmanaged(Scope) = .empty,

// Helper functions

pub inline fn getAst(ana: Analyzer) *Ast {
    return &ana.module.get(ana.sema).ast;
}

pub inline fn create(ana: *Analyzer, data: Sir.Data, edges: []const Sir.Edge, node: Node.Index) !Sir.Index {
    return ana.graph.create(ana.dataAllocator(), data, edges, node);
}

pub inline fn dataAllocator(ana: Analyzer) std.mem.Allocator {
    return ana.sema.data_allocator;
}
pub inline fn tempAllocator(ana: Analyzer) std.mem.Allocator {
    return ana.sema.temp_allocator;
}

pub fn process(ana: *Analyzer) Error!void {
    const node = ana.symbol.getCommon(ana.sema).node;
    const fn_def = ana.getAst().nodeData(node).fn_def;

    try ana.handleBlock(fn_def.block);

    // if (ana.ir.items.len != 0 and ana.ir.getLast().tag == .@"return") {
    //     return; // Already explicitly returned
    // }

    var queue: std.ArrayListUnmanaged(Sir.Index) = .empty;
    defer queue.deinit(ana.sema.allocator);

    // TODO: Handle return values
    const ret_node = try ana.create(.@"return", &.{}, node);
    _ = ret_node; // autofix

    // Depend on latest assignment for each symbol
    // var returned_symbols: std.AutoArrayHashMapUnmanaged(Symbol.Index, []bool) = .empty;
    // defer {
    //     for (returned_symbols.values()) |value| {
    //         ana.sema.allocator.free(value);
    //     }
    //     returned_symbols.deinit(ana.sema.allocator);
    // }

    // ana.graph.refresh();
    // var i: usize = ana.graph.list.len - 1;
    // while (ana.graph.slice.tags[i] != .block_start) : (i -= 1) {
    //     const index: Sir.Index = .cast(i);

    //     // Identify symbol assignments
    //     if (index.getTag(&ana.graph) == .symbol and index.getParents(&ana.graph).left != .none) {
    //         const data = index.getData(&ana.graph).symbol;
    //         const gop = try returned_symbols.getOrPut(ana.sema.allocator, data.symbol);
    //         if (!gop.found_existing) {
    //             const ty = switch (data.symbol.get(ana.sema).*) {
    //                 .constant => |const_sym| const_sym.type,
    //                 .variable => |var_sym| var_sym.type,
    //                 .register => |reg_sym| reg_sym.type,
    //                 else => unreachable,
    //             };
    //             gop.value_ptr.* = try ana.sema.allocator.alloc(bool, ty.byteSize(ana.sema));
    //         }

    //         const range = gop.value_ptr.*[data.byte_start..data.byte_end];
    //         if (!std.mem.allEqual(bool, range, true)) {
    //             @memset(range, true);
    //             try ret_node.addParent(ana.sema.allocator, &ana.graph, .cast(i), .both);
    //         }
    //     }
    // }
}

/// Searches for the Semantic IR index of the `declare_variable` for the target local variable
fn findLocalVariable(ana: *const Analyzer, name: []const u8) ?Sir.Index {
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

    index: std.meta.Tag(Sir.Index),
    curr_depth: i16 = 0,
    min_depth: i16 = 0,

    pub fn init(ana: *const Analyzer) ScopeIterator {
        return .{
            .analyzer = ana,
            .index = @intCast(ana.graph.items.len),
        };
    }

    pub fn next(iter: *ScopeIterator) ?Sir {
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
    const start = try ana.create(.block_start, &.{}, node);
    const end = try ana.create(.block_end, &.{}, node);

    try ana.scopes.append(ana.tempAllocator(), .{ .start = start, .end = end });
    defer _ = ana.scopes.pop();

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

    // Ensure start and end are somehow connected
    // var it = start.iterateChildren(&ana.graph);
    // if (it.next() == null) {
    try ana.graph.addEdge(ana.dataAllocator(), .initDependency(end, start));
    // }
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
    var idx: std.meta.Tag(Sir.Index) = @intCast(ana.graph.items.len);
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
            // try builtin.write(ana, assign.value, try .resolve(ana.sema, try builtin.getType(ana.sema), assign.value, ana.module));
            std.log.err("TODO: {}", .{builtin});
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

    const field_offset, const field_type = try sir_gen.calcFieldTarget(ana.sema, root_type, fields[1..], ana.module);
    const field_size = field_type.bitSize(ana.sema);

    const write_start: u16 = @divFloor(field_offset, 8);
    const write_end: u16 = std.math.divCeil(u16, field_offset + field_size, 8) catch unreachable;

    // Depend on previous symbol, to keep a correct order
    const scope = ana.scopes.getLast();
    var it = scope.end.iterateParents(&ana.graph);
    const prev_store_node = while (it.next()) |edge| {
        if (edge.parent.getTag(&ana.graph) != .symbol) {
            continue;
        }

        const data = edge.parent.getData(&ana.graph).symbol;
        if (data.symbol == symbol and
            (data.byte_start >= write_start and data.byte_start < write_end or
                data.byte_end > write_start and data.byte_end <= write_end or
                data.byte_start <= write_start and data.byte_end >= write_end))
        {
            break edge.parent;
        }
    } else .none;

    const expr_node, _, const int_reg = try sir_gen.parseExpression(ana.sema, &ana.graph, assign.value, ana.module, .{
        .start_node = scope.start,
        .end_node = scope.end,
        .target_type = field_type,
    });

    if (field_offset % 8 == 0 and field_size % 8 == 0) {
        // Byte-aligned
        const store_node = try ana.create(.{ .symbol = .{
            .symbol = symbol,
            .byte_start = write_start,
            .byte_end = write_end,
        } }, &.{.withDataParent(expr_node, .a, 0, field_size)}, node);
        if (prev_store_node != .none) {
            try ana.graph.addEdge(ana.dataAllocator(), .initDependency(store_node, prev_store_node));
        }

        // Update `block_end` connection
        if (prev_store_node != scope.start) {
            ana.graph.removeConnection(scope.end, prev_store_node);
        }
        try ana.graph.addEdge(ana.dataAllocator(), .initDependency(scope.end, store_node));
    } else {
        // Packed
        const access_size = (write_end - write_start) * 8;
        const field_start = field_offset - write_start * 8;

        // Create a mask for the field
        const limbs_count = std.math.divCeil(u16, access_size, @bitSizeOf(std.math.big.Limb)) catch unreachable;
        const mask_value: std.math.big.int.Mutable = .{
            .limbs = try ana.tempAllocator().alloc(std.math.big.Limb, limbs_count),
            .len = limbs_count,
            .positive = true,
        };
        @memset(mask_value.limbs, std.math.maxInt(std.math.big.Limb));

        std.debug.assert(field_size > 0);
        const field_start_limb = @divFloor(field_start, @bitSizeOf(std.math.big.Limb));
        const field_end_limb = @divFloor(field_start + field_size - 1, @bitSizeOf(std.math.big.Limb));

        if (field_start_limb == field_end_limb) {
            mask_value.limbs[field_start_limb] = ~((@as(std.math.big.Limb, 1) << @intCast(field_size)) - 1 << @intCast(field_start % @bitSizeOf(std.math.big.Limb)));
        } else {
            mask_value.limbs[field_start_limb] = (@as(std.math.big.Limb, 1) << @intCast(field_start % @bitSizeOf(std.math.big.Limb)));
            mask_value.limbs[field_end_limb] = ~((@as(std.math.big.Limb, 1) << @intCast((field_start + field_size) % @bitSizeOf(std.math.big.Limb))) - 1);

            if (field_start_limb + 1 < field_end_limb) {
                @memset(mask_value.limbs[(field_start_limb + 1)..field_end_limb], 0);
            }
        }
        mask_value.limbs[mask_value.len - 1] &= (@as(std.math.big.Limb, 1) << @intCast(root_type.bitSize(ana.sema) % @bitSizeOf(std.math.big.Limb))) - 1;

        const load_node = try ana.create(.{ .symbol = .{
            .symbol = symbol,
            .byte_start = write_start,
            .byte_end = write_end,
        } }, &.{}, node);
        if (prev_store_node != .none) {
            try ana.graph.addEdge(ana.dataAllocator(), .initDependency(load_node, prev_store_node));
        }

        const and_mask = try ana.create(.{ .value = mask_value.toConst() }, &.{}, node);
        const and_node = try ana.create(.bit_and, &.{ .withDataParent(load_node, int_reg, 0, access_size), .withDataParent(and_mask, int_reg, 0, access_size) }, node);

        const store_cleared_node = try ana.create(.{ .symbol = .{
            .symbol = symbol,
            .byte_start = write_start,
            .byte_end = write_end,
        } }, &.{.withDataParent(and_node, int_reg, 0, access_size)}, node);

        // Place value into field
        const shift_node = try ana.create(.{ .bit_shift_left = field_start }, &.{.withDataParent(expr_node, int_reg, 0, field_size)}, node);
        const or_node = try ana.create(.bit_or, &.{ .withDataParent(store_cleared_node, int_reg, 0, access_size), .withDataParent(shift_node, int_reg, field_start, field_start + field_size) }, node);

        const store_node = try ana.create(.{ .symbol = .{
            .symbol = symbol,
            .byte_start = write_start,
            .byte_end = write_end,
        } }, &.{.withDataParent(or_node, int_reg, 0, access_size)}, node);

        // Update `block_end` connection
        if (prev_store_node != .none) {
            ana.graph.removeConnection(scope.end, prev_store_node);
        }
        try ana.graph.addEdge(ana.dataAllocator(), .initDependency(scope.end, store_node));
    }

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

//! Helper functions for parsing AST nodes into SIR nodes
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

const Module = @import("../Module.zig");
const Ast = @import("../Ast.zig");
const Token = Ast.Token;
const Node = Ast.Node;
const Sema = @import("../Sema.zig");
const Symbol = Sema.Symbol;
const SymbolLocation = Sema.SymbolLocation;
const TypeExpression = Sema.TypeExpression;
const Error = Sema.AnalyzeError;
const Sir = Sema.Sir;
const BuiltinFn = @import("BuiltinFn.zig");
const BuiltinVar = @import("BuiltinVar.zig");

const ParseExpressionOptions = struct {
    /// `block_start` node of the current SSA
    start_node: Sir.Index,
    /// `block_end` node of the current SSA
    end_node: Sir.Index,

    /// Result type of the expression
    /// Can be `.none` if the type can be inferred from the expression itself
    target_type: TypeExpression.Index,
    // TODO: Local variables
};

/// Parses an expression value
pub fn parseExpression(sema: *Sema, graph: *Sir.Graph, node: Node.Index, module: Module.Index, options: ParseExpressionOptions) Error!struct { Sir.Index, TypeExpression, builtin_module.CpuRegister } {
    const ast = &module.get(sema).ast;
    switch (ast.nodeTag(node)) {
        .expr_int_value => {
            const value_literal = ast.nodeToken(node);
            const value = ast.parseBigIntLiteral(std.math.big.int.Const, value_literal, sema.tempAllocator()) catch {
                try sema.emitError(value_literal, module, .invalid_number, .{ast.tokenSource(value_literal)});
                return error.AnalyzeFailed;
            };

            if (options.target_type != .none) {
                if (!options.target_type.get(sema).isAssignableFrom(&.comptime_integer)) {
                    try sema.emitError(value_literal, module, .expected_type, .{ options.target_type.fmt(sema), "comptime_int" });
                    return error.AnalyzeFailed;
                }

                // Validate size
                switch (options.target_type.get(sema).*) {
                    .comptime_integer => {},
                    .integer => |info| switch (info.signedness) {
                        .signed => if (!value.fitsInTwosComp(.signed, info.bits)) {
                            try sema.emitError(value_literal, module, .value_too_large, .{ value, "a", "signed", info.bits });
                            return error.AnalyzeFailed;
                        },
                        .unsigned => if (!value.fitsInTwosComp(.unsigned, info.bits)) {
                            try sema.emitError(value_literal, module, .value_too_large, .{ value, "an", "unsigned", info.bits });
                            return error.AnalyzeFailed;
                        },
                    },
                    else => unreachable,
                }
            }

            const int_reg = try parseIntermediateRegister(sema, node.getData(ast).expr.intermediate_register, module);
            return .{ try graph.create(sema.dataAllocator(), .{ .value = value }, &.{}, node), .comptime_integer, int_reg };
        },
        .expr_ident => {
            const symbol_ident = ast.nodeToken(node);
            const symbol = try sema.resolveSymbol(symbol_ident, module);

            const source_type = switch (symbol.get(sema).*) {
                .constant => |const_sym| const_sym.type,
                .variable => |var_sym| var_sym.type,
                .register => |reg_sym| reg_sym.type,
                else => {
                    try sema.emitError(symbol_ident, module, .expected_const_var_reg_symbol, .{@tagName(symbol.get(sema).*)});
                    return error.AnalyzeFailed;
                },
            };

            // Validate type
            if (options.target_type != .none and !options.target_type.get(sema).isAssignableFrom(source_type.get(sema))) {
                try sema.emitError(symbol_ident, module, .expected_type, .{ options.target_type.fmt(sema), source_type.fmt(sema) });
                return error.AnalyzeFailed;
            }

            // TODO: Field access support
            const read_start = 0;
            const read_end = source_type.byteSize(sema);

            // Add dependency on previous `symbol` store
            var it = options.end_node.iterateParents(graph);
            const prev_store_node = while (it.next()) |edge| {
                if (edge.parent.getTag(graph) != .symbol) {
                    continue;
                }

                const data = edge.parent.getData(graph).symbol;
                if (data.symbol == symbol and
                    (data.byte_start >= read_start and data.byte_start < read_end or
                        data.byte_end > read_start and data.byte_end <= read_end or
                        data.byte_start <= read_start and data.byte_end >= read_end))
                {
                    break edge.parent;
                }
            } else .none;

            const load_node = try graph.create(sema.dataAllocator(), .{ .symbol = .{
                .symbol = symbol,
                .byte_start = 0,
                .byte_end = source_type.byteSize(sema),
            } }, if (prev_store_node != .none) &.{.withParent(prev_store_node)} else &.{}, node);

            const int_reg = try parseIntermediateRegister(sema, node.getData(ast).expr.intermediate_register, module);
            return .{ load_node, .comptime_integer, int_reg };
        },
        else => std.log.err("TODO: {s}", .{@tagName(ast.nodeTag(node))}),
    }

    return .{ .none, undefined, .none };
}
fn parseIntermediateRegister(sema: *Sema, token: Token.Index, module: Module.Index) Error!builtin_module.CpuRegister {
    if (token == .none) {
        return .none;
    }

    const ast = &module.get(sema).ast;

    const register_name = ast.parseIdentifier(token);
    const register = std.meta.stringToEnum(builtin_module.CpuRegister, register_name) orelse {
        try sema.emitError(token, module, .invalid_intermediate_register, .{register_name});
        return error.AnalyzeFailed;
    };
    if (register == .none) {
        try sema.emitError(token, module, .invalid_intermediate_register, .{register_name});
        return error.AnalyzeFailed;
    }

    return register;
}

/// Calculates a target type and offset into a symbol, for a field-chain
pub fn calcFieldTarget(sema: *Sema, root_type: TypeExpression.Index, fields: []const Ast.CommonIndex, module: Module.Index) !struct { u16, TypeExpression.Index } {
    const ast = &module.get(sema).ast;

    var current_offset: u16 = 0;
    var current_type = root_type;
    for (fields) |field_idx| {
        const field_token: Ast.TokenIndex = .cast(field_idx);
        const field_name = ast.parseIdentifier(field_token);

        current_type = switch (current_type.get(sema).*) {
            .@"struct" => |symbol| b: {
                const struct_sym = symbol.getStruct(sema);
                for (struct_sym.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        break :b field.type;
                    }

                    current_offset += field.type.byteSize(sema) * 8;
                }

                try sema.emitError(field_token, module, .unknown_field, .{ field_name, current_type.fmt(sema) });
                return error.AnalyzeFailed;
            },
            .@"packed" => |symbol| b: {
                const packed_sym = symbol.getPacked(sema);
                for (packed_sym.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        break :b field.type;
                    }

                    current_offset += field.type.bitSize(sema);
                }

                try sema.emitError(field_token, module, .unknown_field, .{ field_name, current_type.fmt(sema) });
                return error.AnalyzeFailed;
            },
            else => {
                try sema.emitError(field_token, module, .no_members, .{current_type.fmt(sema)});
                return error.AnalyzeFailed;
            },
        };
    }

    return .{ current_offset, current_type };
}

// /// Loads the value of the field into a node and provides it's type
// pub fn loadField(sema: *Sema, graph: *Sir.Graph, parent: Sir.Index, parent_type: TypeExpression.Index, field_token: Token.Index, module: Module.Index, int_reg: builtin_module.CpuRegister) !struct { Sir.Index, TypeExpression.Index } {
//     const field_offset, const field_type = calc(sema, parent_type, field_token, module);

//     const parent_size = parent_type.bitSize(sema);
//     const field_size = field_type.bitSize(sema);

//     // Shift field into LSB
//     const shift_node = try graph.create(sema.dataAllocator(), .{ .bit_shift_left = field_offset }, &.{.withDataParent(parent, int_reg, parent_size)}, parent.getSourceNode(graph));

//     // Create a mask for only the field
//     const limbs_count = std.math.divCeil(u16, parent_size - field_offset, @bitSizeOf(std.math.big.Limb)) catch unreachable;
//     const mask_value: std.math.big.int.Mutable = .{ .limbs = try sema.tempAllocator().alloc(std.math.big.Limb, limbs_count), .len = limbs_count, .positive = true };

//     const last_limb = (std.math.divCeil(u16, field_size, @bitSizeOf(std.math.big.Limb)) catch unreachable) - 1;
//     @memset(mask_value.limbs[0..last_limb], std.math.maxInt(std.math.big.Limb));
//     mask_value.limbs[last_limb] = @as(std.math.big.Limb, 1) << (field_size % @bitSizeOf(std.math.big.Limb)) - 1;
//     if (last_limb < limbs_count - 1) {
//         @memset(mask_value.limbs[(last_limb + 1)..], 0);
//     }

//     const and_mask = try graph.create(sema.dataAllocator(), .{ .value = mask_value }, &.{}, parent.getSourceNode(graph));
//     const and_node = try graph.create(sema.dataAllocator(), .bit_and, &.{ .withDataParent(shift_node, int_reg, parent_size - field_offset), .withDataParent(and_mask, int_reg, parent_size - field_offset) }, parent.getSourceNode(graph));

//     return and_node;
// }

// /// Stores the value into the target field-chain
// pub fn storeFields(sema: *Sema, graph: *Sir.Graph, symbol: Symbol.Index, fields: []const Ast.CommonIndex, value: Sir.Index, module: Module.Index, int_reg: builtin_module.CpuRegister) !struct { Sir.Index, TypeExpression.Index } {
//     const field_offset, const field_type = calcFieldData(sema, symbol, fields, module);

//     const parent_size = parent_type.bitSize(sema);
//     const field_size = field_type.bitSize(sema);

//     // Shift into correct offset
//     const shift_node = try graph.create(sema.dataAllocator(), .{ .bit_shift_right = field_offset }, &.{.withDataParent(parent, int_reg, field_size)}, parent.getSourceNode(graph));

//     // Create a mask for only the field
//     const limbs_count = std.math.divCeil(u16, parent_size - field_offset, @bitSizeOf(std.math.big.Limb)) catch unreachable;
//     const mask_value: std.math.big.int.Mutable = .{ .limbs = try sema.tempAllocator().alloc(std.math.big.Limb, limbs_count), .len = limbs_count, .positive = true };

//     const last_limb = (std.math.divCeil(u16, field_size, @bitSizeOf(std.math.big.Limb)) catch unreachable) - 1;
//     @memset(mask_value.limbs[0..last_limb], std.math.maxInt(std.math.big.Limb));
//     mask_value.limbs[last_limb] = @as(std.math.big.Limb, 1) << (field_size % @bitSizeOf(std.math.big.Limb)) - 1;
//     if (last_limb < limbs_count - 1) {
//         @memset(mask_value.limbs[(last_limb + 1)..], 0);
//     }

//     const and_mask = try graph.create(sema.dataAllocator(), .{ .value = mask_value }, &.{}, parent.getSourceNode(graph));
//     const and_node = try graph.create(sema.dataAllocator(), .bit_and, &.{ .withDataParent(shift_node, int_reg, parent_size - field_offset), .withDataParent(and_mask, int_reg, parent_size - field_offset) }, parent.getSourceNode(graph));

//     return and_node;
// }

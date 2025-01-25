const std = @import("std");

const Ast = @import("../Ast.zig");
const NodeIndex = Ast.NodeIndex;
const BuiltinFn = @import("BuiltinFn.zig");
const BuiltinVar = @import("BuiltinVar.zig");
const Relocation = @import("../CodeGen.zig").Relocation;
const Instruction = @import("../instruction.zig").Instruction;
const Opcode = @import("../instruction.zig").Opcode;
const Ir = @import("../ir.zig").Ir;
const Module = @import("../Module.zig");
const Sema = @import("../Sema.zig");
const SymbolIndex = @import("../Sema.zig").SymbolIndex;
const ModuleIndex = @import("../Sema.zig").ModuleIndex;
const Error = Sema.AnalyzeError;
const Symbol = @import("../symbol.zig").Symbol;
const SymbolLocation = @import("../symbol.zig").SymbolLocation;
const TypeSymbol = @import("../symbol.zig").TypeSymbol;

const ExpressionIndex = Sema.ExpressionIndex;
const TypeExpressionIndex = Sema.TypeExpressionIndex;

/// A parsed value, used for assignments and function calls
pub const Expression = struct {
    pub const Field = struct {
        type: TypeExpressionIndex,
        value: ExpressionIndex,

        bit_offset: u16,
    };

    value: union(enum) {
        /// Compile-time known arbitrarily sized value
        immediate: std.math.big.int.Const,

        /// Runtime symbol reference
        symbol: SymbolIndex,

        /// Initializer for fields on objects (struct, packed)
        field_initializer: Field,

        /// Initializer for full objects (struct, packed)
        /// Colletion of `field_initializer`s for all fields
        object_initializer: struct {
            fields_start: ExpressionIndex,
            fields_end: ExpressionIndex,
        },
    },
    node: NodeIndex,

    pub fn parse(sema: *Sema, target_type_idx: TypeExpressionIndex, node_idx: NodeIndex, module_idx: ModuleIndex) Error!Expression {
        const module = sema.getModule(module_idx);
        const ast = &module.ast;

        switch (ast.nodeTag(node_idx)) {
            .expr_ident => {
                const source_symbol_ident = ast.nodeToken(node_idx);
                const source_symbol, const source_symbol_idx = try sema.resolveSymbol(source_symbol_ident, module_idx);

                const source_type = sema.getTypeExpression(switch (source_symbol.*) {
                    .constant => |const_sym| const_sym.type,
                    .variable => |var_sym| var_sym.type,
                    .register => |reg_sym| reg_sym.type,
                    else => {
                        try sema.emitError(source_symbol_ident, module_idx, .expected_const_var_reg_symbol, .{@tagName(source_symbol.*)});
                        return error.AnalyzeFailed;
                    },
                });

                // Validate type
                const target_type = sema.getTypeExpression(target_type_idx);
                if (!target_type.isAssignableFrom(source_type)) {
                    try sema.emitError(source_symbol_ident, module_idx, .expected_type, .{ target_type.fmt(sema), source_type.fmt(sema) });
                    return error.AnalyzeFailed;
                }

                return switch (source_symbol.*) {
                    .constant => |const_sym| .{ .value = .{ .immediate = sema.getExpression(const_sym.value).value.immediate }, .node = node_idx },
                    .variable, .register => .{ .value = .{ .symbol = source_symbol_idx }, .node = node_idx },
                    else => unreachable,
                };
            },
            .expr_int_value => {
                const value_lit = ast.nodeToken(node_idx);
                const value = try sema.parseBigInt(std.math.big.int.Const, value_lit, module_idx);

                const target_type = sema.getTypeExpression(target_type_idx);
                if (!target_type.isAssignableFrom(&.comptime_integer)) {
                    try sema.emitError(value_lit, module_idx, .expected_type, .{ target_type.fmt(sema), "comptime_int" });
                    return error.AnalyzeFailed;
                }

                // Validate size
                switch (target_type.*) {
                    .comptime_integer => {},
                    .integer => |info| switch (info.signedness) {
                        .signed => if (!value.fitsInTwosComp(.signed, info.bits)) {
                            try sema.emitError(value_lit, module_idx, .value_too_large, .{ value, "a", "signed", info.bits });
                            return error.AnalyzeFailed;
                        },
                        .unsigned => if (!value.fitsInTwosComp(.unsigned, info.bits)) {
                            try sema.emitError(value_lit, module_idx, .value_too_large, .{ value, "an", "unsigned", info.bits });
                            return error.AnalyzeFailed;
                        },
                    },
                    else => unreachable,
                }

                return .{ .value = .{ .immediate = value }, .node = node_idx };
            },
            .expr_enum_value => {
                const value_lit = ast.nodeToken(node_idx);
                const literal_name = ast.parseIdentifier(value_lit);

                const target_type = sema.getTypeExpression(target_type_idx);
                switch (target_type.*) {
                    .@"enum" => |symbol_idx| {
                        const enum_sym = sema.getSymbol(symbol_idx).@"enum";
                        for (enum_sym.fields) |field| {
                            if (std.mem.eql(u8, field.name, literal_name)) {
                                return sema.getExpression(field.value).*;
                            }
                        }

                        // TODO: Support decl-literals
                        try sema.emitError(value_lit, module_idx, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                    .@"struct", .@"packed" => {
                        // TODO: Support decl-literals
                        try sema.emitError(value_lit, module_idx, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                    else => {
                        try sema.emitError(value_lit, module_idx, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                }
            },
            .expr_init => {
                const target_type = sema.getTypeExpression(target_type_idx);
                switch (target_type.*) {
                    inline .@"struct", .@"packed" => |symbol_idx, tag| {
                        var fields: std.StringArrayHashMapUnmanaged(Field) = .empty;
                        defer fields.deinit(sema.allocator);

                        const symbol = switch (tag) {
                            .@"struct" => &sema.getSymbol(symbol_idx).@"struct",
                            .@"packed" => &sema.getSymbol(symbol_idx).@"packed",
                            else => unreachable,
                        };

                        try fields.ensureUnusedCapacity(sema.allocator, symbol.fields.len);

                        // Apply specified values
                        const sub_range = ast.nodeData(node_idx).sub_range;
                        for (@intFromEnum(sub_range.extra_start)..@intFromEnum(sub_range.extra_end)) |extra_idx| {
                            const field_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);
                            const field_token = ast.nodeToken(field_idx);
                            const field_data = ast.nodeData(field_idx).expr_init_field;

                            const field_name = ast.parseIdentifier(field_token);
                            const field_type = get_type: {
                                for (symbol.fields) |field| {
                                    if (std.mem.eql(u8, field.name, field_name)) {
                                        break :get_type field.type;
                                    }
                                }

                                try sema.emitError(field_token, module_idx, .unknown_field, .{ field_name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            };
                            const field_value: ExpressionIndex = try .resolve(sema, field_type, field_data.value, module_idx);

                            const field_bit_offset = get_offset: switch (tag) {
                                .@"struct" => {
                                    var byte_offset: u16 = 0;
                                    for (symbol.fields) |field| {
                                        if (std.mem.eql(u8, field.name, field_name)) {
                                            break :get_offset byte_offset * 8;
                                        }

                                        byte_offset += field.type.byteSize(sema);
                                    }

                                    unreachable;
                                },
                                .@"packed" => {
                                    var bit_offset: u16 = 0;
                                    for (symbol.fields) |field| {
                                        if (std.mem.eql(u8, field.name, field_name)) {
                                            break :get_offset bit_offset;
                                        }

                                        bit_offset += field.type.bitSize(sema);
                                    }

                                    unreachable;
                                },
                                else => unreachable,
                            };

                            if (fields.contains(field_name)) {
                                try sema.emitError(field_token, module_idx, .duplicate_field, .{ field_name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            }

                            fields.putAssumeCapacityNoClobber(field_name, .{
                                .type = field_type,
                                .value = field_value,
                                .bit_offset = field_bit_offset,
                            });
                        }

                        // Fill default values / check missing
                        for (symbol.fields, 0..) |defined_field, def_idx| {
                            if (fields.contains(defined_field.name)) {
                                continue;
                            }

                            if (defined_field.default_value != .none) {
                                // Apply default value
                                var bit_offset: u16 = 0;
                                for (symbol.fields, 0..) |field, field_idx| {
                                    if (def_idx == field_idx) {
                                        fields.putAssumeCapacityNoClobber(field.name, .{
                                            .type = field.type,
                                            .value = field.default_value,
                                            .bit_offset = bit_offset,
                                        });
                                        break;
                                    }

                                    bit_offset += switch (tag) {
                                        .@"struct" => field.type.byteSize(sema) * 8,
                                        .@"packed" => field.type.bitSize(sema),
                                        else => unreachable,
                                    };
                                }
                            } else {
                                // Field value not specified
                                try sema.emitError(ast.nodeToken(node_idx), module_idx, .missing_field, .{ defined_field.name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            }
                        }

                        const start_idx: ExpressionIndex = @enumFromInt(@as(u32, @intCast(sema.expressions.items.len)));
                        const end_idx: ExpressionIndex = @enumFromInt(@as(u32, @intCast(sema.expressions.items.len + fields.count())));

                        try sema.expressions.ensureUnusedCapacity(sema.allocator, fields.count());
                        for (fields.values()) |field| {
                            sema.expressions.appendAssumeCapacity(.{ .value = .{ .field_initializer = field }, .node = sema.getExpression(field.value).node });
                        }

                        return .{ .value = .{ .object_initializer = .{
                            .fields_start = start_idx,
                            .fields_end = end_idx,
                        } }, .node = node_idx };
                    },
                    else => {
                        try sema.emitError(ast.nodeToken(node_idx), module_idx, .expected_type, .{ target_type.fmt(sema), "object-initializer" });
                        return error.AnalyzeFailed;
                    },
                }
            },
            else => unreachable,
        }
    }
};

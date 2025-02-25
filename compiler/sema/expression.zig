const std = @import("std");

const Module = @import("../Module.zig");
const Ast = @import("../Ast.zig");
const NodeIndex = Ast.NodeIndex;
const Sema = @import("../Sema.zig");
const Symbol = Sema.Symbol;
const SymbolLocation = Sema.SymbolLocation;
const RegisterType = Sema.RegisterType;
const TypeExpression = Sema.TypeExpression;
const BuiltinFn = @import("BuiltinFn.zig");
const BuiltinVar = @import("BuiltinVar.zig");

const Error = Sema.AnalyzeError;

/// A parsed value, used for assignments and function calls
pub const Expression = struct {
    pub const Index = enum(u32) {
        none = std.math.maxInt(u32),
        _,

        /// Fetches the underlying value
        pub fn get(index: Index, sema: *Sema) *Expression {
            return &sema.expressions.items[@intFromEnum(index)];
        }

        /// Helper function to cast a generic number to a `Expression.Index`
        pub inline fn cast(x: anytype) Index {
            return switch (@typeInfo(@TypeOf(x))) {
                .null => .none,
                .int, .comptime_int => @enumFromInt(@as(u32, @intCast(x))),
                .optional => if (x) |value| @enumFromInt(@as(u32, @intCast(value))) else .none,
                else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a Index"),
            };
        }

        /// Parses the expression and resolves an index to it
        pub fn resolve(sema: *Sema, target_type: TypeExpression.Index, node: NodeIndex, module: Module.Index) Error!Index {
            try sema.expressions.append(sema.allocator, try .parse(sema, target_type, node, module));
            return @enumFromInt(@as(u32, @intCast(sema.expressions.items.len - 1)));
        }

        /// Tries to resolve the expression into a compile-time known value of the specifed type
        pub fn toValue(index: Index, comptime T: type, sema: *Sema) Error!T {
            return index.get(sema).toValue(T, sema);
        }
    };

    value: union(enum) {
        /// Compile-time known arbitrarily sized value
        immediate: std.math.big.int.Const,

        /// Runtime symbol reference
        symbol: Symbol.Index,

        /// Initializer for all fields of objects
        /// Child expressions are in the same order as the fields of the target type
        object_initializer: struct {
            fields_start: Index,
            fields_end: Index,
        },
    },
    intermediate_register: RegisterType,

    node: NodeIndex,
    module: Module.Index,

    pub fn parse(sema: *Sema, target_type: TypeExpression.Index, node: NodeIndex, module: Module.Index) Error!Expression {
        const ast = &module.get(sema).ast;

        switch (ast.nodeTag(node)) {
            .expr_ident => {
                const source_symbol_ident = ast.nodeToken(node);
                const source_symbol = try sema.resolveSymbol(source_symbol_ident, module);

                const source_type = switch (source_symbol.get(sema).*) {
                    .constant => |const_sym| const_sym.type,
                    .variable => |var_sym| var_sym.type,
                    .register => |reg_sym| reg_sym.type,
                    else => {
                        try sema.emitError(source_symbol_ident, module, .expected_const_var_reg_symbol, .{@tagName(source_symbol.get(sema).*)});
                        return error.AnalyzeFailed;
                    },
                };

                // Validate type
                if (!target_type.get(sema).isAssignableFrom(source_type.get(sema))) {
                    try sema.emitError(source_symbol_ident, module, .expected_type, .{ target_type.fmt(sema), source_type.fmt(sema) });
                    return error.AnalyzeFailed;
                }

                return .{
                    .value = switch (source_symbol.get(sema).*) {
                        .constant => |const_sym| .{ .immediate = const_sym.value.get(sema).value.immediate },
                        .variable, .register => .{ .symbol = source_symbol },
                        else => unreachable,
                    },
                    .intermediate_register = try parseIntermediateRegister(sema, module, ast.nodeData(node).expr.intermediate_register),
                    .node = node,
                    .module = module,
                };
            },
            .expr_int_value => {
                const value_lit = ast.nodeToken(node);
                const value = try sema.parseBigInt(std.math.big.int.Const, value_lit, module);

                if (!target_type.get(sema).isAssignableFrom(&.comptime_integer)) {
                    try sema.emitError(value_lit, module, .expected_type, .{ target_type.fmt(sema), "comptime_int" });
                    return error.AnalyzeFailed;
                }

                // Validate size
                switch (target_type.get(sema).*) {
                    .comptime_integer => {},
                    .integer => |info| switch (info.signedness) {
                        .signed => if (!value.fitsInTwosComp(.signed, info.bits)) {
                            try sema.emitError(value_lit, module, .value_too_large, .{ value, "a", "signed", info.bits });
                            return error.AnalyzeFailed;
                        },
                        .unsigned => if (!value.fitsInTwosComp(.unsigned, info.bits)) {
                            try sema.emitError(value_lit, module, .value_too_large, .{ value, "an", "unsigned", info.bits });
                            return error.AnalyzeFailed;
                        },
                    },
                    else => unreachable,
                }

                return .{
                    .value = .{ .immediate = value },
                    .intermediate_register = try parseIntermediateRegister(sema, module, ast.nodeData(node).expr.intermediate_register),
                    .node = node,
                    .module = module,
                };
            },
            .expr_enum_value => {
                const value_lit = ast.nodeToken(node);
                const literal_name = ast.parseIdentifier(value_lit);

                switch (target_type.get(sema).*) {
                    .@"enum" => |symbol| {
                        const enum_sym = symbol.getEnum(sema);
                        for (enum_sym.fields) |field| {
                            if (std.mem.eql(u8, field.name, literal_name)) {
                                return .{
                                    .value = field.value.get(sema).value,
                                    .intermediate_register = try parseIntermediateRegister(sema, module, ast.nodeData(node).expr.intermediate_register),
                                    .node = node,
                                    .module = module,
                                };
                            }
                        }

                        // TODO: Support decl-literals
                        try sema.emitError(value_lit, module, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                    .@"struct", .@"packed" => {
                        // TODO: Support decl-literals
                        try sema.emitError(value_lit, module, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                    else => {
                        try sema.emitError(value_lit, module, .unknown_enum_field_or_decl, .{ literal_name, target_type.fmt(sema) });
                        return error.AnalyzeFailed;
                    },
                }
            },
            .expr_init => {
                const data = ast.nodeData(node).expr_init;

                switch (target_type.get(sema).*) {
                    inline .@"struct", .@"packed" => |symbol_idx, tag| {
                        const symbol = switch (tag) {
                            .@"struct" => symbol_idx.getStruct(sema),
                            .@"packed" => symbol_idx.getPacked(sema),
                            else => unreachable,
                        };

                        const init_exprs = try sema.allocator.alloc(?Expression, symbol.fields.len);
                        defer sema.allocator.free(init_exprs);

                        @memset(init_exprs, null);

                        // Apply specified values
                        const sub_range = ast.readExtraData(Ast.Node.SubRange, data.extra);
                        for (@intFromEnum(sub_range.extra_start)..@intFromEnum(sub_range.extra_end)) |extra_idx| {
                            const field_node: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);
                            const field_token = ast.nodeToken(field_node);
                            const field_data = ast.nodeData(field_node).expr_init_field;
                            const field_name = ast.parseIdentifier(field_token);

                            const field_idx = get_idx: {
                                for (symbol.fields, 0..) |field, idx| {
                                    if (std.mem.eql(u8, field.name, field_name)) {
                                        break :get_idx idx;
                                    }
                                }

                                try sema.emitError(field_token, module, .unknown_field, .{ field_name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            };
                            if (init_exprs[field_idx] != null) {
                                try sema.emitError(field_token, module, .duplicate_field, .{ field_name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            }

                            init_exprs[field_idx] = try .parse(sema, symbol.fields[field_idx].type, field_data.value, module);
                        }

                        // Fill default values / check missing
                        for (symbol.fields, 0..) |field, field_idx| {
                            if (init_exprs[field_idx] != null) {
                                continue;
                            }

                            if (field.default_value != .none) {
                                // Apply default value
                                init_exprs[field_idx] = field.default_value.get(sema).*;
                            } else {
                                // Field value not specified
                                try sema.emitError(ast.nodeToken(node), module, .missing_field, .{ field.name, target_type.fmt(sema) });
                                return error.AnalyzeFailed;
                            }
                        }

                        const start_idx: Expression.Index = @enumFromInt(@as(u32, @intCast(sema.expressions.items.len)));
                        const end_idx: Expression.Index = @enumFromInt(@as(u32, @intCast(sema.expressions.items.len + init_exprs.len)));

                        try sema.expressions.ensureUnusedCapacity(sema.allocator, init_exprs.len + 1); // +1 for this expression
                        for (init_exprs) |expr| {
                            sema.expressions.appendAssumeCapacity(expr.?);
                        }

                        return .{
                            .value = .{ .object_initializer = .{
                                .fields_start = start_idx,
                                .fields_end = end_idx,
                            } },
                            .intermediate_register = try parseIntermediateRegister(sema, module, data.intermediate_register),
                            .node = node,
                            .module = module,
                        };
                    },
                    else => {
                        try sema.emitError(ast.nodeToken(node), module, .expected_type, .{ target_type.fmt(sema), "object-initializer" });
                        return error.AnalyzeFailed;
                    },
                }
            },
            else => unreachable,
        }
    }

    pub fn toValue(expr: *Expression, comptime T: type, sema: *Sema) Error!T {
        const ast = &expr.module.get(sema).ast;

        if (expr.value == .symbol) {
            try sema.emitError(ast.nodeToken(expr.node), expr.module, .expected_comptime_value, .{});
            try sema.emitError(ast.nodeToken(expr.value.symbol.getCommon(sema).node), expr.module, .existing_symbol, .{});
            return error.AnalyzeFailed;
        }

        switch (@typeInfo(T)) {
            .int => {
                if (expr.value != .immediate) {
                    try sema.emitError(ast.nodeToken(expr.node), expr.module, .expected_comptime_value, .{});
                    return error.AnalyzeFailed;
                }

                return expr.value.immediate.to(T) catch {
                    const info = @typeInfo(T).int;
                    switch (info.signedness) {
                        .signed => try sema.emitError(ast.nodeToken(expr.node), expr.module, .value_too_large, .{ expr.value.immediate, "a", "signed", info.bits }),
                        .unsigned => try sema.emitError(ast.nodeToken(expr.node), expr.module, .value_too_large, .{ expr.value.immediate, "an", "unsigned", info.bits }),
                    }
                    return error.AnalyzeFailed;
                };
            },
            .@"enum" => {
                const tag = try expr.toValue(std.meta.Tag(T), sema);
                return @enumFromInt(tag);
            },
            else => @compileError("Cannot resolve expression into type " ++ @typeName(T)),
        }
    }

    fn parseIntermediateRegister(sema: *Sema, module: Module.Index, token: Ast.TokenIndex) Error!RegisterType {
        if (token == .none) {
            return .none;
        }

        const ast = &module.get(sema).ast;

        const register_name = ast.parseIdentifier(token);
        const register = std.meta.stringToEnum(RegisterType, register_name) orelse {
            try sema.emitError(token, module, .invalid_intermediate_register, .{register_name});
            return error.AnalyzeFailed;
        };
        if (register == .none) {
            try sema.emitError(token, module, .invalid_intermediate_register, .{register_name});
            return error.AnalyzeFailed;
        }

        return register;
    }
};

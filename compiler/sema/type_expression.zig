const std = @import("std");

const Module = @import("../Module.zig");
const Node = @import("../Ast.zig").Node;
const Sema = @import("../Sema.zig");
const Symbol = Sema.Symbol;
const Error = Sema.AnalyzeError;

/// A tree of type-expressions, representing a certain concrete type
pub const TypeExpression = union(enum) {
    pub const Index = enum(u32) {
        none = std.math.maxInt(u32),
        _,

        /// Fetches the underlying value
        pub fn get(index: TypeExpression.Index, sema: *Sema) *TypeExpression {
            return &sema.type_expressions.items[@intFromEnum(index)];
        }

        /// Parses the type-expr ession and resolves an index to it
        pub fn resolve(sema: *Sema, node: Node.Index, module: Module.Index, parent: Symbol.Index) Error!TypeExpression.Index {
            try sema.type_expressions.append(sema.dataAllocator(), try .parse(sema, node, module, parent));
            return @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len - 1)));
        }

        /// Computes the exact byte-size required for this type
        pub fn byteSize(index: TypeExpression.Index, sema: *const Sema) u16 {
            return sema.type_expressions.items[@intFromEnum(index)].byteSize(sema);
        }
        /// Computes the exact bit-size required for this type
        pub fn bitSize(index: TypeExpression.Index, sema: *const Sema) u16 {
            return sema.type_expressions.items[@intFromEnum(index)].bitSize(sema);
        }

        /// Creates a formatter for this type-expression
        pub fn fmt(index: TypeExpression.Index, sema: *const Sema) @typeInfo(@TypeOf(TypeExpression.fmt)).@"fn".return_type.? {
            return sema.type_expressions.items[@intFromEnum(index)].fmt(sema);
        }
    };

    /// Indicate the absence of an value
    void: void,

    /// Runtime integer with a specific size
    integer: struct {
        signedness: enum { signed, unsigned },
        bits: u16,
    },

    /// Compile-time interger with an arbitrary size
    comptime_integer: void,

    /// Byte-aligned structure of associated fields
    @"struct": Symbol.Index,

    /// Bit-packed structured of associated fields
    @"packed": Symbol.Index,

    /// Enumeration of interger-backed options
    @"enum": Symbol.Index,

    /// Primitive types excluding integers, since they are dynamic
    const primitives: std.StaticStringMap(TypeExpression) = .initComptime(.{
        .{ "comptime_int", .comptime_integer },
    });

    pub fn parse(sema: *Sema, node: Node.Index, module: Module.Index, parent: Symbol.Index) Sema.AnalyzeError!TypeExpression {
        const ast = &module.get(sema).ast;

        switch (ast.nodeTag(node)) {
            .type_ident => {
                const ident_name = ast.nodeToken(node);
                const type_name = ast.parseIdentifier(ident_name);

                // Primitive types
                if (primitives.get(type_name)) |primitive| {
                    return primitive;
                }
                // Dynamic integers
                if (type_name[0] == 'i' or type_name[0] == 'u') {
                    var is_int = true;
                    for (type_name[1..]) |c| switch (c) {
                        '0'...'9' => {},
                        else => {
                            is_int = false;
                            break;
                        },
                    };

                    if (is_int) {
                        const bits = std.fmt.parseInt(u16, type_name[1..], 10) catch {
                            try sema.emitError(ident_name, module, .int_bitwidth_too_large, .{ type_name, std.math.maxInt(u16) });
                            return error.AnalyzeFailed;
                        };

                        return if (type_name[0] == 'i')
                            .{ .integer = .{ .signedness = .signed, .bits = bits } }
                        else
                            .{ .integer = .{ .signedness = .unsigned, .bits = bits } };
                    }
                }

                const symbol = try sema.resolveSymbol(ident_name, module);
                switch (symbol.get(sema).*) {
                    .@"struct" => return .{ .@"struct" = symbol },
                    .@"packed" => return .{ .@"packed" = symbol },
                    .@"enum" => return .{ .@"enum" = symbol },
                    else => {
                        try sema.emitError(ident_name, module, .expected_type_symbol, .{@tagName(symbol.get(sema).*)});
                        return error.AnalyzeFailed;
                    },
                }
            },
            inline .struct_def, .packed_def, .enum_def => |tag| {
                var container_symbol: switch (tag) {
                    .struct_def => Symbol.Struct,
                    .packed_def => Symbol.Packed,
                    .enum_def => Symbol.Enum,
                    else => unreachable,
                } = undefined;
                container_symbol.common = .{
                    .node = node,
                    .is_pub = false,
                };
                // Remaining values to be determined during analyzation

                const parent_name = if (parent != .none) b: {
                    const common = parent.getCommon(sema);
                    const parent_module = common.module_index.get(sema);
                    break :b parent_module.symbol_map.keys()[@intFromEnum(common.module_symbol_index)];
                } else "";
                const container_name = switch (tag) {
                    .struct_def => "struct",
                    .packed_def => "packed",
                    .enum_def => "enum",
                    else => unreachable,
                };

                const symbol: Symbol.Index = .cast(sema.symbols.items.len);
                const symbol_name = try std.fmt.allocPrint(sema.dataAllocator(), "{s}__" ++ container_name ++ "_{d}", .{ parent_name, @intFromEnum(symbol) });

                if (module.get(sema).symbol_map.get(symbol_name)) |existing_symbol| {
                    try sema.emitError(ast.nodeToken(node).next(), module, .duplicate_symbol, .{symbol_name});
                    try sema.emitNote(ast.nodeToken(existing_symbol.getCommon(sema).node), module, .existing_symbol, .{});
                    return error.AnalyzeFailed;
                }

                try sema.createSymbol(module, symbol_name, switch (tag) {
                    .struct_def => .{ .@"struct" = container_symbol },
                    .packed_def => .{ .@"packed" = container_symbol },
                    .enum_def => .{ .@"enum" = container_symbol },
                    else => unreachable,
                });
                try sema.analyzeSymbol(symbol, module, ast.nodeToken(node));

                return switch (tag) {
                    .struct_def => .{ .@"struct" = symbol },
                    .packed_def => .{ .@"packed" = symbol },
                    .enum_def => .{ .@"enum" = symbol },
                    else => unreachable,
                };
            },
            else => unreachable,
        }
    }

    /// Checks if the other type-expression can be implicitly converted to this one
    pub fn isAssignableFrom(target: *const TypeExpression, other: *const TypeExpression) bool {
        return switch (target.*) {
            .void => other.* == .void,
            .comptime_integer => other.* == .comptime_integer,

            .integer => |info| switch (other.*) {
                .comptime_integer => true,
                .integer => |other_info| switch (info.signedness) {
                    .signed => switch (other_info.signedness) {
                        .signed => info.bits >= other_info.bits,
                        .unsigned => info.bits > other_info.bits,
                    },
                    .unsigned => switch (other_info.signedness) {
                        .signed => false,
                        .unsigned => info.bits >= other_info.bits,
                    },
                },
                else => false,
            },

            // Need to represent the exact same symbol
            .@"struct", .@"packed", .@"enum" => |symbol| switch (other.*) {
                .@"struct", .@"packed", .@"enum" => |other_symbol| symbol == other_symbol,
                else => false,
            },
        };
    }

    /// Computes the exact byte-size required for this type
    pub fn byteSize(type_expr: *const TypeExpression, sema: *const Sema) u16 {
        return std.mem.alignForward(u16, type_expr.bitSize(sema), 8) / 8;
    }
    /// Computes the exact bit-size required for this type
    pub fn bitSize(type_expr: *const TypeExpression, sema: *const Sema) u16 {
        switch (type_expr.*) {
            .void => return 0,
            .comptime_integer => return 0,
            .integer => |info| return info.bits,
            .@"struct" => |symbol| {
                var byte_size: u16 = 0;

                const struct_sym: *const Symbol.Struct = symbol.getStruct(@constCast(sema));
                for (struct_sym.fields) |field| {
                    byte_size += field.type.byteSize(sema);
                }

                return byte_size * 8;
            },
            .@"packed" => |symbol| {
                const packed_sym: *const Symbol.Packed = symbol.getPacked(@constCast(sema));
                return packed_sym.backing_type.bitSize(sema);
            },
            .@"enum" => |symbol| {
                const enum_sym: *const Symbol.Enum = symbol.getEnum(@constCast(sema));
                return enum_sym.backing_type.bitSize(sema);
            },
        }
    }

    pub fn fmt(type_expr: *const TypeExpression, sema: *const Sema) std.fmt.Formatter(format) {
        return .{ .data = .{ .sema = sema, .type_expr = type_expr } };
    }
    fn format(data: struct { sema: *const Sema, type_expr: *const TypeExpression }, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (data.type_expr.*) {
            .void => try writer.writeAll("void"),
            .comptime_integer => try writer.writeAll("comptime_int"),
            .integer => |info| switch (info.signedness) {
                .signed => try writer.print("i{d}", .{info.bits}),
                .unsigned => try writer.print("u{d}", .{info.bits}),
            },
            .@"struct" => |symbol| {
                const symbol_loc = data.sema.getSymbolLocation(symbol);
                try writer.print("struct {}", .{symbol_loc});
            },
            .@"packed" => |symbol| {
                const symbol_loc = data.sema.getSymbolLocation(symbol);
                const packed_sym: *const Symbol.Packed = symbol.getPacked(@constCast(data.sema));
                try writer.print("packed({}) {}", .{ packed_sym.backing_type.fmt(data.sema), symbol_loc });
            },
            .@"enum" => |symbol| {
                const symbol_loc = data.sema.getSymbolLocation(symbol);
                const enum_sym: *const Symbol.Enum = symbol.getEnum(@constCast(data.sema));
                try writer.print("enum({}) {}", .{ enum_sym.backing_type.fmt(data.sema), symbol_loc });
            },
        }
    }
};

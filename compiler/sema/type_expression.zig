const std = @import("std");

const NodeIndex = @import("../Ast.zig").NodeIndex;
const Sema = @import("../Sema.zig");
const SymbolIndex = Sema.SymbolIndex;
const ModuleIndex = Sema.ModuleIndex;
const Error = Sema.AnalyzeError;
const Symbol = @import("../symbol.zig").Symbol;

/// A tree of type-expressions, representing a certain concrete type
pub const TypeExpression = union(enum) {
    pub const Index = Sema.TypeExpressionIndex;

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
    @"struct": SymbolIndex,

    /// Bit-packed structured of associated fields
    @"packed": SymbolIndex,

    /// Enumeration of interger-backed options
    @"enum": SymbolIndex,

    /// Primitive types excluding integers, since they are dynamic
    const primitives: std.StaticStringMap(TypeExpression) = .initComptime(.{
        .{ "comptime_int", .comptime_integer },
    });

    pub fn parse(sema: *Sema, node_idx: NodeIndex, module_idx: ModuleIndex, parent: SymbolIndex) Sema.AnalyzeError!TypeExpression {
        const module = sema.getModule(module_idx);
        const ast = &module.ast;

        switch (ast.nodeTag(node_idx)) {
            .type_ident => {
                const ident_name = ast.nodeToken(node_idx);
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
                            try sema.emitError(ident_name, module_idx, .int_bitwidth_too_large, .{ type_name, std.math.maxInt(u16) });
                            return error.AnalyzeFailed;
                        };

                        return if (type_name[0] == 'i')
                            .{ .integer = .{ .signedness = .signed, .bits = bits } }
                        else
                            .{ .integer = .{ .signedness = .unsigned, .bits = bits } };
                    }
                }

                const symbol_idx = try sema.resolveSymbol(ident_name, module_idx);
                switch (symbol_idx.get(sema).*) {
                    .@"struct" => return .{ .@"struct" = symbol_idx },
                    .@"packed" => return .{ .@"packed" = symbol_idx },
                    .@"enum" => return .{ .@"enum" = symbol_idx },
                    else => {
                        try sema.emitError(ident_name, module_idx, .expected_type_symbol, .{@tagName(symbol_idx.get(sema).*)});
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
                    .node = node_idx,
                    .is_pub = false,
                };
                // Remaining values to be determined during analyzation

                const parent_name = if (parent != .none) b: {
                    const symbol = sema.getSymbol(parent);
                    const common = symbol.commonConst();

                    const parent_module = sema.getModule(common.module_index);
                    break :b parent_module.symbol_map.keys()[@intFromEnum(common.module_symbol_index)];
                } else "";
                const container_name = switch (tag) {
                    .struct_def => "struct",
                    .packed_def => "packed",
                    .enum_def => "enum",
                    else => unreachable,
                };

                const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len);
                const symbol_name = try std.fmt.allocPrint(sema.allocator, "{s}__" ++ container_name ++ "_{d}", .{ parent_name, @intFromEnum(symbol_idx) });
                try sema.string_pool.append(sema.allocator, symbol_name);

                if (module.symbol_map.get(symbol_name)) |existing_symbol_idx| {
                    try sema.emitError(ast.nodeToken(node_idx).next(), module_idx, .duplicate_symbol, .{symbol_name});
                    try sema.emitNote(ast.nodeToken(existing_symbol_idx.getCommon(sema).node), module_idx, .existing_symbol, .{});
                    return error.AnalyzeFailed;
                }

                try sema.createSymbol(module_idx, symbol_name, switch (tag) {
                    .struct_def => .{ .@"struct" = container_symbol },
                    .packed_def => .{ .@"packed" = container_symbol },
                    .enum_def => .{ .@"enum" = container_symbol },
                    else => unreachable,
                });
                try sema.analyzeSymbol(symbol_idx, module_idx, ast.nodeToken(node_idx));

                return switch (tag) {
                    .struct_def => .{ .@"struct" = symbol_idx },
                    .packed_def => .{ .@"packed" = symbol_idx },
                    .enum_def => .{ .@"enum" = symbol_idx },
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
            .@"struct", .@"packed", .@"enum" => |symbol_idx| switch (other.*) {
                .@"struct", .@"packed", .@"enum" => |other_symbol_idx| symbol_idx == other_symbol_idx,
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
            .@"struct" => |symbol_idx| {
                var byte_size: u16 = 0;

                const struct_sym = sema.symbols.items[@intFromEnum(symbol_idx)].@"struct";
                for (struct_sym.fields) |field| {
                    byte_size += field.type.byteSize(sema);
                }

                return byte_size * 8;
            },
            .@"packed" => |symbol_idx| {
                const packed_sym = sema.symbols.items[@intFromEnum(symbol_idx)].@"packed";
                return packed_sym.backing_type.bitSize(sema);
            },
            .@"enum" => |symbol_idx| {
                const enum_sym = sema.symbols.items[@intFromEnum(symbol_idx)].@"enum";
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
            .@"struct" => |symbol_idx| {
                const symbol_loc = data.sema.getSymbolLocation(symbol_idx);
                try writer.print("struct {}", .{symbol_loc});
            },
            .@"packed" => |symbol_idx| {
                const symbol_loc = data.sema.getSymbolLocation(symbol_idx);
                const packed_sym = data.sema.symbols.items[@intFromEnum(symbol_idx)].@"packed";
                try writer.print("packed({}) {}", .{ packed_sym.backing_type.fmt(data.sema), symbol_loc });
            },
            .@"enum" => |symbol_idx| {
                const symbol_loc = data.sema.getSymbolLocation(symbol_idx);
                const enum_sym = data.sema.symbols.items[@intFromEnum(symbol_idx)].@"enum";
                try writer.print("enum({}) {}", .{ enum_sym.backing_type.fmt(data.sema), symbol_loc });
            },
        }
    }
};

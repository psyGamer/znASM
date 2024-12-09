const std = @import("std");
const symbol = @import("symbol.zig");
const memory_map = @import("memory_map.zig");
const rich = @import("util/rich.zig");

const Token = @import("Lexer.zig").Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Relocation = @import("CodeGen.zig").Relocation;
const Ir = @import("ir.zig").Ir;
const Symbol = symbol.Symbol;
const TypeSymbol = symbol.TypeSymbol;
const SymbolLocation = symbol.SymbolLocation;
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const FunctionAnalyzer = @import("sema/FunctionAnalyzer.zig");
const Sema = @This();

/// Module -> Name -> Symbol
pub const SymbolIndex = u32;
pub const SymbolMap = std.StringArrayHashMapUnmanaged(std.StringArrayHashMapUnmanaged(SymbolIndex));

pub const ModuleIndex = u32;

pub const RegisterType = enum { a8, a16, x8, x16, y8, y16 };

/// Represents a parsed value of an expression
pub const ExpressionValue = union(enum) {
    immediate: TypeSymbol.ComptimeIntValue,
    symbol: SymbolLocation,
    register: RegisterType,

    pub fn resolve(expr: ExpressionValue, comptime T: type, sema: *const Sema) T {
        return switch (expr) {
            .immediate => |value| @intCast(value),
            .symbol => |sym| switch (sema.lookupSymbol(sym).?.*) {
                .function, .variable, .@"enum" => unreachable,
                .constant => |const_sym| @intCast(const_sym.value.resolve(T, sema)),
            },
            .register => unreachable,
        };
    }
};

const InterruptVectors = struct {
    native_cop: ?SymbolLocation = null,
    native_brk: ?SymbolLocation = null,
    native_abort: ?SymbolLocation = null,
    native_nmi: ?SymbolLocation = null,
    native_irq: ?SymbolLocation = null,
    emulation_cop: ?SymbolLocation = null,
    emulation_abort: ?SymbolLocation = null,
    emulation_nmi: ?SymbolLocation = null,
    emulation_reset: ?SymbolLocation = null,
    emulation_irqbrk: ?SymbolLocation = null,
};

modules: []Module,
mapping_mode: MappingMode,

symbols: std.MultiArrayList(struct { sym: Symbol, loc: SymbolLocation }) = .empty,
symbol_map: SymbolMap = .empty,

errors: std.ArrayListUnmanaged(Error) = .empty,

interrupt_vectors: InterruptVectors = .{},
allocator: std.mem.Allocator,

pub fn process(allocator: std.mem.Allocator, modules: []Module, mapping_mode: MappingMode) !?Sema {
    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    var sema: Sema = .{
        .modules = modules,
        .mapping_mode = mapping_mode,
        .allocator = allocator,
    };

    // Gather symbols
    for (0..modules.len) |module_idx| {
        try sema.gatherSymbols(@intCast(module_idx));
    }

    // Only reset is required
    if (sema.interrupt_vectors.emulation_reset == null) {
        try sema.errors.append(allocator, .{
            .tag = .missing_reset_vector,
            .ast = undefined,
            .token = Ast.null_token,
        });
    }

    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        sema.deinit(allocator);
        return null;
    }

    // Recursivly analayze symbols, starting from the interrupt vectors
    inline for (std.meta.fields(InterruptVectors)) |field| {
        if (@field(sema.interrupt_vectors, field.name)) |vector_loc| {
            const vector_sym = sema.lookupSymbol(vector_loc).?;
            const vector_mod = sema.findSymbolModuleIndex(vector_loc);

            sema.analyzeSymbol(vector_sym, vector_loc, Ast.null_token, vector_mod) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze symbol {}", .{vector_loc}),
                else => |e| return e,
            };

            if (try sema.detectErrors(stderr.writer(), tty_config)) {
                sema.deinit(allocator);
                return null;
            }
        }
    }

    return sema;
}

pub fn deinit(sema: *Sema, allocator: std.mem.Allocator) void {
    for (sema.symbols.items(.sym)) |*sym| {
        sym.deinit(allocator);
    }
    sema.symbols.deinit(allocator);
    for (sema.symbol_map.values()) |*module_symbols| {
        module_symbols.deinit(allocator);
    }
    sema.symbol_map.deinit(allocator);

    sema.errors.deinit(allocator);
}

pub const AnalyzeError = error{AnalyzeFailed} || std.mem.Allocator.Error;

fn gatherSymbols(sema: *Sema, module_idx: u32) AnalyzeError!void {
    const module = &sema.modules[module_idx];
    const ast = &module.ast;

    const range = ast.node_data[Ast.root_node].sub_range;
    for (range.extra_start..range.extra_end) |extra_idx| {
        const node_idx = ast.extra_data[extra_idx];
        const token_idx = ast.node_tokens[node_idx];

        if (ast.node_tags[node_idx] == .module) {
            module.name = ast.parseIdentifier(token_idx);
            continue;
        }

        const sym_loc: SymbolLocation = .{
            .module = module.name.?,
            .name = ast.parseIdentifier(token_idx + 1),
        };

        if (sema.lookupSymbol(sym_loc)) |existing_sym| {
            try sema.errors.append(sema.allocator, .{
                .tag = .duplicate_symbol,
                .ast = ast,
                .token = token_idx + 1,
            });
            try sema.errors.append(sema.allocator, .{
                .tag = .existing_symbol,
                .is_note = true,
                .ast = ast,
                .token = ast.node_tokens[existing_sym.common().node],
            });

            return;
        }

        const result = switch (ast.node_tags[node_idx]) {
            .fn_def => sema.gatherFunctionSymbol(sym_loc, module_idx, @intCast(node_idx)),
            .const_def => sema.gatherConstantSymbol(sym_loc, module_idx, @intCast(node_idx)),
            .var_def => sema.gatherVariableSymbol(sym_loc, module_idx, @intCast(node_idx)),
            .enum_def => sema.gatherEnumSymbol(sym_loc, module_idx, @intCast(node_idx)),
            else => unreachable,
        };
        result catch |err| switch (err) {
            error.AnalyzeFailed => {
                std.log.err("Failed to gather symbol of {}", .{sym_loc});
                continue;
            },
            else => |e| return e,
        };
    }
}

fn gatherFunctionSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: u32, node_idx: NodeIndex) !void {
    const ast = &sema.modules[module_idx].ast;
    const fn_def = ast.node_data[node_idx].fn_def;
    const data = ast.extraData(Ast.Node.FnDefData, fn_def.extra);

    const bank = if (data.bank_attr == Ast.null_node)
        memory_map.mapRealRomBank(sema.mapping_mode, 0x00)
    else
        memory_map.mapRealRomBank(sema.mapping_mode, try sema.parseInt(u8, ast, ast.node_tokens[data.bank_attr]));

    if (bank == null) {
        std.debug.assert(data.bank_attr != Ast.null_node);
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_rom_bank,
            .ast = ast,
            .token = data.bank_attr,
        });
        return error.AnalyzeFailed;
    }

    const fn_token = ast.node_tokens[node_idx];

    try sema.createSymbol(sym_loc, .{
        .function = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            },
            .bank = bank.?,

            // To be determined during analyzation
            .ir = &.{},
            .labels = &.{},
            .instructions = &.{},
            .assembly_data = &.{},
        },
    });

    // Handle interrupt vectors
    inline for (std.meta.fields(InterruptVectors), 0..) |field, i| {
        if (std.mem.eql(u8, "@" ++ field.name, sym_loc.name)) {
            if (@field(sema.interrupt_vectors, field.name)) |existing_loc| {
                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_vector,
                    .ast = ast,
                    .token = fn_token + 1,
                    .extra = .{ .vector = @enumFromInt(i) },
                });

                const existing_sym = sema.lookupSymbol(existing_loc).?;
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_symbol,
                    .is_note = true,
                    .ast = ast,
                    .token = ast.node_tokens[existing_sym.common().node],
                });
            } else {
                @field(sema.interrupt_vectors, field.name) = sym_loc;
            }

            // Enforce bank $00
            if (bank != memory_map.getRealRomBank(sema.mapping_mode, 0x00)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .invalid_vector_bank,
                    .ast = ast,
                    .token = ast.node_tokens[data.bank_attr],
                    .extra = .{ .bank = .{
                        .fn_name = fn_token + 1,
                        .actual = bank.?,
                    } },
                });
            }

            return;
        }
    }

    // Only interrupt vectors are allowed to start with an @
    if (std.mem.startsWith(u8, sym_loc.name, "@")) {
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_vector_name,
            .ast = ast,
            .token = fn_token + 1,
        });
    }
}
fn gatherConstantSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, node_idx: NodeIndex) !void {
    const ast = &sema.modules[module_idx].ast;
    const const_def = ast.node_data[node_idx].const_def;
    const data = ast.extraData(Ast.Node.ConstDefData, const_def.extra);

    const bank = if (data.bank_attr == Ast.null_node)
        memory_map.mapRealRomBank(sema.mapping_mode, 0x00)
    else
        memory_map.mapRealRomBank(sema.mapping_mode, try sema.parseInt(u8, ast, ast.node_tokens[data.bank_attr]));

    if (bank == null) {
        std.debug.assert(data.bank_attr != Ast.null_node);
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_rom_bank,
            .ast = ast,
            .token = data.bank_attr,
        });
        return error.AnalyzeFailed;
    }

    try sema.createSymbol(sym_loc, .{
        .constant = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            },
            .bank = bank.?,

            // To be determined during analyzation
            .type = undefined,
            .value = undefined,
        },
    });
}
fn gatherVariableSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, node_idx: NodeIndex) !void {
    const ast = &sema.modules[module_idx].ast;
    const var_def = ast.node_data[node_idx].var_def;
    const data = ast.extraData(Ast.Node.VarDefData, var_def.extra);

    const bank: u8 = if (data.bank_attr == Ast.null_node)
        0x00
    else
        try sema.parseInt(u8, ast, ast.node_tokens[data.bank_attr]);

    // Choose addr-range default based on bank
    const offset_min: u17, const offset_max: u17 = switch (bank) {
        // LoRAM mirrors (avoid zero-page)
        0x00...0x3F, 0x80...0xBF => .{ 0x00100, 0x01fff },
        // LoRAM + HiRAM (avoid shared LoRAM)
        0x7E => .{ 0x02000, 0x0ffff },
        // ExHiRAM
        0x7F => .{ 0x10000, 0x1ffff },
        else => {
            std.debug.assert(data.bank_attr != Ast.null_node);
            try sema.errors.append(sema.allocator, .{
                .tag = .invalid_ram_bank,
                .ast = ast,
                .token = data.bank_attr,
            });
            return error.AnalyzeFailed;
        },
    };

    try sema.createSymbol(sym_loc, .{
        .variable = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            },

            .wram_offset_min = offset_min,
            .wram_offset_max = offset_max,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}
fn gatherEnumSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, node_idx: NodeIndex) !void {
    const ast = &sema.modules[module_idx].ast;
    const enum_def = ast.node_data[node_idx].enum_def;
    const data = ast.extraData(Ast.Node.EnumDefData, enum_def.extra);
    _ = data; // autofix

    try sema.createSymbol(sym_loc, .{
        .@"enum" = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            },

            // To be determined during analyzation
            .backing_type = undefined,
            .fields = &.{},
        },
    });
}

/// Recursivly analyzes the specified symbol
pub fn analyzeSymbol(sema: *Sema, sym: *Symbol, sym_loc: SymbolLocation, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!void {
    var common = sym.common();
    if (common.analyze_status == .done) {
        return; // Nothing to do
    }
    if (common.analyze_status == .active) {
        try sema.errors.append(sema.allocator, .{
            .tag = .dependency_loop,
            .ast = &sema.modules[module_idx].ast,
            .token = token_idx,
        });
        return error.AnalyzeFailed;
    }

    common.analyze_status = .active;
    defer common.analyze_status = .done;

    return switch (sym.*) {
        .function => |*fn_sym| sema.analyzeFunction(fn_sym, sym_loc, module_idx),
        .constant => |*const_sym| sema.analyzeConstant(const_sym, sym_loc, module_idx),
        .variable => |*var_sym| sema.analyzeVariable(var_sym, sym_loc, module_idx),
        .@"enum" => |*enum_sym| sema.analyzeEnum(enum_sym, sym_loc, module_idx),
    };
}

fn analyzeFunction(sema: *Sema, fn_sym: *Symbol.Function, sym_loc: SymbolLocation, module_idx: ModuleIndex) AnalyzeError!void {
    // Mark as done early, to prevent recursion counting as a dependency loop
    fn_sym.common.analyze_status = .done;

    var analyzer: FunctionAnalyzer = .{
        .ast = &sema.modules[module_idx].ast,
        .sema = sema,
        .symbol = fn_sym,
        .symbol_location = sym_loc,
    };
    errdefer {
        for (analyzer.ir.items) |ir| {
            ir.deinit(sema.allocator);
        }
        analyzer.ir.deinit(sema.allocator);
    }

    try analyzer.handleFnDef(fn_sym.common.node);

    std.log.debug("IR for {s}::{s}", .{ sym_loc.module, sym_loc.name });
    for (analyzer.ir.items) |ir| {
        std.log.debug(" - {}", .{ir});
    }
    fn_sym.ir = try analyzer.ir.toOwnedSlice(sema.allocator);
}
fn analyzeConstant(sema: *Sema, const_sym: *Symbol.Constant, sym_loc: SymbolLocation, module_idx: ModuleIndex) AnalyzeError!void {
    const ast = &sema.modules[module_idx].ast;
    const const_def = ast.node_data[const_sym.common.node].const_def;
    const data = ast.extraData(Ast.Node.ConstDefData, const_def.extra);

    const_sym.type = try sema.resolveType(ast, data.type, sym_loc.module);
    const_sym.value = try sema.resolveExprValue(ast, const_def.value, const_sym.type, sym_loc.module);
}
fn analyzeVariable(sema: *Sema, var_sym: *Symbol.Variable, sym_loc: SymbolLocation, module_idx: ModuleIndex) AnalyzeError!void {
    const ast = &sema.modules[module_idx].ast;
    const var_def = ast.node_data[var_sym.common.node].var_def;
    const data = ast.extraData(Ast.Node.VarDefData, var_def.extra);

    var_sym.type = try sema.resolveType(ast, data.type, sym_loc.module);
}
fn analyzeEnum(sema: *Sema, enum_sym: *Symbol.Enum, sym_loc: SymbolLocation, module_idx: ModuleIndex) AnalyzeError!void {
    const ast = &sema.modules[module_idx].ast;
    const enum_def = ast.node_data[enum_sym.common.node].enum_def;
    const data = ast.extraData(Ast.Node.EnumDefData, enum_def.extra);

    enum_sym.backing_type = try sema.resolveType(ast, data.backing_type, sym_loc.module);
    // TODO: Validate integer type

    var fields: std.ArrayListUnmanaged(Symbol.Enum.Field) = .empty;
    defer fields.deinit(sema.allocator);

    const member_range = ast.node_data[enum_def.block].sub_range;
    for (member_range.extra_start..member_range.extra_end) |extra_idx| {
        const member_idx = ast.extra_data[extra_idx];

        switch (ast.node_tags[member_idx]) {
            .enum_field => {
                const field_data = ast.node_data[member_idx].enum_field;
                const ident_name = ast.node_tokens[member_idx];

                try fields.append(sema.allocator, .{
                    .name = ast.parseIdentifier(ident_name),
                    .value = try sema.resolveExprValue(ast, field_data.value, enum_sym.backing_type, sym_loc.module),
                });
            },
            else => unreachable,
        }
    }

    enum_sym.fields = try fields.toOwnedSlice(sema.allocator);
}

const primitives: std.StaticStringMap(TypeSymbol) = .initComptime(.{});
const max_int_bitwidth = 64;

/// Resolves a TypeExpr-node into the associated `TypeSymbol`
pub fn resolveType(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, current_module: []const u8) !TypeSymbol {
    switch (ast.node_tags[node_idx]) {
        .type_ident => {
            const token_idx = ast.node_tokens[node_idx];
            const ident_name = ast.tokenSource(token_idx);

            if (primitives.get(ident_name)) |primitive| {
                return primitive;
            }
            if (ident_name[0] == 'i' or ident_name[0] == 'u') {
                var is_int = true;
                for (ident_name[1..]) |c| switch (c) {
                    '0'...'9' => {},
                    else => {
                        is_int = false;
                        break;
                    },
                };

                if (is_int) {
                    const bits = std.fmt.parseInt(u16, ident_name[1..], 10) catch {
                        try sema.errors.append(sema.allocator, .{
                            .tag = .int_bitwidth_too_large,
                            .ast = ast,
                            .token = token_idx,
                        });
                        return error.AnalyzeFailed;
                    };
                    if (bits > max_int_bitwidth) {
                        try sema.errors.append(sema.allocator, .{
                            .tag = .int_bitwidth_too_large,
                            .ast = ast,
                            .token = token_idx,
                        });
                        return error.AnalyzeFailed;
                    }

                    if (ident_name[0] == 'i') {
                        return .{ .raw = .{ .signed_int = bits } };
                    } else {
                        return .{ .raw = .{ .unsigned_int = bits } };
                    }
                }
            }

            const sym, const sym_loc = try sema.resolveSymbol(ast, token_idx, current_module);
            const sym_idx = sema.findSymbolIndex(sym_loc);
            switch (sym.*) {
                .function, .constant, .variable => {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .expected_type_symbol,
                        .ast = ast,
                        .token = token_idx,
                    });
                    return error.AnalyzeFailed;
                },
                .@"enum" => |enum_sym| {
                    return .{ .raw = .{ .@"enum" = if (enum_sym.backing_type.raw == .signed_int)
                        .{ .is_signed = true, .bits = enum_sym.backing_type.raw.signed_int, .symbol_index = sym_idx }
                    else
                        .{ .is_signed = false, .bits = enum_sym.backing_type.raw.unsigned_int, .symbol_index = sym_idx } } };
                },
            }
        },
        else => unreachable,
    }
}

/// Resolves a `name` or `module::name` into the associated `Symbol`
pub fn resolveSymbol(sema: *Sema, ast: *const Ast, token_idx: Ast.TokenIndex, current_module: []const u8) AnalyzeError!struct { *Symbol, SymbolLocation } {
    const sym_loc = try sema.parseSymbolLocation(ast, token_idx, current_module);

    if (sema.lookupSymbol(sym_loc)) |sym| {
        // Ensure symbol is analyzed
        try sema.analyzeSymbol(sym, sym_loc, token_idx, sema.findSymbolModuleIndex(sym_loc));

        return .{ sym, sym_loc };
    }

    try sema.errors.append(sema.allocator, .{
        .tag = .undefined_symbol,
        .ast = ast,
        .token = token_idx,
    });
    return error.AnalyzeFailed;
}

// Helper functions

/// Resolves an expression into a parsed value
pub fn resolveExprValue(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, target_type: TypeSymbol, current_module: []const u8) !ExpressionValue {
    switch (ast.node_tags[node_idx]) {
        .expr_ident => {
            const source_symbol_ident = ast.node_tokens[node_idx];
            const source_symbol, const source_symbol_loc = try sema.resolveSymbol(ast, source_symbol_ident, current_module);

            const source_type = switch (source_symbol.*) {
                .constant => |const_sym| const_sym.type,
                .variable => |var_sym| var_sym.type,
                else => {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .expected_const_var_symbol,
                        .ast = ast,
                        .token = source_symbol_ident,
                        .extra = .{ .actual_symbol = source_symbol.* },
                    });
                    return error.AnalyzeFailed;
                },
            };

            // Validate type
            if (!source_type.isAssignableTo(target_type)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .expected_type,
                    .ast = ast,
                    .token = source_symbol_ident,
                    .extra = .{ .expected_type = .{ .expected = target_type, .actual = source_type } },
                });
                return error.AnalyzeFailed;
            }

            return .{ .symbol = source_symbol_loc };
        },
        .expr_int_value => {
            const value_lit = ast.node_tokens[node_idx];
            const value = try sema.parseInt(TypeSymbol.ComptimeIntValue, ast, value_lit);
            const value_type: TypeSymbol = .{ .raw = .{ .comptime_int = value } };

            if (!value_type.isAssignableTo(target_type)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .expected_type,
                    .ast = ast,
                    .token = value_lit,
                    .extra = .{ .expected_type = .{
                        .expected = target_type,
                        .actual = .{ .raw = .{ .comptime_int = value } },
                    } },
                });
                return error.AnalyzeFailed;
            }

            // Validate size
            if (target_type.raw != .comptime_int) {
                const is_signed = target_type.raw == .signed_int;
                const bits = switch (target_type.raw) {
                    .signed_int => |bits| bits,
                    .unsigned_int => |bits| bits,
                    .comptime_int => unreachable,
                    .@"enum" => |backing_type| backing_type.bits,
                };

                const min: TypeSymbol.ComptimeIntValue = if (bits == 0) 0 else -(@as(TypeSymbol.ComptimeIntValue, 1) << @intCast(bits - 1));
                const max: TypeSymbol.ComptimeIntValue = if (bits == 0) 0 else (@as(TypeSymbol.ComptimeIntValue, 1) << @intCast(bits - @intFromBool(is_signed))) -% 1;

                if (value < min or value > max) {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .value_too_large,
                        .ast = ast,
                        .token = value_lit,
                        .extra = .{ .int_size = .{
                            .is_signed = is_signed,
                            .bits = bits,
                        } },
                    });
                    return error.AnalyzeFailed;
                }

                return .{ .immediate = value };
            }

            return .{ .immediate = value };
        },
        .expr_enum_value => {
            const value_lit = ast.node_tokens[node_idx];
            const literal_name = ast.tokenSource(value_lit)[1..];

            switch (target_type) {
                .raw => |payload| switch (payload) {
                    .signed_int, .unsigned_int, .comptime_int => {
                        try sema.errors.append(sema.allocator, .{
                            .tag = .expected_enum_field_or_decl,
                            .ast = ast,
                            .token = value_lit,
                            .extra = .{ .expected_enum_field_or_decl = target_type },
                        });
                        return error.AnalyzeFailed;
                    },
                    .@"enum" => |enum_type_sym| {
                        const enum_sym = sema.symbols.items(.sym)[enum_type_sym.symbol_index].@"enum";

                        for (enum_sym.fields) |field| {
                            if (std.mem.eql(u8, field.name, literal_name)) {
                                return field.value;
                            }
                        }

                        // Not found
                        try sema.errors.append(sema.allocator, .{
                            .tag = .expected_enum_field_or_decl,
                            .ast = ast,
                            .token = value_lit,
                            .extra = .{ .expected_enum_field_or_decl = target_type },
                        });
                        return error.AnalyzeFailed;
                    },
                },
            }
            unreachable;
        },
        else => unreachable,
    }
}

fn createSymbol(sema: *Sema, sym_loc: SymbolLocation, sym: Symbol) !void {
    const gop = try sema.symbol_map.getOrPut(sema.allocator, sym_loc.module);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{};
    }
    try gop.value_ptr.put(sema.allocator, sym_loc.name, @intCast(sema.symbols.len));

    try sema.symbols.append(sema.allocator, .{ .sym = sym, .loc = sym_loc });
}

/// Searches for the specified symbol
pub fn lookupSymbol(sema: Sema, sym_loc: SymbolLocation) ?*Symbol {
    if (sema.symbol_map.get(sym_loc.module)) |module_symbols| {
        if (module_symbols.get(sym_loc.name)) |sym_idx| {
            return &sema.symbols.items(.sym)[sym_idx];
        }
    }
    return null;
}

/// Checks if the symbol is accessible in the specified bank
pub fn isSymbolAccessibleInBank(sema: Sema, sym: Symbol, bank: u8) bool {
    return switch (sym) {
        .function,
        => |fn_sym| memory_map.isAddressAccessibleInBank(sema.mapping_mode, memory_map.bankOffsetToAddr(sema.mapping_mode, fn_sym.bank, 0), bank),
        .constant,
        => |const_sym| memory_map.isAddressAccessibleInBank(sema.mapping_mode, memory_map.bankOffsetToAddr(sema.mapping_mode, const_sym.bank, 0), bank),
        .variable,
        => |var_sym| memory_map.isAddressAccessibleInBank(sema.mapping_mode, memory_map.wramOffsetToAddr(var_sym.wram_offset_min), bank) and
            memory_map.isAddressAccessibleInBank(sema.mapping_mode, memory_map.wramOffsetToAddr(var_sym.wram_offset_max), bank),
        .@"enum" => unreachable,
    };
}

/// Finds the `SymbolIndex` of the specified symbol location
pub fn findSymbolIndex(sema: Sema, sym_loc: SymbolLocation) SymbolIndex {
    for (sema.symbols.items(.loc), 0..) |other_sym_loc, i| {
        if (std.mem.eql(u8, sym_loc.name, other_sym_loc.name) and std.mem.eql(u8, sym_loc.module, other_sym_loc.module)) {
            return @intCast(i);
        }
    }
    unreachable;
}
/// Finds the `ModuleIndex` of the specified symbol location
pub fn findSymbolModuleIndex(sema: Sema, sym_loc: SymbolLocation) ModuleIndex {
    for (sema.modules, 0..) |module, module_idx| {
        if (std.mem.eql(u8, module.name.?, sym_loc.module)) {
            return @intCast(module_idx);
        }
    }
    unreachable;
}

/// Tries parsing an integer and reports an error on failure
pub fn parseInt(sema: *Sema, comptime T: type, ast: *const Ast, token_idx: Ast.TokenIndex) AnalyzeError!T {
    return ast.parseIntLiteral(T, token_idx) catch |err| {
        try sema.errors.append(sema.allocator, .{
            .tag = switch (err) {
                error.Overflow => .value_too_large,
                error.InvalidCharacter => .invalid_number,
            },
            .ast = ast,
            .token = token_idx,
            .extra = .{ .int_size = .{
                .is_signed = @typeInfo(T).int.signedness == .signed,
                .bits = @typeInfo(T).int.bits,
            } },
        });
        return error.AnalyzeFailed;
    };
}

/// Parse either a `name` or `module::name` symbol location
pub fn parseSymbolLocation(sema: *Sema, ast: *const Ast, token_idx: Ast.TokenIndex, current_module: []const u8) AnalyzeError!SymbolLocation {
    if (ast.token_tags[token_idx] != .ident and ast.token_tags[token_idx] != .builtin_ident) {
        try sema.errors.append(sema.allocator, .{
            .tag = .expected_token,
            .ast = ast,
            .token = token_idx,
            .extra = .{ .expected_token = .ident },
        });
        return error.AnalyzeFailed;
    }

    if (ast.token_tags[token_idx + 1] == .double_colon) {
        if (ast.token_tags[token_idx + 2] != .ident) {
            try sema.errors.append(sema.allocator, .{
                .tag = .expected_token,
                .ast = ast,
                .token = token_idx + 2,
                .extra = .{ .expected_token = .ident },
            });
            return error.AnalyzeFailed;
        }

        return .{
            .module = ast.tokenSource(token_idx),
            .name = ast.tokenSource(token_idx + 2),
        };
    } else {
        return .{
            .module = current_module,
            .name = ast.tokenSource(token_idx),
        };
    }
}

pub fn expectToken(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, tag: Token.Tag) AnalyzeError!Ast.TokenIndex {
    const token_idx = ast.node_tokens[node_idx];

    if (ast.token_tags[token_idx] != tag) {
        try sema.errors.append(sema.allocator, .{
            .tag = .expected_token,
            .ast = ast,
            .token = token_idx,
            .extra = .{ .expected_token = tag },
        });
        return error.AnalyzeFailed;
    }

    return token_idx;
}

// Error handling

pub const Error = struct {
    pub const Tag = enum {
        // Extra: node
        duplicate_symbol,
        duplicate_label,
        existing_symbol,
        existing_label,
        undefined_symbol,
        undefined_label,
        missing_reset_vector,
        invalid_vector_name,
        invalid_opcode,
        invalid_builtin,
        invalid_rom_bank,
        invalid_ram_bank,
        invalid_size_mode,
        invalid_register,
        int_bitwidth_too_large,
        expected_intermediate_register,
        dependency_loop,
        // Extra: vector
        duplicate_vector,
        // Extra: bank
        invalid_vector_bank,
        // Extra: size_type
        undefined_size_mode,
        // Extra: int_size
        value_too_large,
        invalid_number,
        // Extra: arguments
        expected_arguments,
        // Extra: expected_token
        expected_token,
        // Extra: expected_type
        expected_type,
        // Extra: actual_symbol
        expected_var_symbol,
        expected_const_var_symbol,
        expected_fn_symbol,
        expected_type_symbol,
        // Extra: expected_register_size
        expected_register_size,
        // Extra: expected_enum_field_or_decl
        expected_enum_field_or_decl,
        // Extra: unsupported_register
        unsupported_register,
    };

    tag: Tag,

    /// Notes are associated with the previous error
    is_note: bool = false,

    ast: *const Ast,
    token: Ast.TokenIndex,

    extra: union {
        none: void,
        vector: std.meta.FieldEnum(InterruptVectors),
        bank: struct {
            fn_name: Ast.TokenIndex,
            actual: u8,
        },
        size_type: Instruction.SizeType,
        int_size: struct {
            is_signed: bool,
            bits: u16,
        },
        arguments: struct {
            expected: u8,
            actual: u8,
        },
        expected_token: Token.Tag,
        expected_type: struct {
            expected: TypeSymbol,
            actual: TypeSymbol,
        },
        actual_symbol: std.meta.Tag(Symbol),
        expected_register_size: struct {
            expected: u16,
            actual: u16,
        },
        expected_enum_field_or_decl: TypeSymbol,
        unsupported_register: struct {
            register: RegisterType,
            message: []const u8,
        },
    } = .{ .none = {} },
};

pub fn detectErrors(sema: Sema, writer: std.fs.File.Writer, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (sema.errors.items) |err| {
        if (!err.is_note) {
            try writer.writeByte('\n');
        }

        if (err.token != Ast.null_token) {
            const token_loc = err.ast.token_locs[err.token];
            const src_loc = std.zig.findLineColumn(err.ast.source, token_loc.start);

            const args = .{ err.ast.source_path, src_loc.line + 1, src_loc.column + 1 };
            if (err.is_note) {
                try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", args);
            } else {
                try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", args);
            }

            try sema.renderError(writer, tty_config, err);
            try writer.writeByte('\n');

            try writer.writeAll(src_loc.source_line);
            try writer.writeByte('\n');

            try tty_config.setColor(writer, .green);
            try writer.writeByteNTimes(' ', src_loc.column);
            try writer.writeByte('^');
            try writer.writeByteNTimes('~', token_loc.end - token_loc.start - 1);
            try tty_config.setColor(writer, .reset);
            try writer.writeByte('\n');
        } else {
            if (err.is_note) {
                try rich.print(writer, tty_config, "[bold cyan]note: ", .{});
            } else {
                try rich.print(writer, tty_config, "[bold red]error: ", .{});
            }

            try sema.renderError(writer, tty_config, err);
            try writer.writeByte('\n');
        }
    }

    return sema.errors.items.len != 0;
}
fn renderError(sema: Sema, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    const highlight = "bold bright_magenta";

    return switch (err.tag) {
        .duplicate_symbol => rich.print(writer, tty_config, "Found duplicate symbol [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .duplicate_label => rich.print(writer, tty_config, "Found duplicate label [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .existing_symbol => writer.writeAll("Symbol already defined here"),
        .existing_label => writer.writeAll("Label already defiend here"),
        .undefined_symbol => if (err.ast.token_tags[err.token + 1] == .double_colon)
            rich.print(writer, tty_config, "Use of undefined symbol [" ++ highlight ++ "]{s}::{s}", .{ err.ast.tokenSource(err.token), err.ast.tokenSource(err.token + 2) })
        else
            rich.print(writer, tty_config, "Use of undefined symbol [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .undefined_label => rich.print(writer, tty_config, "Use of undefined label [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .missing_reset_vector => rich.print(writer, tty_config, "Interrupt vector [" ++ highlight ++ "]@emulation_reset [reset]is not defined", .{}),
        .invalid_vector_name => rich.print(writer, tty_config, "Unknown interrupt vector [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_opcode => rich.print(writer, tty_config, "Invalid instruction opcode [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_builtin => rich.print(writer, tty_config, "Invalid built-in function [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_rom_bank => rich.print(writer, tty_config, "Invalid ROM bank [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_ram_bank => rich.print(writer, tty_config, "Invalid RAM bank [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_size_mode => rich.print(writer, tty_config, "Invalid size-mode [" ++ highlight ++ "]{s}[reset], expected [" ++ highlight ++ "]8 [reset]or [" ++ highlight ++ "]16", .{err.ast.tokenSource(err.token)}),
        .invalid_register => rich.print(writer, tty_config, "Invalid register type [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .int_bitwidth_too_large => rich.print(writer, tty_config, "Bit-width of primitive integer type [" ++ highlight ++ "]{s}[reset] exceedes maximum bit-width of [" ++ highlight ++ "]{d}", .{
            err.ast.tokenSource(err.token),
            max_int_bitwidth,
        }),
        .expected_intermediate_register => rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]intermediate register[reset], to hold [" ++ highlight ++ "]non-zero [reset]or [" ++ highlight ++ "]non-register[reset] values", .{err.ast.tokenSource(err.token)}),
        .dependency_loop => rich.print(writer, tty_config, "Detected a [" ++ highlight ++ "]dependency loop", .{}),

        .duplicate_vector => rich.print(writer, tty_config, "Found duplicate interrupt vector [" ++ highlight ++ "]{s}", .{@tagName(err.extra.vector)}),

        .invalid_vector_bank => rich.print(
            writer,
            tty_config,
            "Expected interrupt vector [" ++ highlight ++ "]{s} [reset]to be located in bank [" ++ highlight ++ "]${x}[reset], but got [" ++ highlight ++ "]${x}",
            .{ err.ast.tokenSource(err.extra.bank.fn_name), memory_map.getRealRomBank(sema.mapping_mode, 0x00), err.extra.bank.actual },
        ),

        .undefined_size_mode => rich.print(writer, tty_config, "Size-mode is undefined for [" ++ highlight ++ "]{s}", .{switch (err.extra.size_type) {
            .none => unreachable,
            .mem => "A-Register / Memory access",
            .idx => "X/Y-Registers",
        }}),

        .value_too_large => rich.print(
            writer,
            tty_config,
            "The value [" ++ highlight ++ "]{!} [reset]is too large to fit into {s} [" ++ highlight ++ "]{s} {}-bit number",
            .{
                err.ast.parseIntLiteral(u64, err.token),
                if (err.extra.int_size.is_signed) "a" else "an",
                if (err.extra.int_size.is_signed) "signed" else "unsigned",
                err.extra.int_size.bits,
            },
        ),
        .invalid_number => rich.print(
            writer,
            tty_config,
            "The value [" ++ highlight ++ "]{s} [reset]is not a valid [" ++ highlight ++ "]{s} {}-bit number",
            .{ err.ast.tokenSource(err.token), if (err.extra.int_size.is_signed) "signed" else "unsigned", err.extra.int_size.bits },
        ),

        .expected_arguments => rich.print(
            writer,
            tty_config,
            "Expected [" ++ highlight ++ "]{d} argument{s}[reset], found [" ++ highlight ++ "]{d}",
            .{ err.extra.arguments.expected, if (err.extra.arguments.expected == 1) "" else "s", err.extra.arguments.actual },
        ),

        .expected_token => rich.print(writer, tty_config, "Expected [" ++ highlight ++ "]{s}[reset], found [" ++ highlight ++ "]{s}", .{
            err.extra.expected_token.symbol(),
            err.ast.token_tags[err.token].symbol(),
        }),

        .expected_type => rich.print(writer, tty_config, "Expected value of type [" ++ highlight ++ "]{}[reset], found [" ++ highlight ++ "]{}", .{
            err.extra.expected_type.expected,
            err.extra.expected_type.actual,
        }),

        .expected_var_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]variable[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_const_var_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]constant or variable[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_fn_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]function[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_type_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]type[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),

        .expected_register_size => rich.print(writer, tty_config, "Expected register size to evenly divide [" ++ highlight ++ "]{d}-bit value[reset], found [" ++ highlight ++ "]{d}-bit register", .{
            err.extra.expected_register_size.expected,
            err.extra.expected_register_size.actual,
        }),

        .expected_enum_field_or_decl => rich.print(writer, tty_config, "Could not find an [" ++ highlight ++ "]enum field[reset] or a [" ++ highlight ++ "]declaration[reset] called [" ++ highlight ++ "]\"{s}\"[reset], on type [" ++ highlight ++ "]{}[reset]", .{
            err.ast.tokenSource(err.token)[1..],
            err.extra.expected_enum_field_or_decl,
        }),

        .unsupported_register => rich.print(writer, tty_config, "Unsupported [" ++ highlight ++ "]intermediate register {s}[reset], for use with [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.unsupported_register.register),
            err.extra.unsupported_register.message,
        }),
    };
}

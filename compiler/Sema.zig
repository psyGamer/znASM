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
                .function, .variable => unreachable,
                .constant => |const_sym| @intCast(const_sym.value.resolve(T, sema)),
            },
            .register => unreachable,
        };
    }
};

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
        // Extra: expected_register_size
        expected_register_size,
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
    } = .{ .none = {} },
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

    // Analyse symbols in the order: constants / variables -> functions,
    // to preserve the dependency graph
    const symbol_slice = sema.symbols.slice();
    const symbols = symbol_slice.items(.sym);
    const locations = symbol_slice.items(.loc);

    // Variables / Constants
    for (symbols, locations) |*sym, loc| switch (sym.*) {
        .constant => {
            const module_idx: ModuleIndex = @intCast(sema.symbol_map.getIndex(loc.module).?);
            sema.analyzeConstant(loc, module_idx, &sym.constant) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze constant {}", .{loc}),
                else => |e| return e,
            };
        },
        .variable => {
            const module_idx: ModuleIndex = @intCast(sema.symbol_map.getIndex(loc.module).?);
            sema.analyzeVariable(loc, module_idx, &sym.variable) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze variable {}", .{loc}),
                else => |e| return e,
            };
        },
        else => {},
    };
    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        sema.deinit(allocator);
        return null;
    }

    // Functions
    for (symbols, locations) |*sym, loc| switch (sym.*) {
        .function => {
            const module_idx: ModuleIndex = @intCast(sema.symbol_map.getIndex(loc.module).?);
            sema.analyzeFunction(loc, module_idx, &sym.function) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze function {}", .{loc}),
                else => |e| return e,
            };
        },
        else => {},
    };
    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        sema.deinit(allocator);
        return null;
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

        switch (ast.node_tags[node_idx]) {
            .module => {
                module.name = ast.parseIdentifier(token_idx);
            },
            .fn_def, .const_def, .var_def => {
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
                        .token = ast.node_tokens[
                            switch (existing_sym.*) {
                                .function => |existing_func| existing_func.node,
                                .constant => |existing_const| existing_const.node,
                                .variable => |existing_var| existing_var.node,
                            }
                        ],
                    });

                    return;
                }

                const result = switch (ast.node_tags[node_idx]) {
                    .fn_def => sema.gatherFunctionSymbol(sym_loc, module_idx, @intCast(node_idx)),
                    .const_def => sema.gatherConstantSymbol(sym_loc, module_idx, @intCast(node_idx)),
                    .var_def => sema.gatherVariableSymbol(sym_loc, module_idx, @intCast(node_idx)),
                    else => unreachable,
                };
                result catch |err| switch (err) {
                    error.AnalyzeFailed => {
                        std.log.err("Failed to gather symbol of {}", .{sym_loc});
                        continue;
                    },
                    else => |e| return e,
                };
            },
            else => unreachable,
        }
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
            .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            .bank = bank.?,
            .node = node_idx,

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
                    .token = ast.node_tokens[
                        switch (existing_sym.*) {
                            .function => |existing_func| existing_func.node,
                            .constant => |existing_const| existing_const.node,
                            .variable => |existing_var| existing_var.node,
                        }
                    ],
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
            .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            .bank = bank.?,
            .node = node_idx,

            // To be determined during analyzation
            .type = undefined,
            .value = undefined,
        },
    });
}
fn gatherVariableSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, node_idx: NodeIndex) !void {
    const ast = &sema.modules[module_idx].ast;
    const const_def = ast.node_data[node_idx].var_def;
    const data = ast.extraData(Ast.Node.ConstDefData, const_def.extra);

    const bank: u8 = if (data.bank_attr == Ast.null_node)
        0x00
    else
        try sema.parseInt(u8, ast, ast.node_tokens[data.bank_attr]);

    // Choose addr-range default based on bank, but don't implictly put variables into the zeropage
    const offset_min: u17, const offset_max: u17 = switch (bank) {
        // LoRAM mirrors
        0x00...0x3F, 0x80...0xBF => .{ 0x00100, 0x01fff },
        // LoRAM + HiRAM
        0x7E => .{ 0x00100, 0x0ffff },
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
            .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            .node = node_idx,

            .wram_offset_min = offset_min,
            .wram_offset_max = offset_max,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}

fn analyzeFunction(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, func_sym: *Symbol.Function) AnalyzeError!void {
    var analyzer: @import("sema/FunctionAnalyzer.zig") = .{
        .ast = &sema.modules[module_idx].ast,
        .sema = sema,
        .symbol = func_sym,
        .symbol_location = sym_loc,
    };
    errdefer {
        for (analyzer.ir.items) |ir| {
            ir.deinit(sema.allocator);
        }
        analyzer.ir.deinit(sema.allocator);
    }

    try analyzer.handleFnDef(func_sym.node);

    std.log.debug("IR for {s}::{s}", .{ sym_loc.module, sym_loc.name });
    for (analyzer.ir.items) |ir| {
        std.log.debug(" - {}", .{ir});
    }
    func_sym.ir = try analyzer.ir.toOwnedSlice(sema.allocator);
}
fn analyzeConstant(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, const_sym: *Symbol.Constant) AnalyzeError!void {
    const ast = &sema.modules[module_idx].ast;
    const const_def = ast.node_data[const_sym.node].const_def;
    const data = ast.extraData(Ast.Node.ConstDefData, const_def.extra);

    const_sym.type = try sema.resolveType(ast, data.type, sym_loc.module);
    const_sym.value = try sema.resolveExprValue(ast, const_def.value, const_sym.type, sym_loc.module);
}
fn analyzeVariable(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, var_sym: *Symbol.Variable) AnalyzeError!void {
    const ast = &sema.modules[module_idx].ast;
    const var_def = ast.node_data[var_sym.node].var_def;
    const data = ast.extraData(Ast.Node.VarDefData, var_def.extra);

    var_sym.type = try sema.resolveType(ast, data.type, sym_loc.module);
}

const primitives: std.StaticStringMap(TypeSymbol) = .initComptime(.{});
// const max_int_bitwidth = @bitSizeOf(TypeSymbol.ComptimeIntValue) - 1;
const max_int_bitwidth = 65535;

fn resolveType(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, current_module: []const u8) !TypeSymbol {
    _ = current_module; // autofix
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

            // TODO: Resolve custom types
            try sema.errors.append(sema.allocator, .{
                .tag = .undefined_symbol,
                .ast = ast,
                .token = token_idx,
            });
            return error.AnalyzeFailed;
        },
        else => unreachable,
    }
}

// Helper functions

/// Resolves an expression into a parsed value
pub fn resolveExprValue(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, target_type: TypeSymbol, current_module: []const u8) !ExpressionValue {
    _ = current_module; // autofix
    switch (ast.node_tags[node_idx]) {
        .expr_ident => {
            @panic("TODO");
        },
        .expr_value => {
            const value_ident = ast.node_tokens[node_idx];
            const value = try sema.parseInt(TypeSymbol.ComptimeIntValue, ast, value_ident);

            if (target_type != .raw or (target_type.raw != .signed_int and target_type.raw != .unsigned_int and target_type.raw != .comptime_int)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .expected_type,
                    .ast = ast,
                    .token = value_ident,
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
                _ = is_signed; // autofix
                const bits = switch (target_type.raw) {
                    .signed_int => |bits| bits,
                    .unsigned_int => |bits| bits,
                    .comptime_int => unreachable,
                };
                _ = bits; // autofix

                // const min: TypeSymbol.ComptimeIntValue = if (bits == 0) 0 else -(@as(TypeSymbol.ComptimeIntValue, 1) << @intCast(bits - 1));
                // const max: TypeSymbol.ComptimeIntValue = if (bits == 0) 0 else (@as(TypeSymbol.ComptimeIntValue, 1) << @intCast(bits - @intFromBool(is_signed))) -% 1;

                // if (value < min or value > max) {
                //     try sema.errors.append(sema.allocator, .{
                //         .tag = .value_too_large,
                //         .ast = ast,
                //         .token = value_ident,
                //         .extra = .{ .int_size = .{
                //             .is_signed = is_signed,
                //             .bits = bits,
                //         } },
                //     });
                //     return error.AnalyzeFailed;
                // }

                return .{ .immediate = value };
            }

            return .{ .immediate = value };
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
    };
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
    if (ast.token_tags[token_idx] != .ident) {
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
/// Resolves a `name` or `module::name` into the associated symbol
pub fn resolveSymbolLocation(sema: *Sema, ast: *const Ast, token_idx: Ast.TokenIndex, current_module: []const u8) AnalyzeError!struct { *Symbol, SymbolLocation } {
    const sym_loc = try sema.parseSymbolLocation(ast, token_idx, current_module);

    if (sema.lookupSymbol(sym_loc)) |sym|
        return .{ sym, sym_loc };

    try sema.errors.append(sema.allocator, .{
        .tag = .undefined_symbol,
        .ast = ast,
        .token = token_idx,
    });
    return error.AnalyzeFailed;
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

        .expected_type => rich.print(writer, tty_config, "Expected value of type [" ++ highlight ++ "]{s}[reset], found [" ++ highlight ++ "]{s}", .{
            err.extra.expected_type.expected,
            err.extra.expected_type.actual,
        }),

        .expected_var_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]variable[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_const_var_symbol => rich.print(writer, tty_config, "Expected symbol to a[" ++ highlight ++ "]constant or variable[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),

        .expected_register_size => rich.print(writer, tty_config, "Expected register size to evenly divide [" ++ highlight ++ "]{d}-bit value[reset], found [" ++ highlight ++ "]{d}-bit register", .{
            err.extra.expected_register_size.expected,
            err.extra.expected_register_size.actual,
        }),
    };
}

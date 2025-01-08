const std = @import("std");
const memory_map = @import("memory_map.zig");
const rich = @import("util/rich.zig");
const builtin_module = @import("builtin_module.zig");

const Token = @import("Tokenizer.zig").Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Relocation = @import("CodeGen.zig").Relocation;
const Ir = @import("ir.zig").Ir;
const Symbol = @import("symbol.zig").Symbol;
const TypeSymbol = @import("symbol.zig").TypeSymbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const FunctionAnalyzer = @import("sema/FunctionAnalyzer.zig");
const Sema = @This();

pub const SymbolIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    /// Helper function to cast a generic number to a SymbolIndex
    pub inline fn cast(x: anytype) SymbolIndex {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(SymbolIndex), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(SymbolIndex), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a SymbolIndex"),
        };
    }
};
pub const ModuleIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    /// Helper function to cast a generic number to a ModuleIndex
    pub inline fn cast(x: anytype) ModuleIndex {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(std.meta.Tag(ModuleIndex), @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(std.meta.Tag(ModuleIndex), @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a ModuleIndex"),
        };
    }
};

pub const RegisterType = enum { a8, a16, x8, x16, y8, y16 };

/// Represents a parsed value of an expression
pub const ExpressionValue = union(enum) {
    pub const PackedField = struct {
        value: ExpressionValue,
        bit_offset: u8,
        bit_size: u8,
        node_idx: NodeIndex,
    };

    value: TypeSymbol.ComptimeIntValue,
    variable: SymbolIndex,
    packed_fields: []const PackedField,
    register: RegisterType,

    pub fn deinit(expr: ExpressionValue, allocator: std.mem.Allocator) void {
        switch (expr) {
            .value, .variable, .register => {},
            .packed_fields => |fields| {
                for (fields) |field| {
                    field.value.deinit(allocator);
                }
                allocator.free(fields);
            },
        }
    }

    pub fn resolve(expr: ExpressionValue, comptime T: type, sema: *const Sema) T {
        _ = sema; // autofix
        return switch (expr) {
            .value => |value| @intCast(value),
            .variable, .packed_fields, .register => unreachable, // Cannot be determined at compile time
        };
    }
};

const InterruptVectors = struct {
    const Location = struct {
        pub const none: Location = .{
            .module = .none,
            .symbol = .none,
        };

        pub inline fn unpack(loc: Location) ?Location {
            if (loc.module == .none or loc.symbol == .none) {
                return null;
            }
            return loc;
        }
        pub inline fn unpackSymbol(loc: Location) ?SymbolIndex {
            if (loc.symbol == .none) {
                return null;
            }
            return loc.symbol;
        }

        module: ModuleIndex,
        symbol: SymbolIndex,
    };

    native_cop: Location = .none,
    native_brk: Location = .none,
    native_abort: Location = .none,
    native_nmi: Location = .none,
    native_irq: Location = .none,
    emulation_cop: Location = .none,
    emulation_abort: Location = .none,
    emulation_nmi: Location = .none,
    emulation_reset: Location = .none,
    emulation_irqbrk: Location = .none,
};

mapping_mode: MappingMode,

modules: []Module,
module_map: std.StringArrayHashMapUnmanaged(ModuleIndex) = .empty,

symbols: std.ArrayListUnmanaged(Symbol) = .empty,
errors: std.ArrayListUnmanaged(Error) = .empty,

interrupt_vectors: InterruptVectors = .{},

/// Collection of strings which needed to be dynamically allocated
string_pool: std.ArrayListUnmanaged([]u8) = .empty,
allocator: std.mem.Allocator,

pub inline fn getModule(sema: *Sema, index: ModuleIndex) *Module {
    return &sema.modules[@intFromEnum(index)];
}
pub inline fn getSymbol(sema: *Sema, index: SymbolIndex) *Symbol {
    return &sema.symbols.items[@intFromEnum(index)];
}

pub fn process(allocator: std.mem.Allocator, modules: []Module, mapping_mode: MappingMode) !?Sema {
    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    var sema: Sema = .{
        .modules = modules,
        .mapping_mode = mapping_mode,
        .allocator = allocator,
    };
    errdefer sema.deinit();

    // Gather symbols
    try sema.module_map.ensureUnusedCapacity(sema.allocator, modules.len);
    for (0..modules.len) |module_idx| {
        try sema.gatherSymbols(.cast(module_idx));
    }

    // Only reset is required
    if (sema.interrupt_vectors.emulation_reset.unpack() == null) {
        try sema.errors.append(allocator, .{
            .tag = .missing_reset_vector,
            .ast = undefined,
            .token = .none,
        });
    }

    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        sema.deinit();
        return null;
    }

    // Recursivly analayze symbols, starting from the interrupt vectors
    inline for (std.meta.fields(InterruptVectors)) |field| {
        if (@field(sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
            const vector_sym = sema.getSymbol(vector_loc.symbol);

            sema.analyzeSymbol(vector_sym, .none, vector_loc.module) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze symbol {}", .{sema.getSymbolLocation(vector_loc.symbol)}),
                else => |e| return e,
            };

            if (try sema.detectErrors(stderr.writer(), tty_config)) {
                sema.deinit();
                return null;
            }
        }
    }

    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        sema.deinit();
        return null;
    }

    // Also analyze built-in fallback interrupt vector
    const builtin_mod_idx = sema.module_map.get(builtin_module.module_name).?;
    const builtin_mod = sema.getModule(builtin_mod_idx);
    const fallback_vector_sym = sema.getSymbol(builtin_mod.symbol_map.get(builtin_module.empty_vector_name).?);

    // The built-in modules is NEVER allowed to error
    sema.analyzeSymbol(fallback_vector_sym, .none, builtin_mod_idx) catch unreachable;

    return sema;
}

pub fn deinit(sema: *Sema) void {
    sema.module_map.deinit(sema.allocator);
    for (sema.symbols.items) |*symbol| {
        symbol.deinit(sema.allocator);
    }
    sema.symbols.deinit(sema.allocator);
    sema.errors.deinit(sema.allocator);
    for (sema.string_pool.items) |str| {
        sema.allocator.free(str);
    }
    sema.string_pool.deinit(sema.allocator);
}

pub const AnalyzeError = error{AnalyzeFailed} || std.mem.Allocator.Error;

fn gatherSymbols(sema: *Sema, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const range = ast.nodeData(.root).sub_range;
    for (@intFromEnum(range.extra_start)..@intFromEnum(range.extra_end)) |extra_idx| {
        const node_idx: NodeIndex = @enumFromInt(ast.extraData(.cast(extra_idx)));
        const token_idx = ast.nodeToken(node_idx);

        if (ast.nodeTag(node_idx) == .module) {
            std.debug.assert(module.name.len == 0); // Should've been caught while parsing

            module.name = ast.parseIdentifier(token_idx);
            sema.module_map.putAssumeCapacityNoClobber(module.name, module_idx);
            continue;
        }

        std.debug.assert(module.name.len != 0); // Should've been caught while parsing

        const symbol_name = ast.parseIdentifier(token_idx.next());

        if (module.symbol_map.get(symbol_name)) |existing_sym_idx| {
            const existing_sym = sema.getSymbol(existing_sym_idx);

            try sema.errors.append(sema.allocator, .{
                .tag = .duplicate_symbol,
                .ast = ast,
                .token = token_idx.next(),
            });
            try sema.errors.append(sema.allocator, .{
                .tag = .existing_symbol,
                .is_note = true,
                .ast = ast,
                .token = ast.nodeToken(existing_sym.common().node),
            });

            return;
        }

        const result = switch (ast.nodeTag(node_idx)) {
            .fn_def => sema.gatherFunctionSymbol(symbol_name, node_idx, module_idx),
            .const_def => sema.gatherConstantSymbol(symbol_name, node_idx, module_idx),
            .var_def => sema.gatherVariableSymbol(symbol_name, node_idx, module_idx),
            .reg_def => sema.gatherRegisterSymbol(symbol_name, node_idx, module_idx),
            .packed_def => sema.gatherPackedSymbol(symbol_name, node_idx, module_idx),
            .enum_def => sema.gatherEnumSymbol(symbol_name, node_idx, module_idx),
            else => unreachable,
        };
        result catch |err| switch (err) {
            error.AnalyzeFailed => {
                std.log.err("Failed to gather symbol of {s}::{s}", .{ module.name, symbol_name });
                continue;
            },
            else => |e| return e,
        };
    }
}

fn gatherFunctionSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const fn_def = ast.nodeData(node_idx).fn_def;
    const data = ast.readExtraData(Ast.Node.FnDefData, fn_def.extra);

    const bank = if (data.bank_attr == .none)
        memory_map.mapRealRomBank(sema.mapping_mode, 0x00)
    else
        memory_map.mapRealRomBank(sema.mapping_mode, try sema.parseInt(u8, ast, ast.nodeToken(data.bank_attr)));

    if (bank == null) {
        std.debug.assert(data.bank_attr != .none);
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_rom_bank,
            .ast = ast,
            .token = ast.nodeToken(data.bank_attr),
        });
        return error.AnalyzeFailed;
    }

    const fn_token = ast.nodeToken(node_idx);

    try sema.createSymbol(module_idx, symbol_name, .{
        .function = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },
            .bank = bank.?,

            // To be determined during analyzation
            .ir = &.{},
            .labels = &.{},
            .instructions = &.{},
            .assembly_data = &.{},
        },
    });
    const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len - 1);

    // Handle interrupt vectors
    inline for (std.meta.fields(InterruptVectors), 0..) |field, i| {
        if (std.mem.eql(u8, "@" ++ field.name, symbol_name)) {
            if (@field(sema.interrupt_vectors, field.name).unpack()) |existing_loc| {
                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_vector,
                    .ast = ast,
                    .token = fn_token.next(),
                    .extra = .{ .vector = @enumFromInt(i) },
                });

                const existing_sym = sema.getSymbol(existing_loc.symbol);
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_symbol,
                    .is_note = true,
                    .ast = ast,
                    .token = ast.nodeToken(existing_sym.common().node),
                });
            } else {
                @field(sema.interrupt_vectors, field.name) = .{
                    .module = module_idx,
                    .symbol = symbol_idx,
                };
            }

            // Enforce bank $00
            if (bank != memory_map.getRealRomBank(sema.mapping_mode, 0x00)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .invalid_vector_bank,
                    .ast = ast,
                    .token = ast.nodeToken(data.bank_attr),
                    .extra = .{ .bank = .{
                        .fn_name = fn_token.next(),
                        .actual = bank.?,
                    } },
                });
            }

            return;
        }
    }

    // Only interrupt vectors are allowed to start with an @
    if (symbol_name[0] == '@') {
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_vector_name,
            .ast = ast,
            .token = fn_token.next(),
        });
    }
}
fn gatherConstantSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const const_def = ast.nodeData(node_idx).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    const bank = if (data.bank_attr == .none)
        memory_map.mapRealRomBank(sema.mapping_mode, 0x00)
    else
        memory_map.mapRealRomBank(sema.mapping_mode, try sema.parseInt(u8, ast, ast.nodeToken(data.bank_attr)));

    if (bank == null) {
        std.debug.assert(data.bank_attr != .none);
        try sema.errors.append(sema.allocator, .{
            .tag = .invalid_rom_bank,
            .ast = ast,
            .token = ast.nodeToken(data.bank_attr),
        });
        return error.AnalyzeFailed;
    }

    try sema.createSymbol(module_idx, symbol_name, .{
        .constant = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },
            .bank = bank.?,

            // To be determined during analyzation
            .type = undefined,
            .value = undefined,
        },
    });
}
fn gatherVariableSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const var_def = ast.nodeData(node_idx).var_def;
    const data = ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

    const bank: u8 = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast, ast.nodeToken(data.bank_attr));

    // Choose addr-range default based on bank
    const offset_min: u17, const offset_max: u17 = switch (bank) {
        // LoRAM mirrors (avoid zero-page)
        0x00...0x3F, 0x80...0xBF => .{ 0x00100, 0x01fff },
        // LoRAM + HiRAM (avoid shared LoRAM)
        0x7E => .{ 0x02000, 0x0ffff },
        // ExHiRAM
        0x7F => .{ 0x10000, 0x1ffff },
        else => {
            std.debug.assert(data.bank_attr != .none);
            try sema.errors.append(sema.allocator, .{
                .tag = .invalid_ram_bank,
                .ast = ast,
                .token = ast.nodeToken(data.bank_attr),
            });
            return error.AnalyzeFailed;
        },
    };

    try sema.createSymbol(module_idx, symbol_name, .{
        .variable = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },

            .wram_offset_min = offset_min,
            .wram_offset_max = offset_max,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}
fn gatherRegisterSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const reg_def = ast.nodeData(node_idx).reg_def;
    const data = ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

    const access_type = try sema.parseEnum(Symbol.Register.AccessType, ast, ast.nodeToken(data.access_attr));
    const address = try sema.parseInt(u16, ast, data.address);

    try sema.createSymbol(module_idx, symbol_name, .{
        .register = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },

            .access = access_type,
            .address = address,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}
fn gatherPackedSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    try sema.createSymbol(module_idx, symbol_name, .{
        .@"packed" = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .backing_type = undefined,
            .fields = &.{},
        },
    });
}
fn gatherEnumSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    try sema.createSymbol(module_idx, symbol_name, .{
        .@"enum" = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .backing_type = undefined,
            .fields = &.{},
        },
    });
}

/// Recursivly analyzes the specified symbol
pub fn analyzeSymbol(sema: *Sema, symbol: *Symbol, referring_token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!void {
    var common = symbol.common();
    if (common.analyze_status == .done) {
        return; // Nothing to do
    }
    if (common.analyze_status == .active) {
        try sema.errors.append(sema.allocator, .{
            .tag = .dependency_loop,
            .ast = &sema.getModule(module_idx).ast,
            .token = referring_token_idx,
        });
        return error.AnalyzeFailed;
    }

    common.analyze_status = .active;
    defer common.analyze_status = .done;

    return switch (symbol.*) {
        .function => |*fn_sym| sema.analyzeFunction(fn_sym, module_idx),
        .constant => |*const_sym| sema.analyzeConstant(const_sym, module_idx),
        .variable => |*var_sym| sema.analyzeVariable(var_sym, module_idx),
        .register => |*reg_sym| sema.analyzeRegister(reg_sym, module_idx),
        .@"packed" => |*packed_sym| sema.analyzePacked(packed_sym, module_idx),
        .@"enum" => |*enum_sym| sema.analyzeEnum(enum_sym, module_idx),
    };
}

fn analyzeFunction(sema: *Sema, fn_sym: *Symbol.Function, module_idx: ModuleIndex) AnalyzeError!void {
    // Mark as done early, to prevent recursion counting as a dependency loop
    fn_sym.common.analyze_status = .done;

    var analyzer: FunctionAnalyzer = .{
        .sema = sema,
        .symbol = fn_sym,
        .module_idx = module_idx,
    };
    errdefer analyzer.deinit();

    try analyzer.handleFnDef(fn_sym.common.node);

    const ast = &sema.getModule(module_idx).ast;
    std.log.debug("IR for {}", .{sema.parseSymbolLocation(ast, ast.nodeToken(fn_sym.common.node).next()) catch unreachable});
    for (analyzer.ir.items) |ir| {
        std.log.debug(" - {}", .{ir});
    }
    fn_sym.ir = try analyzer.ir.toOwnedSlice(sema.allocator);
}
fn analyzeConstant(sema: *Sema, const_sym: *Symbol.Constant, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const const_def = ast.nodeData(const_sym.common.node).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    const_sym.type = try sema.resolveType(data.type, module_idx, .{ .constant = const_sym.* });
    const_sym.value = try sema.resolveExprValue(const_sym.type, const_def.value, module_idx);
}
fn analyzeVariable(sema: *Sema, var_sym: *Symbol.Variable, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const var_def = ast.nodeData(var_sym.common.node).var_def;
    const data = ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

    var_sym.type = try sema.resolveType(data.type, module_idx, .{ .variable = var_sym.* });
}
fn analyzeRegister(sema: *Sema, reg_sym: *Symbol.Register, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const reg_def = ast.nodeData(reg_sym.common.node).reg_def;
    const data = ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

    reg_sym.type = try sema.resolveType(data.type, module_idx, .{ .register = reg_sym.* });
}
fn analyzePacked(sema: *Sema, packed_sym: *Symbol.Packed, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const packed_def = ast.nodeData(packed_sym.common.node).packed_def;
    const data = ast.readExtraData(Ast.Node.PackedDefData, packed_def.extra);

    packed_sym.backing_type = try sema.resolveType(data.backing_type, module_idx, .{ .@"packed" = packed_sym.* });
    if (packed_sym.backing_type != .raw and packed_sym.backing_type.raw != .signed_int or packed_sym.backing_type.raw != .unsigned_int) {
        try sema.errors.append(sema.allocator, .{
            .tag = .expected_int_backing_type,
            .ast = ast,
            .token = ast.nodeToken(packed_sym.common.node).next(),
            .extra = .{ .actual_type = packed_sym.backing_type },
        });
        return error.AnalyzeFailed;
    }

    var fields: std.ArrayListUnmanaged(Symbol.Packed.Field) = .empty;
    defer fields.deinit(sema.allocator);

    var total_bit_size: u8 = 0;

    const member_range = ast.nodeData(packed_def.block).sub_range;
    for (@intFromEnum(member_range.extra_start)..@intFromEnum(member_range.extra_end)) |extra_idx| {
        const member_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);

        switch (ast.nodeTag(member_idx)) {
            .struct_field => {
                const field_data = ast.nodeData(member_idx).struct_field;
                const field_extra = ast.readExtraData(Node.StructFieldData, field_data.extra);
                const ident_name = ast.nodeToken(member_idx);

                const field: Symbol.Packed.Field = .{
                    .name = ast.parseIdentifier(ident_name),
                    .type = try sema.resolveType(field_extra.type, module_idx, .{ .@"packed" = packed_sym.* }),
                    .default_value = if (field_extra.value != .none)
                        try sema.resolveExprValue(packed_sym.backing_type, field_extra.value, module_idx)
                    else
                        null,
                };
                try fields.append(sema.allocator, field);

                total_bit_size += field.type.bitSize();
            },
            else => unreachable,
        }
    }

    if (total_bit_size > packed_sym.backing_type.bitSize()) {
        try sema.errors.append(sema.allocator, .{
            .tag = .packed_struct_too_big,
            .ast = ast,
            .token = ast.nodeToken(packed_sym.common.node).next(),
            .extra = .{ .exceeds_backing_type = .{
                .backing = packed_sym.backing_type,
                .value = total_bit_size,
            } },
        });
        return error.AnalyzeFailed;
    }
    packed_sym.fields = try fields.toOwnedSlice(sema.allocator);
}
fn analyzeEnum(sema: *Sema, enum_sym: *Symbol.Enum, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const enum_def = ast.nodeData(enum_sym.common.node).enum_def;
    const data = ast.readExtraData(Ast.Node.EnumDefData, enum_def.extra);

    enum_sym.backing_type = try sema.resolveType(data.backing_type, module_idx, .{ .@"enum" = enum_sym.* });
    if (enum_sym.backing_type != .raw and enum_sym.backing_type.raw != .signed_int or enum_sym.backing_type.raw != .unsigned_int) {
        try sema.errors.append(sema.allocator, .{
            .tag = .expected_int_backing_type,
            .ast = ast,
            .token = ast.nodeToken(enum_sym.common.node).next(),
            .extra = .{ .actual_type = enum_sym.backing_type },
        });
        return error.AnalyzeFailed;
    }

    var fields: std.ArrayListUnmanaged(Symbol.Enum.Field) = .empty;
    defer fields.deinit(sema.allocator);

    const member_range = ast.nodeData(enum_def.block).sub_range;
    for (@intFromEnum(member_range.extra_start)..@intFromEnum(member_range.extra_end)) |extra_idx| {
        const member_idx: NodeIndex = .cast(ast.extra_data[extra_idx]);

        switch (ast.nodeTag(member_idx)) {
            .enum_field => {
                const field_data = ast.nodeData(member_idx).enum_field;
                const ident_name = ast.nodeToken(member_idx);

                try fields.append(sema.allocator, .{
                    .name = ast.parseIdentifier(ident_name),
                    .value = try sema.resolveExprValue(enum_sym.backing_type, field_data.value, module_idx),
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
pub fn resolveType(sema: *Sema, node_idx: NodeIndex, module_idx: ModuleIndex, parent: ?Symbol) !TypeSymbol {
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
                    const bits = std.fmt.parseInt(u8, type_name[1..], 10) catch {
                        try sema.errors.append(sema.allocator, .{
                            .tag = .int_bitwidth_too_large,
                            .ast = ast,
                            .token = ident_name,
                        });
                        return error.AnalyzeFailed;
                    };
                    if (bits > max_int_bitwidth) {
                        try sema.errors.append(sema.allocator, .{
                            .tag = .int_bitwidth_too_large,
                            .ast = ast,
                            .token = ident_name,
                        });
                        return error.AnalyzeFailed;
                    }

                    if (type_name[0] == 'i') {
                        return .{ .raw = .{ .signed_int = bits } };
                    } else {
                        return .{ .raw = .{ .unsigned_int = bits } };
                    }
                }
            }

            const symbol, const symbol_idx = try sema.resolveSymbol(ident_name, module_idx);
            switch (symbol.*) {
                .function, .constant, .variable, .register => {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .expected_type_symbol,
                        .ast = ast,
                        .token = ident_name,
                    });
                    return error.AnalyzeFailed;
                },
                .@"packed" => |packed_sym| {
                    return .{ .raw = .{ .@"packed" = if (packed_sym.backing_type.raw == .signed_int)
                        .{ .is_signed = true, .bits = packed_sym.backing_type.raw.signed_int, .symbol_index = symbol_idx }
                    else
                        .{ .is_signed = false, .bits = packed_sym.backing_type.raw.unsigned_int, .symbol_index = symbol_idx } } };
                },
                .@"enum" => |enum_sym| {
                    return .{ .raw = .{ .@"enum" = if (enum_sym.backing_type.raw == .signed_int)
                        .{ .is_signed = true, .bits = enum_sym.backing_type.raw.signed_int, .symbol_index = symbol_idx }
                    else
                        .{ .is_signed = false, .bits = enum_sym.backing_type.raw.unsigned_int, .symbol_index = symbol_idx } } };
                },
            }
        },
        .packed_def => {
            const packed_symbol: Symbol.Packed = .{
                .common = .{
                    .node = node_idx,
                    .is_pub = false,
                },

                // To be determined during analyzation
                .backing_type = undefined,
                .fields = &.{},
            };

            const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len);
            const parent_name = if (parent) |p|
                sema.getModule(p.commonConst().module_index).symbol_map.keys()[@intFromEnum(p.commonConst().module_symbol_index)]
            else
                "";
            const symbol_name = try std.fmt.allocPrint(sema.allocator, "{s}__packed_{d}", .{ parent_name, @intFromEnum(symbol_idx) });
            try sema.string_pool.append(sema.allocator, symbol_name);

            if (module.symbol_map.get(symbol_name)) |existing_sym_idx| {
                const existing_sym = sema.getSymbol(existing_sym_idx);

                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_symbol,
                    .ast = ast,
                    .token = ast.nodeToken(node_idx).next(),
                });
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_symbol,
                    .is_note = true,
                    .ast = ast,
                    .token = ast.nodeToken(existing_sym.commonConst().node),
                });

                return error.AnalyzeFailed;
            }

            try sema.createSymbol(module_idx, symbol_name, .{ .@"packed" = packed_symbol });
            var symbol = sema.getSymbol(symbol_idx);

            symbol.common().analyze_status = .active;
            try sema.analyzePacked(&symbol.@"packed", module_idx);
            symbol.common().analyze_status = .done;

            return .{ .raw = .{ .@"packed" = if (symbol.@"packed".backing_type.raw == .signed_int)
                .{ .is_signed = true, .bits = symbol.@"packed".backing_type.raw.signed_int, .symbol_index = symbol_idx }
            else
                .{ .is_signed = false, .bits = symbol.@"packed".backing_type.raw.unsigned_int, .symbol_index = symbol_idx } } };
        },
        .enum_def => {
            const enum_symbol: Symbol.Enum = .{
                .common = .{
                    .node = node_idx,
                    .is_pub = false,
                },

                // To be determined during analyzation
                .backing_type = undefined,
                .fields = &.{},
            };

            const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len);
            const parent_name = if (parent) |p|
                sema.getModule(p.commonConst().module_index).symbol_map.keys()[@intFromEnum(p.commonConst().module_symbol_index)]
            else
                "";
            const symbol_name = try std.fmt.allocPrint(sema.allocator, "{s}__enum_{d}", .{ parent_name, @intFromEnum(symbol_idx) });
            try sema.string_pool.append(sema.allocator, symbol_name);

            if (module.symbol_map.get(symbol_name)) |existing_sym_idx| {
                const existing_sym = sema.getSymbol(existing_sym_idx);

                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_symbol,
                    .ast = ast,
                    .token = ast.nodeToken(node_idx),
                });
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_symbol,
                    .is_note = true,
                    .ast = ast,
                    .token = ast.nodeToken(existing_sym.common().node),
                });

                return error.AnalyzeFailed;
            }

            try sema.createSymbol(module_idx, symbol_name, .{ .@"enum" = enum_symbol });
            var symbol = sema.getSymbol(symbol_idx);

            symbol.common().analyze_status = .active;
            try sema.analyzeEnum(&symbol.@"enum", module_idx);
            symbol.common().analyze_status = .done;

            return .{ .raw = .{ .@"enum" = if (symbol.@"enum".backing_type.raw == .signed_int)
                .{ .is_signed = true, .bits = symbol.@"enum".backing_type.raw.signed_int, .symbol_index = symbol_idx }
            else
                .{ .is_signed = false, .bits = symbol.@"enum".backing_type.raw.unsigned_int, .symbol_index = symbol_idx } } };
        },
        else => unreachable,
    }
}

/// Resolves a `name` or `module::name` into the associated `Symbol`
pub fn resolveSymbol(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!struct { *Symbol, SymbolIndex } {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const symbol_loc = try sema.parseSymbolLocation(ast, token_idx);

    const target_module, const target_module_idx: ModuleIndex = if (symbol_loc.module.len == 0)
        // Current module
        .{ module, module_idx }
    else find_module: {
        // Other module
        for (sema.modules, 0..) |*other_module, other_module_idx| {
            if (std.mem.eql(u8, other_module.name, symbol_loc.module)) {
                break :find_module .{ other_module, .cast(other_module_idx) };
            }
        }

        try sema.errors.append(sema.allocator, .{
            .tag = .undefined_module,
            .ast = ast,
            .token = token_idx,
        });
        return error.AnalyzeFailed;
    };

    if (target_module.symbol_map.get(symbol_loc.name)) |symbol_idx| {
        const symbol = sema.getSymbol(symbol_idx);

        // Ensure symbol is analyzed
        try sema.analyzeSymbol(symbol, token_idx, target_module_idx);

        return .{ symbol, symbol_idx };
    }

    try sema.errors.append(sema.allocator, .{
        .tag = .undefined_symbol,
        .ast = ast,
        .token = token_idx,
    });
    return error.AnalyzeFailed;
}

/// Resolves an expression into a parsed value
pub fn resolveExprValue(sema: *Sema, target_type: TypeSymbol, node_idx: NodeIndex, module_idx: ModuleIndex) !ExpressionValue {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    switch (ast.nodeTag(node_idx)) {
        .expr_ident => {
            const source_symbol_ident = ast.nodeToken(node_idx);
            const source_symbol, const source_symbol_idx = try sema.resolveSymbol(source_symbol_ident, module_idx);

            const source_type = switch (source_symbol.*) {
                .constant => |const_sym| const_sym.type,
                .variable => |var_sym| var_sym.type,
                .register => |reg_sym| reg_sym.type,
                else => {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .expected_const_var_reg_symbol,
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
                    .extra = .{ .expected_actual_type = .{ .expected = target_type, .actual = source_type } },
                });
                return error.AnalyzeFailed;
            }

            return switch (source_symbol.*) {
                .constant => |const_sym| .{ .value = const_sym.value.resolve(TypeSymbol.ComptimeIntValue, sema) },
                .variable, .register => .{ .variable = source_symbol_idx },
                else => unreachable,
            };
        },
        .expr_int_value => {
            const value_lit = ast.nodeToken(node_idx);
            const value = try sema.parseInt(TypeSymbol.ComptimeIntValue, ast, value_lit);
            const value_type: TypeSymbol = .{ .raw = .{ .comptime_int = value } };

            if (!value_type.isAssignableTo(target_type)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .expected_type,
                    .ast = ast,
                    .token = value_lit,
                    .extra = .{ .expected_actual_type = .{
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
                    .@"packed" => |backing_type| backing_type.bits,
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
            }

            return .{ .value = value };
        },
        .expr_enum_value => {
            const value_lit = ast.nodeToken(node_idx);
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
                    .@"packed" => {
                        // TODO: Support decl-literals
                        try sema.errors.append(sema.allocator, .{
                            .tag = .expected_enum_field_or_decl,
                            .ast = ast,
                            .token = value_lit,
                            .extra = .{ .expected_enum_field_or_decl = target_type },
                        });
                        return error.AnalyzeFailed;
                    },
                    .@"enum" => |enum_type_sym| {
                        const enum_sym = sema.getSymbol(enum_type_sym.symbol_index).@"enum";

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
        .expr_init => {
            var fields: std.StringArrayHashMapUnmanaged(ExpressionValue.PackedField) = .empty;
            defer fields.deinit(sema.allocator);

            switch (target_type.raw) {
                .signed_int, .unsigned_int, .comptime_int, .@"enum" => {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .expected_type_got_init,
                        .ast = ast,
                        .token = ast.nodeToken(node_idx),
                        .extra = .{ .expected_type = target_type },
                    });
                    return error.AnalyzeFailed;
                },
                .@"packed" => |packed_type_sym| {
                    const packed_sym = sema.getSymbol(packed_type_sym.symbol_index).@"packed";

                    try fields.ensureUnusedCapacity(sema.allocator, packed_sym.fields.len);
                },
            }

            // Apply specified values
            const sub_range = ast.nodeData(node_idx).sub_range;
            for (@intFromEnum(sub_range.extra_start)..@intFromEnum(sub_range.extra_end)) |extra_idx| {
                const field_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);
                const field_token = ast.nodeToken(field_idx);
                const field_data = ast.nodeData(field_idx).expr_init_field;

                const field_name = ast.tokenSource(field_token)[1..];
                const field_type = get_type: {
                    switch (target_type.raw) {
                        .signed_int, .unsigned_int, .comptime_int, .@"enum" => unreachable,
                        .@"packed" => |packed_type_sym| {
                            const packed_sym = sema.getSymbol(packed_type_sym.symbol_index).@"packed";

                            for (packed_sym.fields) |field| {
                                if (std.mem.eql(u8, field.name, field_name)) {
                                    break :get_type field.type;
                                }
                            }

                            try sema.errors.append(sema.allocator, .{
                                .tag = .unknown_field,
                                .ast = ast,
                                .token = field_token,
                                .extra = .{ .unknown_field = .{ .symbol = target_type } },
                            });
                            return error.AnalyzeFailed;
                        },
                    }
                };
                const field_offset = get_offset: {
                    switch (target_type.raw) {
                        .signed_int, .unsigned_int, .comptime_int, .@"enum" => unreachable,
                        .@"packed" => |packed_type_sym| {
                            const packed_sym = sema.getSymbol(packed_type_sym.symbol_index).@"packed";

                            var bit_offset: u8 = 0;
                            for (packed_sym.fields) |field| {
                                if (std.mem.eql(u8, field.name, field_name)) {
                                    break :get_offset bit_offset;
                                }

                                bit_offset += field.type.bitSize();
                            }

                            unreachable;
                        },
                    }
                };
                const field_value = try sema.resolveExprValue(field_type, field_data.value, module_idx);

                if (fields.contains(field_name)) {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .unknown_field,
                        .ast = ast,
                        .token = field_token,
                        .extra = .{ .field = .{
                            .type = target_type,
                            .name = field_name,
                        } },
                    });
                    return error.AnalyzeFailed;
                }

                fields.putAssumeCapacityNoClobber(field_name, .{
                    .value = field_value,
                    .bit_offset = field_offset,
                    .bit_size = field_type.bitSize(),
                    .node_idx = field_idx,
                });
            }

            // Fill default values / check missing
            switch (target_type.raw) {
                .signed_int, .unsigned_int, .comptime_int, .@"enum" => unreachable,
                .@"packed" => |packed_type_sym| {
                    const packed_sym = sema.getSymbol(packed_type_sym.symbol_index).@"packed";

                    for (packed_sym.fields, 0..) |defined_field, def_idx| {
                        if (fields.contains(defined_field.name)) {
                            continue;
                        }

                        if (defined_field.default_value) |default| {
                            // Apply default value
                            var bit_offset: u8 = 0;
                            for (packed_sym.fields, 0..) |field, field_idx| {
                                if (def_idx == field_idx) {
                                    fields.putAssumeCapacityNoClobber(field.name, .{
                                        .value = default,
                                        .bit_offset = bit_offset,
                                        .bit_size = field.type.bitSize(),
                                        .node_idx = node_idx,
                                    });
                                    break;
                                }

                                bit_offset += field.type.bitSize();
                            }
                        } else {
                            // Field value not specified
                            try sema.errors.append(sema.allocator, .{
                                .tag = .missing_field,
                                .ast = ast,
                                .token = ast.nodeToken(node_idx),
                                .extra = .{ .field = .{
                                    .type = target_type,
                                    .name = defined_field.name,
                                } },
                            });
                            return error.AnalyzeFailed;
                        }
                    }
                },
            }

            const field_values = try sema.allocator.alloc(ExpressionValue.PackedField, fields.count());
            @memcpy(field_values, fields.values());

            return .{ .packed_fields = field_values };
        },
        else => unreachable,
    }
}

// Helper functions

/// Creates a new symbol and associates it with the given module
fn createSymbol(sema: *Sema, module_idx: ModuleIndex, name: []const u8, symbol: Symbol) !void {
    const module = sema.getModule(module_idx);

    const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len);
    try sema.symbols.append(sema.allocator, symbol);
    try module.symbol_map.putNoClobber(module.allocator, name, symbol_idx);

    var common = sema.getSymbol(symbol_idx).common();
    common.module_index = module_idx;
    common.module_symbol_index = .cast(module.symbol_map.count() - 1);
}

/// Searches for the specified symbol
pub fn lookupSymbol(sema: Sema, symbol_name: []const u8, module_idx: ModuleIndex) ?*Symbol {
    const module = sema.getModule(module_idx);
    return module.symbol_map.get(symbol_name);
}

/// Re-creates the symbol location of the specifed symbol
pub fn getSymbolLocation(sema: Sema, symbol_idx: SymbolIndex) SymbolLocation {
    const symbol = &sema.symbols.items[@intFromEnum(symbol_idx)];
    const common = symbol.commonConst();
    const module = sema.modules[@intFromEnum(common.module_index)];

    return .{
        .name = module.symbol_map.keys()[@intFromEnum(common.module_symbol_index)],
        .module = module.name,
    };
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
        .register => true,
        .@"packed", .@"enum" => unreachable,
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

/// Finds the `ModuleIndex` of the specified module name
pub fn findModuleIndex(sema: Sema, name: []const u8) ModuleIndex {
    for (sema.modules, 0..) |module, module_idx| {
        if (std.mem.eql(u8, module.name.?, name)) {
            return @intCast(module_idx);
        }
    }
    unreachable;
}
/// Finds the `ModuleIndex` of the specified symbol location
pub fn findSymbolModuleIndex(sema: Sema, sym_loc: SymbolLocation) ModuleIndex {
    return sema.findModuleIndex(sym_loc.module);
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
/// Tries parsing an enum and reports an error on failure
pub fn parseEnum(sema: *Sema, comptime T: type, ast: *const Ast, token_idx: Ast.TokenIndex) AnalyzeError!T {
    return ast.parseEnumLiteral(T, token_idx) catch {
        try sema.errors.append(sema.allocator, .{
            .tag = .unknown_field,
            .ast = ast,
            .token = token_idx,
            .extra = .{ .unknown_field = .{ .name = @typeName(T) } },
        });
        return error.AnalyzeFailed;
    };
}

/// Parse either a `name` or `module::name` symbol location
pub fn parseSymbolLocation(sema: *Sema, ast: *const Ast, token_idx: Ast.TokenIndex) AnalyzeError!SymbolLocation {
    if (ast.tokenTag(token_idx) != .ident and ast.tokenTag(token_idx) != .builtin_ident) {
        try sema.errors.append(sema.allocator, .{
            .tag = .expected_token,
            .ast = ast,
            .token = token_idx,
            .extra = .{ .expected_token = .ident },
        });
        return error.AnalyzeFailed;
    }

    if (ast.tokenTag(token_idx.next()) == .double_colon) {
        if (ast.tokenTag(token_idx.offset(2)) != .ident) {
            try sema.errors.append(sema.allocator, .{
                .tag = .expected_token,
                .ast = ast,
                .token = token_idx.offset(2),
                .extra = .{ .expected_token = .ident },
            });
            return error.AnalyzeFailed;
        }

        return .{
            .module = ast.tokenSource(token_idx),
            .name = ast.tokenSource(token_idx.offset(2)),
        };
    } else {
        return .{
            .module = "",
            .name = ast.tokenSource(token_idx),
        };
    }
}

pub fn expectToken(sema: *Sema, ast: *const Ast, node_idx: NodeIndex, tag: Token.Tag) AnalyzeError!Ast.TokenIndex {
    const token_idx = ast.nodeToken(node_idx);

    if (ast.tokenTag(token_idx) != tag) {
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

const FormatTypeData = struct { sema: *const Sema, type: *const TypeSymbol };
pub fn fmtType(sema: *const Sema, type_sym: *const TypeSymbol) std.fmt.Formatter(formatType) {
    return .{ .data = .{
        .sema = sema,
        .type = type_sym,
    } };
}
fn formatType(data: FormatTypeData, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    return data.type.formatDetailed(data.sema, writer);
}

pub const Error = struct {
    pub const Tag = enum {
        // Extra: none
        duplicate_symbol,
        duplicate_label,
        existing_symbol,
        existing_label,
        undefined_module,
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
        // Extra: register_type
        fields_cross_register_boundry,
        // Extra: int_size
        value_too_large,
        invalid_number,
        // Extra: arguments
        expected_arguments,
        // Extra: expected_token
        expected_token,
        // Extra: expected_actual_type
        expected_type,
        // Extra: expected_type
        expected_type_got_init,
        // Extra: actual_type
        expected_int_backing_type,
        // Extra: actual_symbol
        expected_var_symbol,
        expected_const_var_reg_symbol,
        expected_fn_symbol,
        expected_type_symbol,
        // Extra: expected_register_size
        expected_register_size,
        expected_mod_register_size,
        // Extra: expected_enum_field_or_decl
        expected_enum_field_or_decl,
        // Extra: unsupported_register
        unsupported_register,
        // Extra: supported_registers
        supported_registers,
        // Extra: unknown_field
        unknown_field,
        // Extra: field
        duplicate_field,
        missing_field,
        // Extra: exceeds_backing_type
        packed_struct_too_big,
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
        bit_size: u8,
        int_size: struct {
            is_signed: bool,
            bits: u16,
        },
        arguments: struct {
            expected: u8,
            actual: u8,
        },
        expected_token: Token.Tag,
        expected_actual_type: struct {
            expected: TypeSymbol,
            actual: TypeSymbol,
        },
        expected_type: TypeSymbol,
        actual_type: TypeSymbol,
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
        supported_registers: []const ?RegisterType,
        // TODO: Remove `name` payload
        unknown_field: union(enum) {
            name: []const u8,
            symbol: TypeSymbol,
        },
        field: struct {
            type: TypeSymbol,
            name: []const u8,
        },
        exceeds_backing_type: struct {
            backing: TypeSymbol,
            value: u16,
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

        if (err.token != .none) {
            const token_loc = err.ast.tokenLoc(err.token);
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
        .undefined_module => rich.print(writer, tty_config, "Use of undefined module [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .undefined_symbol => if (err.ast.tokenTag(err.token.next()) == .double_colon)
            rich.print(writer, tty_config, "Use of undefined symbol [" ++ highlight ++ "]{s}::{s}", .{ err.ast.tokenSource(err.token), err.ast.tokenSource(err.token.offset(2)) })
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

        .fields_cross_register_boundry => rich.print(writer, tty_config, "Packed fields cross [" ++ highlight ++ "]{d}-bit register boudry[reset]", .{err.extra.bit_size}),

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
            err.ast.tokenTag(err.token).symbol(),
        }),

        .expected_type => rich.print(writer, tty_config, "Expected value of type [" ++ highlight ++ "]{}[reset], found [" ++ highlight ++ "]{}", .{
            sema.fmtType(&err.extra.expected_actual_type.expected),
            sema.fmtType(&err.extra.expected_actual_type.actual),
        }),

        .expected_type_got_init => rich.print(writer, tty_config, "Expected value of type [" ++ highlight ++ "]{}[reset], found [" ++ highlight ++ "]object-initializer", .{
            sema.fmtType(&err.extra.expected_type),
        }),

        .expected_int_backing_type => rich.print(writer, tty_config, "Expected backing-type to be [" ++ highlight ++ "]an integer[reset], found [" ++ highlight ++ "]{}", .{
            sema.fmtType(&err.extra.actual_type),
        }),

        .expected_var_symbol => rich.print(writer, tty_config, "Expected symbol to a [" ++ highlight ++ "]variable[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_const_var_reg_symbol => rich.print(writer, tty_config, "Expected symbol to a [" ++ highlight ++ "]constant, variable or register[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_fn_symbol => rich.print(writer, tty_config, "Expected symbol to a [" ++ highlight ++ "]function[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),
        .expected_type_symbol => rich.print(writer, tty_config, "Expected symbol to a [" ++ highlight ++ "]type[reset], found [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.actual_symbol),
        }),

        .expected_register_size => rich.print(writer, tty_config, "Expected register size to be [" ++ highlight ++ "]{d}-bits[reset], found [" ++ highlight ++ "]{d}-bit register", .{
            err.extra.expected_register_size.expected,
            err.extra.expected_register_size.actual,
        }),
        .expected_mod_register_size => rich.print(writer, tty_config, "Expected register size to evenly divide [" ++ highlight ++ "]{d}-bit value[reset], found [" ++ highlight ++ "]{d}-bit register", .{
            err.extra.expected_register_size.expected,
            err.extra.expected_register_size.actual,
        }),

        .expected_enum_field_or_decl => rich.print(writer, tty_config, "Could not find an [" ++ highlight ++ "]enum field[reset] or a [" ++ highlight ++ "]declaration[reset] called [" ++ highlight ++ "]'{s}'[reset] on type [" ++ highlight ++ "]{}[reset]", .{
            err.ast.tokenSource(err.token)[1..],
            sema.fmtType(&err.extra.expected_enum_field_or_decl),
        }),

        .unsupported_register => rich.print(writer, tty_config, "Unsupported [" ++ highlight ++ "]intermediate register {s}[reset], for use with [" ++ highlight ++ "]{s}", .{
            @tagName(err.extra.unsupported_register.register),
            err.extra.unsupported_register.message,
        }),
        .supported_registers => {
            try rich.print(writer, tty_config, "Supported intermediate registers are: ", .{});
            for (err.extra.supported_registers, 0..) |opt_register, i| {
                if (i > 0) {
                    if (i == err.extra.supported_registers.len - 1) {
                        try rich.print(writer, tty_config, " and [" ++ highlight ++ "]{s}", .{if (opt_register) |register| @tagName(register) else "<none>"});
                    } else {
                        try rich.print(writer, tty_config, ", [" ++ highlight ++ "]{s}", .{if (opt_register) |register| @tagName(register) else "<none>"});
                    }
                } else {
                    try rich.print(writer, tty_config, "[" ++ highlight ++ "]{s}", .{if (opt_register) |register| @tagName(register) else "<none>"});
                }
            }
        },

        // TODO: Remove the `name` variant
        .unknown_field => switch (err.extra.unknown_field) {
            .name => |name| rich.print(writer, tty_config, "Unknown field [" ++ highlight ++ "]'{s}'[reset], for type [" ++ highlight ++ "]{s}", .{
                err.ast.tokenSource(err.token)[1..], name,
            }),
            .symbol => |*sym| rich.print(writer, tty_config, "Unknown field [" ++ highlight ++ "]'{s}'[reset], for type [" ++ highlight ++ "]{s}", .{
                err.ast.tokenSource(err.token)[1..], sema.fmtType(sym),
            }),
        },

        .duplicate_field => rich.print(writer, tty_config, "Duplicate field [" ++ highlight ++ "]'{s}'[reset], in object-initializer for type [" ++ highlight ++ "]{s}", .{
            err.extra.field.name,
            sema.fmtType(&err.extra.field.type),
        }),
        .missing_field => rich.print(writer, tty_config, "Missing field [" ++ highlight ++ "]'{s}'[reset], in object-initializer for type [" ++ highlight ++ "]{s}", .{
            err.extra.field.name,
            sema.fmtType(&err.extra.field.type),
        }),

        .packed_struct_too_big => rich.print(writer, tty_config, "Backing-type [" ++ highlight ++ "]{}[reset], has a maximum bit-size of [" ++ highlight ++ "]{d}[reset], but the packed struct fields have a total bit-size of [" ++ highlight ++ "]{d}[reset]", .{
            sema.fmtType(&err.extra.exceeds_backing_type.backing),
            err.extra.exceeds_backing_type.backing.bitSize(),
            err.extra.exceeds_backing_type.value,
        }),
    };
}

const std = @import("std");
const memory_map = @import("memory_map.zig");
const rich = @import("util/rich.zig");
const builtin_module = @import("builtin_module.zig");

const TypedIndex = @import("util/typed_index.zig").TypedIndex;
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
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const FunctionAnalyzer = @import("sema/FunctionAnalyzer.zig");
const Expression = @import("sema/expression.zig").Expression;
const TypeExpression = @import("sema/type_expression.zig").TypeExpression;
const Sema = @This();

pub const ModuleIndex = TypedIndex(u32, std.math.maxInt(u32));
pub const SymbolIndex = TypedIndex(u32, std.math.maxInt(u32));

pub const ExpressionIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    /// Parses the expression and resolves an index to it
    pub fn resolve(sema: *Sema, target_type_idx: TypeExpressionIndex, node_idx: NodeIndex, module_idx: ModuleIndex) AnalyzeError!ExpressionIndex {
        const index: ExpressionIndex = @enumFromInt(@as(u32, @intCast(sema.expressions.items.len)));
        try sema.expressions.append(sema.allocator, try .parse(sema, target_type_idx, node_idx, module_idx));
        return index;
    }
};
pub const TypeExpressionIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    /// Parses the type-expression and resolves an index to it
    pub fn resolve(sema: *Sema, node_idx: NodeIndex, module_idx: ModuleIndex, parent: SymbolIndex) AnalyzeError!TypeExpressionIndex {
        const index: TypeExpressionIndex = @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len)));
        try sema.type_expressions.append(sema.allocator, try .parse(sema, node_idx, module_idx, parent));
        return index;
    }

    /// Computes the exact byte-size required for this type
    pub fn byteSize(index: TypeExpressionIndex, sema: *const Sema) u16 {
        return sema.type_expressions.items[@intFromEnum(index)].byteSize(sema);
    }
    /// Computes the exact bit-size required for this type
    pub fn bitSize(index: TypeExpressionIndex, sema: *const Sema) u16 {
        return sema.type_expressions.items[@intFromEnum(index)].bitSize(sema);
    }

    /// Creates a formatter for this type-expression
    pub fn fmt(index: TypeExpressionIndex, sema: *const Sema) @typeInfo(@TypeOf(TypeExpression.fmt)).@"fn".return_type.? {
        return sema.type_expressions.items[@intFromEnum(index)].fmt(sema);
    }
};

pub const RegisterType = enum { a8, a16, x8, x16, y8, y16 };

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

    // Should always be implemented by the built-in module
    fallback: Location = .none,

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

expressions: std.ArrayListUnmanaged(Expression) = .empty,
type_expressions: std.ArrayListUnmanaged(TypeExpression) = .empty,

interrupt_vectors: InterruptVectors = .{},

has_errors: bool = false,

/// Collection of strings which needed to be dynamically allocated
string_pool: std.ArrayListUnmanaged([]u8) = .empty,
allocator: std.mem.Allocator,

pub inline fn getModule(sema: *Sema, index: ModuleIndex) *Module {
    return &sema.modules[@intFromEnum(index)];
}
pub inline fn getSymbol(sema: *Sema, index: SymbolIndex) *Symbol {
    return &sema.symbols.items[@intFromEnum(index)];
}
pub inline fn getExpression(sema: *Sema, index: ExpressionIndex) *Expression {
    return &sema.expressions.items[@intFromEnum(index)];
}
pub inline fn getTypeExpression(sema: *Sema, index: TypeExpressionIndex) *TypeExpression {
    return &sema.type_expressions.items[@intFromEnum(index)];
}

pub fn process(allocator: std.mem.Allocator, modules: []Module, mapping_mode: MappingMode) AnalyzeError!Sema {
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
        try sema.emitError(.none, undefined, .missing_reset_vector, .{});
    }

    if (sema.has_errors) {
        return error.AnalyzeFailed;
    }

    // Recursivly analayze symbols, starting from the interrupt vectors
    inline for (std.meta.fields(InterruptVectors)) |field| {
        if (@field(sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
            sema.analyzeSymbol(vector_loc.symbol, vector_loc.module, .none) catch |err| switch (err) {
                error.AnalyzeFailed => std.log.err("Failed to analyze symbol {}", .{sema.getSymbolLocation(vector_loc.symbol)}),
                else => |e| return e,
            };
        }
    }

    if (sema.has_errors) {
        return error.AnalyzeFailed;
    }

    return sema;
}

pub fn deinit(sema: *Sema) void {
    sema.module_map.deinit(sema.allocator);
    for (sema.symbols.items) |*symbol| {
        symbol.deinit(sema.allocator);
    }
    sema.symbols.deinit(sema.allocator);
    for (sema.string_pool.items) |str| {
        sema.allocator.free(str);
    }
    sema.string_pool.deinit(sema.allocator);
}

pub const AnalyzeError = error{AnalyzeFailed} || std.mem.Allocator.Error || std.fs.File.WriteError;

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

        if (module.symbol_map.get(symbol_name)) |existing_symbol_idx| {
            try sema.errDuplicateSymbol(token_idx.next(), module_idx, symbol_name, existing_symbol_idx, .symbol);
            return;
        }

        const result = switch (ast.nodeTag(node_idx)) {
            .fn_def => sema.gatherFunctionSymbol(symbol_name, node_idx, module_idx),
            .const_def => sema.gatherConstantSymbol(symbol_name, node_idx, module_idx),
            .var_def => sema.gatherVariableSymbol(symbol_name, node_idx, module_idx),
            .reg_def => sema.gatherRegisterSymbol(symbol_name, node_idx, module_idx),
            .struct_def => sema.gatherStructSymbol(symbol_name, node_idx, module_idx),
            .packed_def => sema.gatherPackedSymbol(symbol_name, node_idx, module_idx),
            .enum_def => sema.gatherEnumSymbol(symbol_name, node_idx, module_idx),
            else => unreachable,
        };
        result catch |err| switch (err) {
            error.AnalyzeFailed => std.log.err("Failed to gather symbol {s}::{s}", .{ module.name, symbol_name }),
            else => |e| return e,
        };
    }
}

fn gatherFunctionSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const fn_def = ast.nodeData(node_idx).fn_def;
    const data = ast.readExtraData(Ast.Node.FnDefData, fn_def.extra);

    const mapped_bank = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module_idx);

    const real_bank = memory_map.mapRealRomBank(sema.mapping_mode, mapped_bank) orelse {
        std.debug.assert(data.bank_attr != .none);

        try sema.errInvalidRomBank(ast.nodeToken(data.bank_attr), module_idx, mapped_bank);
        return error.AnalyzeFailed;
    };

    const fn_token = ast.nodeToken(node_idx);

    try sema.createSymbol(module_idx, symbol_name, .{
        .function = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },
            .bank = real_bank,

            // To be determined during analyzation
            .calling_convention = .{},

            .ir = &.{},
            .labels = &.{},
            .instructions = &.{},
            .assembly_data = &.{},
        },
    });
    const symbol_idx: SymbolIndex = .cast(sema.symbols.items.len - 1);

    // Handle interrupt vectors
    inline for (std.meta.fields(InterruptVectors)) |field| {
        const vector_name = "@" ++ field.name;
        if (std.mem.eql(u8, vector_name, symbol_name)) {
            if (@field(sema.interrupt_vectors, field.name).unpack()) |existing_loc| {
                try sema.errDuplicateSymbol(fn_token.next(), module_idx, vector_name, existing_loc.symbol, .vector);
            } else {
                @field(sema.interrupt_vectors, field.name) = .{
                    .module = module_idx,
                    .symbol = symbol_idx,
                };
            }

            // Enforce bank $00
            if (real_bank != memory_map.getRealRomBank(sema.mapping_mode, 0x00)) {
                try sema.errInvalidVectorBank(ast.nodeToken(data.bank_attr), module_idx, vector_name, real_bank);
            }

            return;
        }
    }

    // Only interrupt vectors are allowed to start with an @
    if (symbol_name[0] == '@') {
        try sema.errUnknownInterruptVector(fn_token.next(), module_idx, symbol_name);
    }
}
fn gatherConstantSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const const_def = ast.nodeData(node_idx).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    const mapped_bank = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module_idx);

    const real_bank = memory_map.mapRealRomBank(sema.mapping_mode, mapped_bank) orelse {
        std.debug.assert(data.bank_attr != .none);

        try sema.errInvalidRomBank(ast.nodeToken(data.bank_attr), module_idx, mapped_bank);
        return error.AnalyzeFailed;
    };

    try sema.createSymbol(module_idx, symbol_name, .{
        .constant = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },
            .bank = real_bank,

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
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module_idx);

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

            try sema.errInvalidRamBank(ast.nodeToken(data.bank_attr), module_idx, bank);
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

    const access_type = try sema.parseEnum(Symbol.Register.AccessType, ast.nodeToken(data.access_attr), module_idx);
    const address = try sema.parseInt(u16, data.address, module_idx);

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
fn gatherStructSymbol(sema: *Sema, symbol_name: []const u8, node_idx: NodeIndex, module_idx: ModuleIndex) !void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    try sema.createSymbol(module_idx, symbol_name, .{
        .@"struct" = .{
            .common = .{
                .node = node_idx,
                .is_pub = ast.tokenTag(ast.nodeToken(node_idx).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .fields = &.{},
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
pub fn analyzeSymbol(sema: *Sema, symbol_idx: SymbolIndex, module_idx: ModuleIndex, referring_token_idx: Ast.TokenIndex) AnalyzeError!void {
    const symbol = sema.getSymbol(symbol_idx);
    var common = symbol.common();

    if (common.analyze_status == .done) {
        return; // Nothing to do
    }
    if (common.analyze_status == .active) {
        try sema.emitError(referring_token_idx, module_idx, .dependency_loop, .{});
        return error.AnalyzeFailed;
    }

    common.analyze_status = .active;
    defer common.analyze_status = .done;

    return switch (symbol.*) {
        .function => |*fn_sym| sema.analyzeFunction(fn_sym, symbol_idx, module_idx),
        .constant => |*const_sym| sema.analyzeConstant(const_sym, symbol_idx, module_idx),
        .variable => |*var_sym| sema.analyzeVariable(var_sym, symbol_idx, module_idx),
        .register => |*reg_sym| sema.analyzeRegister(reg_sym, symbol_idx, module_idx),
        .@"struct" => |*struct_sym| sema.analyzeStruct(struct_sym, symbol_idx, module_idx),
        .@"packed" => |*packed_sym| sema.analyzePacked(packed_sym, symbol_idx, module_idx),
        .@"enum" => |*enum_sym| sema.analyzeEnum(enum_sym, symbol_idx, module_idx),
    };
}

fn analyzeFunction(sema: *Sema, fn_sym: *Symbol.Function, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    // Mark as done early, to prevent recursion counting as a dependency loop
    fn_sym.common.analyze_status = .done;

    const is_interrupt_vector = check_vectors: {
        inline for (std.meta.fields(InterruptVectors)) |field| {
            if (@field(sema.interrupt_vectors, field.name).unpack()) |vector_loc| {
                if (vector_loc.symbol == symbol_idx) {
                    break :check_vectors true;
                }
            }
        }
        break :check_vectors false;
    };

    var analyzer: FunctionAnalyzer = .{
        .sema = sema,
        .symbol = fn_sym,
        .module_idx = module_idx,
        .is_interrupt_vector = is_interrupt_vector,
    };
    errdefer analyzer.deinit();

    // Interrupt vectors cannot rely on a calling convention

    try analyzer.handleFnDef(fn_sym.common.node);

    const ast = &sema.getModule(module_idx).ast;
    std.log.debug("IR for {}", .{ast.parseSymbolLocation(ast.nodeToken(fn_sym.common.node).next())});
    for (analyzer.ir.items) |ir| {
        std.log.debug(" - {}", .{ir});
    }

    fn_sym.calling_convention = analyzer.calling_conv;
    fn_sym.ir = try analyzer.ir.toOwnedSlice(sema.allocator);
}
fn analyzeConstant(sema: *Sema, const_sym: *Symbol.Constant, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const const_def = ast.nodeData(const_sym.common.node).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    const_sym.type = try .resolve(sema, data.type, module_idx, symbol_idx);
    const_sym.value = try .resolve(sema, const_sym.type, const_def.value, module_idx);
}
fn analyzeVariable(sema: *Sema, var_sym: *Symbol.Variable, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const var_def = ast.nodeData(var_sym.common.node).var_def;
    const data = ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

    var_sym.type = try .resolve(sema, data.type, module_idx, symbol_idx);
}
fn analyzeRegister(sema: *Sema, reg_sym: *Symbol.Register, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const reg_def = ast.nodeData(reg_sym.common.node).reg_def;
    const data = ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

    reg_sym.type = try .resolve(sema, data.type, module_idx, symbol_idx);
}
fn analyzeStruct(sema: *Sema, struct_sym: *Symbol.Struct, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const struct_def = ast.nodeData(struct_sym.common.node).struct_def;

    var fields: std.ArrayListUnmanaged(Symbol.Struct.Field) = .empty;
    defer fields.deinit(sema.allocator);

    const member_range = ast.nodeData(struct_def.block).sub_range;
    for (@intFromEnum(member_range.extra_start)..@intFromEnum(member_range.extra_end)) |extra_idx| {
        const member_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);

        switch (ast.nodeTag(member_idx)) {
            .struct_field => {
                const field_data = ast.nodeData(member_idx).struct_field;
                const field_extra = ast.readExtraData(Node.StructFieldData, field_data.extra);
                const field_type: TypeExpressionIndex = try .resolve(sema, field_extra.type, module_idx, symbol_idx);

                try fields.append(sema.allocator, .{
                    .name = ast.parseIdentifier(ast.nodeToken(member_idx)),
                    .type = field_type,
                    .default_value = if (field_extra.value != .none)
                        try .resolve(sema, field_type, field_extra.value, module_idx)
                    else
                        .none,
                });
            },
            else => unreachable,
        }
    }

    struct_sym.fields = try fields.toOwnedSlice(sema.allocator);
}
fn analyzePacked(sema: *Sema, packed_sym: *Symbol.Packed, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const packed_def = ast.nodeData(packed_sym.common.node).packed_def;
    const data = ast.readExtraData(Ast.Node.PackedDefData, packed_def.extra);

    packed_sym.backing_type = try .resolve(sema, data.backing_type, module_idx, symbol_idx);
    if (sema.getTypeExpression(packed_sym.backing_type).* != .integer) {
        try sema.emitError(ast.nodeToken(packed_sym.common.node).next(), module_idx, .expected_int_backing_type, .{packed_sym.backing_type.fmt(sema)});
        return error.AnalyzeFailed;
    }

    var fields: std.ArrayListUnmanaged(Symbol.Packed.Field) = .empty;
    defer fields.deinit(sema.allocator);

    var total_bit_size: u16 = 0;

    const member_range = ast.nodeData(packed_def.block).sub_range;
    for (@intFromEnum(member_range.extra_start)..@intFromEnum(member_range.extra_end)) |extra_idx| {
        const member_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);

        switch (ast.nodeTag(member_idx)) {
            .struct_field => {
                const field_data = ast.nodeData(member_idx).struct_field;
                const field_extra = ast.readExtraData(Node.StructFieldData, field_data.extra);
                const field_type: TypeExpressionIndex = try .resolve(sema, field_extra.type, module_idx, symbol_idx);

                const field: Symbol.Packed.Field = .{
                    .name = ast.parseIdentifier(ast.nodeToken(member_idx)),
                    .type = field_type,
                    .default_value = if (field_extra.value != .none)
                        try .resolve(sema, field_type, field_extra.value, module_idx)
                    else
                        .none,
                };
                try fields.append(sema.allocator, field);

                total_bit_size += field.type.bitSize(sema);
            },
            else => unreachable,
        }
    }

    if (total_bit_size > packed_sym.backing_type.bitSize(sema)) {
        try sema.emitError(ast.nodeToken(packed_sym.common.node).next(), module_idx, .packed_too_large, .{ packed_sym.backing_type.fmt(sema), packed_sym.backing_type.bitSize(sema), total_bit_size });
        return error.AnalyzeFailed;
    }
    packed_sym.fields = try fields.toOwnedSlice(sema.allocator);
}
fn analyzeEnum(sema: *Sema, enum_sym: *Symbol.Enum, symbol_idx: SymbolIndex, module_idx: ModuleIndex) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const enum_def = ast.nodeData(enum_sym.common.node).enum_def;
    const data = ast.readExtraData(Ast.Node.EnumDefData, enum_def.extra);

    enum_sym.backing_type = try .resolve(sema, data.backing_type, module_idx, symbol_idx);
    if (sema.getTypeExpression(enum_sym.backing_type).* != .integer) {
        try sema.emitError(ast.nodeToken(enum_sym.common.node).next(), module_idx, .expected_int_backing_type, .{enum_sym.backing_type.fmt(sema)});
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
                    .value = try .resolve(sema, enum_sym.backing_type, field_data.value, module_idx),
                });
            },
            else => unreachable,
        }
    }

    enum_sym.fields = try fields.toOwnedSlice(sema.allocator);
}

// Helper functions

/// Resolves a `name` or `module::name` into the associated `Symbol`
pub fn resolveSymbol(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!struct { *Symbol, SymbolIndex } {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const symbol_loc = ast.parseSymbolLocation(token_idx);

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

        try sema.emitError(token_idx, module_idx, .unknown_module, .{symbol_loc.module});
        return error.AnalyzeFailed;
    };

    if (target_module.symbol_map.get(symbol_loc.name)) |symbol_idx| {
        const symbol = sema.getSymbol(symbol_idx);

        // Ensure symbol is analyzed
        try sema.analyzeSymbol(symbol_idx, target_module_idx, token_idx);

        return .{ symbol, symbol_idx };
    }

    try sema.emitError(token_idx, module_idx, .unknown_symbol, .{symbol_loc});
    return error.AnalyzeFailed;
}

/// Creates a new symbol and associates it with the given module
pub fn createSymbol(sema: *Sema, module_idx: ModuleIndex, name: []const u8, symbol: Symbol) !void {
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
        .@"struct", .@"packed", .@"enum" => unreachable,
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
pub fn parseInt(sema: *Sema, comptime T: type, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!T {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    return ast.parseIntLiteral(T, token_idx) catch |err| {
        switch (err) {
            error.Overflow => {
                // The int must be in a valid format, otherwise error.InvalidCharacter would've been triggered
                var int = sema.parseBigInt(std.math.big.int.Managed, token_idx, module_idx) catch unreachable;
                defer int.deinit();

                const info = @typeInfo(T).int;
                switch (info.signedness) {
                    .signed => try sema.emitError(token_idx, module_idx, .value_too_large, .{ int, "a", "signed", info.bits }),
                    .unsigned => try sema.emitError(token_idx, module_idx, .value_too_large, .{ int, "an", "unsigned", info.bits }),
                }
            },
            error.InvalidCharacter => try sema.emitError(token_idx, module_idx, .invalid_number, .{ast.tokenSource(token_idx)}),
        }
        return error.AnalyzeFailed;
    };
}
/// Tries parsing a big integer and reports an error on failure
pub fn parseBigInt(sema: *Sema, comptime T: type, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!T {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    return ast.parseBigIntLiteral(T, token_idx, sema.allocator) catch {
        try sema.emitError(token_idx, module_idx, .invalid_number, .{ast.tokenSource(token_idx)});
        return error.AnalyzeFailed;
    };
}

/// Tries parsing an enum and reports an error on failure
pub fn parseEnum(sema: *Sema, comptime T: type, token_idx: Ast.TokenIndex, module_idx: ModuleIndex) AnalyzeError!T {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    return ast.parseEnumLiteral(T, token_idx) catch {
        // TODO: Use types from built-in modules instead of Zig enums
        try sema.emitError(token_idx, module_idx, .unknown_field, .{ ast.tokenSource(token_idx), @typeName(T) });
        return error.AnalyzeFailed;
    };
}

// Error handling

pub const ErrorTag = enum {
    duplicate_symbol,
    duplicate_field,
    duplicate_vector,
    duplicate_label,

    existing_symbol,
    existing_label,

    unknown_module,
    unknown_symbol,
    unknown_field,
    unknown_enum_field_or_decl,

    missing_field,
    missing_reset_vector,

    expected_type,
    expected_type_symbol,
    expected_const_var_reg_symbol,
    expected_fn_symbol,
    expected_int_backing_type,
    expected_intermediate_register,
    expected_arguments,

    invalid_number,
    invalid_builtin_fn,
    invalid_builtin_var,
    invalid_rom_bank,
    invalid_ram_bank,
    invalid_size_mode,
    invalid_intermediate_register,

    value_too_large,
    int_bitwidth_too_large,
    packed_too_large,
    no_members,
    dependency_loop,

    pub fn message(tag: ErrorTag) []const u8 {
        return switch (tag) {
            .duplicate_symbol => "Duplicate symbol [!]{s}",
            .duplicate_field => "Duplicate field [!]'{s}'[], in object-initializer for type [!]{}",
            .duplicate_vector => "Duplicate interrupt vector [!]{s}",
            .duplicate_label => "Duplicate [!]{s}",

            .existing_symbol => "Symbol already defined here",
            .existing_label => "Label already defined here",

            .unknown_module => "Use of undefined module [!]{s}",
            .unknown_symbol => "Use of undefined symbol [!]{}",
            .unknown_field => "Use of undefined field [!]'{s}'[], for type [!]{s}",
            .unknown_enum_field_or_decl => "Use of undefined [!]enum field[] or a [!]declaration[] called [!]'{s}'[] on type [!]{}",

            .missing_field => "Missing field [!]'{s}'[], in object-initializer for type [!]{}",
            .missing_reset_vector => "Interrupt vector [!]@emulation_reset[] is not defined",

            .expected_type => "Expected value of type [!]{}[], found [!]{s}",
            .expected_type_symbol => "Expected a symbol to a [!]type[], found [!]{s}",
            .expected_const_var_reg_symbol => "Expected symbol to a [!]constant, variable or register[], found [!]{s}",
            .expected_fn_symbol => "Expected symbol to a [!]function[], found [!]{s}",
            .expected_int_backing_type => "Expected backing-type to be [!]an integer[], found [!]{}",
            .expected_intermediate_register => "Expected [!]intermediate register[], to hold [!]value being set",
            .expected_arguments => "Expected [!]{d} argument{s}[], found [!]{d}",

            .invalid_number => "Value [!]{s}[] is not a [!]valid number",
            .invalid_builtin_fn => "Invalid built-in function [!]{s}",
            .invalid_builtin_var => "Invalid built-in variable [!]{s}",
            .invalid_rom_bank => "Invalid ROM bank [!]{s}",
            .invalid_ram_bank => "Invalid RAM bank [!]{s}",
            .invalid_size_mode => "Invalid size-mode [!]{s}[], expected [!]8[] or [!]16",
            .invalid_intermediate_register => "Invalid register type [!]{s}",

            .value_too_large => "Value [!]{}[] is too large to fit into {s} [!]{s} {}-bit number",
            .int_bitwidth_too_large => "Bit-width of primitive integer type [!]{s}[] exceedes maximum bit-width of [!]{d}",
            .packed_too_large => "Backing-type [!]{}[], has a maximum bit-size of [!]{d}[], but the packed struct fields have a total bit-size of [!]{d}",
            .no_members => "Type [!]{}[] has no members",
            .dependency_loop => "Detected a [!]dependency loop",
        };
    }
};

pub fn emitError(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, comptime tag: ErrorTag, args: anytype) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print(comptime tag.message(), args);
    try writeErrorEpilogue(err_ctx);
}
pub fn emitNote(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, comptime tag: ErrorTag, args: anytype) AnalyzeError!void {
    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    try note_ctx.print(comptime tag.message(), args);
    try writeErrorEpilogue(note_ctx);
}

pub fn errDuplicateSymbol(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, symbol_name: []const u8, existing_symbol_idx: SymbolIndex, comptime variant: enum { symbol, vector, label }) AnalyzeError!void {
    const module = sema.getModule(module_idx);
    const ast = &module.ast;

    const existing_symbol = sema.getSymbol(existing_symbol_idx);

    try sema.emitError(token_idx, module_idx, switch (variant) {
        .symbol => .duplicate_symbol,
        .vector => .duplicate_vector,
        .label => .duplicate_label,
    }, .{symbol_name});
    try sema.emitNote(ast.nodeToken(existing_symbol.common().node), module_idx, .existing_symbol, .{});
}
pub fn errUnknownInterruptVector(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, symbol_name: []const u8) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print("Unknown interrupt vector [!]{s}", .{symbol_name});
    try writeErrorEpilogue(err_ctx);

    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    try note_ctx.print("Possible [!]interrupt vectors[] are: ", .{});

    const fields = std.meta.fields(InterruptVectors);
    comptime std.debug.assert(std.mem.eql(u8, fields[0].name, "fallback"));
    inline for (fields, 0..) |field, i| {
        // Don't print @fallback, since thats already defined by the built-in module
        if (i != 0) {
            const vector_name = "@" ++ field.name;
            if (i > 1) {
                if (i == fields.len - 1) {
                    try note_ctx.print(" and [!]{s}", .{vector_name});
                } else {
                    try note_ctx.print(", [!]{s}", .{vector_name});
                }
            } else {
                try note_ctx.print("[!]{s}", .{vector_name});
            }
        }
    }
    try writeErrorEpilogue(note_ctx);
}
pub fn errInvalidRomBank(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, bank: u8) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for data to be placed in ROM", .{bank});
    try writeErrorEpilogue(err_ctx);

    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    switch (sema.mapping_mode) {
        .lorom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$80..$FF[] or mirrors of them", .{}),
        .hirom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$C0..$FF[] or mirrors of them", .{}),
        .exhirom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$3E..$7D[], [!]$C0..$FF[] or mirrors of them", .{}),
    }
    try writeErrorEpilogue(note_ctx);
}
pub fn errInvalidRamBank(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, bank: u8) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for variables to be placed in RAM", .{bank});
    try writeErrorEpilogue(err_ctx);

    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    try note_ctx.print("[!]RAM variables[] needs to be in banks [!]7E[], [!]7F[] or mirrors of them", .{});
    try writeErrorEpilogue(note_ctx);
}
pub fn errInvalidVectorBank(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, symbol_name: []const u8, bank: u8) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for interrupt vector [!]{s}", .{ bank, symbol_name });
    try writeErrorEpilogue(err_ctx);

    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    try note_ctx.print("[!]Interrupt vectors[] need to be accessible from bank [!]$00", .{});
    try writeErrorEpilogue(note_ctx);
}

pub fn failUnsupportedIntermediateRegister(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, used_register: Ir.RegisterType, allowed_registers: []const Ir.RegisterType, usage: []const u8) AnalyzeError!void {
    const err_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .err);
    try err_ctx.print("Unsupported [!]intermediate register {s}[], for use with [!]{s}", .{ @tagName(used_register), usage });
    try writeErrorEpilogue(err_ctx);

    const note_ctx = try sema.writeErrorPrologue(token_idx, module_idx, .note);
    try note_ctx.print("Supported intermediate registers are: ", .{});
    for (allowed_registers, 0..) |register, i| {
        if (i > 0) {
            if (i == allowed_registers.len - 1) {
                try note_ctx.print(" and [!]{s}", .{@tagName(register)});
            } else {
                try note_ctx.print(", [!]{s}", .{@tagName(register)});
            }
        } else {
            try note_ctx.print("[!]{s}", .{@tagName(register)});
        }
    }
    try writeErrorEpilogue(note_ctx);

    return error.AnalyzeFailed;
}

const ErrorMessageContext = struct {
    src_loc: std.zig.Loc,
    token_len: u32,

    writer: std.fs.File.Writer,
    tty_config: std.io.tty.Config,

    pub fn print(ctx: ErrorMessageContext, comptime fmt: []const u8, args: anytype) !void {
        return rich.print(ctx.writer, ctx.tty_config, fmt, args);
    }
};
fn writeErrorPrologue(sema: *Sema, token_idx: Ast.TokenIndex, module_idx: ModuleIndex, comptime msg_type: enum { err, note }) !ErrorMessageContext {
    std.debug.lockStdErr();
    errdefer std.debug.unlockStdErr();

    sema.has_errors = true;

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    const writer = stderr.writer();

    if (token_idx != .none) {
        const module = sema.getModule(module_idx);
        const ast = &module.ast;

        const token_loc = ast.tokenLoc(token_idx);
        const src_loc = std.zig.findLineColumn(ast.source, token_loc.start);

        const src_args = .{ ast.source_path, src_loc.line + 1, src_loc.column + 1 };
        switch (msg_type) {
            .err => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", src_args),
            .note => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", src_args),
        }

        return .{
            .src_loc = src_loc,
            .token_len = @intCast(token_loc.end - token_loc.start),

            .writer = writer,
            .tty_config = tty_config,
        };
    } else {
        switch (msg_type) {
            .err => try rich.print(writer, tty_config, "[bold cyan]note: ", .{}),
            .note => try rich.print(writer, tty_config, "[bold red]error: ", .{}),
        }

        return .{
            .src_loc = undefined,
            .token_len = std.math.maxInt(u32),

            .writer = writer,
            .tty_config = tty_config,
        };
    }
}
fn writeErrorEpilogue(ctx: ErrorMessageContext) !void {
    if (ctx.token_len != std.math.maxInt(u32)) {
        try ctx.writer.writeByte('\n');

        try ctx.writer.writeAll(ctx.src_loc.source_line);
        try ctx.writer.writeByte('\n');

        try ctx.tty_config.setColor(ctx.writer, .green);
        try ctx.writer.writeByteNTimes(' ', ctx.src_loc.column);
        try ctx.writer.writeByte('^');
        try ctx.writer.writeByteNTimes('~', ctx.token_len - 1);
        try ctx.tty_config.setColor(ctx.writer, .reset);
        try ctx.writer.writeByte('\n');
    } else {
        try ctx.writer.writeByte('\n');
    }

    std.debug.unlockStdErr();
}

const std = @import("std");
const memory_map = @import("memory_map.zig");
const rich = @import("util/rich.zig");
const builtin_module = @import("builtin_module.zig");

const ErrorSystem = @import("error.zig").ErrorSystem;
const TypedIndex = @import("util/typed_index.zig").TypedIndex;
const Ast = @import("Ast.zig");
const Token = Ast.Token;
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Relocation = @import("CodeGen.zig").Relocation;
const Ir = @import("codegen/AssemblyIr.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const FunctionAnalyzer = @import("sema/FunctionAnalyzer.zig");

pub const Symbol = @import("symbol.zig").Symbol;
pub const SymbolLocation = @import("symbol.zig").SymbolLocation;
pub const Expression = @import("sema/expression.zig").Expression;
pub const TypeExpression = @import("sema/type_expression.zig").TypeExpression;
pub const Sir = @import("sema/Sir.zig");
pub const SirIter = @import("sema/SirIter.zig");
pub const sir_gen = @import("sema/sir_gen.zig");

const Sema = @This();

const InterruptVectors = struct {
    const Location = struct {
        pub const none: Location = .{
            .module = undefined,
            .symbol = .none,
        };

        pub inline fn unpack(loc: Location) ?Location {
            if (loc.symbol == .none) {
                return null;
            }
            return loc;
        }
        pub inline fn unpackSymbol(loc: Location) ?Symbol.Index {
            if (loc.symbol == .none) {
                return null;
            }
            return loc.symbol;
        }

        module: Module.Index,
        symbol: Symbol.Index,
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
module_map: std.StringArrayHashMapUnmanaged(Module.Index) = .empty,

symbols: std.ArrayListUnmanaged(Symbol) = .empty,

expressions: std.ArrayListUnmanaged(Expression) = .empty,
type_expressions: std.ArrayListUnmanaged(TypeExpression) = .empty,

interrupt_vectors: InterruptVectors = .{},

has_errors: bool = false,

/// General purpose allocator
allocator: std.mem.Allocator,
/// Arena allocator for storing permanent data, which lives until the end of the compilation
data_arena: *std.heap.ArenaAllocator,
/// Arena allocator for storing temporary data
temp_arena: *std.heap.ArenaAllocator,

/// Allocator for permanent data storage
pub inline fn dataAllocator(sema: Sema) std.mem.Allocator {
    return sema.data_arena.allocator();
}
/// Allocator for temporary data storage
pub inline fn tempAllocator(sema: Sema) std.mem.Allocator {
    return sema.temp_arena.allocator();
}

pub fn process(sema: *Sema) AnalyzeError!void {
    // Gather symbols
    try sema.module_map.ensureUnusedCapacity(sema.dataAllocator(), sema.modules.len);
    for (0..sema.modules.len) |module_idx| {
        try sema.gatherSymbols(.cast(module_idx));
    }

    // Only reset is required
    if (sema.interrupt_vectors.emulation_reset.unpack() == null) {
        const err_ctx = try Error.begin(undefined, .none, .err);
        try err_ctx.print(comptime Error.tagMessage(.missing_reset_vector), .{});
        try err_ctx.end();
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
}

pub const AnalyzeError = error{AnalyzeFailed} || std.mem.Allocator.Error || std.fs.File.WriteError;

fn gatherSymbols(sema: *Sema, module_idx: Module.Index) AnalyzeError!void {
    const module = module_idx.get(sema);
    const ast = &module.ast;

    const range = ast.nodeData(.root).sub_range;
    for (@intFromEnum(range.extra_start)..@intFromEnum(range.extra_end)) |extra_idx| {
        const node_idx: NodeIndex = .cast(ast.extra_data[extra_idx]);
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
            try sema.emitError(token_idx.next(), module_idx, .duplicate_symbol, .{symbol_name});
            try sema.emitNote(ast.nodeToken(existing_symbol_idx.getCommon(sema).node), module_idx, .existing_symbol, .{});
            return error.AnalyzeFailed;
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

fn gatherFunctionSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    const fn_def = ast.nodeData(node).fn_def;
    const data = ast.readExtraData(Ast.Node.FnDefData, fn_def.extra);

    const mapped_bank = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module);

    const real_bank = memory_map.mapRealRomBank(sema.mapping_mode, mapped_bank) orelse {
        std.debug.assert(data.bank_attr != .none);

        try sema.errInvalidRomBank(ast.nodeToken(data.bank_attr), module, mapped_bank);
        return error.AnalyzeFailed;
    };

    const fn_token = ast.nodeToken(node);

    try sema.createSymbol(module, symbol_name, .{
        .function = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },
            .bank = real_bank,

            // To be determined during analyzation
            // .calling_convention = .{},

            // .local_variables = &.{},
            .labels = &.{},

            .semantic_ir = .empty,
            .assembly_ir = &.{},
            .instructions = &.{},
            .assembly_data = &.{},
        },
    });
    const symbol: Symbol.Index = .cast(sema.symbols.items.len - 1);

    // Handle interrupt vectors
    inline for (std.meta.fields(InterruptVectors)) |field| {
        const vector_name = "@" ++ field.name;
        if (std.mem.eql(u8, vector_name, symbol_name)) {
            if (@field(sema.interrupt_vectors, field.name).unpack()) |existing_loc| {
                try sema.emitError(fn_token.next(), module, .duplicate_vector, .{vector_name});
                try sema.emitNote(ast.nodeToken(existing_loc.symbol.getCommon(sema).node), module, .existing_vector, .{});
            } else {
                @field(sema.interrupt_vectors, field.name) = .{
                    .module = module,
                    .symbol = symbol,
                };
            }

            // Enforce bank $00
            if (real_bank != memory_map.getRealRomBank(sema.mapping_mode, 0x00)) {
                try sema.errInvalidVectorBank(ast.nodeToken(data.bank_attr), module, vector_name, real_bank);
            }

            return;
        }
    }

    // Only interrupt vectors are allowed to start with an @
    if (symbol_name[0] == '@') {
        try sema.errUnknownInterruptVector(fn_token.next(), module, symbol_name);
    }
}
fn gatherConstantSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    const const_def = ast.nodeData(node).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    const mapped_bank = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module);

    const real_bank = memory_map.mapRealRomBank(sema.mapping_mode, mapped_bank) orelse {
        std.debug.assert(data.bank_attr != .none);

        try sema.errInvalidRomBank(ast.nodeToken(data.bank_attr), module, mapped_bank);
        return error.AnalyzeFailed;
    };

    try sema.createSymbol(module, symbol_name, .{
        .constant = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },
            .bank = real_bank,

            // To be determined during analyzation
            .type = .none,
            .value = .none,
            .sir_graph = .empty,
        },
    });
}
fn gatherVariableSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    const var_def = ast.nodeData(node).var_def;
    const data = ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

    const bank: u8 = if (data.bank_attr == .none)
        0x00
    else
        try sema.parseInt(u8, ast.nodeToken(data.bank_attr), module);

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

            try sema.errInvalidRamBank(ast.nodeToken(data.bank_attr), module, bank);
            return error.AnalyzeFailed;
        },
    };

    try sema.createSymbol(module, symbol_name, .{
        .variable = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },

            .wram_offset_min = offset_min,
            .wram_offset_max = offset_max,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}
fn gatherRegisterSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    const reg_def = ast.nodeData(node).reg_def;
    const data = ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

    const access_type = try sema.parseEnum(Symbol.Register.AccessType, ast.nodeToken(data.access_attr), module);
    const address = try sema.parseInt(u16, data.address, module);

    try sema.createSymbol(module, symbol_name, .{
        .register = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },

            .access = access_type,
            .address = address,

            // To be determined during analyzation
            .type = undefined,
        },
    });
}
fn gatherStructSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    try sema.createSymbol(module, symbol_name, .{
        .@"struct" = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .fields = &.{},
            .sir_graph = .empty,
        },
    });
}
fn gatherPackedSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    try sema.createSymbol(module, symbol_name, .{
        .@"packed" = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .backing_type = undefined,
            .fields = &.{},
            .sir_graph = .empty,
        },
    });
}
fn gatherEnumSymbol(sema: *Sema, symbol_name: []const u8, node: NodeIndex, module: Module.Index) !void {
    const ast = &module.get(sema).ast;

    try sema.createSymbol(module, symbol_name, .{
        .@"enum" = .{
            .common = .{
                .node = node,
                .is_pub = ast.tokenTag(ast.nodeToken(node).prev()) == .keyword_pub,
            },

            // To be determined during analyzation
            .backing_type = undefined,
            .fields = &.{},
        },
    });
}

/// Recursivly analyzes the specified symbol
pub fn analyzeSymbol(sema: *Sema, symbol_idx: Symbol.Index, module_idx: Module.Index, referring_token_idx: Ast.TokenIndex) AnalyzeError!void {
    const common = symbol_idx.getCommon(sema);

    if (common.analyze_status == .done) {
        return; // Nothing to do
    }
    if (common.analyze_status == .active) {
        try sema.emitError(referring_token_idx, module_idx, .dependency_loop, .{});
        return error.AnalyzeFailed;
    }

    common.analyze_status = .active;
    defer symbol_idx.getCommon(sema).analyze_status = .done;

    return switch (symbol_idx.get(sema).*) {
        .function => sema.analyzeFunction(symbol_idx, module_idx),
        .constant => sema.analyzeConstant(symbol_idx, module_idx),
        .variable => sema.analyzeVariable(symbol_idx, module_idx),
        .register => sema.analyzeRegister(symbol_idx, module_idx),
        .@"struct" => sema.analyzeStruct(symbol_idx, module_idx),
        .@"packed" => sema.analyzePacked(symbol_idx, module_idx),
        // .@"enum" => sema.analyzeEnum(symbol_idx, module_idx),
        inline else => |_, t| std.log.err("TODO: {s}", .{@tagName(t)}),
    };
}

fn analyzeFunction(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    // Mark as done early, to prevent recursion counting as a dependency loop
    symbol.getFn(sema).common.analyze_status = .done;

    var analyzer: FunctionAnalyzer = .{
        .sema = sema,
        .symbol = symbol,
        .module = module,
    };

    try analyzer.process();
    try SirIter.reduce(sema, &analyzer.graph);

    const fn_sym = symbol.getFn(sema);
    fn_sym.semantic_ir = analyzer.graph;
}
fn analyzeConstant(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const node = symbol.getCommon(sema).node;
    const const_def = ast.nodeData(node).const_def;
    const data = ast.readExtraData(Ast.Node.ConstDefData, const_def.extra);

    var graph = symbol.getConst(sema).sir_graph;
    const ty: TypeExpression.Index = try .resolve(sema, data.type, module, symbol);

    const start = try graph.create(sema.dataAllocator(), .block_start, &.{}, node);
    const end = try graph.create(sema.dataAllocator(), .block_end, &.{}, node);
    // TODO: Support implicit typing
    const value, _, _ = try sir_gen.parseExpression(sema, &graph, const_def.value, module, .{
        .start_node = start,
        .end_node = end,
        .target_type = ty,
    });
    try graph.addEdge(sema.dataAllocator(), .initDependency(value, start));
    try graph.addEdge(sema.dataAllocator(), .initDependency(end, value));

    const const_sym = symbol.getConst(sema);
    const_sym.type = ty;
    const_sym.value = value;
    const_sym.sir_graph = graph;
}
fn analyzeVariable(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const var_def = ast.nodeData(symbol.getCommon(sema).node).var_def;
    const data = ast.readExtraData(Ast.Node.VarDefData, var_def.extra);

    const type_expr: TypeExpression.Index = try .resolve(sema, data.type, module, symbol);
    symbol.getVar(sema).type = type_expr;
}
fn analyzeRegister(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const reg_def = ast.nodeData(symbol.getCommon(sema).node).reg_def;
    const data = ast.readExtraData(Ast.Node.RegDefData, reg_def.extra);

    const type_expr: TypeExpression.Index = try .resolve(sema, data.type, module, symbol);
    symbol.getReg(sema).type = type_expr;
}
fn analyzeStruct(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const node = symbol.getCommon(sema).node;
    const struct_def = ast.nodeData(symbol.getCommon(sema).node).struct_def;

    var graph = symbol.getStruct(sema).sir_graph;

    var fields: std.ArrayListUnmanaged(Symbol.Struct.Field) = .empty;
    defer fields.deinit(sema.allocator);

    const member_range = ast.nodeData(struct_def.block).sub_range;
    for (@intFromEnum(member_range.extra_start)..@intFromEnum(member_range.extra_end)) |extra_idx| {
        const member_idx: NodeIndex = @enumFromInt(ast.extra_data[extra_idx]);

        switch (ast.nodeTag(member_idx)) {
            .struct_field => {
                const field_data = ast.nodeData(member_idx).struct_field;
                const field_extra = ast.readExtraData(Node.StructFieldData, field_data.extra);
                const field_type: TypeExpression.Index = try .resolve(sema, field_extra.type, module, symbol);

                try fields.append(sema.allocator, .{
                    .name = ast.parseIdentifier(ast.nodeToken(member_idx)),
                    .type = field_type,
                    .default_value = if (field_extra.value != .none) b: {
                        const start = try graph.create(sema.dataAllocator(), .block_start, &.{}, node);
                        const end = try graph.create(sema.dataAllocator(), .block_end, &.{}, node);
                        const value, _, _ = try sir_gen.parseExpression(sema, &graph, field_extra.value, module, .{
                            .start_node = start,
                            .end_node = end,
                            .target_type = field_type,
                        });
                        try graph.addEdge(sema.dataAllocator(), .initDependency(value, start));
                        try graph.addEdge(sema.dataAllocator(), .initDependency(end, value));

                        break :b value;
                    } else .none,
                });
            },
            else => unreachable,
        }
    }

    const struct_sym = symbol.getStruct(sema);
    struct_sym.fields = try fields.toOwnedSlice(sema.allocator);
    struct_sym.sir_graph = graph;
}
fn analyzePacked(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const node = symbol.getCommon(sema).node;
    const packed_def = ast.nodeData(symbol.getCommon(sema).node).packed_def;
    const data = ast.readExtraData(Ast.Node.PackedDefData, packed_def.extra);

    const backing_type: TypeExpression.Index = try .resolve(sema, data.backing_type, module, symbol);
    if (backing_type.get(sema).* != .integer) {
        try sema.emitError(ast.nodeToken(symbol.getCommon(sema).node).next(), module, .expected_int_backing_type, .{backing_type.fmt(sema)});
        return error.AnalyzeFailed;
    }

    var graph = symbol.getPacked(sema).sir_graph;

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
                const field_type: TypeExpression.Index = try .resolve(sema, field_extra.type, module, symbol);

                const field: Symbol.Packed.Field = .{
                    .name = ast.parseIdentifier(ast.nodeToken(member_idx)),
                    .type = field_type,
                    .default_value = if (field_extra.value != .none) b: {
                        const start = try graph.create(sema.dataAllocator(), .block_start, &.{}, node);
                        const end = try graph.create(sema.dataAllocator(), .block_end, &.{}, node);
                        const value, _, _ = try sir_gen.parseExpression(sema, &graph, field_extra.value, module, .{
                            .start_node = start,
                            .end_node = end,
                            .target_type = field_type,
                        });
                        try graph.addEdge(sema.dataAllocator(), .initDependency(value, start));
                        try graph.addEdge(sema.dataAllocator(), .initDependency(end, value));

                        break :b value;
                    } else .none,
                };
                try fields.append(sema.allocator, field);

                total_bit_size += field.type.bitSize(sema);
            },
            else => unreachable,
        }
    }

    const packed_sym = symbol.getPacked(sema);
    if (total_bit_size > backing_type.bitSize(sema)) {
        try sema.emitError(ast.nodeToken(packed_sym.common.node).next(), module, .packed_too_large, .{ backing_type.fmt(sema), backing_type.bitSize(sema), total_bit_size });
        return error.AnalyzeFailed;
    }

    packed_sym.backing_type = backing_type;
    packed_sym.fields = try fields.toOwnedSlice(sema.allocator);
    packed_sym.sir_graph = graph;
}
fn analyzeEnum(sema: *Sema, symbol: Symbol.Index, module: Module.Index) AnalyzeError!void {
    const ast = &module.get(sema).ast;

    const enum_def = ast.nodeData(symbol.getCommon(sema).node).enum_def;
    const data = ast.readExtraData(Ast.Node.EnumDefData, enum_def.extra);

    const backing_type: TypeExpression.Index = try .resolve(sema, data.backing_type, module, symbol);
    if (backing_type.get(sema).* != .integer) {
        try sema.emitError(ast.nodeToken(symbol.getCommon(sema).node).next(), module, .expected_int_backing_type, .{backing_type.fmt(sema)});
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
                    .value = try .resolve(sema, backing_type, field_data.value, module),
                });
            },
            else => unreachable,
        }
    }

    const enum_sym = symbol.getEnum(sema);
    enum_sym.backing_type = backing_type;
    enum_sym.fields = try fields.toOwnedSlice(sema.allocator);
}

// Symbol mangement

/// Creates a new symbol and associates it with the given module
pub fn createSymbol(sema: *Sema, module_idx: Module.Index, name: []const u8, symbol: Symbol) !void {
    const module = module_idx.get(sema);

    const symbol_idx: Symbol.Index = .cast(sema.symbols.items.len);
    try sema.symbols.append(sema.dataAllocator(), symbol);
    try module.symbol_map.putNoClobber(sema.dataAllocator(), name, symbol_idx);

    var common = symbol_idx.getCommon(sema);
    common.module_index = module_idx;
    common.module_symbol_index = .cast(module.symbol_map.count() - 1);
}

/// Resolves a `name` or `module::name` into the associated `Symbol`
pub fn resolveSymbol(sema: *Sema, token: Ast.TokenIndex, module: Module.Index) AnalyzeError!Symbol.Index {
    const ast = &module.get(sema).ast;
    const symbol_loc = ast.parseSymbolLocation(token);

    const target_module: Module.Index = if (symbol_loc.module.len == 0)
        // Current module
        module
    else find_module: {
        // Other module
        for (sema.modules, 0..) |other_module, other_module_idx| {
            if (std.mem.eql(u8, other_module.name, symbol_loc.module)) {
                break :find_module .cast(other_module_idx);
            }
        }

        try sema.emitError(token, module, .unknown_module, .{symbol_loc.module});
        return error.AnalyzeFailed;
    };

    if (target_module.get(sema).symbol_map.get(symbol_loc.name)) |symbol_idx| {
        // Ensure symbol is analyzed
        try sema.analyzeSymbol(symbol_idx, target_module, token);

        return symbol_idx;
    }

    try sema.emitError(token, module, .unknown_symbol, .{symbol_loc});
    return error.AnalyzeFailed;
}

/// Re-creates the symbol location of the specifed symbol
pub fn getSymbolLocation(sema: Sema, symbol_idx: Symbol.Index) SymbolLocation {
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

// Parsing (TODO: Remove!!)

/// Tries parsing an integer and reports an error on failure
pub fn parseInt(sema: *Sema, comptime T: type, token: Ast.TokenIndex, module: Module.Index) AnalyzeError!T {
    const ast = &module.get(sema).ast;

    return ast.parseIntLiteral(T, token) catch |err| {
        switch (err) {
            error.Overflow => {
                // The int must be in a valid format, otherwise error.InvalidCharacter would've been triggered
                var int = sema.parseBigInt(std.math.big.int.Managed, token, module) catch unreachable;
                defer int.deinit();

                const info = @typeInfo(T).int;
                switch (info.signedness) {
                    .signed => try sema.emitError(token, module, .value_too_large, .{ int, "a", "signed", info.bits }),
                    .unsigned => try sema.emitError(token, module, .value_too_large, .{ int, "an", "unsigned", info.bits }),
                }
            },
            error.InvalidCharacter => try sema.emitError(token, module, .invalid_number, .{ast.tokenSource(token)}),
        }
        return error.AnalyzeFailed;
    };
}
/// Tries parsing a big integer and reports an error on failure
pub fn parseBigInt(sema: *Sema, comptime T: type, token: Ast.TokenIndex, module: Module.Index) AnalyzeError!T {
    const ast = &module.get(sema).ast;
    return ast.parseBigIntLiteral(T, token, sema.allocator) catch {
        try sema.emitError(token, module, .invalid_number, .{ast.tokenSource(token)});
        return error.AnalyzeFailed;
    };
}

/// Tries parsing an enum and reports an error on failure
pub fn parseEnum(sema: *Sema, comptime T: type, token: Ast.TokenIndex, module: Module.Index) AnalyzeError!T {
    const ast = &module.get(sema).ast;
    return ast.parseEnumLiteral(T, token) catch {
        // TODO: Use types from built-in modules instead of Zig enums
        try sema.emitError(token, module, .unknown_field, .{ ast.tokenSource(token), @typeName(T) });
        return error.AnalyzeFailed;
    };
}

/// Stores the type-expression and provides an index for it
pub fn addTypeExpression(sema: *Sema, type_expr: TypeExpression) !TypeExpression.Index {
    const index: TypeExpression.Index = @enumFromInt(@as(u32, @intCast(sema.type_expressions.items.len)));
    try sema.type_expressions.append(sema.allocator, type_expr);
    return index;
}

// Error handling

pub const Error = ErrorSystem(.{
    .duplicate_symbol = "Duplicate symbol [!]'{s}'",
    .duplicate_vector = "Duplicate interrupt vector [!]'{s}'",
    .duplicate_local = "Duplicate local variable [!]'{s}'",
    .duplicate_label = "Duplicate label [!]'{s}'",
    .duplicate_field = "Duplicate field [!]'{s}'[], in object-initializer for type [!]{}",

    .local_shadow_global = "Local variable [!]'{s}'[] shadows existing symbol",
    .local_shadow_label = "Local variable [!]'{s}'[] shadows existing label",
    .label_shadow_global = "Label [!]'{s}'[] shadows existing symbol",
    .label_shadow_local = "Label [!]'{s}'[] shadows existing local variable",

    .existing_symbol = "Symbol already defined here",
    .existing_vector = "Interrupt vector already defined here",
    .existing_local = "Local variable already defined here",
    .existing_label = "Label already defined here",

    .unknown_module = "Use of undefined module [!]'{s}'",
    .unknown_symbol = "Use of undefined symbol [!]'{}'",
    .unknown_field = "Use of undefined field [!]'{s}'[], for type [!]{s}",
    .unknown_enum_field_or_decl = "Use of undefined [!]enum field[] or a [!]declaration[] called [!]'{s}'[] on type [!]{}",

    .missing_field = "Missing field [!]'{s}'[], in object-initializer for type [!]{}",
    .missing_reset_vector = "Interrupt vector [!]@emulation_reset[] is not defined",

    .expected_type = "Expected value of type [!]{}[], found [!]{s}",
    .expected_type_symbol = "Expected a symbol to a [!]type[], found [!]{s}",
    .expected_const_var_reg_symbol = "Expected symbol to a [!]constant, variable or register[], found [!]{s}",
    .expected_fn_symbol = "Expected symbol to a [!]function[], found [!]{s}",
    .expected_int_backing_type = "Expected backing-type to be [!]an integer[], found [!]{}",
    .expected_intermediate_register = "Expected [!]intermediate register[], to hold [!]value being set",
    .expected_arguments = "Expected [!]{d} argument{s}[], found [!]{d}",
    .expected_comptime_value = "Expected [!]compile-time known value[], found reference to runtime symbol",

    .invalid_number = "Value [!]{s}[] is not a [!]valid number",
    .invalid_builtin_fn = "Invalid built-in function [!]{s}",
    .invalid_builtin_var = "Invalid built-in variable [!]{s}",
    .invalid_rom_bank = "Invalid ROM bank [!]{s}",
    .invalid_ram_bank = "Invalid RAM bank [!]{s}",
    .invalid_size_mode = "Invalid size-mode [!]{s}[], expected [!]8[] or [!]16",
    .invalid_intermediate_register = "Invalid register type [!]{s}",

    .value_too_large = "Value [!]{}[] is too large to fit into {s} [!]{s} {}-bit number",
    .int_bitwidth_too_large = "Bit-width of primitive integer type [!]{s}[] exceedes maximum bit-width of [!]{d}",
    .packed_too_large = "Backing-type [!]{}[], has a maximum bit-size of [!]{d}[], but the packed struct fields have a total bit-size of [!]{d}",
    .intermediate_register_too_large = "Intermediate register [!]{s}[] cannot write [!]single byte[] values",
    .no_members = "Type [!]{}[] has no members",
    .dependency_loop = "Detected a [!]dependency loop",
    .fields_cross_register_boundry = "Packed fields cross [!]{d}-bit register boudry",
});

pub fn emitError(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, comptime tag: Error.Tag, args: anytype) AnalyzeError!void {
    const err_ctx = try Error.begin(&module.get(sema).ast, token, .err);
    try err_ctx.print(comptime Error.tagMessage(tag), args);
    try err_ctx.end();
}
pub fn emitNote(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, comptime tag: Error.Tag, args: anytype) AnalyzeError!void {
    const note_ctx = try Error.begin(&module.get(sema).ast, token, .note);
    try note_ctx.print(comptime Error.tagMessage(tag), args);
    try note_ctx.end();
}

pub fn errDuplicateSymbol(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, symbol_name: []const u8, existing_symbol: Symbol.Index, comptime variant: enum { symbol, vector, label }) AnalyzeError!void {
    try sema.emitError(token, module, switch (variant) {
        .symbol => .duplicate_symbol,
        .vector => .duplicate_vector,
        .label => .duplicate_label,
    }, .{symbol_name});
    try sema.emitNote(module.get(sema).ast.nodeToken(existing_symbol.getCommon(sema).node), module, .existing_symbol, .{});
}
pub fn errUnknownInterruptVector(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, symbol_name: []const u8) AnalyzeError!void {
    const err_ctx = try Error.begin(&module.get(sema).ast, token, .err);
    try err_ctx.print("Unknown interrupt vector [!]{s}", .{symbol_name});
    try err_ctx.end();

    const note_ctx = try Error.begin(&module.get(sema).ast, token, .note);
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
    try note_ctx.end();
}
pub fn errInvalidRomBank(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, bank: u8) AnalyzeError!void {
    const err_ctx = try Error.begin(&module.get(sema).ast, token, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for data to be placed in ROM", .{bank});
    try err_ctx.end();

    const note_ctx = try Error.begin(&module.get(sema).ast, token, .note);
    switch (sema.mapping_mode) {
        .lorom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$80..$FF[] or mirrors of them", .{}),
        .hirom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$C0..$FF[] or mirrors of them", .{}),
        .exhirom => try note_ctx.print("[!]ROM data[] needs to be in banks [!]$3E..$7D[], [!]$C0..$FF[] or mirrors of them", .{}),
    }
    try note_ctx.end();
}
pub fn errInvalidRamBank(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, bank: u8) AnalyzeError!void {
    const err_ctx = try Error.begin(&module.get(sema).ast, token, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for variables to be placed in RAM", .{bank});
    try err_ctx.end();

    const note_ctx = try Error.begin(&module.get(sema).ast, token, .note);
    try note_ctx.print("[!]RAM variables[] needs to be in banks [!]7E[], [!]7F[] or mirrors of them", .{});
    try note_ctx.end();
}
pub fn errInvalidVectorBank(sema: *Sema, token: Ast.TokenIndex, module: Module.Index, symbol_name: []const u8, bank: u8) AnalyzeError!void {
    const err_ctx = try Error.begin(&module.get(sema).ast, token, .err);
    try err_ctx.print("Invalid bank [!]${x}[] for interrupt vector [!]{s}", .{ bank, symbol_name });
    try err_ctx.end();

    const note_ctx = try Error.begin(&module.get(sema).ast, token, .note);
    try note_ctx.print("[!]Interrupt vectors[] need to be accessible from bank [!]$00", .{});
    try note_ctx.end();
}

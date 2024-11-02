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
const InstructionType = @import("instruction.zig").InstructionType;
const Relocation = @import("CodeGen.zig").Relocation;
const Ir = @import("ir.zig").Ir;
const Symbol = symbol.Symbol;
const SymbolLocation = symbol.SymbolLocation;
const MappingMode = @import("Rom.zig").Header.Mode.Map;
const Sema = @This();

/// Module -> Name -> Symbol
pub const SymbolIndex = u32;
pub const SymbolMap = std.StringArrayHashMapUnmanaged(std.StringArrayHashMapUnmanaged(SymbolIndex));

pub const ModuleIndex = u32;

pub const Error = struct {
    pub const Tag = enum {
        // Extra: node
        duplicate_symbol,
        duplicate_label,
        existing_sym,
        existing_label,
        missing_reset_vector,
        invalid_vector_name,
        invalid_opcode,
        // Extra: vector
        duplicate_vector,
        // Extra: bank
        invalid_vector_bank,
        // Extra: size_type
        undefined_size_mode,
        // Extra: max_bit_size
        value_too_large,
        invalid_number,
    };

    tag: Tag,
    /// Errors of type `note` are associated with the previous `err` / `warn`
    type: enum { err, warn, note },

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
        max_bit_size: u16,
    } = .{ .none = {} },

    pub fn getNotes(err: Error) u32 {
        return switch (err.tag) {
            .unexpected_top_level_node => 0,
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

pub fn process(allocator: std.mem.Allocator, modules: []Module, mapping_mode: MappingMode) !Sema {
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
            .type = .err,
            .ast = undefined,
            .token = Ast.null_token,
        });
    }

    // Analyse symbols
    const symbol_slice = sema.symbols.slice();
    for (symbol_slice.items(.sym), symbol_slice.items(.loc)) |*sym, loc| {
        const module_idx: ModuleIndex = @intCast(sema.symbol_map.getIndex(loc.module).?);
        switch (sym.*) {
            .function => sema.analyzeFunction(loc, module_idx, &sym.function) catch |err| switch (err) {
                error.AnalyzeFailed => {},
                else => |e| return e,
            },
            else => @panic("unsupported"),
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

pub fn detectErrors(sema: Sema, writer: std.fs.File.Writer, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (sema.errors.items) |err| {
        if (err.type == .err or err.type == .warn) {
            try writer.writeByte('\n');
        }

        if (err.token != Ast.null_token) {
            const token_loc = err.ast.token_locs[err.token];
            const src_loc = std.zig.findLineColumn(err.ast.source, token_loc.start);

            const args = .{ err.ast.source_path, src_loc.line + 1, src_loc.column + 1 };
            switch (err.type) {
                .err => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", args),
                .warn => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [yellow]warning: ", args),
                .note => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", args),
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
            switch (err.type) {
                .err => try rich.print(writer, tty_config, "[bold red]error: ", .{}),
                .warn => try rich.print(writer, tty_config, "[bold yellow]warning: ", .{}),
                .note => try rich.print(writer, tty_config, "[bold cyan] note: ", .{}),
            }

            try sema.renderError(writer, tty_config, err);
            try writer.writeByte('\n');
        }
    }

    return sema.errors.items.len != 0;
}

pub fn renderError(sema: Sema, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    const highlight = "bold bright_magenta";

    switch (err.tag) {
        .duplicate_symbol => return rich.print(writer, tty_config, "Found duplicate symbol [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .duplicate_label => return rich.print(writer, tty_config, "Found duplicate label [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .missing_reset_vector => return rich.print(writer, tty_config, "Interrupt vector [" ++ highlight ++ "]@emulation_reset [reset]is not defined", .{}),
        .invalid_vector_name => return rich.print(writer, tty_config, "Unknown interrupt vector [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .invalid_opcode => return rich.print(writer, tty_config, "Invalid instruction opcode [" ++ highlight ++ "]{s}", .{err.ast.tokenSource(err.token)}),
        .existing_sym => return writer.writeAll("Symbol already defined here"),
        .existing_label => return writer.writeAll("Label already defiend here"),

        .duplicate_vector => return rich.print(writer, tty_config, "Found duplicate interrupt vector [" ++ highlight ++ "]{s}", .{@tagName(err.extra.vector)}),

        .invalid_vector_bank => return rich.print(
            writer,
            tty_config,
            "Expected interrupt vector [" ++ highlight ++ "]{s} [reset]to be located in bank [" ++ highlight ++ "]${x}[reset], but got [" ++ highlight ++ "]${x}",
            .{ err.ast.tokenSource(err.extra.bank.fn_name), memory_map.getRealBank(sema.mapping_mode, 0x00), err.extra.bank.actual },
        ),

        .undefined_size_mode => return rich.print(writer, tty_config, "Size-mode is undefined for [" ++ highlight ++ "]{s}", .{switch (err.extra.size_type) {
            .none => unreachable,
            .mem => "A-Register / Memory access",
            .idx => "X/Y-Registers",
        }}),

        .value_too_large => return rich.print(
            writer,
            tty_config,
            "The value [" ++ highlight ++ "]{!} [reset]is too large to fit into a [" ++ highlight ++ "]{}-bit number",
            .{ err.ast.parseIntLiteral(u64, err.token), err.extra.max_bit_size },
        ),
        .invalid_number => return rich.print(
            writer,
            tty_config,
            "The value [" ++ highlight ++ "]{s} [reset]is not a valid [" ++ highlight ++ "]{}-bit number",
            .{ err.ast.tokenSource(err.token), err.extra.max_bit_size },
        ),
    }
}

const AnalyzeError = error{AnalyzeFailed} || std.mem.Allocator.Error;

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
            .fn_def => {
                const sym_loc: SymbolLocation = .{
                    .module = module.name.?,
                    .name = ast.parseIdentifier(token_idx + 1),
                };

                if (sema.lookupSymbol(sym_loc)) |existing_sym| {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .duplicate_symbol,
                        .type = .err,
                        .ast = ast,
                        .token = token_idx + 1,
                    });
                    try sema.errors.append(sema.allocator, .{
                        .tag = .existing_sym,
                        .type = .note,
                        .ast = ast,
                        .token = ast.node_tokens[
                            switch (existing_sym) {
                                .function => |existing_func| existing_func.node,
                                else => @panic("TODO"),
                            }
                        ],
                    });

                    return;
                }

                sema.gatherFunctionSymbol(sym_loc, module_idx, @intCast(node_idx)) catch |err| switch (err) {
                    error.AnalyzeFailed => continue,
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

    const bank: u8 = if (data.bank_attr == Ast.null_node)
        memory_map.getRealBank(sema.mapping_mode, 0x00)
    else
        memory_map.getRealBank(sema.mapping_mode, try sema.parseInt(u8, ast, ast.node_tokens[data.bank_attr]));

    const fn_token = ast.node_tokens[node_idx];

    try sema.createSymbol(sym_loc, .{
        .function = .{
            .is_pub = ast.token_tags[ast.node_tokens[node_idx] - 1] == .keyword_pub,
            .bank = bank,

            .node = node_idx,
        },
    });

    // Handle interrupt vectors
    inline for (std.meta.fields(InterruptVectors), 0..) |field, i| {
        if (std.mem.eql(u8, "@" ++ field.name, sym_loc.name)) {
            if (@field(sema.interrupt_vectors, field.name)) |existing_loc| {
                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_vector,
                    .type = .err,
                    .ast = ast,
                    .token = fn_token + 1,
                    .extra = .{ .vector = @enumFromInt(i) },
                });

                const existing_sym = sema.lookupSymbol(existing_loc).?;
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_sym,
                    .type = .note,
                    .ast = ast,
                    .token = ast.node_tokens[
                        switch (existing_sym) {
                            .function => |existing_func| existing_func.node,
                            else => @panic("TODO"),
                        }
                    ],
                });
            } else {
                @field(sema.interrupt_vectors, field.name) = sym_loc;
            }

            // Enforce bank $00
            if (bank != memory_map.getRealBank(sema.mapping_mode, 0x00)) {
                try sema.errors.append(sema.allocator, .{
                    .tag = .invalid_vector_bank,
                    .type = .err,
                    .ast = ast,
                    .token = ast.node_tokens[data.bank_attr],
                    .extra = .{ .bank = .{
                        .fn_name = fn_token + 1,
                        .actual = bank,
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
            .type = .err,
            .ast = ast,
            .token = fn_token + 1,
        });
    }
}

fn analyzeFunction(sema: *Sema, sym_loc: SymbolLocation, module_idx: ModuleIndex, func: *Symbol.Function) AnalyzeError!void {
    var analyzer: @import("sema/FunctionAnalyzer.zig") = .{
        .ast = &sema.modules[module_idx].ast,
        .sema = sema,
        .func = func,
        .sym_loc = sym_loc,
    };
    try analyzer.handleFnDef(func.node);

    std.log.debug("IR for {s}::{s}", .{ sym_loc.module, sym_loc.name });
    for (analyzer.ir.items) |ir| {
        std.log.debug(" - {}", .{ir});
    }
    func.ir = try analyzer.ir.toOwnedSlice(sema.allocator);
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
pub fn lookupSymbol(sema: Sema, sym_loc: SymbolLocation) ?Symbol {
    if (sema.symbol_map.get(sym_loc.module)) |module_symbols| {
        if (module_symbols.get(sym_loc.name)) |sym_idx| {
            return sema.symbols.items(.sym)[sym_idx];
        }
    }
    return null;
}

/// Tries parsing an integer and reports an error on failure
pub fn parseInt(sema: *Sema, comptime T: type, ast: *const Ast, token_idx: Ast.TokenIndex) AnalyzeError!T {
    return ast.parseIntLiteral(T, token_idx) catch |err| {
        try sema.errors.append(sema.allocator, .{
            .tag = switch (err) {
                error.Overflow => .value_too_large,
                error.InvalidCharacter => .invalid_number,
            },
            .type = .err,
            .ast = ast,
            .token = token_idx,
            .extra = .{ .max_bit_size = 16 },
        });
        return error.AnalyzeFailed;
    };
}

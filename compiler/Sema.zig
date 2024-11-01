const std = @import("std");
const symbol = @import("symbol.zig");

const Sema = @This();
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("ir.zig").Ir;
const Symbol = symbol.Symbol;
const SymbolLocation = symbol.SymbolLocation;

/// Module -> Name -> Symbol
pub const SymbolIndex = u32;
pub const SymbolMap = std.StringArrayHashMapUnmanaged(std.StringArrayHashMapUnmanaged(SymbolIndex));

pub const ModuleIndex = u32;

pub const Error = struct {
    pub const Tag = enum {
        // Extra: node
        unexpected_top_level_node,
        missing_module,
        duplicate_symbol,
        existing_sym,
        missing_reset_vector,
        invalid_vector_name,
        // Extra: vector
        duplicate_vector,
    };

    tag: Tag,
    /// Errors of type `note` are associated with the previous `err` / `warn`
    type: enum { err, warn, note },

    module: ModuleIndex,
    node: Ast.NodeIndex,

    extra: union {
        none: void,
        vector: std.meta.FieldEnum(InterruptVectors),
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

symbols: std.MultiArrayList(struct { sym: Symbol, loc: SymbolLocation }) = .empty,
symbol_map: SymbolMap = .empty,

errors: std.ArrayListUnmanaged(Error) = .empty,

interrupt_vectors: InterruptVectors = .{},
allocator: std.mem.Allocator,

pub fn process(allocator: std.mem.Allocator, modules: []Module) !Sema {
    var sema: Sema = .{
        .modules = modules,
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
            .module = undefined,
            .node = Ast.null_node,
        });
    }

    // Analyse symbols
    const symbol_slice = sema.symbols.slice();
    for (symbol_slice.items(.sym), symbol_slice.items(.loc)) |*sym, loc| {
        const module_idx: ModuleIndex = @intCast(sema.symbol_map.getIndex(loc.module).?);
        switch (sym.*) {
            .function => try sema.analyzeFunction(loc, module_idx, &sym.function),
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

        if (err.node == Ast.null_node) {
            switch (err.type) {
                .err => {
                    try tty_config.setColor(writer, .red);
                    try writer.writeAll("error: ");
                },
                .warn => {
                    try tty_config.setColor(writer, .yellow);
                    try writer.writeAll("warning: ");
                },
                .note => {
                    try tty_config.setColor(writer, .cyan);
                    try writer.writeAll("note: ");
                },
            }
            try tty_config.setColor(writer, .reset);
            try sema.renderError(writer, tty_config, err);
            try writer.writeByte('\n');
        } else {
            const module = sema.modules[err.module];
            const token_loc = module.ast.token_locs[module.ast.node_tokens[err.node]];

            const src_loc = std.zig.findLineColumn(module.source, token_loc.start);

            try tty_config.setColor(writer, .bold);
            try writer.print("{s}:{}:{}: ", .{ module.source_path, src_loc.line + 1, src_loc.column + 1 });

            switch (err.type) {
                .err => {
                    try tty_config.setColor(writer, .red);
                    try writer.writeAll("error: ");
                },
                .warn => {
                    try tty_config.setColor(writer, .yellow);
                    try writer.writeAll("warning: ");
                },
                .note => {
                    try tty_config.setColor(writer, .cyan);
                    try writer.writeAll("note: ");
                },
            }
            try tty_config.setColor(writer, .reset);
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
        }
    }

    return sema.errors.items.len != 0;
}

pub fn renderError(sema: Sema, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    const ast = sema.modules[err.module].ast;
    switch (err.tag) {
        .unexpected_top_level_node => return writer.writeAll("Unexpected top level node"),
        .missing_module => return writer.writeAll("Missing module"),
        // .missing_segment => return writer.writeAll("Missing segment"),
        .duplicate_symbol => return writer.writeAll("Duplicate symbol"),
        .duplicate_vector => {
            try writer.writeAll("Duplicate interrupt vector ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.print("@{s}", .{@tagName(err.extra.vector)});
            try tty_config.setColor(writer, .reset);
        },
        .missing_reset_vector => {
            try writer.writeAll("Interrupt vector ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll("@emulation_reset");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(" is not defined");
        },
        .invalid_vector_name => {
            try writer.writeAll("Unknown interrupt vector ");
            try tty_config.setColor(writer, .bold);
            try tty_config.setColor(writer, .bright_magenta);
            try writer.writeAll(ast.parseIdentifier(ast.node_tokens[err.node]));
            try tty_config.setColor(writer, .reset);
        },
        .existing_sym => {
            try writer.writeAll("Symbol already declarded here");
        },
    }
}

fn gatherSymbols(sema: *Sema, module_idx: u32) !void {
    const module = &sema.modules[module_idx];
    const ast = module.ast;

    const range = ast.node_data[Ast.root_node].sub_range;
    for (range.start..range.end) |extra_idx| {
        const node_idx = ast.extra_data[extra_idx];
        switch (ast.node_tags[node_idx]) {
            .module => {
                module.name = ast.parseIdentifier(ast.node_tokens[node_idx]);
            },
            .fn_def => {
                const sym_loc: SymbolLocation = .{
                    .module = module.name.?,
                    .name = ast.parseIdentifier(ast.node_tokens[node_idx] + 1),
                };

                if (sema.lookupSymbol(sym_loc) != null) {
                    try sema.errors.append(sema.allocator, .{
                        .tag = .duplicate_symbol,
                        .type = .err,
                        .module = module_idx,
                        .node = @intCast(node_idx),
                    });
                    return;
                }

                try sema.gatherFunctionSymbol(sym_loc, module_idx, @intCast(node_idx));
            },
            else => {
                try sema.errors.append(sema.allocator, .{
                    .tag = .unexpected_top_level_node,
                    .type = .err,
                    .module = module_idx,
                    .node = @intCast(node_idx),
                });
                return;
            },
        }
    }
}

fn gatherFunctionSymbol(sema: *Sema, sym_loc: SymbolLocation, module_idx: u32, node_idx: Ast.NodeIndex) !void {
    const ast = sema.modules[module_idx].ast;
    const fn_def = ast.node_data[node_idx].fn_def;

    const bank: u8 = if (fn_def.bank_attr == Ast.null_node)
        0x80
    else
        try ast.parseIntLiteral(u8, ast.node_tokens[fn_def.bank_attr]);

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
            if (@field(sema.interrupt_vectors, field.name) == null) {
                @field(sema.interrupt_vectors, field.name) = sym_loc;
            } else {
                try sema.errors.append(sema.allocator, .{
                    .tag = .duplicate_vector,
                    .type = .err,
                    .module = module_idx,
                    .node = node_idx,
                    .extra = .{ .vector = @enumFromInt(i) },
                });

                const existing_loc = @field(sema.interrupt_vectors, field.name).?;
                const existing_module = sema.symbol_map.getIndex(existing_loc.module);
                const existing_sym = sema.lookupSymbol(existing_loc).?;
                try sema.errors.append(sema.allocator, .{
                    .tag = .existing_sym,
                    .type = .note,
                    .module = @intCast(existing_module.?),
                    .node = existing_sym.function.node,
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
            .module = module_idx,
            .node = node_idx,
        });
    }
}

fn analyzeFunction(sema: *Sema, sym_loc: SymbolLocation, module_idx: u32, node_idx: *Symbol.Function) !void {
    _ = sema; // autofix
    _ = sym_loc; // autofix
    _ = module_idx; // autofix
    _ = node_idx; // autofix
}

fn createSymbol(sema: *Sema, sym_loc: SymbolLocation, sym: Symbol) !void {
    const gop = try sema.symbol_map.getOrPut(sema.allocator, sym_loc.module);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{};
    }
    try gop.value_ptr.put(sema.allocator, sym_loc.name, @intCast(sema.symbols.len));

    try sema.symbols.append(sema.allocator, .{ .sym = sym, .loc = sym_loc });
}

fn lookupSymbol(sema: Sema, sym_loc: SymbolLocation) ?Symbol {
    if (sema.symbol_map.get(sym_loc.module)) |module_symbols| {
        if (module_symbols.get(sym_loc.name)) |sym_idx| {
            return sema.symbols.items(.sym)[sym_idx];
        }
    }
    return null;
}

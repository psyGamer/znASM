const std = @import("std");
const symbol = @import("symbol.zig");

const Sema = @This();
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Symbol = symbol.Symbol;
const SymbolLocation = symbol.SymbolLocation;

/// Module -> Name -> Symbol
pub const SymbolMap = std.StringArrayHashMapUnmanaged(std.StringArrayHashMapUnmanaged(Symbol));

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

    module_idx: u32,
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

symbols: SymbolMap,
interrupt_vectors: InterruptVectors,
errors: std.ArrayListUnmanaged(Error),

pub fn process(allocator: std.mem.Allocator, modules: []Module) !Sema {
    var sema: Sema = .{
        .modules = modules,
        .symbols = .{},
        .interrupt_vectors = .{},
        .errors = .{},
    };

    // Gather symbols
    for (0..modules.len) |module_idx| {
        try sema.gatherSymbols(allocator, @intCast(module_idx));
    }

    // Only reset is required
    if (sema.interrupt_vectors.emulation_reset == null) {
        try sema.errors.append(allocator, .{
            .tag = .missing_reset_vector,
            .type = .err,
            .module_idx = undefined,
            .node = Ast.null_node,
        });
    }

    return sema;
}

pub fn deinit(sema: *Sema, allocator: std.mem.Allocator) void {
    sema.errors.deinit(allocator);

    for (sema.symbols.values()) |*module_symbols| {
        for (module_symbols.values()) |*sym| {
            sym.deinit(allocator);
        }
        module_symbols.deinit(allocator);
    }
    sema.symbols.deinit(allocator);
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
            const module = sema.modules[err.module_idx];
            const node = module.ast.nodes[err.node];
            const token = module.ast.tokens[node.main_token];

            const src_loc = std.zig.findLineColumn(module.source, token.loc.start);

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
            try writer.writeByteNTimes('~', token.loc.end - token.loc.start - 1);
            try tty_config.setColor(writer, .reset);
            try writer.writeByte('\n');
        }
    }

    return sema.errors.items.len != 0;
}

pub fn renderError(sema: Sema, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
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
            try writer.writeAll(sema.modules[err.module_idx].ast.nodes[err.node].tag.fn_def.name);
            try tty_config.setColor(writer, .reset);
        },
        .existing_sym => {
            try writer.writeAll("Symbol already declarded here");
        },
    }
}

fn gatherSymbols(sema: *Sema, allocator: std.mem.Allocator, module_idx: u32) !void {
    const module = &sema.modules[module_idx];
    const nodes = module.ast.nodes;
    _ = nodes; // autofix
    const ast = module.ast;

    const root = 0;

    std.log.err("Nodes:", .{});
    var child_iter = ast.iterChildren(root);
    node_loop: while (child_iter.next()) |child| {
        const child_idx = child_iter.index;
        std.log.err(" - {}", .{child});
        switch (child.tag) {
            .module => |module_name| module.name = module_name,
            .global_var_decl => |global_var_decl| {
                const module_name = module.name orelse {
                    try sema.errors.append(allocator, .{
                        .tag = .missing_module,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                };

                const sym_loc: SymbolLocation = .{
                    .module = module_name,
                    .name = global_var_decl.name,
                };

                if (sema.lookupSymbol(sym_loc) != null) {
                    try sema.errors.append(allocator, .{
                        .tag = .duplicate_symbol,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                }

                const sym: Symbol = .{
                    .variable = .{
                        .type = .parse(global_var_decl.type, module_name),
                        .is_const = global_var_decl.is_const,
                        .is_pub = global_var_decl.is_pub,

                        .bank = null,
                        .addr_min = null,
                        .addr_max = null,

                        .node = child_idx,
                    },
                };

                const gop = try sema.symbols.getOrPut(allocator, module_name);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                try gop.value_ptr.put(allocator, sym_loc.name, sym);
            },
            .fn_def => |fn_def| {
                const module_name = module.name orelse {
                    try sema.errors.append(allocator, .{
                        .tag = .missing_module,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                };

                const sym_loc: SymbolLocation = .{
                    .module = module_name,
                    .name = fn_def.name,
                };

                if (sema.lookupSymbol(sym_loc) != null) {
                    try sema.errors.append(allocator, .{
                        .tag = .duplicate_symbol,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                }

                const sym: Symbol = .{
                    .function = .{
                        // .return_type = .parse(fn_def.return_type),
                        // .segment = current_segment.?,
                        // .is_inline = fn_def.is_inline,
                        .is_pub = fn_def.is_pub,

                        .node = child_idx,
                    },
                };

                const gop = try sema.symbols.getOrPut(allocator, module_name);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                try gop.value_ptr.put(allocator, sym_loc.name, sym);

                // Handle interrupt vectors
                inline for (std.meta.fields(InterruptVectors), 0..) |field, i| {
                    if (std.mem.eql(u8, "@" ++ field.name, sym_loc.name)) {
                        if (@field(sema.interrupt_vectors, field.name) == null) {
                            @field(sema.interrupt_vectors, field.name) = sym_loc;
                        } else {
                            try sema.errors.append(allocator, .{
                                .tag = .duplicate_vector,
                                .type = .err,
                                .module_idx = module_idx,
                                .node = child_idx,
                                .extra = .{ .vector = @enumFromInt(i) },
                            });

                            const existing_loc = @field(sema.interrupt_vectors, field.name).?;
                            const existing_module = sema.symbols.getIndex(existing_loc.module);
                            const existing_sym = sema.lookupSymbol(existing_loc).?;
                            try sema.errors.append(allocator, .{
                                .tag = .existing_sym,
                                .type = .note,
                                .module_idx = @intCast(existing_module.?),
                                .node = existing_sym.function.node,
                            });
                        }

                        continue :node_loop;
                    }
                }

                if (std.mem.startsWith(u8, sym_loc.name, "@")) {
                    try sema.errors.append(allocator, .{
                        .tag = .invalid_vector_name,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                }
            },
            else => {
                try sema.errors.append(allocator, .{
                    .tag = .unexpected_top_level_node,
                    .type = .err,
                    .module_idx = module_idx,
                    .node = child_idx,
                });
                return;
            },
        }
    }
}

fn lookupSymbol(sema: Sema, sym_loc: SymbolLocation) ?Symbol {
    if (sema.symbols.get(sym_loc.module)) |module_symbols| {
        if (module_symbols.get(sym_loc.name)) |sym| {
            return sym;
        }
    }
    return null;
}

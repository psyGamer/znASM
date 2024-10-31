const std = @import("std");
const symbol = @import("symbol.zig");

const Sema = @This();
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Symbol = symbol.Symbol;
const SymbolLocation = symbol.SymbolLocation;

pub const Error = struct {
    pub const Tag = enum {
        unexpected_top_level_node,
        missing_module,
        // missing_segment,
        duplicate_symbol,
    };

    tag: Tag,
    type: enum { err, warn, note },

    module_idx: u32,
    node: Ast.NodeIndex,

    extra: union {
        none: void,
    } = .{ .none = {} },
};

modules: []Module,

/// Module -> Name -> Symbol
symbols: std.StringHashMapUnmanaged(std.StringHashMapUnmanaged(Symbol)),
errors: std.ArrayListUnmanaged(Error),

pub fn process(allocator: std.mem.Allocator, modules: []Module) !Sema {
    var sema: Sema = .{
        .modules = modules,
        .symbols = .{},
        .errors = .{},
    };

    // Gather symbols
    for (0..modules.len) |module_idx| {
        try sema.gatherSymbols(allocator, @intCast(module_idx));
    }

    // Generate IR
    for (modules) |module| {
        for (module.symbols.items) |*sym| {
            if (sym.* != .function) {
                continue;
            }

            // try sema.generateIR(allocator, module, &sym.function);
        }
    }

    return sema;
}

pub fn deinit(sema: *Sema, allocator: std.mem.Allocator) void {
    sema.errors.deinit(allocator);

    var iter = sema.symbols.valueIterator();
    while (iter.next()) |name_table| {
        name_table.deinit(allocator);
    }
    sema.symbols.deinit(allocator);
}

pub fn detectErrors(sema: Sema, writer: std.fs.File.Writer, tty_config: std.io.tty.Config) !bool {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    for (sema.errors.items) |err| {
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
        try writer.writeByte('\n');
    }

    return sema.errors.items.len != 0;
}

pub fn renderError(sema: Sema, writer: anytype, tty_config: std.io.tty.Config, err: Error) !void {
    _ = tty_config; // autofix
    _ = sema; // autofix
    switch (err.tag) {
        .unexpected_top_level_node => return writer.writeAll("Unexpected top level node"),
        .missing_module => return writer.writeAll("Missing module"),
        // .missing_segment => return writer.writeAll("Missing segment"),
        .duplicate_symbol => return writer.writeAll("Duplicate symbol"),
    }
}

fn gatherSymbols(sema: *Sema, allocator: std.mem.Allocator, module_idx: u32) !void {
    const module = &sema.modules[module_idx];
    // var current_module: ?[]const u8 = null;
    // var current_segment: ?[]const u8 = null;
    var opt_current_module: ?[]const u8 = null;
    _ = &opt_current_module;
    opt_current_module = "hi";

    const nodes = module.ast.nodes;

    const root = nodes[0];
    std.debug.assert(root.tag == .root);

    std.log.err("Nodes:", .{});
    for (root.children.items) |child_idx| {
        const child = nodes[child_idx];
        std.log.err(" - {}", .{child});
        switch (child.tag) {
            // .namespace => |namespace| current_namespace = namespace,
            // .segment => |segment| current_segment = segment,
            .global_var_decl => |global_var_decl| {
                const current_module = opt_current_module orelse {
                    try sema.errors.append(allocator, .{
                        .tag = .missing_module,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                };

                const sym_loc: SymbolLocation = .{
                    .module = current_module,
                    .name = global_var_decl.name,
                };

                if (sema.lookupSymbol(sym_loc, current_module) != null) {
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
                        .type = .parse(global_var_decl.type),
                        .is_const = global_var_decl.is_const,
                        .is_pub = global_var_decl.is_pub,

                        .bank = null,
                        .addr_min = null,
                        .addr_max = null,

                        .node = child_idx,
                    },
                };

                try module.symbols.append(allocator, sym);
                const gop = try sema.symbols.getOrPut(allocator, current_module);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                try gop.value_ptr.put(allocator, sym_loc.name, sym);
            },
            .fn_def => |fn_def| {
                const current_module = opt_current_module orelse {
                    try sema.errors.append(allocator, .{
                        .tag = .missing_module,
                        .type = .err,
                        .module_idx = module_idx,
                        .node = child_idx,
                    });
                    return;
                };

                const sym_loc: SymbolLocation = .{
                    .module = current_module,
                    .name = fn_def.name,
                };

                if (sema.lookupSymbol(sym_loc, current_module) != null) {
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

                try module.symbols.append(allocator, sym);
                const gop = try sema.symbols.getOrPut(allocator, current_module);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                try gop.value_ptr.put(allocator, sym_loc.name, sym);
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

fn generateIR(sema: Sema, allocator: std.mem.Allocator, module: Module, func_sym: *Symbol.Function) !void {
    _ = sema; // autofix

    const nodes = module.ast.nodes;

    const func = nodes[func_sym.node];
    std.debug.assert(func.tag == .fn_def);
    const block = nodes[func.children.items[0]];
    std.debug.assert(block.tag == .block_expr);

    for (block.children.items) |expr_idx| {
        const expr = nodes[expr_idx];
        _ = expr; // autofix
        // TODO: :catplush:
    }

    // Insert explicit return at end
    try func_sym.ir.append(allocator, .@"return");
}

fn lookupSymbol(sema: Sema, sym_loc: SymbolLocation, current_module: []const u8) ?Symbol {
    if (sema.symbols.get(sym_loc.module orelse current_module)) |module_symbols| {
        if (module_symbols.get(sym_loc.name)) |sym| {
            return sym;
        }
    }
    return null;
}

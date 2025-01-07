//! A processed .znasm file
const std = @import("std");

const Ast = @import("Ast.zig");
const Sema = @import("Sema.zig");
const Symbol = @import("symbol.zig").Symbol;
const Module = @This();

source: [:0]const u8,
source_path: []const u8,

name: []const u8,
ast: Ast,

symbol_map: std.StringArrayHashMapUnmanaged(Sema.SymbolIndex),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, source: [:0]const u8, source_path: []const u8) !?Module {
    var ast = try Ast.parse(allocator, source, source_path);

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    if (try ast.detectErrors(stderr.writer(), tty_config)) {
        allocator.free(source);
        ast.deinit(allocator);
        return null;
    }

    return .{
        .source = source,
        .source_path = source_path,

        .name = "",
        .ast = ast,

        .symbol_map = .empty,

        .allocator = allocator,
    };
}

pub fn deinit(module: *Module) void {
    module.allocator.free(module.source);

    module.ast.deinit(module.allocator);
    module.symbol_map.deinit(module.allocator);
}

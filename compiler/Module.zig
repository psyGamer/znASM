//! A processed .znasm file
const std = @import("std");

const Ast = @import("Ast.zig");
const Symbol = @import("symbol.zig").Symbol;
const Module = @This();

source: [:0]const u8,
source_path: []const u8,

name: ?[]const u8,
ast: Ast,

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, source: [:0]const u8, source_path: []const u8) !?Module {
    const ast = try Ast.parse(allocator, source, source_path);

    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    if (try ast.detectErrors(stderr.writer(), tty_config)) {
        return null;
    }

    return .{
        .source = source,
        .source_path = source_path,

        .name = null,
        .ast = ast,

        .allocator = allocator,
    };
}

pub fn deinit(self: *Module) void {
    self.allocator.free(self.source);
    self.ast.deinit(self.allocator);
}

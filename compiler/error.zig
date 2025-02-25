const std = @import("std");
const rich = @import("util/rich.zig");

const Module = @import("Module.zig");
const Ast = @import("Ast.zig");

const runtime_safty = switch (@import("builtin").mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

/// Unified error handling system
pub fn ErrorSystem(comptime error_set: anytype) type {
    const set_info = @typeInfo(@TypeOf(error_set)).@"struct";

    var tag_fields = [_]std.builtin.Type.EnumField{undefined} ** set_info.fields.len;
    for (set_info.fields, &tag_fields, 0..) |set_field, *tag_field, i| {
        tag_field.* = .{ .name = set_field.name, .value = i };
    }

    const tag_info: std.builtin.Type.Enum = .{
        .tag_type = std.math.IntFittingRange(0, set_info.fields.len - 1),
        .fields = &tag_fields,
        .decls = &.{},
        .is_exhaustive = true,
    };
    const TagType = @Type(.{ .@"enum" = tag_info });

    return struct {
        pub const Tag = TagType;

        /// Context for writing actual error message
        pub const MessageContext = struct {
            src_loc: std.zig.Loc,
            token_len: u32,

            writer: std.fs.File.Writer,
            tty_config: std.io.tty.Config,

            pub fn print(ctx: MessageContext, comptime fmt: []const u8, args: anytype) !void {
                return rich.print(ctx.writer, ctx.tty_config, fmt, args);
            }

            pub fn end(ctx: *const MessageContext) !void {
                defer std.debug.unlockStdErr();

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

                // Avoid accidentally re-using this context
                // Contexts are always stack-allocated, so this is safe
                const mut_ctx: *MessageContext = @constCast(ctx);
                mut_ctx.* = undefined;
            }
        };

        /// Formats the error-tag into a human readable message
        pub fn tagMessage(tag: Tag) []const u8 {
            switch (tag) {
                inline else => |t| {
                    for (set_info.fields) |set_field| {
                        if (std.mem.eql(u8, set_field.name, @tagName(t))) {
                            return @field(error_set, set_field.name);
                        }
                    }

                    unreachable;
                },
            }
        }

        pub const MessageType = enum { err, note };

        /// Starts a new message context for outputting information
        pub fn begin(module: *const Module, token_idx: Ast.TokenIndex, comptime msg_type: MessageType) !MessageContext {
            std.debug.lockStdErr();
            errdefer std.debug.unlockStdErr();

            const stderr = std.io.getStdErr();
            const tty_config = std.io.tty.detectConfig(stderr);

            const writer = stderr.writer();

            if (token_idx != .none) {
                const ast = &module.ast;

                const token_loc = ast.tokenLoc(token_idx);
                const src_loc = std.zig.findLineColumn(ast.source, token_loc.start);

                const src_args = .{ ast.source_path, src_loc.line + 1, src_loc.column + 1 };
                switch (msg_type) {
                    .err => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [red]error: ", src_args),
                    .note => try rich.print(writer, tty_config, "[bold]{s}:{}:{}: [cyan]note: ", src_args),
                }

                return .{
                    .src_loc = src_loc,
                    .token_len = @intCast(token_loc.end - token_loc.start),

                    .writer = writer,
                    .tty_config = tty_config,
                };
            } else {
                switch (msg_type) {
                    .err => try rich.print(writer, tty_config, "[bold red]error: ", .{}),
                    .note => try rich.print(writer, tty_config, "[bold cyan]note: ", .{}),
                }

                return .{
                    .src_loc = undefined,
                    .token_len = std.math.maxInt(u32),

                    .writer = writer,
                    .tty_config = tty_config,
                };
            }
        }
    };
}

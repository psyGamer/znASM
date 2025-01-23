//! Rich terminal printing, through comptime parsing
const std = @import("std");

/// [!]Hello World[] is highlighed with this style
pub const highlight_style = "bold bright_magenta";

/// Parsed [] as printing escape sequences, keeping {} for formatting
/// Example: "Hello [bold red]World[reset]: [blue]{s}"
pub fn print(writer: anytype, tty_config: std.io.tty.Config, comptime fmt: []const u8, args: anytype) !void {
    try tty_config.setColor(writer, .reset);

    const ArgsType = @TypeOf(args);
    const args_type_info = @typeInfo(ArgsType);
    if (args_type_info != .@"struct") {
        @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields_info = args_type_info.@"struct".fields;
    if (fields_info.len > @typeInfo(std.fmt.ArgSetType).int.bits) {
        @compileError("32 arguments max are supported per format call");
    }

    @setEvalBranchQuota(2000000);
    comptime var arg_state: std.fmt.ArgState = .{ .args_len = fields_info.len };
    comptime var i = 0;
    inline while (i < fmt.len) {
        const style_start_index = i;

        inline while (i < fmt.len) : (i += 1) {
            switch (fmt[i]) {
                '[', ']' => break,
                else => {},
            }
        }

        comptime var style_end_index = i;
        comptime var unescape_bracket = false;

        // Handle [[ and ]], those are un-escaped as single bracket
        if (i + 1 < fmt.len and fmt[i + 1] == fmt[i]) {
            unescape_bracket = true;
            // Make the first brace part of the literal...
            style_end_index += 1;
            // ...and skip both
            i += 2;
        }

        // Handle normal std.fmt.format logic
        if (style_start_index != style_end_index) {
            const sub_fmt = fmt[0..style_end_index];
            i = style_start_index;

            inline while (i < sub_fmt.len) {
                const fmt_start_index = i;

                inline while (i < sub_fmt.len) : (i += 1) {
                    switch (sub_fmt[i]) {
                        '{', '}' => break,
                        else => {},
                    }
                }

                comptime var fmt_end_index = i;
                comptime var unescape_brace = false;

                // Handle {{ and }}, those are un-escaped as single braces
                if (i + 1 < sub_fmt.len and sub_fmt[i + 1] == sub_fmt[i]) {
                    unescape_brace = true;
                    // Make the first brace part of the literal...
                    fmt_end_index += 1;
                    // ...and skip both
                    i += 2;
                }

                // Write out the literal
                if (fmt_start_index != fmt_end_index) {
                    try writer.writeAll(sub_fmt[fmt_start_index..fmt_end_index]);
                }

                // We've already skipped the other brace, restart the loop
                if (unescape_brace) continue;

                if (i >= sub_fmt.len) break;

                if (sub_fmt[i] == '}') {
                    @compileError("missing opening {");
                }

                // Get past the {
                comptime std.debug.assert(sub_fmt[i] == '{');
                i += 1;

                const fmt_begin = i;
                // Find the closing brace
                inline while (i < sub_fmt.len and sub_fmt[i] != '}') : (i += 1) {}
                const fmt_end = i;

                if (i >= sub_fmt.len) {
                    @compileError("missing closing }");
                }

                // Get past the }
                comptime std.debug.assert(sub_fmt[i] == '}');
                i += 1;

                const placeholder = comptime std.fmt.Placeholder.parse(sub_fmt[fmt_begin..fmt_end].*);
                const arg_pos = comptime switch (placeholder.arg) {
                    .none => null,
                    .number => |pos| pos,
                    .named => |arg_name| std.meta.fieldIndex(ArgsType, arg_name) orelse
                        @compileError("no argument with name '" ++ arg_name ++ "'"),
                };

                const width = switch (placeholder.width) {
                    .none => null,
                    .number => |v| v,
                    .named => |arg_name| blk: {
                        const arg_i = comptime std.meta.fieldIndex(ArgsType, arg_name) orelse
                            @compileError("no argument with name '" ++ arg_name ++ "'");
                        _ = comptime arg_state.nextArg(arg_i) orelse @compileError("too few arguments");
                        break :blk @field(args, arg_name);
                    },
                };

                const precision = switch (placeholder.precision) {
                    .none => null,
                    .number => |v| v,
                    .named => |arg_name| blk: {
                        const arg_i = comptime std.meta.fieldIndex(ArgsType, arg_name) orelse
                            @compileError("no argument with name '" ++ arg_name ++ "'");
                        _ = comptime arg_state.nextArg(arg_i) orelse @compileError("too few arguments");
                        break :blk @field(args, arg_name);
                    },
                };

                const arg_to_print = comptime arg_state.nextArg(arg_pos) orelse {
                    @compileLog(fmt, args);
                    @compileError("too few arguments");
                };

                try std.fmt.formatType(
                    @field(args, fields_info[arg_to_print].name),
                    placeholder.specifier_arg,
                    std.fmt.FormatOptions{
                        .fill = placeholder.fill,
                        .alignment = placeholder.alignment,
                        .width = width,
                        .precision = precision,
                    },
                    writer,
                    std.options.fmt_max_depth,
                );
            }

            i = style_end_index;
        }

        // We've already skipped the other bracket, restart the loop
        if (unescape_bracket) continue;

        if (i >= fmt.len) break;

        if (fmt[i] == ']') {
            @compileError("missing opening [");
        }

        // Get past the [
        comptime std.debug.assert(fmt[i] == '[');
        i += 1;

        const style_begin = i;
        // Find the closing bracket
        inline while (i < fmt.len and fmt[i] != ']') : (i += 1) {}
        const style_end = i;

        if (i >= fmt.len) {
            @compileError("missing closing ]");
        }

        // Get past the }
        comptime std.debug.assert(fmt[i] == ']');
        i += 1;

        const style_text = fmt[style_begin..style_end];

        // Reset for empty block
        if (style_text.len == 0) {
            try tty_config.setColor(writer, .reset);
            continue;
        }
        // Highlight for !
        if (style_text.len == 1 and style_text[0] == '!') {
            comptime var style_iter = std.mem.tokenizeAny(u8, highlight_style, " \t\n\r");
            inline while (comptime style_iter.next()) |style| {
                try tty_config.setColor(writer, comptime std.meta.stringToEnum(std.io.tty.Color, style).?);
            }
            continue;
        }

        // Regular style handling
        comptime var style_iter = std.mem.tokenizeAny(u8, style_text, " \t\n\r");
        inline while (comptime style_iter.next()) |style| {
            try tty_config.setColor(writer, comptime std.meta.stringToEnum(std.io.tty.Color, style).?);
        }
    }

    try tty_config.setColor(writer, .reset);
}

const std = @import("std");

pub fn logFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const prefix = comptime formatPrefix(level, scope);
    const text = comptime foramtText(level, format);

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    const stderr = std.io.getStdErr().writer();
    nosuspend std.fmt.format(stderr, prefix ++ text ++ "\n", args) catch return;
}

pub fn startLog(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
) std.fs.File.Writer {
    const prefix = comptime formatPrefix(level, scope);

    const escape_seq = "\x1b";
    const text_color = switch (level) {
        .debug => escape_seq ++ "[94m",
        .info => escape_seq ++ "[92m",
        .warn => escape_seq ++ "[93m",
        .err => escape_seq ++ "[91m",
    };

    std.debug.lockStdErr();
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll(prefix ++ text_color) catch {};
    return stderr;
}
pub fn endLog() void {
    const escape_seq = "\x1b";
    const clear_color = escape_seq ++ "[0m";

    const stderr = std.io.getStdErr().writer();
    stderr.writeAll(clear_color ++ "\n") catch return;
    std.debug.unlockStdErr();
}

fn formatPrefix(comptime level: std.log.Level, comptime scope: @TypeOf(.EnumLiteral)) []const u8 {
    const escape_seq = "\x1b";
    const gray_color = escape_seq ++ "[90m";

    const color = switch (level) {
        .debug => escape_seq ++ "[34m",
        .info => escape_seq ++ "[32m",
        .warn => escape_seq ++ "[33m",
        .err => escape_seq ++ "[31m",
    };

    const level_text = switch (level) {
        .debug => "Debug",
        .info => "Info",
        .warn => "Warn",
        .err => "Error",
    };
    // Used to align all log messages
    const padding = switch (level) {
        .debug => " ",
        .info => "  ",
        .warn => "  ",
        .err => " ",
    };

    if (scope == .default) {
        return gray_color ++ "[" ++ color ++ level_text ++ gray_color ++ "]:" ++ padding;
    } else {
        const scope_text = gray_color ++ "[" ++ color ++ @tagName(scope) ++ gray_color ++ "]: ";
        return gray_color ++ "[" ++ color ++ level_text ++ gray_color ++ "]" ++ padding ++ scope_text;
    }
}

fn foramtText(comptime level: std.log.Level, comptime text: []const u8) []const u8 {
    const escape_seq = "\x1b";
    const clear_color = escape_seq ++ "[0m";
    const text_color = switch (level) {
        .debug => escape_seq ++ "[94m",
        .info => escape_seq ++ "[92m",
        .warn => escape_seq ++ "[93m",
        .err => escape_seq ++ "[91m",
    };
    return text_color ++ text ++ clear_color;
}

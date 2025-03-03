//! Formatting utilities
const std = @import("std");

/// Escapes a stirng into an HTML-safe version
/// Example: "<A & B>" -> "&lt;A &amp; B&gt;"
pub fn fmtHtmlEscape(text: []const u8) std.fmt.Formatter(formatHtmlEscape) {
    return .{ .data = text };
}
fn formatHtmlEscape(text: []const u8, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    for (text) |byte| {
        switch (byte) {
            '<' => try writer.writeAll("&lt;"),
            '>' => try writer.writeAll("&gt;"),
            '&' => try writer.writeAll("&amp;"),
            '"' => try writer.writeAll("&quot;"),
            '\'' => try writer.writeAll("&apos;"),
            else => try writer.writeByte(byte),
        }
    }
}

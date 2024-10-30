const std = @import("std");

pub const std_options: std.Options = .{
    .logFn = @import("logging.zig").logFn,
    .log_level = .debug,
};

pub fn main() void {
    std.log.info("Hello World", .{});
}

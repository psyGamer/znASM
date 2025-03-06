const std = @import("std");
const Allocator = std.mem.Allocator;

/// This allocator is used in front of another allocator and tracks all memory operations
/// It can log to `std.log` with a given configuration
pub fn TrackingAllocator(
    comptime logging: ?struct {
        scope: @Type(.enum_literal),
        success_log_level: std.log.Level,
        failure_log_level: std.log.Level,
    },
) type {
    return struct {
        parent_allocator: Allocator,

        total_alloc_bytes: usize = 0,
        total_success_shrink_bytes: usize = 0,
        total_success_expand_bytes: usize = 0,
        total_attempt_shrink_bytes: usize = 0,
        total_attempt_expand_bytes: usize = 0,
        total_free_bytes: usize = 0,

        total_alloc_times: usize = 0,
        total_success_shrink_times: usize = 0,
        total_success_expand_times: usize = 0,
        total_attempt_shrink_times: usize = 0,
        total_attempt_expand_times: usize = 0,
        total_free_times: usize = 0,

        const Self = @This();

        pub fn init(parent_allocator: Allocator) Self {
            return .{
                .parent_allocator = parent_allocator,
            };
        }

        pub fn allocator(self: *Self) Allocator {
            return .{
                .ptr = self,
                .vtable = &.{
                    .alloc = alloc,
                    .resize = resize,
                    .remap = remap,
                    .free = free,
                },
            };
        }

        // This function is required as the `std.log.log` function is not public
        inline fn logHelper(comptime log_type: enum { success, failure }, comptime format: []const u8, args: anytype) void {
            if (logging) |config| {
                const log = std.log.scoped(config.scope);
                const log_level = switch (log_type) {
                    .success => config.success_log_level,
                    .failure => config.failure_log_level,
                };
                switch (log_level) {
                    .err => log.err(format, args),
                    .warn => log.warn(format, args),
                    .info => log.info(format, args),
                    .debug => log.debug(format, args),
                }
            }
        }

        fn alloc(
            context: *anyopaque,
            len: usize,
            alignment: std.mem.Alignment,
            ret_addr: usize,
        ) ?[*]u8 {
            const self: *Self = @ptrCast(@alignCast(context));
            self.total_alloc_bytes += len;
            self.total_alloc_times += 1;
            const result = self.parent_allocator.rawAlloc(len, alignment, ret_addr);
            if (result != null) {
                logHelper(
                    .success,
                    "alloc - success - len: {}, alignment: {}",
                    .{ len, alignment },
                );
            } else {
                logHelper(
                    .failure,
                    "alloc - failure: OutOfMemory - len: {}, alignment: {}",
                    .{ len, alignment },
                );
            }
            return result;
        }

        fn resize(
            context: *anyopaque,
            memory: []u8,
            alignment: std.mem.Alignment,
            new_len: usize,
            ret_addr: usize,
        ) bool {
            const self: *Self = @ptrCast(@alignCast(context));
            if (new_len <= memory.len) {
                self.total_attempt_shrink_bytes += memory.len - new_len;
                self.total_attempt_shrink_times += 1;
            } else {
                self.total_attempt_expand_bytes += new_len - memory.len;
                self.total_attempt_expand_times += 1;
            }

            if (self.parent_allocator.rawResize(memory, alignment, new_len, ret_addr)) {
                if (new_len <= memory.len) {
                    self.total_success_shrink_bytes += memory.len - new_len;
                    self.total_success_shrink_times += 1;
                    logHelper(
                        .success,
                        "resize shrink - success - {} to {}, alignment: {}",
                        .{ memory.len, new_len, alignment },
                    );
                } else {
                    self.total_success_expand_bytes += new_len - memory.len;
                    self.total_success_expand_times += 1;
                    logHelper(
                        .success,
                        "resize expand - success - {} to {}, alignment: {}",
                        .{ memory.len, new_len, alignment },
                    );
                }

                return true;
            }

            logHelper(
                .failure,
                "expand - failure - {} to {}, alignment: {}",
                .{ memory.len, new_len, alignment },
            );
            return false;
        }

        fn remap(
            context: *anyopaque,
            memory: []u8,
            alignment: std.mem.Alignment,
            new_len: usize,
            ret_addr: usize,
        ) ?[*]u8 {
            const self: *Self = @ptrCast(@alignCast(context));
            if (new_len <= memory.len) {
                self.total_attempt_shrink_bytes += memory.len - new_len;
                self.total_attempt_shrink_times += 1;
            } else {
                self.total_attempt_expand_bytes += new_len - memory.len;
                self.total_attempt_expand_times += 1;
            }

            const result = self.parent_allocator.rawRemap(memory, alignment, new_len, ret_addr);
            if (result != null) {
                if (new_len <= memory.len) {
                    self.total_success_shrink_bytes += memory.len - new_len;
                    self.total_success_shrink_times += 1;
                    logHelper(
                        .success,
                        "remap shrink - success - {} to {}, alignment: {}",
                        .{ memory.len, new_len, alignment },
                    );
                } else {
                    self.total_success_expand_bytes += new_len - memory.len;
                    self.total_success_expand_times += 1;
                    logHelper(
                        .success,
                        "remap expand - success - {} to {}, alignment: {}",
                        .{ memory.len, new_len, alignment },
                    );
                }
            } else {
                logHelper(
                    .failure,
                    "remap - failure - {} to {}, alignment: {}",
                    .{ memory.len, new_len, alignment },
                );
            }
            return result;
        }

        fn free(
            context: *anyopaque,
            memory: []u8,
            alignment: std.mem.Alignment,
            ret_addr: usize,
        ) void {
            const self: *Self = @ptrCast(@alignCast(context));
            self.parent_allocator.rawFree(memory, alignment, ret_addr);
            self.total_free_bytes += memory.len;
            self.total_free_times += 1;
            logHelper(.success, "free - len: {}", .{memory.len});
        }
    };
}

//! CRC implementation using slicing-by-8
const std = @import("std");

pub fn Crc32SliceBy8(comptime polynomial: u32) type {
    return struct {
        const Self = @This();
        pub const lookup_tables = block: {
            @setEvalBranchQuota(20000);

            const poly: u32 = @bitReverse(polynomial);

            var tables: [8][256]u32 = undefined;
            for (&tables[0], 0..) |*e, i| {
                var crc: u32 = i;
                for (0..8) |_| {
                    crc = (crc >> 1) ^ ((crc & 1) * poly);
                }
                e.* = crc;
            }

            for (0..tables[0].len) |i| {
                var crc = tables[0][i];
                for (tables[1..]) |*table| {
                    const index: u8 = @truncate(crc);
                    crc = tables[0][index] ^ (crc >> 8);
                    table[i] = crc;
                }
            }

            break :block tables;
        };

        crc: u32,

        pub fn init() Self {
            return Self{ .crc = 0xffffffff };
        }

        pub fn update(self: *Self, input: []const u8) void {
            var i: usize = 0;
            while (i + 8 <= input.len) : (i += 8) {
                const p = input[i..][0..8];

                // Unrolling this way gives ~50Mb/s increase
                self.crc ^= std.mem.readInt(u32, p[0..4], .little);

                self.crc =
                    lookup_tables[0][p[7]] ^
                    lookup_tables[1][p[6]] ^
                    lookup_tables[2][p[5]] ^
                    lookup_tables[3][p[4]] ^
                    lookup_tables[4][@as(u8, @truncate(self.crc >> 24))] ^
                    lookup_tables[5][@as(u8, @truncate(self.crc >> 16))] ^
                    lookup_tables[6][@as(u8, @truncate(self.crc >> 8))] ^
                    lookup_tables[7][@as(u8, @truncate(self.crc >> 0))];
            }

            while (i < input.len) : (i += 1) {
                const index = @as(u8, @truncate(self.crc)) ^ input[i];
                self.crc = (self.crc >> 8) ^ lookup_tables[0][index];
            }
        }

        pub fn final(self: *Self) u32 {
            return ~self.crc;
        }

        pub fn hash(input: []const u8) u32 {
            var c = Self.init();
            c.update(input);
            return c.final();
        }
    };
}

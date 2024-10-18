const std = @import("std");
const logging = @import("logging.zig");

const znasm = @import("znasm");

pub const std_options: std.Options = .{
    .logFn = logging.logFn,
};

pub fn main() !void {
    const config: znasm.Config = .{
        .title = "znASM Test",
        .mode = .{
            .map = .lorom,
            .speed = .fast,
        },
        .chipset = .{
            .components = .rom_ram,
            .coprocessor = .none,
        },
        .country = .usa,
        .rom_size = znasm.size.KiB(128),
        .ram_size = 0,
        .version = 0,
        .vectors = .{
            .native = .{},
            .emulation = .{
                .reset = Reset,
            },
        },
    };

    // Always a multiple of 4KiB (page size), so optimal allocator for the ROM
    const rom_allocator = std.heap.page_allocator;

    // const rom = try znasm.Rom.init(config);

    // const rom_data = try rom.generate(rom_allocator);
    // defer rom_allocator.free(rom_data);

    var rom_file = try std.fs.cwd().createFile("Testbed.sfc", .{});
    defer rom_file.close();

    var mlb_file = try std.fs.cwd().createFile("Testbed.mlb", .{});
    defer mlb_file.close();
    var cdl_file = try std.fs.cwd().createFile("Testbed.cdl", .{});
    defer cdl_file.close();

    try znasm.compile(config, rom_allocator, rom_file.writer(), mlb_file.writer(), cdl_file.writer());

    // try rom_file.writeAll(rom_data);

    // const testing_x = testing("x");
    // const s: std.builtin.Type.Struct = .{
    //     .layout = .auto,
    //     .fields = &.{
    //         .{
    //             .name = "write_x",
    //             .type = @TypeOf(testing_x),
    //             .default_value = testing_x,
    //             .is_comptime = true,
    //             .alignment = @alignOf(@TypeOf(testing_x)),
    //         },
    //     },
    //     .decls = &.{},
    //     .is_tuple = false,
    // };
    // const MyType = @Type(.{ .@"struct" = s });
    // const my_var: MyType = .{};
    // my_var.write_x();

    // // Always a multiple of 4KiB (page size), so optimal allocator for the ROM
    // const rom_allocator = std.heap.page_allocator;

    // var rom: znasm.Rom = .{
    //     .header = header,
    //     .vectors = vectors,
    // };

    // const rom_data = try rom.generateROM(rom_allocator);
    // defer rom_allocator.free(rom_data);

}

fn Reset(b: *znasm.Builder) void {
    b.setup_debug(@src(), @This(), null);
    const loop = b.define_label();
    // for (0..100) |_| {
    b.emit(.nop);
    // }
    b.emit_bra(loop);
}

// fn testing(comptime str: []const u8) fn () void {
//     return struct {
//         pub fn func() void {
//             std.log.info("Curried method: {s}", .{str});
//         }
//     }.func;
// }

// const Player = struct {
//     x: u16,
//     y: u16,
// };

fn myCoolFunc(b: *znasm.FunctionBuilder) void {
    const my_label = b.defineLabel();
    b.emitNop();
    b.emitBra(my_label);

    // const my_local = b.defineLocal(Player);
    // // b.write(my_local, "x", )
    // b.emitLda(20);
    // b.emitSta(my_local);
}

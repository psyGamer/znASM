const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const znasm_mod = b.addModule("znasm", .{
        .root_source_file = b.path("znasm/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    const snes_mod = b.addModule("snes", .{
        .root_source_file = b.path("snes/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    snes_mod.addImport("znasm", znasm_mod);

    // Only required for ZLS
    const lib = b.addStaticLibrary(.{
        .name = "znasm",
        .root_source_file = b.path("znasm/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.root_module.addImport("znasm", znasm_mod);
    lib.root_module.addImport("snes", snes_mod);

    b.installArtifact(lib);
}

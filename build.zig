const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const znasm_mod = b.addModule("znASM", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Only required for ZLS
    const lib = b.addStaticLibrary(.{
        .name = "znASM",
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.root_module.addImport("znasm", znasm_mod);

    b.installArtifact(lib);
}

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use LLVM for compiling") orelse (optimize != .Debug and target.result.os.tag == .linux);

    const exe = b.addExecutable(.{
        .name = "znASM-test",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.use_llvm = use_llvm;
    exe.use_lld = use_llvm;

    const znasm_dep = b.dependency("znasm", .{});
    exe.root_module.addImport("znasm", znasm_dep.module("znasm"));
    exe.root_module.addImport("snes", znasm_dep.module("snes"));

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

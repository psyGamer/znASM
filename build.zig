const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use LLVM for compiling SNasm") orelse (optimize != .Debug or target.result.os.tag == .windows);

    // const znasm_mod = b.addModule("znasm", .{
    //     .root_source_file = b.path("znasm/lib.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // const snes_mod = b.addModule("snes", .{
    //     .root_source_file = b.path("snes/lib.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // snes_mod.addImport("znasm", znasm_mod);

    const compiler_exe = b.addExecutable(.{
        .name = "compiler",
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    compiler_exe.use_llvm = use_llvm;
    compiler_exe.use_lld = use_llvm;
    b.installArtifact(compiler_exe);

    const run_cmd = b.addRunArtifact(compiler_exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_compiler_step = b.step("run", "Run the znASM compiler");
    run_compiler_step.dependOn(&run_cmd.step);

    const compiler_tests = b.addTest(.{
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    compiler_tests.test_server_mode = false;
    const run_compiler_tests = b.addRunArtifact(compiler_tests);

    const test_compiler_step = b.step("test", "Run znASM compiler unit tests");
    test_compiler_step.dependOn(&run_compiler_tests.step);

    // Only required for ZLS
    // const lib = b.addStaticLibrary(.{
    //     .name = "znasm",
    //     .root_source_file = b.path("znasm/lib.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // lib.root_module.addImport("znasm", znasm_mod);
    // lib.root_module.addImport("snes", snes_mod);

    // b.installArtifact(lib);
}

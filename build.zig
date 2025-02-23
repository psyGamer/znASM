const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use LLVM for compiling znASM") orelse (optimize != .Debug or target.result.os.tag == .windows);

    buildCompiler(b, target, optimize, use_llvm);
}

fn buildCompiler(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode, use_llvm: bool) void {
    const exe = b.addExecutable(.{
        .name = "znasm",
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.use_llvm = use_llvm;
    exe.use_lld = use_llvm;
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the znASM compiler");
    run_step.dependOn(&run_cmd.step);

    const docs_obj = b.addObject(.{
        .name = "znasm",
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = .Debug,
    });
    const docs = docs_obj.getEmittedDocs();
    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs,
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate znASM compiler documentation");
    docs_step.dependOn(&install_docs.step);
}

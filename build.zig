const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const options: Options = .{
        .use_llvm = b.option(bool, "use-llvm", "Use LLVM for compiling znASM") orelse (optimize != .Debug or target.result.os.tag == .windows),
        .track_allocations = b.option(Options.AllocationTracking, "track-allocations", "Tracks all performed memory allocations") orelse .none,
    };

    buildCompiler(b, target, optimize, options);
}

const Options = struct {
    pub const AllocationTracking = enum { all, total, none };

    use_llvm: bool,
    track_allocations: AllocationTracking,
};

fn buildCompiler(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode, options: Options) void {
    const build_options = b.addOptions();
    build_options.addOption(Options.AllocationTracking, @tagName(.track_allocations), options.track_allocations);

    const module = b.createModule(.{
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    module.addImport("build_options", build_options.createModule());

    // Executable
    const exe = b.addExecutable(.{
        .name = "znasm",
        .root_module = module,
    });
    exe.use_llvm = options.use_llvm;
    exe.use_lld = options.use_llvm;
    b.installArtifact(exe);

    // 'run' step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the znASM compiler");
    run_step.dependOn(&run_cmd.step);

    // 'check' step
    const exe_check = b.addExecutable(.{
        .name = "znasm",
        .root_module = module,
    });

    const check = b.step("check", "Check the znASM compiler for compilation errors");
    check.dependOn(&exe_check.step);

    // 'docs' step
    const docs_obj = b.addObject(.{
        .name = "znasm",
        .root_module = module,
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

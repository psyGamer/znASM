const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const logging = @import("logging.zig");
const builtin_module = @import("builtin_module.zig");

const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const SemanticIr = @import("sema/SemanticIr.zig");
const CodeGen = @import("CodeGen.zig");
const Linker = @import("Linker.zig");
const Rom = @import("Rom.zig");
const SymbolLocation = @import("symbol.zig").SymbolLocation;

const TrackingAllocator = @import("util/allocation.zig").TrackingAllocator;

pub const std_options: std.Options = .{
    .logFn = logging.logFn,
    .log_level = .debug,
    .fmt_max_depth = 4,
};

const CompilerOptions = struct {
    name: [21]u8 = [_]u8{' '} ** 21,
    output: []const u8 = "",
    source_files: std.ArrayListUnmanaged([]const u8) = .empty,

    dump_sir_graph: []const u8 = "",
};

pub fn main() !u8 {
    // Setup allocators
    var gpa = switch (builtin.mode) {
        .Debug => @as(std.heap.DebugAllocator(.{ .safety = true, .never_unmap = true, .retain_metadata = true, .resize_stack_traces = true }), .init),
        else => @as(void, {}),
    };
    defer switch (builtin.mode) {
        .Debug => std.debug.assert(gpa.deinit() == .ok),
        else => {},
    };
    const gpa_allocator = switch (builtin.mode) {
        .Debug => gpa.allocator(),
        else => std.heap.smp_allocator,
    };

    var tracking_allocator = switch (build_options.track_allocations) {
        .none => {},
        .total => @as(TrackingAllocator(null), .init(gpa.allocator())),
        .all => @as(TrackingAllocator(.{ .scope = .memory, .success_log_level = .debug, .failure_log_level = .err }), .init(gpa.allocator())),
    };

    const allocator =
        if (build_options.track_allocations == .none)
            gpa_allocator
        else
            tracking_allocator.allocator();

    var data_arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer data_arena.deinit();

    var temp_arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer temp_arena.deinit();

    defer if (build_options.track_allocations != .none) {
        const log = std.log.scoped(.memory);
        log.debug("Allocation Tracker results:", .{});

        if (tracking_allocator.total_expand_times == 0) {
            log.debug(" - Allocated {d} total bytes", .{tracking_allocator.total_expand_bytes});
        } else {
            log.debug(" - Allocated {d} total bytes in {d} calls with ~{d:.3} average bytes / call", .{
                tracking_allocator.total_alloc_bytes,
                tracking_allocator.total_alloc_times,
                @as(f32, @floatFromInt(tracking_allocator.total_alloc_bytes)) / @as(f32, @floatFromInt(tracking_allocator.total_alloc_times)),
            });
        }

        if (tracking_allocator.total_expand_times == 0) {
            log.debug(" - Freed {d} total bytes", .{tracking_allocator.total_expand_bytes});
        } else {
            log.debug(" - Freed {d} total bytes in {d} calls with ~{d:.3} average bytes / call", .{
                tracking_allocator.total_free_bytes,
                tracking_allocator.total_free_times,
                @as(f32, @floatFromInt(tracking_allocator.total_free_bytes)) / @as(f32, @floatFromInt(tracking_allocator.total_free_times)),
            });
        }

        if (tracking_allocator.total_expand_times == 0) {
            log.debug(" - Shrunken allocations by {d} total bytes", .{tracking_allocator.total_expand_bytes});
        } else {
            log.debug(" - Shrunken allocations by {d} total bytes in {d} calls with ~{d:.3} average bytes / call", .{
                tracking_allocator.total_shrink_bytes,
                tracking_allocator.total_shrink_times,
                @as(f32, @floatFromInt(tracking_allocator.total_shrink_bytes)) / @as(f32, @floatFromInt(tracking_allocator.total_shrink_times)),
            });
        }

        if (tracking_allocator.total_expand_times == 0) {
            log.debug(" - Expended allocations by {d} total bytes", .{tracking_allocator.total_expand_bytes});
        } else {
            log.debug(" - Expended allocations by {d} total bytes in {d} calls with ~{d:.3} average bytes / call", .{
                tracking_allocator.total_expand_bytes,
                tracking_allocator.total_expand_times,
                @as(f32, @floatFromInt(tracking_allocator.total_expand_bytes)) / @as(f32, @floatFromInt(tracking_allocator.total_expand_times)),
            });
        }
        log.debug("", .{});

        log.debug("Temporary Arena Capacity: {d} bytes", .{temp_arena.queryCapacity()});
        log.debug("Data Arena Capacity: {d} bytes", .{data_arena.queryCapacity()});
    };

    // Parse arguments
    var arg_iter =
        if (builtin.os.tag == .windows)
            std.process.argsWithAllocator(allocator)
        else
            std.process.args();
    defer arg_iter.deinit();

    // Skip executable
    _ = arg_iter.skip();

    // Parse CLI arguments
    var options: CompilerOptions = .{};

    while (arg_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--name")) {
            if (!std.mem.allEqual(u8, &options.name, ' ')) {
                std.log.err("Duplicate '--name' attribute", .{});
                return 1;
            }
            const name = arg_iter.next() orelse {
                std.log.err("Missing ROM name after '--name' attribute", .{});
                return 1;
            };
            if (name.len > options.name.len) {
                std.log.err("ROM name has a maximum length of {} characters, got {}", .{ options.name.len, arg.len });
                return 1;
            }

            @memcpy(options.name[0..name.len], name);
            continue;
        }
        if (std.mem.eql(u8, arg, "--output")) {
            if (options.output.len != 0) {
                std.log.err("Duplicate '--output' attribute", .{});
                return 1;
            }
            options.output = arg_iter.next() orelse {
                std.log.err("Missing output file name after '--output' attribute", .{});
                return 1;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--source")) {
            try options.source_files.append(data_arena.allocator(), arg_iter.next() orelse {
                std.log.err("Missing source file name after '--source' attribute", .{});
                return 1;
            });
            continue;
        }
        if (std.mem.eql(u8, arg, "--dump-sir-graph")) {
            if (options.dump_sir_graph.len != 0) {
                std.log.err("Duplicate '--dump-sir-graph' attribute", .{});
                return 1;
            }
            options.dump_sir_graph = arg_iter.next() orelse {
                std.log.err("Missing graph output file name after '--dump-sir-graph' attribute", .{});
                return 1;
            };
            continue;
        }

        std.log.err("Unsupported command-line argument: '{s}'", .{arg});
        return 1;
    }

    // Validate data
    if (std.mem.allEqual(u8, &options.name, ' ')) {
        std.log.err("No ROM name was specified ('--name')", .{});
        return 1;
    }
    if (options.output.len == 0) {
        std.log.err("No output file was specified ('--output')", .{});
        return 1;
    }
    if (options.source_files.items.len == 0) {
        std.log.err("No source files were specified ('--source')", .{});
        return 1;
    }

    return compile(allocator, &data_arena, &temp_arena, options);
}

fn compile(allocator: std.mem.Allocator, data_arena: *std.heap.ArenaAllocator, temp_arena: *std.heap.ArenaAllocator, options: CompilerOptions) !u8 {
    const data_allocator = data_arena.allocator();
    const temp_allocator = temp_arena.allocator();

    // Parse all files into modules
    var modules: std.ArrayListUnmanaged(Module) = .{};

    // Built-in module
    try modules.append(data_allocator, try builtin_module.init(data_allocator, temp_allocator));
    _ = temp_arena.reset(.retain_capacity);

    // User modules
    var has_errors = false;
    for (options.source_files.items) |src| {
        const log = std.log.scoped(.module);
        log.debug("Parsing source file '{s}'...", .{src});

        const src_file = try std.fs.cwd().openFile(src, .{});
        defer src_file.close();

        const src_data = try src_file.readToEndAllocOptions(data_allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);

        var ast: Ast = .{
            .source = src_data,
            .source_path = src,
        };
        ast.parse(data_allocator, temp_allocator) catch |err| switch (err) {
            error.ParseFailed => {
                has_errors = true;
                continue;
            },
            else => |e| return e,
        };
        _ = temp_arena.reset(.retain_capacity);

        try modules.append(data_allocator, .init(ast));
    }

    if (has_errors) {
        return 1;
    }

    // Run semantic analysis
    var sema: Sema = .{
        .mapping_mode = .lorom, // TODO: Configurable
        .modules = modules.items,
        .allocator = allocator,
        .data_arena = data_arena,
        .temp_arena = temp_arena,
    };
    errdefer sema.deinit();

    sema.process() catch |err| switch (err) {
        error.AnalyzeFailed => return 1,
        else => |e| return e,
    };
    defer sema.deinit();

    if (options.dump_sir_graph.len != 0) {
        // Delete and re-create file to avoid "File changed" pop-up stacking up in KGraphViewer even tho the setting to ask is disabled... -_-
        const cwd = std.fs.cwd();
        const graph_file = cwd.createFile(options.dump_sir_graph, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => b: {
                try cwd.deleteFile(options.dump_sir_graph);
                break :b try cwd.createFile(options.dump_sir_graph, .{ .exclusive = true });
            },
            else => |e| return e,
        };
        defer graph_file.close();

        const graph_writer = graph_file.writer();

        try SemanticIr.Graph.dumpPrologue(graph_writer);
        for (sema.symbols.items, 0..) |symbol, index| {
            var graph = switch (symbol) {
                .function => |fn_sym| fn_sym.semantic_ir,
                .constant => |const_sym| const_sym.sir_graph,
                else => continue,
            };
            try graph.dumpGraph(graph_writer, &sema, .cast(index));
        }
        try SemanticIr.Graph.dumpEpilogue(graph_writer);

        const abs_path = try std.fs.cwd().realpathAlloc(allocator, options.dump_sir_graph);
        defer allocator.free(abs_path);

        std.log.info("Successfully dumped Semantic IR graph to '{s}'", .{abs_path});
    }

    if (true) return 0;

    // Generate code
    var codegen: CodeGen = .{ .sema = &sema };
    codegen.process() catch |err| switch (err) {
        error.GenerateFailed => return 1,
        else => |e| return e,
    };

    // Link assembly together
    var linker = try Linker.process(&sema) orelse return 1;
    defer linker.deinit();

    // Generate ROM
    std.debug.assert(sema.interrupt_vectors.fallback.unpackSymbol() != null);

    var rom: Rom = .{
        .header = .{
            .title = options.name,
            // TODO: Configurable Mode / Chipset / Country / Version
            .mode = .{
                .map = sema.mapping_mode,
                .speed = .fast,
            },
            .chipset = .{
                .components = .rom,
                .coprocessor = .none,
            },
            .rom_size_log2_kb = undefined,
            .ram_size_log2_kb = 0,
            .country = .usa,
            .rom_version = 0,
        },
        .vectors = .{
            .native_cop = @truncate(linker.symbolLocation(sema.interrupt_vectors.native_cop.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .native_brk = @truncate(linker.symbolLocation(sema.interrupt_vectors.native_brk.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .native_abort = @truncate(linker.symbolLocation(sema.interrupt_vectors.native_abort.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)), // Unused
            .native_nmi = @truncate(linker.symbolLocation(sema.interrupt_vectors.native_nmi.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .native_irq = @truncate(linker.symbolLocation(sema.interrupt_vectors.native_irq.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .emulation_cop = @truncate(linker.symbolLocation(sema.interrupt_vectors.emulation_cop.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .emulation_abort = @truncate(linker.symbolLocation(sema.interrupt_vectors.emulation_abort.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)), // Unused
            .emulation_nmi = @truncate(linker.symbolLocation(sema.interrupt_vectors.emulation_nmi.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .emulation_reset = @truncate(linker.symbolLocation(sema.interrupt_vectors.emulation_reset.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
            .emulation_irqbrk = @truncate(linker.symbolLocation(sema.interrupt_vectors.emulation_irqbrk.unpackSymbol() orelse sema.interrupt_vectors.fallback.symbol)),
        },
        .banks = linker.rom_bank_data,
    };
    rom.computeRomSize();

    const rom_data = try rom.generate(allocator);
    defer allocator.free(rom_data);

    const rom_file = try std.fs.cwd().createFile(options.output, .{});
    defer rom_file.close();

    try rom_file.writeAll(rom_data);

    // Debug Data
    const debug = true; // TODO: Configurable
    if (debug) {
        const mlb_file_path = try changeExtension(allocator, options.output, ".mlb");
        defer allocator.free(mlb_file_path);
        const cdl_file_path = try changeExtension(allocator, options.output, ".cdl");
        defer allocator.free(cdl_file_path);

        const mlb_file = try std.fs.cwd().createFile(mlb_file_path, .{});
        defer mlb_file.close();
        const cdl_file = try std.fs.cwd().createFile(cdl_file_path, .{});
        defer cdl_file.close();

        try linker.writeMlbSymbols(mlb_file.writer());

        const cdl_data = try linker.generateCdlData(rom_data);
        defer allocator.free(cdl_data);
        try cdl_file.writeAll(cdl_data);
    }

    return 0;
}

fn changeExtension(allocator: std.mem.Allocator, src: []const u8, ext: []const u8) ![]const u8 {
    std.debug.assert(ext[0] == '.');

    if (std.mem.endsWith(u8, src, ext)) {
        // We return a duped copy here to prevent a situation where the caller
        // doesn't know whether to free. (They do not know the code path taken)
        return allocator.dupe(u8, src);
    }
    const stem = std.fs.path.stem(src);
    const dir = std.fs.path.dirname(src) orelse {
        return std.mem.join(allocator, "", &[_][]const u8{ stem, ext });
    };
    return std.mem.join(allocator, "", &[_][]const u8{ dir, std.fs.path.sep_str, ext });
}

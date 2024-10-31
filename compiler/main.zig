const std = @import("std");
const builtin = @import("builtin");
const log = @import("logging.zig");
const target_os = builtin.os.tag;

const znasm_builtin = @import("builtin.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const CodeGen = @import("CodeGen.zig");
const Rom = @import("Rom.zig");
const SymbolLocation = @import("symbol.zig").SymbolLocation;

// Required to execute tests
comptime {
    _ = Lexer;
    // _ = Ast;
}

pub const std_options: std.Options = .{
    .logFn = log.logFn,
    .log_level = .debug,
};

pub fn main() !u8 {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    // defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var arg_iter = if (target_os == .windows)
        std.process.argsWithAllocator(allocator)
    else
        std.process.args();
    defer arg_iter.deinit();

    // Skip executable
    _ = arg_iter.skip();

    // Parse CLI arguments
    var rom_name: ?[21]u8 = null;
    var output_file: ?[]const u8 = null;
    var source_files: std.ArrayListUnmanaged([]const u8) = .{};
    defer source_files.deinit(allocator);

    var state: enum { none, name, output, source_files } = .none;

    while (arg_iter.next()) |arg| {
        sw: switch (state) {
            .none => {
                if (std.mem.eql(u8, arg, "--name")) {
                    state = .name;
                    continue;
                }
                if (std.mem.eql(u8, arg, "--output")) {
                    state = .output;
                    continue;
                }
                if (std.mem.eql(u8, arg, "--source")) {
                    state = .source_files;
                    continue;
                }

                std.log.err("Unknown CLI argument '{s}'", .{arg});
                return 1;
            },
            .name => {
                if (arg.len > 21) {
                    std.log.err("ROM name has a maximum length of 21 characters, got {}", .{arg.len});
                    return 1;
                }

                rom_name = [_]u8{' '} ** 21;
                @memcpy(rom_name.?[0..arg.len], arg);

                state = .none;
            },
            .output => {
                output_file = arg;
                state = .none;
            },
            .source_files => {
                if (std.mem.startsWith(u8, arg, "--")) {
                    continue :sw .none;
                }

                try source_files.append(allocator, arg);
            },
        }
    }

    // Validate data
    if (rom_name == null) {
        std.log.err("No ROM name was specified", .{});
        return 1;
    }
    if (output_file == null) {
        std.log.err("No output file was specified", .{});
        return 1;
    }
    if (source_files.items.len == 0) {
        std.log.err("No source files were specified", .{});
        return 1;
    }

    return compile(allocator, rom_name.?, output_file.?, source_files.items);
}

fn compile(allocator: std.mem.Allocator, rom_name: [21]u8, output_file: []const u8, source_files: []const []const u8) !u8 {
    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);
    // Parse all files into modules
    var modules: std.ArrayListUnmanaged(Module) = .{};
    defer {
        for (modules.items) |*mod| {
            mod.deinit();
        }
        modules.deinit(allocator);
    }

    var hasErrors = false;
    for (source_files) |src| {
        std.log.debug("Parsing file '{s}'...", .{src});

        const src_file = try std.fs.cwd().openFile(src, .{});
        defer src_file.close();

        const src_data = try src_file.readToEndAllocOptions(allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);
        errdefer allocator.free(src_data);

        const module: Module = try .init(allocator, src_data, src);
        if (try module.ast.detectErrors(stderr.writer(), tty_config, src, src_data)) {
            hasErrors = true;
        } else {
            try modules.append(allocator, module);
        }
    }

    if (hasErrors) {
        return 1;
    }

    // Run semantic analysis
    var sema = try Sema.process(allocator, modules.items);
    defer sema.deinit(allocator);

    if (try sema.detectErrors(stderr.writer(), tty_config)) {
        return 1;
    }

    // Generate code
    var codegen: CodeGen = .{
        .mapping_mode = .lorom, // TODO: Configurable
        .symbols = sema.symbols,
        .allocator = allocator,
    };
    for (modules.items) |mod| {
        try codegen.generate(mod);
    }

    // Include builtin functions
    const builtin_gop = try codegen.symbols.getOrPut(allocator, znasm_builtin.empty_vector_loc.module);
    if (!builtin_gop.found_existing) {
        builtin_gop.value_ptr.* = .empty;
    }
    try builtin_gop.value_ptr.put(allocator, znasm_builtin.empty_vector_loc.name, znasm_builtin.empty_vector_sym);

    try codegen.generateBanks();
    try codegen.resolveRelocations();

    const banks = try codegen.allocateBanks();

    // Generate ROM
    var rom: Rom = .{
        .header = .{
            .title = rom_name,
            // TODO: Configurable Mode / Chipset / Country / Version
            .mode = .{
                .map = codegen.mapping_mode,
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
            .native_cop = @truncate(codegen.symbolLocation(sema.interrupt_vectors.native_cop orelse znasm_builtin.empty_vector_loc)),
            .native_brk = @truncate(codegen.symbolLocation(sema.interrupt_vectors.native_brk orelse znasm_builtin.empty_vector_loc)),
            .native_abort = @truncate(codegen.symbolLocation(sema.interrupt_vectors.native_abort orelse znasm_builtin.empty_vector_loc)), // Unused
            .native_nmi = @truncate(codegen.symbolLocation(sema.interrupt_vectors.native_nmi orelse znasm_builtin.empty_vector_loc)),
            .native_irq = @truncate(codegen.symbolLocation(sema.interrupt_vectors.native_irq orelse znasm_builtin.empty_vector_loc)),
            .emulation_cop = @truncate(codegen.symbolLocation(sema.interrupt_vectors.emulation_cop orelse znasm_builtin.empty_vector_loc)),
            .emulation_abort = @truncate(codegen.symbolLocation(sema.interrupt_vectors.emulation_abort orelse znasm_builtin.empty_vector_loc)), // Unused
            .emulation_nmi = @truncate(codegen.symbolLocation(sema.interrupt_vectors.emulation_nmi orelse znasm_builtin.empty_vector_loc)),
            .emulation_reset = @truncate(codegen.symbolLocation(sema.interrupt_vectors.emulation_reset orelse znasm_builtin.empty_vector_loc)),
            .emulation_irqbrk = @truncate(codegen.symbolLocation(sema.interrupt_vectors.emulation_irqbrk orelse znasm_builtin.empty_vector_loc)),
        },
        .banks = banks,
    };
    rom.computeRomSize();

    const rom_data = try rom.generate(allocator);
    defer allocator.free(rom_data);

    const rom_file = try std.fs.cwd().createFile(output_file, .{});
    defer rom_file.close();

    try rom_file.writeAll(rom_data);

    // Debug Data
    const debug = true; // TODO: Configurable
    if (debug) {
        const mlb_file_path = try changeExtension(allocator, output_file, ".mlb");
        defer allocator.free(mlb_file_path);
        const cdl_file_path = try changeExtension(allocator, output_file, ".cdl");
        defer allocator.free(cdl_file_path);

        const mlb_file = try std.fs.cwd().createFile(mlb_file_path, .{});
        defer mlb_file.close();
        const cdl_file = try std.fs.cwd().createFile(cdl_file_path, .{});
        defer cdl_file.close();

        try codegen.writeMlbSymbols(mlb_file.writer(), modules.items);

        const cdl_data = try codegen.generateCdlData();
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

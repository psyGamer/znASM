const std = @import("std");
const Symbol = @import("Function.zig").Symbol;
const SymbolPtr = @import("Function.zig").SymbolPtr;
const BuildSystem = @import("BuildSystem.zig");
const Instruction = @import("instruction.zig").Instruction;
const RegA = @import("register/A.zig");

const Builder = @This();

/// Offset to target instruction from function start in bytes
pub const Label = struct {
    offset: ?u16,

    /// Defines the label to point to the next instruction
    pub fn define(label: *Label, b: *const Builder) void {
        label.offset = @intCast(b.instruction_data.items.len);
    }
};

/// Branches need to be relocated to point to their label and use the short / long form
const BranchRelocation = struct {
    pub const Type = enum {
        always,
        jump_long,
    };

    offset: u16,
    target: *Label,
    type: Type,
};

/// Metadata information about an instruction
pub const InstructionInfo = struct {
    const IndexMode = enum { @"8bit", @"16bit" };

    /// A relocatoion indicates that this instruction has an operand to another symbol,
    /// which needs to be fixed after emitting the data into ROM
    const Relocation = struct {
        type: enum { rel8, addr8, addr16, addr24 },
        target_sym: SymbolPtr,
        target_offset: u16,
    };

    instr: Instruction,
    offset: u16,

    reloc: ?Relocation,
    index_mode: IndexMode,

    caller_file: ?[]const u8,
    caller_line: ?u64,
};

build_system: *BuildSystem,

symbol: SymbolPtr,
instruction_data: std.ArrayListUnmanaged(u8) = .{},
instruction_info: std.ArrayListUnmanaged(InstructionInfo) = .{},

reg_a: ?RegA = null,

labels: std.ArrayListUnmanaged(*const Label) = .{},
branch_relocs: std.ArrayListUnmanaged(BranchRelocation) = .{},

id_prng: std.Random.DefaultPrng = .init(0),

// Debug data
symbol_name: ?[]const u8 = null,
source_location: ?std.builtin.SourceLocation = null,

pub fn deinit(b: *Builder) void {
    for (b.labels.items) |label| {
        b.build_system.allocator.destroy(label);
    }

    b.labels.deinit(b.build_system.allocator);
    b.branch_relocs.deinit(b.build_system.allocator);
}

/// Provides debug information to znASM for proper labels
pub fn setup_debug(b: *Builder, src: std.builtin.SourceLocation, declaring_type: type, overwrite_symbol_name: ?[]const u8) void {
    b.symbol_name = overwrite_symbol_name orelse std.fmt.allocPrint(b.build_system.allocator, "{s}@{s}", .{ @typeName(declaring_type), src.fn_name }) catch @panic("Out of memory");
    b.source_location = src;
}

/// Creates a new undefined label
pub fn create_label(b: *Builder) *Label {
    var label = b.build_system.allocator.create(Label) catch @panic("Out of memory");
    label.offset = null;

    b.labels.append(b.build_system.allocator, label) catch @panic("Out of memory");
    return label;
}

/// Creates and defines and new label
pub fn define_label(b: *Builder) *Label {
    var label = b.create_label();
    label.define(b);
    return label;
}

/// Creates a new unique ID to mark the current good state of a register
pub fn register_id(b: *Builder) u64 {
    return b.id_prng.next();
}

/// Sets up the A register in 8-bit mode
pub fn reg_a8(b: *Builder) RegA {
    b.reg_a = .{
        .mode = .@"8bit",
        .builder = b,
        .id = 0,
    };
    // Define ID after returning, since it's undefiend initially
    defer b.reg_a.?.id = b.register_id();

    return b.reg_a.?;
}

// Instrucion Emitting
// NOTE: This intentionally doesn't expose OutOfMemory errors, to keep the API simpler (they would crash the assembler anyway)

pub fn emit(b: *Builder, instr: Instruction) void {
    b.emit_extra(instr, null);
}

pub fn emit_extra(b: *Builder, instr: Instruction, reloc: ?InstructionInfo.Relocation) void {
    const caller_file, const caller_line = b.resolve_caller_src(@returnAddress()) orelse .{ null, null };

    b.instruction_info.append(b.build_system.allocator, .{
        .instr = instr,
        .offset = @intCast(b.instruction_data.items.len),

        .reloc = reloc,
        .index_mode = .@"8bit",

        .caller_file = caller_file,
        .caller_line = caller_line,
    }) catch @panic("Out of memory");

    const indexing_mode = switch (instr.target_register()) {
        .none => .none,
        .a => b.reg_a.?.mode,
        .x, .y => unreachable,
    };
    instr.write_data(b.instruction_data.writer(b.build_system.allocator), indexing_mode) catch @panic("Out of memory");
}

/// Calls the target method
pub fn call(b: *Builder, target: Symbol) void {
    b.build_system.enqueue_function(target) catch @panic("Out of memory");

    b.emit_extra(.{ .jsr = undefined }, .{
        .type = .addr16,
        .target_sym = target,
        .target_offset = 0,
    });
}

/// Always branch to the target label
pub fn branch_always(b: *Builder, target: *Label) void {
    b.branch_relocs.append(b.build_system.allocator, .{
        .offset = @intCast(b.instruction_data.items.len),
        .target = target,
        .type = .always,
    }) catch @panic("Out of memory");
    b.emit(.nop);
}

/// Jumps Long to the target symbol or label
pub fn jump_long(b: *Builder, target: anytype) void {
    if (@TypeOf(target) == *Label) {
        b.branch_relocs.append(b.build_system.allocator, .{
            .offset = @intCast(b.instruction_data.items.len),
            .target = target,
            .type = .jump_long,
        }) catch @panic("Out of memory");
        b.emit(.nop);
    } else if (@TypeOf(target) == Symbol or @TypeOf(target) == SymbolPtr) {
        b.emit_extra(.{ .jml = undefined }, .{
            .type = .addr24,
            .target_sym = target,
            .target_offset = 0,
        });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported target type '{s}'", .{@typeName(@TypeOf(target))}));
    }
}

/// Invokes the generator function associated with this builder
pub fn build(b: *Builder) !void {
    b.symbol(b);
    try b.resolve_branch_relocs();
}

fn resolve_branch_relocs(b: *Builder) !void {
    // New instructions are emitted in the middle, causing other relocs to shift down
    var byte_offset: u16 = 0;

    const short_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
        .always = comptime Instruction.bra.size(),
        .jump_long = comptime Instruction.jml.size(),
    });
    const long_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
        .always = comptime Instruction.jmp.size(),
        .jump_long = comptime Instruction.jml.size(),
    });

    var data_buffer: [16]u8 = undefined;
    var data_fba: std.heap.FixedBufferAllocator = .init(&data_buffer);
    const data_allocator = data_fba.allocator();

    for (b.branch_relocs.items) |reloc| {
        // Assume all branches use the long for to simply the calculation
        var relative_offset: i32 = @as(i32, @intCast(reloc.target.offset.?)) - (@as(i32, @intCast(reloc.offset)));
        for (b.branch_relocs.items) |other_reloc| {
            if (other_reloc.offset <= reloc.offset or other_reloc.offset >= reloc.target.offset.?) {
                continue;
            }

            relative_offset += long_sizes.get(other_reloc.type);
        }

        const use_short =
            relative_offset + short_sizes.get(reloc.type) >= std.math.minInt(i8) and
            relative_offset + short_sizes.get(reloc.type) <= std.math.maxInt(i8);

        const size = if (use_short)
            short_sizes.get(reloc.type)
        else
            long_sizes.get(reloc.type);

        // Shift later targets (including ourselves)
        for (b.branch_relocs.items) |*later_reloc| {
            if (later_reloc.offset >= reloc.offset and later_reloc.target.offset.? > reloc.offset + byte_offset) {
                later_reloc.target.offset = later_reloc.target.offset.? + size - comptime Instruction.nop.size();
            }
        }

        const insert_offset = reloc.offset + byte_offset;
        const target_offset = reloc.target.offset.?;

        switch (reloc.type) {
            .always => {
                if (use_short) {
                    try b.insert_branch_instructions(insert_offset, data_allocator, &.{
                        .{ .bra = undefined },
                    }, &.{.{
                        .type = .rel8,
                        .target_sym = b.symbol,
                        .target_offset = target_offset - comptime Instruction.bra.size(),
                    }});
                } else {
                    try b.insert_branch_instructions(insert_offset, data_allocator, &.{
                        .{ .jmp = undefined },
                    }, &.{.{
                        .type = .addr16,
                        .target_sym = b.symbol,
                        .target_offset = target_offset,
                    }});
                }
            },
            .jump_long => {
                try b.insert_branch_instructions(insert_offset, data_allocator, &.{
                    .{ .jml = undefined },
                }, &.{.{
                    .type = .addr24,
                    .target_sym = b.symbol,
                    .target_offset = target_offset,
                }});
            },
        }

        byte_offset += size - comptime Instruction.nop.size();
    }
}

/// Replaces a branch NOP with the actual instructions
fn insert_branch_instructions(b: *Builder, offset: u16, data_allocator: std.mem.Allocator, instrs: []const Instruction, relocs: []const ?InstructionInfo.Relocation) !void {
    const index = b: {
        for (b.instruction_info.items, 0..) |info, i| {
            if (offset <= info.offset) {
                break :b i;
            }
        }
        @panic("Branch relocation offset outside of bounds of function");
    };

    const instr_data = try Instruction.to_data(instrs, data_allocator);

    // Shift over existing instructions (replacing the current NOP)
    try b.instruction_data.ensureUnusedCapacity(b.build_system.allocator, instr_data.len - comptime Instruction.nop.size());
    const old_instr_data_len = b.instruction_data.items.len;
    b.instruction_data.items.len += instr_data.len - comptime Instruction.nop.size();

    std.mem.copyBackwards(u8, b.instruction_data.items[(offset + instr_data.len)..], b.instruction_data.items[(offset + comptime Instruction.nop.size())..old_instr_data_len]);
    @memcpy(b.instruction_data.items[offset..(offset + instr_data.len)], instr_data);

    try b.instruction_info.ensureUnusedCapacity(b.build_system.allocator, instrs.len - comptime Instruction.nop.size());
    const old_instr_info_len = b.instruction_info.items.len;
    b.instruction_info.items.len += instrs.len - 1;

    var old_info = b.instruction_info.items[index];
    std.mem.copyBackwards(InstructionInfo, b.instruction_info.items[(index + instrs.len)..], b.instruction_info.items[(index + 1)..old_instr_info_len]);
    for (b.instruction_info.items[index..(index + instrs.len)], instrs, relocs) |*info, instr, reloc| {
        info.* = .{
            .instr = instr,
            .offset = old_info.offset,
            .reloc = reloc,
            .index_mode = old_info.index_mode,
            .caller_file = old_info.caller_file,
            .caller_line = old_info.caller_line,
        };

        old_info.offset += instr.size();
    }

    for (b.instruction_info.items) |*info| {
        if (info.offset <= offset) {
            continue;
        }

        info.offset += @intCast(instr_data.len - 1);
    }
}

/// Unwinds the stack to find the line number of the calling function
fn resolve_caller_src(b: Builder, start_addr: usize) ?struct { []const u8, u64 } {
    if (b.source_location == null) {
        return null;
    }

    const debug_info = std.debug.getSelfDebugInfo() catch return null;

    var context: std.debug.ThreadContext = undefined;
    const has_context = std.debug.getContext(&context);

    var it = (if (has_context) blk: {
        break :blk std.debug.StackIterator.initWithContext(start_addr, debug_info, &context) catch null;
    } else null) orelse std.debug.StackIterator.init(start_addr, null);
    defer it.deinit();

    while (it.next()) |return_address| {
        const addr = return_address -| 1;
        const module = debug_info.getModuleForAddress(addr) catch return null;
        const symbol = module.getSymbolAtAddress(b.build_system.allocator, addr) catch return null;
        defer if (symbol.source_location) |sl| debug_info.allocator.free(sl.file_name);

        const srcloc = symbol.source_location orelse continue;

        // TODO: Handle windows stupid \
        if (std.mem.endsWith(u8, srcloc.file_name, b.source_location.?.file)) {
            return .{
                b.build_system.allocator.dupe(u8, srcloc.file_name) catch return null,
                srcloc.line,
            };
        }
    }

    return null;
}

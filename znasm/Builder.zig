const std = @import("std");
const BuildSystem = @import("BuildSystem.zig");
const Instruction = @import("instruction.zig").Instruction;
const Symbol = @import("symbol.zig").Symbol;
const CallConv = @import("Function.zig").CallingConvention;
const RegA = @import("register.zig").Register(.a);
const RegX = @import("register.zig").Register(.x);
const RegY = @import("register.zig").Register(.y);

const Builder = @This();

// NOTE: This intentionally doesn't expose OutOfMemory errors, to keep the API simpler (they would crash the assembler anyway)

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
        target_sym: Symbol,
        target_offset: u16,
    };

    instr: Instruction,
    offset: u16,

    reloc: ?Relocation,
    index_mode: IndexMode,

    caller_file: ?[]const u8,
    caller_line: ?u64,
};

/// A value which is either an input or output of this function
pub const CallValue = union(enum) {
    a: void,
    x: void,
    y: void,
};

build_system: *BuildSystem,

symbol: Symbol.Function,
instruction_data: std.ArrayListUnmanaged(u8) = .{},
instruction_info: std.ArrayListUnmanaged(InstructionInfo) = .{},

// Register State
a_size: Instruction.SizeMode = .none,
xy_size: Instruction.SizeMode = .none,
a_reg_id: ?u64 = null,
x_reg_id: ?u64 = null,
y_reg_id: ?u64 = null,

// Calling convention
start_a_size: Instruction.SizeMode = .none,
start_xy_size: Instruction.SizeMode = .none,
end_a_size: Instruction.SizeMode = .none,
end_xy_size: Instruction.SizeMode = .none,

/// Input values for this function
inputs: std.AutoArrayHashMapUnmanaged(CallValue, void) = .{},
/// Output values from this function
outputs: std.AutoArrayHashMapUnmanaged(CallValue, void) = .{},
/// Values which are modified by this function, potentially leaving them in an invalid state
clobbers: std.AutoArrayHashMapUnmanaged(CallValue, void) = .{},

// Branches
labels: std.ArrayListUnmanaged(*const Label) = .{},
branch_relocs: std.ArrayListUnmanaged(BranchRelocation) = .{},

// Debug data
symbol_name: ?[]const u8 = null,
source_location: ?std.builtin.SourceLocation = null,

pub fn deinit(b: *Builder) void {
    b.inputs.deinit(b.build_system.allocator);
    b.outputs.deinit(b.build_system.allocator);
    b.clobbers.deinit(b.build_system.allocator);

    for (b.labels.items) |label| {
        b.build_system.allocator.destroy(label);
    }

    b.labels.deinit(b.build_system.allocator);
    b.branch_relocs.deinit(b.build_system.allocator);
}

/// Provides debug information for proper labels
pub fn setup_debug(b: *Builder, src: std.builtin.SourceLocation, declaring_type: type, overwrite_symbol_name: ?[]const u8) void {
    b.symbol_name = overwrite_symbol_name orelse std.fmt.allocPrint(b.build_system.allocator, "{s}@{s}", .{ @typeName(declaring_type), src.fn_name }) catch @panic("Out of memory");
    b.source_location = src;
}

// Labels

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

// Registers

/// Sets up the A register in 8-bit mode
pub fn reg_a8(b: *Builder) RegA {
    std.debug.assert(b.start_a_size != .@"16bit");
    b.change_status_flags(.{ .a_8bit = true });
    return .next(b);
}

/// Sets up the A register in 16-bit mode
pub fn reg_a16(b: *Builder) RegA {
    std.debug.assert(b.start_a_size != .@"8bit");
    b.change_status_flags(.{ .a_8bit = false });
    return .next(b);
}

/// Sets up the X register in 8-bit mode
pub fn reg_x8(b: *Builder) RegX {
    std.debug.assert(b.start_xy_size != .@"16bit");
    b.change_status_flags(.{ .xy_8bit = true });
    return .next(b);
}

/// Sets up the X register in 16-bit mode
pub fn reg_x16(b: *Builder) RegX {
    std.debug.assert(b.start_xy_size != .@"8bit");
    b.change_status_flags(.{ .xy_8bit = false });
    return .next(b);
}

/// Sets up the Y register in 8-bit mode
pub fn reg_y8(b: *Builder) RegY {
    std.debug.assert(b.start_xy_size != .@"16bit");
    b.change_status_flags(.{ .xy_8bit = true });
    return .next(b);
}

/// Sets up the Y register in 16-bit mode
pub fn reg_y16(b: *Builder) RegY {
    std.debug.assert(b.start_xy_size != .@"8bit");
    b.change_status_flags(.{ .xy_8bit = false });
    return .next(b);
}

/// Sets up the X and Y registers in 8-bit mode
pub fn reg_xy8(b: *Builder) struct { RegX, RegY } {
    std.debug.assert(b.start_xy_size != .@"16bit");
    b.change_status_flags(.{ .xy_8bit = true });
    return .{ .next(b), .next(b) };
}

/// Sets up the X and Y registers in 8-bit mode
pub fn reg_xy16(b: *Builder) struct { RegX, RegY } {
    std.debug.assert(b.start_xy_size != .@"8bit");
    b.change_status_flags(.{ .xy_8bit = false });
    return .{ .next(b), .next(b) };
}

// Instrucion Emitting

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
        .a => b.a_size,
        .x, .y => b.xy_size,
    };
    instr.write_data(b.instruction_data.writer(b.build_system.allocator), indexing_mode) catch @panic("Out of memory");

    // Ensure every return leaves with the same register sizes
    if (instr == .rts or instr == .rtl) {
        if (b.a_size != .none) {
            if (b.end_a_size == .none) {
                b.end_a_size = b.a_size;
            } else {
                std.debug.assert(b.end_a_size == b.a_size);
            }
        }
        if (b.xy_size != .none) {
            if (b.end_xy_size == .none) {
                b.end_xy_size = b.xy_size;
            } else {
                std.debug.assert(b.end_xy_size == b.xy_size);
            }
        }
    }
}

// Helpers

/// Calls the target method, automatically respecting the calling convention
pub fn call(b: *Builder, target: Symbol.Function) void {
    const target_func = b.build_system.register_function(target) catch @panic("Out of memory");
    if (target_func.code.len == 0) {
        @panic("Circular dependency detected: Target function isn't generated yet! Consider using call_with_convention() or jump_subroutine()");
    }

    b.call_with_convention(target, target_func.call_conv);
}

pub fn call_with_convention(b: *Builder, target: Symbol.Function, call_conv: CallConv) void {
    var change: ChangeStatusRegister = .{};
    if (call_conv.start_a_size != .none) {
        if (b.start_a_size == .none) {
            // Forward size
            b.start_a_size = call_conv.start_a_size;
        } else {
            change.a_8bit = call_conv.start_a_size == .@"8bit";
        }
    }
    if (call_conv.start_xy_size != .none) {
        if (b.start_xy_size == .none) {
            // Forward size
            b.start_xy_size = call_conv.start_xy_size;
        } else {
            change.xy_8bit = call_conv.start_xy_size == .@"8bit";
        }
    }

    if (call_conv.end_a_size != .none) {
        b.a_size = call_conv.end_a_size;
    }
    if (call_conv.end_xy_size != .none) {
        b.xy_size = call_conv.end_xy_size;
    }

    for (call_conv.clobbers) |clobber| {
        switch (clobber) {
            .a => _ = RegA.next(b),
            .x => _ = RegX.next(b),
            .y => _ = RegY.next(b),
        }
    }

    b.change_status_flags(change);
    b.emit_extra(.{ .jsr = undefined }, .{
        .type = .addr16,
        .target_sym = .{ .function = target },
        .target_offset = 0,
    });
}

/// Jumps the target subroutine
pub fn jump_subroutine(b: *Builder, target: Symbol.Function) void {
    b.emit_extra(.{ .jsr = undefined }, .{
        .type = .addr16,
        .target_sym = .{ .function = target },
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
    } else if (@TypeOf(target) == Symbol) {
        std.debug.assert(target == .function);
        b.emit_extra(.{ .jml = undefined }, .{
            .type = .addr24,
            .target_sym = target,
            .target_offset = 0,
        });
    } else if (@TypeOf(target) == Symbol.Function) {
        b.emit_extra(.{ .jml = undefined }, .{
            .type = .addr24,
            .target_sym = .{ .function = target },
            .target_offset = 0,
        });
    } else {
        @compileError(std.fmt.comptimePrint("Unsupported target type '{s}'", .{@typeName(@TypeOf(target))}));
    }
}

const ChangeStatusRegister = struct {
    carry: ?bool = null,
    zero: ?bool = null,
    irq_disable: ?bool = null,
    decimal: ?bool = null,
    xy_8bit: ?bool = null,
    a_8bit: ?bool = null,
    overflow: ?bool = null,
    negative: ?bool = null,
};

/// Changes non-null fields to the specfied value
pub fn change_status_flags(b: *Builder, change: ChangeStatusRegister) void {
    var set: Instruction.StatusRegister = .{};
    var clear: Instruction.StatusRegister = .{};

    inline for (std.meta.fields(Instruction.StatusRegister)) |field| {
        if (@field(change, field.name)) |value| {
            if (value) {
                @field(set, field.name) = true;
            } else {
                @field(clear, field.name) = true;
            }
        }
    }

    if (change.a_8bit) |value| {
        b.a_size = if (value) .@"8bit" else .@"16bit";
        if (b.start_a_size == .none) {
            b.start_a_size = b.a_size;
        }
        _ = RegA.next(b);
    }
    if (change.xy_8bit) |value| {
        b.xy_size = if (value) .@"8bit" else .@"16bit";
        if (b.start_xy_size == .none) {
            b.start_xy_size = b.xy_size;
        }
        _ = RegX.next(b);
        _ = RegY.next(b);

        // Changing the index-register size from 16-bit to 8-bit clears the high-byte
        if (b.xy_size == .@"8bit") {
            b.clobbers.put(b.build_system.allocator, .x, {}) catch @panic("Out of memory");
            b.clobbers.put(b.build_system.allocator, .y, {}) catch @panic("Out of memory");
        }
    }

    if (set != @as(Instruction.StatusRegister, .{})) {
        b.emit(.{ .sep = set });
    }
    if (clear != @as(Instruction.StatusRegister, .{})) {
        b.emit(.{ .rep = clear });
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
                        .target_sym = .{ .function = b.symbol },
                        .target_offset = target_offset - comptime Instruction.bra.size(),
                    }});
                } else {
                    try b.insert_branch_instructions(insert_offset, data_allocator, &.{
                        .{ .jmp = undefined },
                    }, &.{.{
                        .type = .addr16,
                        .target_sym = .{ .function = b.symbol },
                        .target_offset = target_offset,
                    }});
                }
            },
            .jump_long => {
                try b.insert_branch_instructions(insert_offset, data_allocator, &.{
                    .{ .jml = undefined },
                }, &.{.{
                    .type = .addr24,
                    .target_sym = .{ .function = b.symbol },
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

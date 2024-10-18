const std = @import("std");
const Symbol = @import("Function.zig").Symbol;
const SymbolPtr = @import("Function.zig").SymbolPtr;
const BuildSystem = @import("BuildSystem.zig");
const Instruction = @import("instruction.zig").Instruction;

const Builder = @This();

// Labels are just an index to the target instruction (in bytes)
pub const Label = u16;

/// Metadata information about an instruction
pub const InstructionInfo = struct {
    const IndexMode = enum { @"8bit", @"16bit" };

    /// A relocatoion indicates that this instruction has an operand to another symbol,
    /// which needs to be fixed after emitting the data into ROM
    const Relocation = struct {
        type: enum { absolute },
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

// Debug data
symbol_name: ?[]const u8 = null,
source_location: ?std.builtin.SourceLocation = null,

pub fn init(sys: *BuildSystem, sym: SymbolPtr) Builder {
    return .{
        .build_system = sys,
        .symbol = sym,
    };
}

pub fn build(b: *Builder) void {
    b.symbol(b);
}

pub fn setup_debug(b: *Builder, src: std.builtin.SourceLocation, declaring_type: type, overwrite_symbol_name: ?[]const u8) void {
    b.symbol_name = overwrite_symbol_name orelse std.fmt.allocPrint(b.build_system.allocator, "{s}@{s}", .{ @typeName(declaring_type), src.fn_name }) catch @panic("Out of memory");
    b.source_location = src;
}

pub fn define_label(b: Builder) Label {
    return @intCast(b.instruction_data.items.len);
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

    instr.write_data(b.instruction_data.writer(b.build_system.allocator)) catch @panic("Out of memory");
}

/// Calls the target method
pub fn call(b: *Builder, target: Symbol) void {
    b.build_system.enqueue_function(target) catch @panic("Out of memory");

    b.emit_extra(.{ .jsr = undefined }, .{
        .type = .absolute,
        .target_sym = target,
        .target_offset = 0,
    });
}

/// Always branch to the target label
pub fn branch_always(b: *Builder, target: Label) void {
    // The offset is relative to the next instruction
    const offset: i32 = @as(i32, @intCast(target)) - (@as(i32, @intCast(b.instruction_data.items.len)) + comptime Instruction.bra.size());

    if (offset >= std.math.minInt(i8) and offset <= std.math.maxInt(i8)) {
        b.emit(.{ .bra = @intCast(offset) });
    } else {
        b.emit_extra(.{ .jmp = undefined }, .{
            .type = .absolute,
            .target_sym = b.symbol,
            .target_offset = target,
        });
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

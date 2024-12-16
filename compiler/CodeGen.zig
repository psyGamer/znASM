const std = @import("std");
const memory_map = @import("memory_map.zig");

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Module = @import("Module.zig");
const Symbol = @import("symbol.zig").Symbol;
const TypeSymbol = @import("symbol.zig").TypeSymbol;
const SymbolLocation = @import("symbol.zig").SymbolLocation;
const Sema = @import("Sema.zig");
const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("instruction.zig").Opcode;
const Ir = @import("ir.zig").Ir;
const Rom = @import("Rom.zig");
const MappingMode = @import("Rom.zig").Header.Mode.Map;

const CodeGen = @This();

/// A relocatoion indicates that this instruction has an operand to another symbol,
/// which needs to be fixed after emitting the data into ROM
pub const Relocation = struct {
    pub const Type = enum {
        /// Relative 8-bit offset to the symbol
        rel8,
        /// 16-bit Absolute address of the symbol
        addr16,
        /// 24-bit Long address of the symbol
        addr24,
    };

    type: Type,
    target_sym: SymbolLocation,
    target_offset: u16 = 0,
};
/// Branches need to be relocated to point to their label and use the short / long form
pub const BranchRelocation = struct {
    pub const Type = enum {
        always,
        carry_set,
        carry_clear,
        overflow_set,
        overflow_clear,
        equal,
        not_equal,
        plus,
        minus,
    };

    type: Type,
    target: []const u8,
};
/// Metadata information about an instruction
pub const InstructionInfo = struct {
    instr: Instruction,
    /// Offset from the start of the function. Only defined once the assembly as been emitted
    offset: u16 = undefined,

    reloc: ?Relocation = null,
    branch_reloc: ?BranchRelocation = null,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode,

    /// Source-node for this instruction
    source: NodeIndex,
};

pub fn process(sema: *Sema) !void {
    const symbols = sema.symbols.slice();

    for (symbols.items(.sym), symbols.items(.loc)) |*sym, loc| {
        if (sym.* != .function) {
            continue;
        }

        var builder: FunctionBuilder = .{
            .sema = sema,
            .symbol = sym.function,
            .symbol_location = loc,
        };
        errdefer builder.instructions.deinit(sema.allocator);
        defer builder.labels.deinit(sema.allocator);

        try builder.build();

        try builder.resolveBranchRelocs();
        sym.function.assembly_data = try builder.genereateAssemblyData();
        try builder.fixLabelRelocs();
        sym.function.instructions = try builder.instructions.toOwnedSlice(sema.allocator);

        const labels = try sema.allocator.alloc(struct { []const u8, u16 }, builder.labels.count());
        for (builder.labels.keys(), builder.labels.values(), labels) |label_name, index, *label| {
            label.* = .{ label_name, index };
        }
        sym.function.labels = labels;
    }
}

/// Builds instruction information from the AST nodes of a function
const FunctionBuilder = struct {
    /// Index to a target instruction
    pub const Label = u16;

    sema: *Sema,

    symbol: Symbol.Function,
    symbol_location: SymbolLocation,

    instructions: std.ArrayListUnmanaged(InstructionInfo) = .empty,
    labels: std.StringArrayHashMapUnmanaged(Label) = .empty,

    /// Size of the A-Register / Memory instructions
    mem_size: Instruction.SizeMode = .none,
    /// Size of the X/Y-Registers
    idx_size: Instruction.SizeMode = .none,

    pub fn build(b: *FunctionBuilder) !void {
        for (b.symbol.ir) |ir| {
            switch (ir.tag) {
                .instruction => try b.handleInstruction(ir),
                .change_size => try b.handleChangeSize(ir),
                .load => try b.handleLoad(ir),
                .store => try b.handleStore(ir),
                .call => try b.handleCall(ir),
                .branch => try b.handleBranch(ir),
                .label => try b.handleLabel(ir),
            }
        }
    }

    fn handleInstruction(b: *FunctionBuilder, ir: Ir) !void {
        const instruction = ir.tag.instruction;
        try b.instructions.append(b.sema.allocator, .{
            .instr = instruction.instr,
            .reloc = instruction.reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleChangeSize(b: *FunctionBuilder, ir: Ir) !void {
        const change_size = ir.tag.change_size;

        const curr_mode = switch (change_size.target) {
            .mem => b.mem_size,
            .idx => b.idx_size,
            .none => unreachable,
        };
        if (change_size.mode == curr_mode) {
            return;
        }

        const flags: Instruction.StatusRegister = .{
            .mem_8bit = change_size.target == .mem,
            .idx_8bit = change_size.target == .idx,
        };

        try b.instructions.append(b.sema.allocator, .{
            .instr = switch (change_size.mode) {
                .@"8bit" => .{ .sep = flags },
                .@"16bit" => .{ .rep = flags },
                .none => unreachable,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });

        switch (change_size.target) {
            .mem => b.mem_size = change_size.mode,
            .idx => b.idx_size = change_size.mode,
            .none => unreachable,
        }
    }
    fn handleLoad(b: *FunctionBuilder, ir: Ir) !void {
        const load = ir.tag.load;

        const instr: Instruction, const reloc: ?Relocation = switch (load.target) {
            .a8, .a16 => switch (load.value) {
                .immediate, .fields => .{ .{ .lda_imm = if (load.target == .a8)
                    .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                else
                    .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                .symbol => |sym_loc| b: {
                    const sym = b.sema.lookupSymbol(sym_loc).?.*;
                    break :b switch (sym) {
                        .function, .@"packed", .@"enum" => unreachable,
                        .constant => .{ .{ .lda_imm = if (load.target == .a8)
                            .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                        else
                            .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                        .variable => if (b.sema.isSymbolAccessibleInBank(sym, b.symbol.bank))
                            .{ .{ .lda_addr16 = undefined }, .{
                                .type = .addr16,
                                .target_sym = sym_loc,
                                .target_offset = load.source_offset,
                            } }
                        else
                            .{ .{ .lda_addr24 = undefined }, .{
                                .type = .addr24,
                                .target_sym = sym_loc,
                                .target_offset = load.source_offset,
                            } },
                        .register => |reg_sym| .{ .{ .lda_addr16 = @enumFromInt(reg_sym.address) }, null },
                    };
                },
                .register => |register| switch (register) {
                    .a8, .a16 => unreachable,
                    .x8, .x16 => .{ .txa, null },
                    .y8, .y16 => .{ .tya, null },
                },
            },
            .x8, .x16 => switch (load.value) {
                .immediate, .fields => .{ .{ .ldx_imm = if (load.target == .x8)
                    .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                else
                    .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                .symbol => |sym_loc| b: {
                    const sym = b.sema.lookupSymbol(sym_loc).?.*;
                    break :b switch (sym) {
                        .function, .@"packed", .@"enum" => unreachable,
                        .constant => .{ .{ .ldx_imm = if (load.target == .x8)
                            .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                        else
                            .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                        .variable => if (b.sema.isSymbolAccessibleInBank(sym, b.symbol.bank))
                            .{ .{ .ldx_addr16 = undefined }, .{
                                .type = .addr16,
                                .target_sym = sym_loc,
                                .target_offset = load.source_offset,
                            } }
                        else
                            unreachable,
                        .register => |reg_sym| .{ .{ .lda_addr16 = @enumFromInt(reg_sym.address) }, null },
                    };
                },
                .register => |register| switch (register) {
                    .a8, .a16 => .{ .tax, null },
                    .x8, .x16 => unreachable,
                    .y8, .y16 => .{ .tyx, null },
                },
            },
            .y8, .y16 => switch (load.value) {
                .immediate, .fields => .{ .{ .ldy_imm = if (load.target == .y8)
                    .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                else
                    .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                .symbol => |sym_loc| b: {
                    const sym = b.sema.lookupSymbol(sym_loc).?.*;
                    break :b switch (sym) {
                        .function, .@"packed", .@"enum" => unreachable,
                        .constant => .{ .{ .ldy_imm = if (load.target == .y8)
                            .{ .imm8 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) }
                        else
                            .{ .imm16 = @truncate(@as(TypeSymbol.UnsignedComptimeIntValue, @bitCast(load.value.resolve(TypeSymbol.ComptimeIntValue, b.sema))) >> @intCast(load.source_offset * 8)) } }, null },

                        .variable => if (b.sema.isSymbolAccessibleInBank(sym, b.symbol.bank))
                            .{ .{ .ldy_addr16 = undefined }, .{
                                .type = .addr16,
                                .target_sym = sym_loc,
                                .target_offset = load.source_offset,
                            } }
                        else
                            unreachable,
                        .register => |reg_sym| .{ .{ .lda_addr16 = @enumFromInt(reg_sym.address) }, null },
                    };
                },
                .register => |register| switch (register) {
                    .a8, .a16 => .{ .tay, null },
                    .x8, .x16 => .{ .txy, null },
                    .y8, .y16 => unreachable,
                },
            },
        };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleStore(b: *FunctionBuilder, ir: Ir) !void {
        const store = ir.tag.store;
        const target_symbol = b.sema.lookupSymbol(store.target).?;

        const instr: Instruction, const reloc: ?Relocation = switch (store.source) {
            .a8, .a16 => if (b.sema.isSymbolAccessibleInBank(target_symbol.*, b.symbol.bank))
                .{ .{ .sta_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_sym = store.target,
                    .target_offset = store.target_offset,
                } }
            else
                .{ .{ .sta_addr24 = undefined }, .{
                    .type = .addr24,
                    .target_sym = store.target,
                    .target_offset = store.target_offset,
                } },

            .x8, .x16 => if (b.sema.isSymbolAccessibleInBank(target_symbol.*, b.symbol.bank))
                .{ .{ .stx_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_sym = store.target,
                    .target_offset = store.target_offset,
                } }
            else
                unreachable,

            .y8, .y16 => if (b.sema.isSymbolAccessibleInBank(target_symbol.*, b.symbol.bank))
                .{ .{ .sty_addr16 = undefined }, .{
                    .type = .addr16,
                    .target_sym = store.target,
                    .target_offset = store.target_offset,
                } }
            else
                unreachable,
        };

        try b.instructions.append(b.sema.allocator, .{
            .instr = instr,
            .reloc = reloc,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleLabel(b: *FunctionBuilder, ir: Ir) !void {
        try b.labels.put(b.sema.allocator, ir.tag.label, @intCast(b.instructions.items.len));
    }
    fn handleCall(b: *FunctionBuilder, ir: Ir) !void {
        const call = ir.tag.call;
        const target_symbol = b.sema.lookupSymbol(call.target).?;

        // TODO: Support long jumps
        std.debug.assert(target_symbol.function.bank == b.symbol.bank);

        try b.instructions.append(b.sema.allocator, .{
            .instr = .{ .jsr = undefined },
            .reloc = .{
                .type = .addr16,
                .target_sym = call.target,
                .target_offset = call.target_offset,
            },

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }
    fn handleBranch(b: *FunctionBuilder, ir: Ir) !void {
        try b.instructions.append(b.sema.allocator, .{
            .instr = undefined,
            .branch_reloc = ir.tag.branch,

            .mem_size = b.mem_size,
            .idx_size = b.idx_size,

            .source = ir.node,
        });
    }

    /// Generates the raw assembly bytes for all instructions
    fn genereateAssemblyData(b: *FunctionBuilder) ![]u8 {
        var data: std.ArrayListUnmanaged(u8) = .{};
        // TODO: Figure out good instruction/assembly ratio
        try data.ensureTotalCapacity(b.sema.allocator, b.instructions.items.len * 2);

        const data_writer = data.writer(b.sema.allocator);

        for (b.instructions.items) |*info| {
            const size_type = info.instr.sizeType();
            const size_mode = switch (size_type) {
                .none => .none,
                .mem => info.mem_size,
                .idx => info.idx_size,
            };
            if (size_type != .none) {
                std.debug.assert(size_mode != .none);
            }

            info.offset = @intCast(data.items.len);
            try info.instr.writeData(data_writer, size_mode);
        }

        return data.toOwnedSlice(b.sema.allocator);
    }

    /// By default labels point to a symbol in the current module, instead of the current one with an offset
    fn fixLabelRelocs(b: *FunctionBuilder) !void {
        for (b.instructions.items) |*info| {
            const reloc = &(info.reloc orelse continue);
            if (!std.mem.eql(u8, reloc.target_sym.module, b.symbol_location.module)) continue;

            for (b.labels.keys(), b.labels.values()) |label, instr_index| {
                if (std.mem.eql(u8, label, reloc.target_sym.name)) {
                    reloc.target_sym = b.symbol_location;
                    reloc.target_offset = if (instr_index == b.instructions.items.len)
                        @intCast(b.symbol.assembly_data.len)
                    else
                        b.instructions.items[instr_index].offset;
                }
            }
        }
    }

    /// Resolves branch locations into a short / long form
    fn resolveBranchRelocs(b: *FunctionBuilder) !void {
        // Relative offsets to the target instruction to determine short- / long-form
        var reloc_offsets: std.AutoArrayHashMapUnmanaged(usize, i32) = .{};
        defer reloc_offsets.deinit(b.sema.allocator);

        for (b.instructions.items, 0..) |info, i| {
            if (info.branch_reloc != null) {
                // Default to long-form, lower to short-form later
                try reloc_offsets.put(b.sema.allocator, i, std.math.maxInt(i32));
            }
        }

        const short_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.bra.size(.none),
            .carry_set = comptime Instruction.bcs.size(.none),
            .carry_clear = comptime Instruction.bcc.size(.none),
            .overflow_set = comptime Instruction.bvs.size(.none),
            .overflow_clear = comptime Instruction.bvc.size(.none),
            .equal = comptime Instruction.beq.size(.none),
            .not_equal = comptime Instruction.bne.size(.none),
            .plus = comptime Instruction.bpl.size(.none),
            .minus = comptime Instruction.bmi.size(.none),
        });
        const long_sizes: std.EnumArray(BranchRelocation.Type, u8) = .init(.{
            .always = comptime Instruction.jmp.size(.none),
            .carry_set = comptime Instruction.bcc.size(.none) + Instruction.jmp.size(.none),
            .carry_clear = comptime Instruction.bcs.size(.none) + Instruction.jmp.size(.none),
            .overflow_set = comptime Instruction.bvc.size(.none) + Instruction.jmp.size(.none),
            .overflow_clear = comptime Instruction.bvs.size(.none) + Instruction.jmp.size(.none),
            .equal = comptime Instruction.bne.size(.none) + Instruction.jmp.size(.none),
            .not_equal = comptime Instruction.beq.size(.none) + Instruction.jmp.size(.none),
            .plus = comptime Instruction.bmi.size(.none) + Instruction.jmp.size(.none),
            .minus = comptime Instruction.bpl.size(.none) + Instruction.jmp.size(.none),
        });

        // Interativly lower to short-form
        var changed = true;
        while (changed) {
            changed = false;

            for (reloc_offsets.keys(), reloc_offsets.values()) |source_idx, *relative_offset| {
                // If its's already short, don't mark this as a change, but still recalculate the offset
                const already_short = relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8);

                const reloc = b.instructions.items[source_idx].branch_reloc.?;
                const target_idx = b.labels.get(reloc.target).?;

                // Calculate offset to target
                const min = @min(source_idx + 1, target_idx);
                const max = @max(source_idx + 1, target_idx);

                relative_offset.* = 0;
                for (b.instructions.items[min..max], min..max) |info, i| {
                    if (info.branch_reloc) |other_reloc| {
                        const other_offset = reloc_offsets.get(i).?;

                        if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                            relative_offset.* += short_sizes.get(other_reloc.type);
                        } else {
                            relative_offset.* += long_sizes.get(other_reloc.type);
                        }
                    } else {
                        const size_type = info.instr.sizeType();
                        const size_mode = switch (size_type) {
                            .none => .none,
                            .mem => info.mem_size,
                            .idx => info.idx_size,
                        };
                        if (size_type != .none) {
                            std.debug.assert(size_mode != .none);
                        }

                        relative_offset.* += info.instr.size(size_mode);
                    }
                }
                if (target_idx <= source_idx) {
                    relative_offset.* = -relative_offset.*;
                }

                if (!already_short and relative_offset.* >= std.math.minInt(i8) and relative_offset.* <= std.math.maxInt(i8)) {
                    changed = true;
                }
            }
        }

        // Calculate target offsets (for jumps)
        var target_offsets: std.AutoArrayHashMapUnmanaged(usize, u16) = .{};
        defer target_offsets.deinit(b.sema.allocator);

        for (reloc_offsets.keys()) |source_idx| {
            const reloc = b.instructions.items[source_idx].branch_reloc.?;
            const target_idx = b.labels.get(reloc.target).?;

            var offset: usize = 0;
            for (b.instructions.items[0..target_idx], 0..) |info, i| {
                if (info.branch_reloc) |other_reloc| {
                    const other_offset = reloc_offsets.get(i).?;

                    if (other_offset >= std.math.minInt(i8) and other_offset <= std.math.maxInt(i8)) {
                        offset += short_sizes.get(other_reloc.type);
                    } else {
                        offset += long_sizes.get(other_reloc.type);
                    }
                } else {
                    const size_type = info.instr.sizeType();
                    const size_mode = switch (size_type) {
                        .none => .none,
                        .mem => info.mem_size,
                        .idx => info.idx_size,
                    };
                    if (size_type != .none) {
                        std.debug.assert(size_mode != .none);
                    }

                    offset += info.instr.size(size_mode);
                }
            }

            try target_offsets.put(b.sema.allocator, source_idx, @intCast(offset));
        }

        // Insert instructions (reversed to avoid shifting following indices)
        var it = std.mem.reverseIterator(reloc_offsets.keys());

        while (it.next()) |source_idx| {
            const info = &b.instructions.items[source_idx];
            const reloc = info.branch_reloc.?;

            const relative_offset = reloc_offsets.get(source_idx).?;
            const target_offset = target_offsets.get(source_idx).?;

            const use_short = relative_offset >= std.math.minInt(i8) and relative_offset <= std.math.maxInt(i8);

            if (use_short) {
                info.instr = switch (reloc.type) {
                    .always => .{ .bra = @intCast(relative_offset) },
                    .carry_set => .{ .bcs = @intCast(relative_offset) },
                    .carry_clear => .{ .bcc = @intCast(relative_offset) },
                    .overflow_set => .{ .bvs = @intCast(relative_offset) },
                    .overflow_clear => .{ .bvc = @intCast(relative_offset) },
                    .equal => .{ .beq = @intCast(relative_offset) },
                    .not_equal => .{ .bne = @intCast(relative_offset) },
                    .plus => .{ .bpl = @intCast(relative_offset) },
                    .minus => .{ .bmi = @intCast(relative_offset) },
                };
            } else {
                const jmp_size = comptime Opcode.jmp.size(.none);
                info.instr = switch (reloc.type) {
                    .always => .{ .jmp = undefined },
                    .carry_set => .{ .bcc = jmp_size },
                    .carry_clear => .{ .bcs = jmp_size },
                    .overflow_set => .{ .bvc = jmp_size },
                    .overflow_clear => .{ .bvs = jmp_size },
                    .equal => .{ .bne = jmp_size },
                    .not_equal => .{ .beq = jmp_size },
                    .plus => .{ .bmi = jmp_size },
                    .minus => .{ .bpl = jmp_size },
                };

                const jmp_reloc: Relocation = .{
                    .type = .addr16,
                    .target_sym = b.symbol_location,
                    .target_offset = target_offset,
                };

                if (reloc.type == .always) {
                    info.reloc = jmp_reloc;
                } else {
                    try b.instructions.insert(b.sema.allocator, source_idx + 1, .{
                        .instr = .{ .jmp = undefined },
                        .reloc = jmp_reloc,

                        .mem_size = info.mem_size,
                        .idx_size = info.idx_size,

                        .source = info.source,
                    });
                }
            }
        }
    }
};

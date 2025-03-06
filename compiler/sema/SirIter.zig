//! Iterative optimizer for reducing an SIR graph
const std = @import("std");
const runtime_safty = switch (@import("builtin").mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};
const log = std.log.scoped(.sir_iter);

const builtin_module = @import("../builtin_module.zig");

const Node = @import("../Ast.zig").Node;
const Sema = @import("../Sema.zig");
const Sir = Sema.Sir;
const Graph = Sir.Graph;

const Iter = @This();

sema: *Sema,
graph: *Graph,

queue: std.ArrayListUnmanaged(Sir.Index) = .empty,

inline fn dataAllocator(iter: Iter) std.mem.Allocator {
    return iter.sema.data_allocator;
}
inline fn tempAllocator(iter: Iter) std.mem.Allocator {
    return iter.sema.temp_allocator;
}

/// Applies all available optimizations to the graph
pub fn reduce(sema: *Sema, graph: *Graph) !void {
    log.info("Applying SIR-reduction...", .{});

    var iter: Iter = .{ .sema = sema, .graph = graph };
    defer iter.queue.deinit(sema.temp_allocator);

    // Apply pre graph reductions
    next_iter: while (true) {
        inline for (graph_optimizations) |opt| {
            if (try opt(&iter)) {
                continue :next_iter;
            }
        }

        break;
    }

    // Apply node reductions
    next_iter: while (true) {
        for (0..graph.list.len) |i| {
            const index: Sir.Index = .cast(i);

            switch (index.getTag(graph)) {
                inline else => |tag| {
                    // Apply applicable node optimizations
                    inline for (node_optimizations) |opt| {
                        const NodeType = @typeInfo(@TypeOf(opt)).@"fn".params[1].type.?;
                        if (comptime std.mem.indexOfScalar(std.meta.Tag(Sir.Data), NodeType.root_tags, tag) != null) {
                            if (try opt(&iter, .{ .index = index })) {
                                continue :next_iter;
                            }
                        }
                    }
                },
            }
        }

        break;
    }

    // Apply post graph optimizations
    next_iter: while (true) {
        inline for (graph_optimizations) |opt| {
            if (try opt(&iter)) {
                continue :next_iter;
            }
        }

        break;
    }

    log.info("Finished SIR-reduction", .{});
}

/// Wrapper around `Sir.Index` to encode applicable tags in the type system
fn RootNode(comptime tags: []const std.meta.Tag(Sir.Data)) type {
    return struct {
        pub const root_tags = tags;

        index: Sir.Index,
    };
}

/// General optimizations applied to the graph before applying `node_optimizations`
const graph_optimizations = &.{
    optRemoveUnused,
};
/// Optimizations applied to specific nodes
const node_optimizations = &.{
    optInlineConstantValue,
    optRemoveRedundantSymbols,
    optCalculateImmediateValues,
    optMergeBitwise,
    optSimplifyRedundantOperations,
};

// Graph Optimizations

/// Removes unused nodes
fn optRemoveUnused(iter: *Iter) !bool {
    const graph = iter.graph;

    // Traverse from ends to mark visited
    for (graph.slice.iter_data) |*data| {
        data.visited = false;
    }

    iter.queue.clearRetainingCapacity();
    for (0..graph.list.len) |i| {
        if (graph.slice.tags[i] == .block_end) {
            graph.slice.iter_data[i].visited = true;
            try iter.queue.append(iter.tempAllocator(), .cast(i));
        }
    }

    var it: Graph.TraverseIterator(.parent, &.{}, &.{}) = .{
        .graph = graph,
        .queue = &iter.queue,
    };
    std.debug.assert(try it.next(iter.tempAllocator()) == null);

    // Everything not visited is unused
    var removed_any = false;
    for (0..graph.list.len) |i| {
        if (!graph.slice.iter_data[i].visited and graph.slice.tags[i] != .invalid) {
            log.debug("Removed unused node '{s}' at [{}]", .{ @tagName(graph.slice.tags[i]), i });
            graph.remove(.cast(i));
            removed_any = true;
        }
    }
    return removed_any;
}

// Node Optimizations

/// Inlines the value of referenced global constants
fn optInlineConstantValue(iter: *Iter, root: RootNode(&.{.symbol})) !bool {
    const graph = iter.graph;

    const data = root.index.getData(graph).symbol;
    const symbol = data.symbol.get(iter.sema);
    if (symbol.* != .constant) {
        return false;
    }

    const const_sym = symbol.constant;
    const const_value = const_sym.value.getData(&const_sym.sir_graph).value;

    const value =
        if (data.byte_start == 0 and data.byte_end == const_sym.type.byteSize(iter.sema))
            // Entire value is referenced
            const_value
        else b: {
            // Only sub-range is referenced
            var new_value = const_value.toMutable(try iter.tempAllocator().alloc(std.math.big.Limb, const_value.limbs.len));
            new_value.shiftRight(new_value.toConst(), data.byte_start * 8);
            // Manually preform AND to avoid allocations
            const limb_idx = @divFloor(data.byte_end - data.byte_start - 1, @sizeOf(std.math.big.Limb));
            // const limb_mask = @as(std.math.big.Limb, 1) <<| ((symbol_data.byte_end - symbol_data.byte_start - 1) % @sizeOf(std.math.big.Limb) + 1) * 8 -% 1;
            // TODO: Use above calculation once self-hosted backend has shl_sat implemented
            const limb_mask =
                if (data.byte_end - data.byte_start % @sizeOf(std.math.big.Limb) == 0)
                    std.math.maxInt(std.math.big.Limb)
                else
                    @as(std.math.big.Limb, 1) << @intCast(((data.byte_end - data.byte_start - 1) % @sizeOf(std.math.big.Limb) + 1) * 8 -% 1);
            new_value.limbs[limb_idx] &= limb_mask;
            new_value.len = new_value.limbs.len;
            // Check if sign-bit is included
            new_value.positive = const_value.positive or data.byte_end != const_sym.type.byteSize(iter.sema);

            break :b new_value.toConst();
        };
    try graph.replace(iter.dataAllocator(), root.index, .{ .value = value });

    log.debug("Inlined constant value of {}[{d}..{d}] = {} at [{}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, value, @intFromEnum(root.index) });
    return true;
}

/// Removes redundant symbol loads / stores
///  - Loads are redundant if the previous value was a store
///  - Stores are redundant if the output doesn't depend on them
fn optRemoveRedundantSymbols(iter: *Iter, root: RootNode(&.{.symbol})) !bool {
    const graph = iter.graph;

    const data = root.index.getData(graph).symbol;

    var parent_it = graph.iterateParents(root.index);
    while (parent_it.next()) |edge| {
        switch (edge.type) {
            .dependency => {
                if (edge.parent.getTag(graph) != .symbol) {
                    continue;
                }

                const parent = edge.parent;
                const other_data = parent.getData(graph).symbol;
                if (data.symbol == other_data.symbol and data.byte_start == other_data.byte_start and data.byte_end == other_data.byte_end) {
                    // Remove redundant load
                    try graph.relocateChildren(iter.dataAllocator(), root.index, parent);
                    try graph.relocateParents(iter.dataAllocator(), root.index, parent);
                    graph.remove(root.index);

                    log.debug("Removed redudant load of {}[{d}..{d}] at [{d}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, @intFromEnum(root.index) });
                    return true;
                }
            },
            .dataflow => {
                var child_it = graph.iterateChildren(root.index);
                while (child_it.next()) |child_edge| {
                    const child = child_edge.child;
                    if (child.getTag(graph) != .block_end) {
                        // Remove redundant store
                        try graph.relocateChildren(iter.dataAllocator(), root.index, child);
                        try graph.relocateParents(iter.dataAllocator(), root.index, child);
                        graph.remove(root.index);

                        log.debug("Removed redudant store of {}[{d}..{d}] at [{d}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, @intFromEnum(root.index) });
                        return true;
                    }
                }
            },
        }
    }

    return false;
}

/// Calculates the result of operations between immedate values
fn optCalculateImmediateValues(iter: *Iter, root: RootNode(&.{ .bit_shift_left, .bit_shift_right, .bit_and, .bit_or })) !bool {
    var graph = iter.graph;
    const root_tag = root.index.getTag(graph);

    var it = graph.iterateParents(root.index);
    switch (root_tag) {
        .bit_shift_left, .bit_shift_right => {
            const input = it.next().?;
            std.debug.assert(it.next() == null);

            if (input.parent.getTag(graph) != .value) {
                return false;
            }
            const data = input.parent.getData(graph).value;

            var result = try data.toManaged(iter.tempAllocator());
            switch (root.index.getData(graph)) {
                .bit_shift_left => |amount| try result.shiftLeft(&result, amount),
                .bit_shift_right => |amount| try result.shiftRight(&result, amount),
                else => unreachable,
            }
            switch (root.index.getData(graph)) {
                .bit_shift_left => |amount| log.debug("Calculated immediate value of '{} << {d} = {}' at [{d}]", .{ data, amount, result, @intFromEnum(root.index) }),
                .bit_shift_right => |amount| log.debug("Calculated immediate value of '{} >> {d} = {}' at [{d}]", .{ data, amount, result, @intFromEnum(root.index) }),
                else => unreachable,
            }

            try graph.replace(iter.dataAllocator(), root.index, .{ .value = result.toConst() });
            graph.removeEdge(input);

            return true;
        },
        .bit_and, .bit_or => {
            const input_a: *Sir.Edge = while (it.next()) |input_edge| {
                if (input_edge.type == .dataflow) {
                    break input_edge;
                }
            } else return false;
            const input_b: *Sir.Edge = while (it.next()) |input_edge| {
                if (input_edge.type == .dataflow) {
                    break input_edge;
                }
            } else return false;

            if (input_a.parent.getTag(graph) != .value or input_b.parent.getTag(graph) != .value) {
                return false;
            }

            const data_a = try input_a.parent.getData(graph).value.toManaged(iter.tempAllocator());
            const data_b = try input_b.parent.getData(graph).value.toManaged(iter.tempAllocator());

            var result: std.math.big.int.Managed = try .init(iter.tempAllocator());
            switch (root_tag) {
                .bit_and => try result.bitAnd(&data_a, &data_b),
                .bit_or => try result.bitOr(&data_a, &data_b),
                else => unreachable,
            }
            switch (root_tag) {
                .bit_and => log.debug("Calculated immediate value of '{} & {} = {}'", .{ data_a, data_b, result }),
                .bit_or => log.debug("Calculated immediate value of '{} | {} = {}'", .{ data_a, data_b, result }),
                else => unreachable,
            }

            try graph.replace(iter.dataAllocator(), root.index, .{ .value = result.toConst() });
            graph.removeEdge(input_a);
            graph.removeEdge(input_b);

            return true;
        },
        else => unreachable,
    }

    return false;
}

/// Merges immedate values of multiple bitwise operations if possible
fn optMergeBitwise(iter: *Iter, root: RootNode(&.{ .bit_and, .bit_or })) !bool {
    var graph = iter.graph;
    const root_tag = root.index.getTag(graph);

    // Search for immedate value
    var input_it = graph.iterateParents(root.index);
    const value_edge = while (input_it.next()) |input_edge| {
        if (input_edge.type == .dataflow and input_edge.parent.getTag(graph) == .value) {
            break input_edge;
        }
    } else return false;
    const value = value_edge.parent.getData(graph).value;

    var merge_status: packed struct { merged: bool, skipped: bool } = .{ .merged = false, .skipped = false };

    switch (root_tag) {
        inline .bit_and, .bit_or => |t| {
            var traverse_it = try graph.traverseParents(iter.tempAllocator(), &iter.queue, root.index, &.{t}, &.{t});
            traverse_loop: while (try traverse_it.next(iter.tempAllocator())) |prev_op| {
                // Search for immedate value
                input_it = graph.iterateParents(prev_op);
                const other_value_edge = while (input_it.next()) |input_edge| {
                    if (input_edge.type == .dataflow and input_edge.parent.getTag(graph) == .value) {
                        break input_edge;
                    }
                } else {
                    merge_status.skipped = true;
                    continue :traverse_loop;
                };

                // Trace back path and analyze bit-width
                var min_bit_start: u16 = std.math.maxInt(u16);
                var max_bit_end: u16 = 0;

                var current = prev_op;
                while (graph.slice.iter_data[@intFromEnum(current)].visited) {
                    var child_it = graph.iterateChildren(current);
                    const next = while (child_it.next()) |child_edge| {
                        if (child_edge.type != .dataflow) {
                            continue;
                        }
                        if (!graph.slice.iter_data[@intFromEnum(child_edge.child)].visited and child_edge.child != root.index) {
                            // Another not visited branch -> not applicable
                            merge_status.skipped = true;
                            continue :traverse_loop;
                        }

                        break child_edge.child;
                    } else {
                        merge_status.skipped = true;
                        continue :traverse_loop;
                    };
                    if (next == root.index) {
                        break;
                    }

                    input_it = graph.iterateParents(next);
                    while (input_it.next()) |input_edge| {
                        if (input_edge.type != .dataflow or input_edge.parent == current) {
                            continue;
                        }

                        min_bit_start = @min(min_bit_start, input_edge.bit_start_offset);
                        max_bit_end = @max(max_bit_end, input_edge.bit_end_offset);
                    }
                    current = next;
                }

                // Only merge if it doesn't overwrite written bits
                if (min_bit_start != std.math.maxInt(u16) and max_bit_end != 0) {
                    for (min_bit_start..max_bit_end) |bit_offset| {
                        const limb_idx = @divFloor(bit_offset, @bitSizeOf(std.math.big.Limb));
                        const bit_value: u1 = @truncate(value.limbs[limb_idx] >> @intCast(bit_offset % @bitSizeOf(std.math.big.Limb)));
                        if (bit_value != 1) {
                            // Would overwrite
                            merge_status.skipped = true;
                            continue :traverse_loop;
                        }
                    }
                }

                var result: std.math.big.int.Managed = try value.toManaged(iter.tempAllocator());

                const other_value = try other_value_edge.parent.getData(graph).value.toManaged(iter.tempAllocator());
                switch (root_tag) {
                    .bit_and => try result.bitAnd(&result, &other_value),
                    .bit_or => try result.bitOr(&result, &other_value),
                    else => unreachable,
                }
                if (result.toConst().eql(value) or result.eql(other_value)) {
                    continue;
                }

                switch (root_tag) {
                    .bit_and => log.debug("Merged early 'x & ${x}' with late 'x & ${x}' into 'x & ${x}' at [{}]", .{ other_value, value, result, @intFromEnum(root.index) }),
                    .bit_or => log.debug("Merged early 'x | ${x}' with late 'x | ${x}' into 'x | ${x}' at [{}]", .{ other_value, value, result, @intFromEnum(root.index) }),
                    else => unreachable,
                }

                try graph.replace(iter.dataAllocator(), other_value_edge.parent, .{ .value = result.toConst() });
                other_value_edge.bit_start_offset = @min(value_edge.bit_start_offset, other_value_edge.bit_start_offset);
                other_value_edge.bit_end_offset = @max(value_edge.bit_end_offset, other_value_edge.bit_end_offset);

                merge_status.merged = true;
            }
        },
        else => unreachable,
    }

    if (!merge_status.merged) {
        return false;
    }
    if (merge_status.skipped) {
        return true;
    }

    var input: Sir.Index = .none;
    var output: Sir.Index = .none;

    var int_reg: builtin_module.CpuRegister = .none;
    var bit_start: u16 = std.math.maxInt(u16);
    var bit_end: u16 = 0;

    var it = graph.iterateEdges(root.index);
    while (it.next()) |edge| {
        if (edge.type != .dataflow) {
            continue;
        }

        if (edge.parent == root.index) {
            std.debug.assert(output == .none);
            output = edge.child;

            bit_start = @min(bit_start, edge.bit_start_offset);
            bit_end = @max(bit_end, edge.bit_end_offset);
            std.debug.assert(int_reg == .none or int_reg == edge.intermediate_register);
            int_reg = edge.intermediate_register;
        } else if (edge.parent.getTag(graph) != .value) {
            std.debug.assert(input == .none);
            input = edge.parent;

            bit_start = @min(bit_start, edge.bit_start_offset);
            bit_end = @max(bit_end, edge.bit_end_offset);
            std.debug.assert(int_reg == .none or int_reg == edge.intermediate_register);
            int_reg = edge.intermediate_register;
        }
    }
    switch (root_tag) {
        .bit_and => log.debug("Removed now redundant 'x & ${x}'  at [{}]", .{ value, @intFromEnum(root.index) }),
        .bit_or => log.debug("Removed now redundant 'x | ${x}'  at [{}]", .{ value, @intFromEnum(root.index) }),
        else => unreachable,
    }

    std.debug.assert(input != .none and output != .none);
    graph.remove(root.index);
    try graph.addEdge(iter.dataAllocator(), .initDataDependency(output, input, int_reg, bit_start, bit_end));

    return true;
}

/// Simplifies redundant operations to an immediate value
fn optSimplifyRedundantOperations(iter: *Iter, root: RootNode(&.{ .bit_and, .bit_or })) !bool {
    var graph = iter.graph;
    const root_tag = root.index.getTag(graph);

    var input_it = graph.iterateParents(root.index);
    switch (root_tag) {
        .bit_and, .bit_or => {
            const input_a: *Sir.Edge = while (input_it.next()) |input_edge| {
                if (input_edge.type == .dataflow) {
                    break input_edge;
                }
            } else return false;
            const input_b: *Sir.Edge = while (input_it.next()) |input_edge| {
                if (input_edge.type == .dataflow) {
                    break input_edge;
                }
            } else return false;

            // Handle one operand being zero
            inline for (&.{ .a, .b }) |input_type| {
                const input, const other_input = switch (input_type) {
                    .a => .{ input_a, input_b },
                    .b => .{ input_b, input_a },
                    else => unreachable,
                };
                const input_node = input.parent;
                const other_input_node = other_input.parent;

                if (input_node.getTag(graph) == .value and input_node.getData(graph).value.eqlZero()) {
                    switch (root_tag) {
                        .bit_and => {
                            graph.removeEdge(input);
                            try graph.relocateChildren(iter.dataAllocator(), root.index, input_node);
                            graph.remove(root.index);

                            log.debug("Simplified 'x & $0x = 0' at [{d}]", .{@intFromEnum(root.index)});
                            return true;
                        },
                        .bit_or => {
                            graph.removeEdge(other_input);
                            try graph.relocateChildren(iter.dataAllocator(), root.index, other_input_node);
                            graph.remove(root.index);

                            log.debug("Simplified 'x | $0x = x' at [{d}]", .{@intFromEnum(root.index)});
                            return true;
                        },
                        else => unreachable,
                    }
                }
            }
        },
        else => unreachable,
    }

    return false;
}

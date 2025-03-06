//! Iterative optimizer for reducing an SIR graph
const std = @import("std");
const runtime_safty = switch (@import("builtin").mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};
const log = std.log.scoped(.sir_iter);

const Node = @import("../Ast.zig").Node;
const Sema = @import("../Sema.zig");
const Sir = Sema.Sir;
const Graph = Sir.Graph;

const Iter = @This();

sema: *Sema,
graph: *Graph,

queue: std.ArrayListUnmanaged(Sir.Index) = .empty,

inline fn dataAllocator(iter: Iter) std.mem.Allocator {
    return iter.sema.dataAllocator();
}
inline fn tempAllocator(iter: Iter) std.mem.Allocator {
    return iter.sema.tempAllocator();
}

/// Applies all available optimizations to the graph
pub fn reduce(sema: *Sema, graph: *Graph) !void {
    log.info("Applying SIR-reduction...", .{});

    var iter: Iter = .{ .sema = sema, .graph = graph };
    defer iter.queue.deinit(sema.tempAllocator());

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
    try graph.replace(iter.dataAllocator(), root.index, .{ .value = value }, root.index.getSourceNode(graph));

    log.debug("Inlined constant value of {}[{d}..{d}] at [{}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, @intFromEnum(root.index) });
    return true;
}

/// Removes redundant symbol loads / stores
fn optRemoveRedundantSymbols(iter: *Iter, root: RootNode(&.{.symbol})) !bool {
    const graph = iter.graph;

    const data = root.index.getData(graph).symbol;

    var prev_store: Sir.Index = .none;
    var load_value: Sir.Index = .none;

    var parent_it = graph.iterateParents(root.index);
    while (parent_it.next()) |edge| {
        if (edge.intermediate_register == .none and edge.bit_size == 0) {
            if (edge.parent.getTag(graph) != .symbol) {
                continue;
            }

            const other_data = edge.parent.getData(graph).symbol;
            if (data.symbol == other_data.symbol and data.byte_start == other_data.byte_start and data.byte_end == other_data.byte_end) {
                prev_store = edge.parent;
            }
        } else {
            load_value = edge.parent;
        }
    }

    if (load_value == .none and prev_store != .none) {
        // Remove redudntant load
        parent_it = graph.iterateParents(root.index);
        while (parent_it.next()) |edge| {
            if (edge.parent != prev_store) {
                edge.child = prev_store;
                try graph.addEdge(iter.dataAllocator(), edge.*);
            }
        }
        var child_it = graph.iterateChildren(root.index);
        while (child_it.next()) |edge| {
            edge.parent = prev_store;
            try graph.addEdge(iter.dataAllocator(), edge.*);
        }

        graph.remove(root.index);
        log.debug("Removed redudant load of {}[{d}..{d}] at [{d}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, @intFromEnum(root.index) });
        return true;
    } else if (load_value != .none and prev_store != .none) {
        // Remove redundant previous store
        parent_it = graph.iterateParents(prev_store);
        while (parent_it.next()) |edge| {
            if (edge.intermediate_register == .none and edge.bit_size == 0) {
                edge.child = root.index;
                try graph.addEdge(iter.dataAllocator(), edge.*);
            }
        }
        var child_it = graph.iterateChildren(prev_store);
        while (child_it.next()) |edge| {
            if (edge.child != root.index) {
                edge.parent = root.index;
                try graph.addEdge(iter.dataAllocator(), edge.*);
            }
        }

        graph.remove(prev_store);
        log.debug("Removed redudant store of {}[{d}..{d}] at [{d}]", .{ iter.sema.getSymbolLocation(data.symbol), data.byte_start, data.byte_end, @intFromEnum(prev_store) });
        return true;
    }

    return false;
}

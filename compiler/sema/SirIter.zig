//! Iterative optimizer for reducing Semantic IR graph
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

/// Applies all available optimizations to the graph
pub fn reduce(sema: *Sema, graph: *Graph) !void {
    log.info("Applying SIR-reduction...", .{});

    var iter: Iter = .{ .sema = sema, .graph = graph };
    defer iter.queue.deinit(sema.allocator);

    iter.graph.refresh();
    inline for (graph_optimizations) |opt| {
        try opt(&iter);
    }

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
    // optRemoveUnused,
};
/// Optimizations applied to specific nodes
const node_optimizations = &.{
    // optInlineConstantGraphs,
    // optRemoveRedundantLoad,
};

inline fn arena(iter: Iter) std.mem.Allocator {
    // TODO: Actually use an arena
    return iter.sema.allocator;
}

// Graph Optimizations

/// Removes unused nodes
fn optRemoveUnused(iter: *Iter) !void {
    const graph = iter.graph;

    // Traverse from ends to mark visited
    for (graph.slice.iter_data) |*data| {
        data.visited = false;
    }

    iter.queue.clearRetainingCapacity();
    for (0..graph.list.len) |i| {
        if (graph.slice.tags[i] == .block_end) {
            graph.slice.iter_data[i].visited = true;
            try iter.queue.append(iter.arena(), .cast(i));
        }
    }

    // Search for `invalid`, to iterate all at once
    var it: Graph.TraverseIterator(&.{.invalid}) = .{
        .slice = &graph.slice,
        .queue = &iter.queue,
    };
    const found = try it.next(iter.arena());
    std.debug.assert(found == null);

    for (0..graph.list.len) |i| {
        if (!graph.slice.iter_data[i].visited and graph.slice.tags[i] != .invalid) {
            // Unused
            graph.remove(.cast(i));
        }
    }
}

// Node Optimizations

/// Inlines the value of referenced global constants
fn optInlineConstantGraphs(iter: *Iter, root: RootNode(&.{.symbol})) !bool {
    const sema = iter.sema;
    const graph = iter.graph;
    const node = root.index;

    if (node.getParents(graph).left != .none) {
        return false; // Constants can't be assigned to
    }

    const symbol = node.getData(graph).symbol.symbol;
    const sym = symbol.get(sema);
    if (sym.* != .constant) {
        return false;
    }

    const sym_graph = &sym.constant.sir_graph;
    sym_graph.refresh();

    // Global constants **must** only be a single `value` node
    std.debug.assert(sym.constant.value.getTag(sym_graph) == .value);
    std.debug.assert(sym.constant.value.getParents(sym_graph).left == .none and sym.constant.value.getParents(sym_graph).right == .none);
    try graph.replace(sema.allocator, node, .{ .data = sym.constant.value.getData(sym_graph), .source = node.getSourceNode(graph) });

    log.debug("Inlined constant graph of '{}' into [{}]", .{ sema.getSymbolLocation(symbol), @intFromEnum(node) });
    return true;
}

/// Removes redundant symbol loads
fn optRemoveRedundantLoad(iter: *Iter, root: RootNode(&.{.symbol})) !bool {
    const graph = iter.graph;
    const load = root.index;

    if (load.getParents(graph).left != .none) {
        return false; // Not a load
    }

    const store = load.getParents(graph).right;
    if (store == .none or store.getTag(graph) != .symbol) {
        return false;
    }

    const curr_data = load.getData(graph).symbol;
    const dep_data = store.getData(graph).symbol;
    if (!std.meta.eql(curr_data, dep_data)) {
        return false;
    }

    try graph.relocateParents(iter.arena(), &iter.queue, store, load, .right);
    try graph.relocateChildren(iter.arena(), &iter.queue, store, load, .right);
    graph.remove(store);

    log.debug("Merged store and load of {}[{d}..{d}] into [{}]", .{ iter.sema.getSymbolLocation(curr_data.symbol), curr_data.byte_start, curr_data.byte_end, @intFromEnum(load) });
    return false;
}

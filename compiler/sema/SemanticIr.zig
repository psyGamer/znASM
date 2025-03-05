//! High-level Semantic Intermediate Representation
//! Represented as a 'Sea of Nodes' directed acyclic graph
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");
const formatting = @import("../util/formatting.zig");

const runtime_safty = switch (@import("builtin").mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

const Ast = @import("../Ast.zig");
const Sema = @import("../Sema.zig");
const Node = Ast.Node;
const Symbol = @import("../symbol.zig").Symbol;
const Expression = @import("expression.zig").Expression;
const TypeExpression = @import("type_expression.zig").TypeExpression;
const BuiltinVar = @import("BuiltinVar.zig");
const BuiltinFn = @import("BuiltinFn.zig");

const Sir = @This();

pub const Index = enum(u16) {
    none = std.math.maxInt(u16),
    _,

    /// Helper function to cast a generic number to a `SemanticIr.Index`
    pub inline fn cast(x: anytype) Index {
        return switch (@typeInfo(@TypeOf(x))) {
            .null => .none,
            .int, .comptime_int => @enumFromInt(@as(u32, @intCast(x))),
            .optional => if (x) |value| @enumFromInt(@as(u32, @intCast(value))) else .none,
            else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a SemanticIr.Index"),
        };
    }

    pub fn getTag(index: Index, graph: *const Graph) std.meta.Tag(Data) {
        return graph.slice.tags[@intFromEnum(index)];
    }
    pub fn getData(index: Index, graph: *const Graph) Data {
        const tag = index.getTag(graph);
        const bare = graph.slice.data[@intFromEnum(index)];

        return .unpack(switch (tag) {
            inline else => |t| @unionInit(Data.Packed, @tagName(t), @field(bare, @tagName(t))),
        }, graph.extra);
    }
    pub fn getSourceNode(index: Index, graph: *const Graph) Node.Index {
        return graph.slice.sources[@intFromEnum(index)];
    }

    /// Iterates all edges of the current node
    pub fn iterateEdges(index: Index, graph: *Graph) Graph.EdgeIterator(true, true) {
        return graph.iterateEdges(index);
    }
    /// Iterates all parent-edges of the current node
    pub fn iterateChildren(index: Index, graph: *Graph) Graph.EdgeIterator(true, false) {
        return graph.iterateChildren(index);
    }
    /// Iterates all parent-edges of the current node
    pub fn iterateParents(index: Index, graph: *Graph) Graph.EdgeIterator(false, true) {
        return graph.iterateParents(index);
    }

    /// Iterativly traverses all children of the root node, with an optional filter and stop tags
    /// The provided `queue` should be reused often, to avoid allocations.
    ///
    /// Only one traversing iterator may be active at a time!
    pub fn traverseChildren(index: Index, allocator: std.mem.Allocator, graph: *Graph, queue: *std.ArrayListUnmanaged(Index), comptime filter_tags: ?[]const std.meta.Tag(Data), comptime stop_tags: []const std.meta.Tag(Data)) !Graph.TraverseIterator(.child, filter_tags, stop_tags) {
        return graph.traverseChildren(allocator, queue, index, filter_tags, stop_tags);
    }
    /// Iterativly traverses all parents of the root node, with an optional filter and stop tags
    /// The provided `queue` should be reused often, to avoid allocations.
    ///
    /// Only one traversing iterator may be active at a time!
    pub fn traverseParents(index: Index, allocator: std.mem.Allocator, graph: *Graph, queue: *std.ArrayListUnmanaged(Index), comptime filter_tags: ?[]const std.meta.Tag(Data), comptime stop_tags: []const std.meta.Tag(Data)) !Graph.TraverseIterator(.parent, filter_tags, stop_tags) {
        return graph.traverseParents(allocator, queue, index, filter_tags, stop_tags);
    }
};

/// Data-tag of the node
pub const Tag = std.meta.Tag(Data);

/// Data associated with the current node type
pub const Data = union(enum) {
    /// Nodes which indicate the end of an SSA block
    pub const block_ends: []const std.meta.Tag(Data) = &.{.@"return"};

    /// Indicates the start of a Single-Static-Assignment (SSA) block
    block_start: void,
    /// Indicates the end of a Single-Static-Assignment (SSA) block
    block_end: void,

    /// Compile-time known immediate value
    /// Cannot have dependencies
    value: std.math.big.int.Const,

    /// Loads value from the target symbol if used as a depdenncy
    /// Stores value to the target symbol if `left != .none`
    /// Depends on the previous assignment to `symbol` with an overlapping range or `block_start` (`right`)
    symbol: struct {
        symbol: Symbol.Index,
        byte_start: u16,
        byte_end: u16,
    },

    /// Return from the current function
    ///
    /// Dependencies:
    ///  - Assignments to global variables (`var` / `reg`)
    ///  - Assignments to return variables (TODO)
    @"return": void,

    /// Invalid node, which got optimized away
    invalid: void,

    pub fn unpack(packed_data: Packed, extra: Graph.ExtraList) Data {
        switch (packed_data) {
            .value => |info| {
                return .{ .value = .{
                    .positive = info.positive,
                    .limbs = @alignCast(@ptrCast(extra.items[info.limbs_index..(info.limbs_index + info.limbs_len * @sizeOf(std.math.big.Limb))])),
                } };
            },
            inline else => |value, tag| {
                return @unionInit(Data, @tagName(tag), value);
            },
        }
    }
    pub fn pack(data: Data, allocator: std.mem.Allocator, extra: *Graph.ExtraList) !Packed {
        switch (data) {
            .value => |info| {
                try Graph.alignExtraList(extra, allocator, @alignOf(std.math.big.Limb));

                const index: u32 = @intCast(extra.items.len);
                try extra.appendSlice(allocator, @ptrCast(info.limbs));
                return .{ .value = .{
                    .positive = info.positive,
                    .limbs_len = @intCast(info.limbs.len),
                    .limbs_index = index,
                } };
            },
            inline else => |value, tag| {
                return @unionInit(Packed, @tagName(tag), value);
            },
        }
    }

    /// Packed representation, storing excess data into `graph.extra`
    pub const Packed = union(std.meta.Tag(Data)) {
        block_start: void,
        block_end: void,
        value: struct {
            positive: bool,
            limbs_len: u16,
            limbs_index: u32,
        },
        symbol: @TypeOf(@as(Data, undefined).symbol),
        @"return": void,
        invalid: void,
    };

    /// Bare union of the packed data representation
    pub const Bare = b: {
        const data_info = @typeInfo(Data.Packed).@"union";
        break :b @Type(.{ .@"union" = .{
            .layout = data_info.layout,
            .tag_type = null,
            .fields = data_info.fields,
            .decls = &.{},
        } });
    };
};

/// Edge-connection between a parent and child node
pub const Edge = packed struct(u64) {
    pub const Index = enum(u32) {
        const child_flag: u32 = 1 << 31;
        const extra_flag: u32 = 1 << 30;

        none = std.math.maxInt(u32),
        _,

        /// Helper function to cast a generic number to a `Edge.Index`
        pub inline fn cast(x: anytype) Edge.Index {
            return switch (@typeInfo(@TypeOf(x))) {
                .null => .none,
                .int, .comptime_int => @enumFromInt(@as(u32, @intCast(x))),
                .optional => if (x) |value| @enumFromInt(@as(u32, @intCast(value))) else .none,
                else => @compileError("Cannot cast " ++ @typeName(@TypeOf(x)) ++ " to a Edge.Index"),
            };
        }

        /// Resolves the `Edge` of this index
        pub fn get(index: Edge.Index, extra: Graph.ExtraList) *Edge {
            const idx = index.getIndex();
            return @alignCast(@ptrCast(extra.items[idx..(idx + @sizeOf(Edge))]));
        }
        /// Gets the actual stored index
        pub fn getIndex(index: Edge.Index) u32 {
            return @intFromEnum(index) & ~(child_flag | extra_flag);
        }

        /// Marks whether this edges points to a child node
        pub fn markChild(index: *Edge.Index, is_child: bool) void {
            const idx_ptr: *u32 = @ptrCast(index);
            if (is_child) {
                idx_ptr.* |= child_flag;
            } else {
                idx_ptr.* &= ~child_flag;
            }
        }
        /// Check if this edges points to a child node
        pub fn isChild(index: Edge.Index) bool {
            const idx: u32 = @intFromEnum(index);
            return idx & child_flag != 0;
        }

        /// Marks whether this index pointer to further indicies in `graph.extra`
        pub fn markExtra(index: *Edge.Index, is_extra: bool) void {
            const idx_ptr: *u32 = @ptrCast(index);
            if (is_extra) {
                idx_ptr.* |= extra_flag;
            } else {
                idx_ptr.* &= ~extra_flag;
            }
        }
        /// Check if this index pointer to further indicies in `graph.extra`
        pub fn isExtra(index: Edge.Index) bool {
            const idx: u32 = @intFromEnum(index);
            return idx & extra_flag != 0;
        }
    };
    pub const Type = enum(u8) { dependency, dataflow };

    child: Sir.Index,
    parent: Sir.Index,

    /// Bit-size of this connection
    bit_size: u16,

    /// Intermediate register which performs this edge operation
    intermediate_register: builtin_module.CpuRegister,

    /// Connection type which this edge represents
    type: Type,

    /// Declares a non-dataflow dependency between two nodes
    pub fn initDependency(child: Sir.Index, parent: Sir.Index) Edge {
        return .{
            .child = child,
            .parent = parent,
            .intermediate_register = .none,
            .bit_size = 0,
            .type = .dependency,
        };
    }

    /// Non-dataflow connection between the to-be-added child and the parent
    pub fn withParent(parent: Sir.Index) Edge {
        return .{
            .child = .none,
            .parent = parent,
            .intermediate_register = .none,
            .bit_size = 0,
            .type = .dependency,
        };
    }
    /// Non-dataflow connection between the to-be-added parent and the child
    pub fn withChild(child: Sir.Index) Edge {
        return .{
            .child = child,
            .parent = .none,
            .intermediate_register = .none,
            .bit_size = 0,
            .type = .dependency,
        };
    }

    /// Dataflow connection between the to-be-added child and the parent
    pub fn withDataParent(parent: Sir.Index, intermediate_register: builtin_module.CpuRegister, bit_size: u16) Edge {
        return .{
            .child = .none,
            .parent = parent,
            .intermediate_register = intermediate_register,
            .bit_size = bit_size,
            .type = .dataflow,
        };
    }
    /// Dataflow connection between the to-be-added parent and the child
    pub fn withDataChild(child: Sir.Index, intermediate_register: builtin_module.CpuRegister, bit_size: u16) Edge {
        return .{
            .child = child,
            .parent = .none,
            .intermediate_register = intermediate_register,
            .bit_size = bit_size,
            .type = .dataflow,
        };
    }
};

/// Efficent graph for storing SIR nodes
pub const Graph = struct {
    comptime {
        // Limit memory usage of nodes
        std.debug.assert(@sizeOf(Tag) <= @sizeOf(u8));
        std.debug.assert(@sizeOf(Data.Bare) <= @sizeOf(u64) + if (runtime_safty) 4 else 0);
    }

    /// Empty, ready-to-use Semantic IR graph
    pub const empty: Graph = .{
        .list = .empty,
        .extra = .empty,
        .slice = .{
            .tags = &.{},
            .data = &.{},
            .edges = &.{},
            .iter_data = &.{},
            .sources = &.{},
        },
    };

    /// Buffer for storing arbitrary extra data, associated with nodes
    pub const ExtraList = std.ArrayListAlignedUnmanaged(u8, @alignOf(usize));
    pub const ExtraIndex = u32;

    /// References to edges, which are connected to this node
    /// If more than 4 edges are required, they are stored in `extra`
    const Edges = [4]Edge.Index;

    /// Additional data for each node, to avoid extra allocators for iterators
    const IteratorData = packed struct(u8) {
        visited: bool = false,
        visited_2: bool = false,
        _: u6 = 0,
    };

    const ListElement = struct {
        tag: Tag,
        data: Data.Bare,
        edges: Edges,

        iter_data: IteratorData = .{},

        source: Node.Index,
    };
    const List = std.MultiArrayList(ListElement);

    const ComputedSlice = struct {
        tags: []Tag,
        data: []Data.Bare,
        edges: []Edges,

        iter_data: []IteratorData,

        sources: []Node.Index,
    };

    list: List,
    extra: ExtraList,
    slice: ComputedSlice,

    /// Creates a new node in the graph
    pub fn create(graph: *Graph, allocator: std.mem.Allocator, data: Data, edges: []const Edge, node: Node.Index) !Index {
        const index: Index = .cast(graph.list.len);

        try graph.list.append(allocator, try graph.createRaw(allocator, index, data, edges, node));

        const slice = graph.list.slice();
        graph.slice = .{
            .tags = slice.items(.tag),
            .data = slice.items(.data),
            .edges = slice.items(.edges),
            .iter_data = slice.items(.iter_data),
            .sources = slice.items(.source),
        };

        return index;
    }
    fn createRaw(graph: *Graph, allocator: std.mem.Allocator, index: Index, data: Data, edges: []const Edge, node: Node.Index) !ListElement {
        const tag = std.meta.activeTag(data);
        const packed_data = try data.pack(allocator, &graph.extra);

        try alignExtraList(&graph.extra, allocator, @alignOf(Edge));
        const edge_index: ExtraIndex = @intCast(graph.extra.items.len);
        try graph.extra.appendSlice(allocator, @ptrCast(edges));

        // Fix-up edges (one connection **must** be `.none`)
        const extra_edges: []Edge = @alignCast(@ptrCast(graph.extra.items[edge_index..(edge_index + edges.len * @sizeOf(Edge))]));
        for (extra_edges, 0..) |*edge, idx| {
            if (edge.parent == .none and edge.child != .none) {
                edge.parent = index;
                try graph.appendEdge(allocator, edge.*, @intCast(edge_index + idx * @sizeOf(Edge)), .child);
            } else if (edge.child == .none and edge.parent != .none) {
                edge.child = index;
                try graph.appendEdge(allocator, edge.*, @intCast(edge_index + idx * @sizeOf(Edge)), .parent);
            } else {
                unreachable;
            }
        }

        var edge_indices: Edges = @splat(.none);
        if (edges.len <= edge_indices.len) {
            for (edges, edge_indices[0..edges.len], 0..) |edge, *edge_idx, idx| {
                edge_idx.* = .cast(edge_index + idx * @sizeOf(Edge));
                edge_idx.markChild(edge.parent == index);
            }
        } else {
            // Write additional edge indices into `extra`
            const extra_index: ExtraIndex = @intCast(graph.extra.items.len);
            try graph.extra.ensureUnusedCapacity(allocator, (edges.len - edge_indices.len) * @sizeOf(Edge.Index));
            for (edges[edge_indices.len..], edge_indices.len..) |edge, idx| {
                var edge_idx: Edge.Index = .cast(edge_index + idx * @sizeOf(Edge));
                edge_idx.markChild(edge.parent == index);

                graph.extra.appendSliceAssumeCapacity(std.mem.asBytes(&edge_idx));
            }

            edge_indices[edge_indices.len - 1] = .cast(extra_index);
            edge_indices[edge_indices.len - 1].markExtra(true);
            edge_indices[edge_indices.len - 2] = .cast(edges.len - edge_indices.len);
            edge_indices[edge_indices.len - 2].markExtra(true);

            for (edges[0 .. edge_indices.len - 2], edge_indices[0 .. edge_indices.len - 2], 0..) |edge, *edge_idx, idx| {
                edge_idx.* = .cast(edge_index + idx * @sizeOf(Edge));
                edge_idx.markChild(edge.parent == index);
            }
        }

        return .{
            .tag = tag,
            .data = switch (tag) {
                inline else => |t| @unionInit(Data.Bare, @tagName(t), @field(packed_data, @tagName(t))),
            },
            .edges = edge_indices,
            .source = node,
        };
    }

    /// Replaces the target node with new data, keeping edge connections
    pub fn replace(graph: *Graph, allocator: std.mem.Allocator, index: Index, data: Data, node: Node.Index) !void {
        const tag = std.meta.activeTag(data);
        const packed_data = try data.pack(allocator, &graph.extra);

        graph.slice.tags[@intFromEnum(index)] = tag;
        graph.slice.data[@intFromEnum(index)] = switch (tag) {
            inline else => |t| @unionInit(Data.Bare, @tagName(t), @field(packed_data, @tagName(t))),
        };
        graph.slice.sources[@intFromEnum(index)] = node;
    }
    /// Fully replaces the target with a new node, including edge connections
    pub fn replaceFull(graph: *Graph, allocator: std.mem.Allocator, index: Index, data: Data, edges: []const Edge, node: Node.Index) !void {
        graph.list.set(@intFromEnum(index), try graph.createRaw(allocator, index, data, edges, node));
    }

    /// Removes the node from the graph, including all edges pointing to/from this node
    pub fn remove(graph: *Graph, index: Index) void {
        graph.slice.tags[@intFromEnum(index)] = .invalid;
        if (runtime_safty) {
            graph.slice.data[@intFromEnum(index)] = .{ .invalid = {} };
        }

        var it = graph.iterateEdges(index);
        while (it.next()) |edge| {
            graph.removeEdge(edge);
        }
    }

    /// Adds a new edge connections between two nodes
    pub fn addConnection(graph: *Graph, allocator: std.mem.Allocator, child: Index, parent: Index, intermedite_register: builtin_module.CpuRegister, bit_size: u16) !void {
        return graph.addEdge(allocator, .{
            .child = child,
            .parent = parent,
            .intermediate_register = intermedite_register,
            .bit_size = bit_size,
        });
    }
    /// Adds a new edge connections between two nodes
    pub fn addEdge(graph: *Graph, allocator: std.mem.Allocator, edge: Edge) !void {
        try alignExtraList(&graph.extra, allocator, @alignOf(Edge));
        const edge_index: ExtraIndex = @intCast(graph.extra.items.len);
        try graph.extra.appendSlice(allocator, std.mem.asBytes(&edge));

        // Update edges
        inline for (&.{ .child, .parent }) |relation| {
            try graph.appendEdge(allocator, edge, edge_index, relation);
        }
    }
    /// Removes an existing edge connection between two nodes
    pub fn removeConnection(graph: *Graph, child: Index, parent: Index) void {
        var it = graph.iterateParents(child);
        while (it.next()) |edge| {
            if (edge.parent == parent) {
                graph.removeEdge(edge);
                return;
            }
        }
    }
    /// Removes an existing edge connection between two nodes
    pub fn removeEdge(_: *Graph, edge: *Edge) void {
        edge.child = .none;
        edge.parent = .none;
    }

    const EdgeRelation = enum { child, parent };
    fn appendEdge(graph: *Graph, allocator: std.mem.Allocator, edge: Edge, edge_index: ExtraIndex, comptime relation: EdgeRelation) !void { // TODO: Maybe iterate edges and replace removed ones?
        const edges = &graph.slice.edges[@intFromEnum(@field(edge, @tagName(relation)))];
        if (edges[edges.len - 1] == .none) {
            // Free slot in node
            for (edges) |*edge_idx| {
                if (edge_idx.* == .none) {
                    edge_idx.* = .cast(edge_index);
                    edge_idx.markChild(relation == .parent);
                    break;
                }
            }
        } else if (!edges[edges.len - 1].isExtra()) {
            // Need to migrate to `extra`
            std.debug.assert(!edges[edges.len - 2].isExtra());

            const extra_index: ExtraIndex = @intCast(graph.extra.items.len);
            try alignExtraListEnsureCapacity(&graph.extra, allocator, 3 * @sizeOf(Edge.Index), @alignOf(Edge.Index));
            for (edges[(edges.len - 2)..]) |edge_idx| {
                graph.extra.appendSliceAssumeCapacity(std.mem.asBytes(&edge_idx));
            }

            var edge_idx: Edge.Index = .cast(edge_index);
            edge_idx.markChild(relation == .parent);
            graph.extra.appendSliceAssumeCapacity(std.mem.asBytes(&edge_idx));

            edges[edges.len - 1] = .cast(extra_index);
            edges[edges.len - 1].markExtra(true);
            edges[edges.len - 2] = .cast(3);
            edges[edges.len - 2].markExtra(true);
        } else {
            // Need to expand `extra`
            std.debug.assert(edges[edges.len - 1].isExtra());
            std.debug.assert(edges[edges.len - 2].isExtra());

            const extra_index: ExtraIndex = edges[edges.len - 1].getIndex();
            const extra_length: ExtraIndex = edges[edges.len - 2].getIndex();

            try alignExtraListEnsureCapacity(&graph.extra, allocator, (extra_length + 1) * @sizeOf(Edge.Index), @alignOf(Edge.Index));

            const new_extra_index: ExtraIndex = @intCast(graph.extra.items.len);
            graph.extra.items.len += extra_length * @sizeOf(Edge.Index);
            @memcpy(graph.extra.items[new_extra_index..(new_extra_index + extra_length * @sizeOf(Edge.Index))], graph.extra.items[extra_index..(extra_index + extra_length * @sizeOf(Edge.Index))]);

            var edge_idx: Edge.Index = .cast(edge_index);
            edge_idx.markChild(relation == .parent);
            graph.extra.appendSliceAssumeCapacity(std.mem.asBytes(&edge_idx));

            edges[edges.len - 1] = .cast(new_extra_index);
            edges[edges.len - 1].markExtra(true);
            edges[edges.len - 2] = .cast(edges[edges.len - 2].getIndex() + 1);
            edges[edges.len - 2].markExtra(true);
        }
    }

    /// Aligns `extra` to the given alignment, for the next write
    pub fn alignExtraList(extra: *ExtraList, allocator: std.mem.Allocator, comptime alignment: comptime_int) !void {
        std.debug.assert(alignment <= @alignOf(usize));
        const aligned_len = std.mem.alignForward(usize, extra.items.len, alignment);
        try extra.ensureTotalCapacity(allocator, aligned_len);
        extra.items.len = std.mem.alignForward(usize, extra.items.len, alignment);
    }
    /// Aligns `extra` to the given alignment and ensures `len` bytes are available, for the next write
    pub fn alignExtraListEnsureCapacity(extra: *ExtraList, allocator: std.mem.Allocator, len: usize, comptime alignment: comptime_int) !void {
        std.debug.assert(alignment <= @alignOf(usize));
        const aligned_len = std.mem.alignForward(usize, extra.items.len, alignment);
        try extra.ensureTotalCapacity(allocator, aligned_len + len);
        extra.items.len = std.mem.alignForward(usize, extra.items.len, alignment);
    }

    pub fn EdgeIterator(comptime yield_children: bool, comptime yield_parents: bool) type {
        return struct {
            const Iterator = @This();

            graph: *Graph,
            edge_index: ExtraIndex,
            access_data: union {
                node: Sir.Index,
                extra_end: ExtraIndex,
            },

            pub fn next(iter: *Iterator) ?*Edge {
                // Stored in the node
                while (iter.edge_index < @as(Edges, undefined).len) : (iter.edge_index += 1) {
                    const edges = iter.graph.slice.edges[@intFromEnum(iter.access_data.node)];
                    if (edges[iter.edge_index] == .none) {
                        return null;
                    }

                    // Check for `extra` encoding
                    if (edges[iter.edge_index].isExtra()) {
                        std.debug.assert(iter.edge_index == @as(Edges, undefined).len - 2);
                        std.debug.assert(edges[iter.edge_index + 1].isExtra());
                        iter.edge_index = edges[iter.edge_index + 1].getIndex();
                        std.debug.assert(iter.edge_index > @as(Edges, undefined).len);
                        iter.access_data = .{ .extra_end = iter.edge_index + edges[@as(Edges, undefined).len - 2].getIndex() * @sizeOf(Edge.Index) };
                        break;
                    }

                    if (yield_children and edges[iter.edge_index].isChild() or
                        yield_parents and !edges[iter.edge_index].isChild())
                    {
                        const edge = edges[iter.edge_index].get(iter.graph.extra);
                        if (edge.child != .none and edge.parent != .none) {
                            iter.edge_index += 1;
                            return edge;
                        }
                    }
                }
                if (iter.edge_index == @as(Edges, undefined).len) {
                    return null;
                }

                // Stored in `extra`
                while (iter.edge_index < iter.access_data.extra_end) : (iter.edge_index += @sizeOf(Edge.Index)) {
                    const index = std.mem.bytesToValue(Edge.Index, iter.graph.extra.items[iter.edge_index..(iter.edge_index + @sizeOf(Edge.Index))]);

                    if (yield_children and index.isChild() or
                        yield_parents and !index.isChild())
                    {
                        const edge = index.get(iter.graph.extra);
                        if (edge.child != .none and edge.parent != .none) {
                            iter.edge_index += @sizeOf(Edge.Index);
                            return edge;
                        }
                    }
                }

                return null;
            }
        };
    }
    /// Iterates all edges of the given node
    pub fn iterateEdges(graph: *Graph, index: Index) EdgeIterator(true, true) {
        return .{
            .graph = graph,
            .edge_index = 0,
            .access_data = .{ .node = index },
        };
    }
    /// Iterates all child-edges of the given node
    pub fn iterateChildren(graph: *Graph, index: Index) EdgeIterator(true, false) {
        return .{
            .graph = graph,
            .edge_index = 0,
            .access_data = .{ .node = index },
        };
    }
    /// Iterates all parent-edges of the given node
    pub fn iterateParents(graph: *Graph, index: Index) EdgeIterator(false, true) {
        return .{
            .graph = graph,
            .edge_index = 0,
            .access_data = .{ .node = index },
        };
    }

    pub fn TraverseIterator(comptime relation: enum { child, parent }, comptime filter_tags: ?[]const Tag, comptime stop_tags: []const Tag) type {
        return struct {
            const Iterator = @This();

            graph: *Graph,
            queue: *std.ArrayListUnmanaged(Index),

            pub fn next(iter: *Iterator, allocator: std.mem.Allocator) !?Sir.Index {
                loop: while (iter.queue.pop()) |current| {
                    if (filter_tags) |filter| {
                        inline for (filter) |tag| {
                            if (tag == current.getTag(iter.graph)) {
                                return current;
                            }
                        }
                    } else {
                        return current;
                    }

                    inline for (stop_tags) |tag| {
                        if (tag == current.getTag(iter.graph)) {
                            continue :loop;
                        }
                    }

                    var it: EdgeIterator(relation == .child, relation == .parent) = .{
                        .graph = iter.graph,
                        .edge_index = 0,
                        .access_data = .{ .node = current },
                    };
                    while (it.next()) |edge| {
                        const next_node = switch (relation) {
                            .child => edge.child,
                            .parent => edge.parent,
                        };

                        if (!iter.graph.slice.iter_data[@intFromEnum(next_node)].visited) {
                            iter.graph.slice.iter_data[@intFromEnum(next_node)].visited = true;
                            try iter.queue.append(allocator, next_node);
                        }
                    }
                }

                return null;
            }
        };
    }

    /// Iterativly traverses all children of the root node, with an optional filter and stop tags
    /// The provided `queue` should be reused often, to avoid allocations.
    ///
    /// Only one traversing iterator may be active at a time!
    pub fn traverseChildren(graph: *Graph, allocator: std.mem.Allocator, queue: *std.ArrayListUnmanaged(Index), root: Index, comptime filter_tags: ?[]const Tag, comptime stop_tags: []const Tag) !TraverseIterator(.child, filter_tags, stop_tags) {
        queue.clearRetainingCapacity();

        for (graph.slice.iter_data) |*data| {
            data.visited = false;
        }

        var it = graph.iterateChildren(root);
        while (it.next()) |edge| {
            graph.slice.iter_data[@intFromEnum(edge.child)].visited = true;
            try queue.append(allocator, edge.child);
        }

        return .{
            .graph = graph,
            .queue = queue,
        };
    }
    /// Iterativly traverses all parents of the root node, with an optional filter and stop tags
    /// The provided `queue` should be reused often, to avoid allocations.
    ///
    /// Only one traversing iterator may be active at a time!
    pub fn traverseParents(graph: *Graph, allocator: std.mem.Allocator, queue: *std.ArrayListUnmanaged(Index), root: Index, comptime filter_tags: ?[]const Tag, comptime stop_tags: []const Tag) !TraverseIterator(.parent, filter_tags, stop_tags) {
        queue.clearRetainingCapacity();

        for (graph.slice.iter_data) |*data| {
            data.visited = false;
        }

        var it = graph.iterateParents(root);
        while (it.next()) |edge| {
            graph.slice.iter_data[@intFromEnum(edge.parent)].visited = true;
            try queue.append(allocator, edge.parent);
        }

        return .{
            .graph = graph,
            .queue = queue,
        };
    }

    pub fn dumpPrologue(writer: std.fs.File.Writer) !void {
        try writer.writeAll(
            \\strict digraph {
            \\    fontname="JetBrainsMono,Arial,sans-serif"
            \\    node [fontname="JetBrainsMono,Arial,sans-serif"]
            \\    edge [fontname="JetBrainsMono,Arial,sans-serif"]
            \\
        );
    }
    pub fn dumpEpilogue(writer: std.fs.File.Writer) !void {
        try writer.writeAll("}\n");
    }

    /// Writes Graphviz DOT file into the writer for debugging purposed
    pub fn dumpGraph(graph: *Graph, writer: std.fs.File.Writer, sema: *const Sema, symbol: Symbol.Index) !void {
        const id = @intFromEnum(symbol);

        try writer.print(
            \\    subgraph {{
            \\        cluster=true
            \\        label="{}"
            \\        bgcolor="#{s}"
            \\        # Nodes
            \\
        , .{ sema.getSymbolLocation(symbol), switch (symbol.get(@constCast(sema)).*) {
            .function => "AED9E6",
            .constant => "E98B64",
            else => unreachable,
        } });

        const indent = "        ";

        for (0..graph.list.len) |idx| {
            // const src_template = "\\n{s}:{d}:{d}"; <BR/><FONT COLOR="#191919" POINT-SIZE="11">test3.znasm:48:11: 'src8  = my_const_value:x;'</FONT>
            const idx_template = "<FONT COLOR=\"#191919\" POINT-SIZE=\"11\">[{d}] = </FONT>";
            const src_template = "<BR/><FONT COLOR=\"#191919\" POINT-SIZE=\"11\">{s}:{d}:{d}: '{s}'</FONT>";
            // const src_template_small = "<BR/><FONT COLOR=\"#191919\" POINT-SIZE=\"11\">{s}:{d}:{d}</FONT>";

            const color_control_flow = "fillcolor=darkorchid2";

            const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));

            const ast = &symbol.getCommon(@constCast(sema)).module_index.get(@constCast(sema)).ast;
            const token_loc = ast.tokenLoc(ast.nodeToken(index.getSourceNode(graph)));
            const src_loc = std.zig.findLineColumn(ast.source, token_loc.start);
            const src_file = formatting.fmtHtmlEscape(ast.source_path);
            const src_line = formatting.fmtHtmlEscape(std.mem.trim(u8, src_loc.source_line, " "));

            switch (index.getTag(graph)) {
                .value => {
                    const int_value = index.getData(graph).value;
                    try writer.print(indent ++ "N{d}_{d} [label=<" ++ idx_template ++ "{}>,fillcolor=darkorange]\n", .{ idx, id, idx, int_value });
                },
                .symbol => {
                    const target = index.getData(graph).symbol;
                    const target_type = switch (target.symbol.get(@constCast(sema)).*) {
                        .constant => |const_sym| const_sym.type,
                        .variable => |var_sym| var_sym.type,
                        .register => |reg_sym| reg_sym.type,
                        else => unreachable,
                    };
                    try writer.print(indent ++ "N{d}_{d} [label=<" ++ idx_template ++ "{}[{d}..{d}] ({})" ++ src_template ++ ">,shape=box,fillcolor=firebrick1]\n", .{ idx, id, idx, sema.getSymbolLocation(target.symbol), target.byte_start, target.byte_end, target_type.fmt(sema), src_file, src_loc.line + 1, src_loc.column + 1, src_line });
                },
                .block_start => try writer.print(indent ++ "N{d}_{d} [label=<" ++ idx_template ++ "SSA Start>,shape=hexagon,fillcolor=blueviolet]\n", .{ idx, id, idx }),
                .block_end => try writer.print(indent ++ "N{d}_{d} [label=<" ++ idx_template ++ "SSA End>,shape=hexagon,fillcolor=blueviolet]\n", .{ idx, id, idx }),
                .@"return" => try writer.print(indent ++ "N{d}_{d} [label=<" ++ idx_template ++ "return>,shape=hexagon," ++ color_control_flow ++ "]\n", .{ idx, id, idx }),
                .invalid => {}, // ignore
                // else => try writer.print(indent ++ "N{d}_{d} [label=<{s}" ++ src_template ++ ">,fillcolor=lightgray]\n", .{ idx, id, @tagName(index.getTag(graph)), src_file, src_loc.line + 1, src_loc.column + 1, src_line }),
            }
        }

        try writer.writeAll("        # Edges\n");
        for (0..graph.list.len) |idx| {
            const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));
            if (index.getTag(graph) == .invalid) {
                continue;
            }

            var it = graph.iterateChildren(index);
            while (it.next()) |edge| {
                switch (edge.type) {
                    .dependency => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d}\n", .{ @intFromEnum(index), id, @intFromEnum(edge.child), id }),
                    .dataflow => switch (edge.intermediate_register) {
                        .none => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=darkgray,fontcolor=darkgray,fontsize=10,label=\"{d}\"]\n", .{ @intFromEnum(index), id, @intFromEnum(edge.child), id, edge.bit_size }),
                        .a => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=crimson,fontcolor=crimson,fontsize=10,label=\"a{d}\"]\n", .{ @intFromEnum(index), id, @intFromEnum(edge.child), id, edge.bit_size }),
                        .x => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=forestgreen,fontcolor=forestgreen,fontsize=10,label=\"x{d}\"]\n", .{ @intFromEnum(index), id, @intFromEnum(edge.child), id, edge.bit_size }),
                        .y => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=dodgerblue,fontcolor=dodgerblue,fontsize=10,label=\"y{d}\"]\n", .{ @intFromEnum(index), id, @intFromEnum(edge.child), id, edge.bit_size }),
                    },
                }
            }
        }

        try writer.writeAll("    }\n");
    }
};

pub const Tag2 = union(enum) {
    /// Indicates n `field_reference` instructions following the current instruction
    pub const FieldTarget = u16;

    /// Starts a new nested scope, containing local variables inside it
    begin_scope: void,
    /// Ends the most recently stated scope
    end_scope: void,

    /// Declares a new local variable for the current scope
    /// `node` is a `local_var_decl`
    declare_variable: struct {
        location: builtin_module.VariableLocation,
        type: TypeExpression.Index,
    },

    /// Declares a new label for the function
    /// `node` is a `label`
    declare_label: void,

    /// References a field on the current target
    field_reference: struct {
        bit_offset: u16,
        type: TypeExpression.Index,
    },

    /// Assigns the expression to the target local variable
    assign_local: struct {
        local_index: Index,
        field_target: FieldTarget,
        value: Expression.Index,
    },
    /// Assigns the expression to the target global symbol
    assign_global: struct {
        symbol: Symbol.Index,
        field_target: FieldTarget,
        value: Expression.Index,
    },
    /// Assigns the expression to the target built-in
    assign_builtin: struct {
        builtin: BuiltinVar.Tag,
        value: Expression.Index,
    },

    /// Sets the current CPU mode
    cpu_mode: builtin_module.CpuMode,

    /// Calls the target function
    call_function: struct {
        symbol: Symbol.Index,
    },
    /// Calls the target built-in
    call_builtin: struct {
        builtin: BuiltinFn.Tag,
    },

    /// Returns from the current function
    @"return": void,
};

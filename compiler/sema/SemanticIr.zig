//! High-level Semantic Intermediate Representation
//! Represented as a 'Sea of Nodes' directed acyclic graph
const std = @import("std");
const builtin_module = @import("../builtin_module.zig");

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

const SemanticIr = @This();

pub const Index = enum(u16) {
    none = std.math.maxInt(u16),
    _,

    pub fn getTag(index: Index, list: NodeList) std.meta.Tag(Data) {
        return list.slice.tags[@intFromEnum(index)];
    }
    pub fn getData(index: Index, list: NodeList) Data {
        const tag = index.getTag(list);
        const bare = list.slice.data[@intFromEnum(index)];

        return .unpack(switch (tag) {
            inline else => |t| @unionInit(Data.Packed, @tagName(t), @field(bare, @tagName(t))),
        }, list.extra);
    }
    pub fn getParents(index: Index, list: NodeList) Parents {
        return list.slice.parents[@intFromEnum(index)];
    }
    pub fn getSourceNode(index: Index, list: NodeList) Node.Index {
        return list.slice.sources[@intFromEnum(index)];
    }

    /// Adds the target node as a dependency to the current node
    pub fn addDependency(index: Index, allocator: std.mem.Allocator, list: *NodeList, dependency: Index, location: enum { left, right, both }) !void {
        var parents = &list.slice.parents[@intFromEnum(index)];

        if (index.getTag(list.*) == .merge) {
            const data = &list.slice.data[@intFromEnum(index)].merge;

            if (parents.left == .none) {
                parents.left = dependency;
                return;
            } else if (parents.right == .none) {
                parents.right = dependency;
                return;
            } else {
                for (data) |*parent| {
                    if (parent.* == .none) {
                        parent.* = dependency;
                        return;
                    }
                }
            }

            // Try forwarding to nested `merge` if they have space
            if (parents.left.getTag(list.*) == .merge and list.slice.data[@intFromEnum(parents.left)].merge[data.len - 1] == .none) {
                return parents.left.addDependency(allocator, list, dependency, undefined);
            }
            if (parents.right.getTag(list.*) == .merge and list.slice.data[@intFromEnum(parents.right)].merge[data.len - 1] == .none) {
                return parents.right.addDependency(allocator, list, dependency, undefined);
            }
            for (data) |*parent| {
                if (parent.getTag(list.*) == .merge and list.slice.data[@intFromEnum(parent.*)].merge[data.len - 1] == .none) {
                    return parent.addDependency(allocator, list, dependency, undefined);
                }
            }

            // Create new `merge` node if not already
            if (parents.left.getTag(list.*) != .merge) {
                parents.left = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(list.*) });
                return;
            }
            if (parents.right.getTag(list.*) != .merge) {
                parents.right = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(list.*) });
                return;
            }
            for (data) |*parent| {
                if (parent.getTag(list.*) != .merge) {
                    parent.* = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parent.*, dependency, .none } }, .source = index.getSourceNode(list.*) });
                    return;
                }
            }

            // Everything is occuped -> forward to next `merge`
            std.debug.assert(parents.left.getTag(list.*) == .merge);
            return parents.left.addDependency(allocator, list, dependency, undefined);
        }

        switch (location) {
            .left => {
                if (parents.left == .none) {
                    parents.left = dependency;
                } else if (parents.left.getTag(list.*) == .merge) {
                    return parents.left.addDependency(allocator, list, dependency, undefined);
                } else {
                    parents.left = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(list.*) });
                }
            },
            .right => {
                if (parents.right == .none) {
                    parents.right = dependency;
                } else if (parents.right.getTag(list.*) == .merge) {
                    return parents.right.addDependency(allocator, list, dependency, undefined);
                } else {
                    parents.right = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(list.*) });
                }
            },
            .both => {
                if (parents.left == .none) {
                    parents.left = dependency;
                } else if (parents.right == .none) {
                    parents.right = dependency;
                } else if (parents.left.getTag(list.*) == .merge) {
                    return parents.left.addDependency(allocator, list, dependency, undefined);
                } else if (parents.right.getTag(list.*) == .merge) {
                    return parents.right.addDependency(allocator, list, dependency, undefined);
                } else if (parents.left.getTag(list.*) != .merge) {
                    parents.left = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(list.*) });
                } else {
                    std.debug.assert(parents.right.getTag(list.*) != .merge);
                    parents.right = try list.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(list.*) });
                }
            },
        }
    }
};
pub const Data = union(enum) {
    /// Merges up to 6 dependencies into a single dependency
    merge: [4]Index,

    /// Compile-time known immediate value
    /// Cannot have dependencies
    value: std.math.big.int.Const,

    /// Stores the `left` parent's value into the symbol
    /// Depends on the `block_start` of the current SSA (`right`)
    store_symbol: Symbol.Index,

    /// Return from the current function
    ///
    /// Dependencies:
    ///  - Assignments to global variables (`var` / `reg`)
    ///  - Assignments to return variables (TODO)
    @"return": void,

    pub fn unpack(packed_data: Packed, extra: NodeList.ExtraList) Data {
        switch (packed_data) {
            .value => |info| {
                return .{ .value = .{
                    .positive = info.positive,
                    .limbs = @alignCast(std.mem.bytesAsSlice(std.math.big.Limb, extra.items[info.limbs_index..(info.limbs_index + info.limbs_len * @sizeOf(std.math.big.Limb))])),
                } };
            },
            inline else => |info, tag| {
                return @unionInit(Data, @tagName(tag), info);
            },
        }
    }
    pub fn pack(data: Data, allocator: std.mem.Allocator, extra: *NodeList.ExtraList) !Packed {
        switch (data) {
            .value => |info| {
                // extra.appendUnalignedSlice(allocator, items)
                const index: u32 = @intCast(extra.items.len);
                try extra.appendSlice(allocator, std.mem.sliceAsBytes(info.limbs));
                return .{ .value = .{
                    .positive = info.positive,
                    .limbs_len = @intCast(info.limbs.len),
                    .limbs_index = index,
                } };
            },
            inline else => |info, tag| {
                return @unionInit(Packed, @tagName(tag), info);
            },
        }
    }

    /// Packed representation, storing excess data into `list.extra`
    pub const Packed = union(std.meta.Tag(Data)) {
        merge: [4]Index,
        value: struct {
            positive: bool,
            limbs_len: u16,
            limbs_index: u32,
        },
        store_symbol: Symbol.Index,
        @"return": void,
    };
};
pub const Parents = packed struct(u32) {
    left: Index = .none,
    right: Index = .none,
};

/// Data associated with the current node type
data: Data,
/// Parent connections for this node
parents: Parents = .{},

/// AST node which is responsible for this graph node
source: Node.Index,

/// Efficent list for storing SIR nodes
pub const NodeList = b: {
    const data_info = @typeInfo(Data.Packed).@"union";

    const Tag = std.meta.Tag(Data);
    const Bare = @Type(.{ .@"union" = .{
        .layout = data_info.layout,
        .tag_type = null,
        .fields = data_info.fields,
        .decls = &.{},
    } });

    // Limit memory usage of nodes
    std.debug.assert(@sizeOf(Tag) <= @sizeOf(u8));
    std.debug.assert(@sizeOf(Bare) <= @sizeOf(u64) + if (runtime_safty) 4 else 0);

    break :b struct {
        const List = @This();
        pub const empty: List = .{
            .graph = .empty,
            .extra = .empty,
        };

        /// Additional data for each node, to avoid extra allocators for iterators
        const IteratorData = packed struct(u8) {
            visited: bool = false,
            _: u7 = 0,
        };
        const Graph = std.MultiArrayList(struct {
            tag: Tag,
            data: Bare,
            parents: Parents,

            iter_data: IteratorData = .{},

            source: Node.Index,
        });
        pub const ExtraList = std.ArrayListAlignedUnmanaged(u8, @alignOf(usize));
        const ComputedSlice = struct {
            tags: []Tag,
            data: []Bare,
            parents: []Parents,

            iter_data: []IteratorData,

            sources: []const Node.Index,
        };

        graph: Graph,
        extra: ExtraList,
        slice: ComputedSlice = undefined,

        pub fn deinit(list: *List, allocator: std.mem.Allocator) void {
            list.graph.deinit(allocator);
            list.extra.deinit(allocator);
        }

        /// Creates a new node in the list
        pub fn create(list: *List, allocator: std.mem.Allocator, node: SemanticIr) !Index {
            const node_index: Index = @enumFromInt(@as(std.meta.Tag(SemanticIr.Index), @intCast(list.graph.len)));

            const tag = std.meta.activeTag(node.data);
            const packed_data = try node.data.pack(allocator, &list.extra);

            try list.graph.append(allocator, .{
                .tag = tag,
                .data = switch (tag) {
                    inline else => |t| @unionInit(Bare, @tagName(t), @field(packed_data, @tagName(t))),
                },
                .parents = node.parents,
                .source = node.source,
            });
            list.slice = undefined;
            return node_index;
        }
        /// Creates a new node in the list and ensures `list.slice` is valid
        pub fn createRefresh(list: *List, allocator: std.mem.Allocator, node: SemanticIr) !Index {
            const will_reallocate = list.graph.capacity == list.graph.len;
            const index = list.create(allocator, node);
            if (runtime_safty or will_reallocate) {
                list.refresh();
            }
            return index;
        }

        fn TraverseIterator(comptime root_tag: Tag, comptime filter_tags: []const Tag) type {
            return struct {
                const Iterator = @This();

                slice: *const ComputedSlice,
                queue: *std.ArrayListUnmanaged(Index),
                root_index: std.meta.Tag(Index),

                pub fn next(iter: *Iterator, allocator: std.mem.Allocator) !?Index {
                    // Search roots
                    while (iter.queue.items.len == 0) {
                        if (iter.root_index == 0) break;
                        iter.root_index -= 1;

                        if (iter.slice.tags[iter.root_index] == root_tag and !iter.slice.iter_data[iter.root_index].visited) {
                            iter.slice.iter_data[iter.root_index].visited = true;
                            try iter.queue.append(allocator, @enumFromInt(iter.root_index));
                            break;
                        }
                    }

                    // Traverse parents
                    while (iter.queue.pop()) |current| {
                        const parents = iter.slice.parents[@intFromEnum(current)];
                        if (parents.left != .none and !iter.slice.iter_data[@intFromEnum(parents.left)].visited) {
                            iter.slice.iter_data[@intFromEnum(parents.left)].visited = true;
                            try iter.queue.append(allocator, parents.left);
                        }
                        if (parents.right != .none and !iter.slice.iter_data[@intFromEnum(parents.right)].visited) {
                            iter.slice.iter_data[@intFromEnum(parents.right)].visited = true;
                            try iter.queue.append(allocator, parents.right);
                        }

                        if (iter.slice.tags[@intFromEnum(current)] == .merge) {
                            const data = iter.slice.data[@intFromEnum(current)].merge;
                            for (data) |parent| {
                                if (parent != .none and !iter.slice.iter_data[@intFromEnum(parent)].visited) {
                                    iter.slice.iter_data[@intFromEnum(parent)].visited = true;
                                    try iter.queue.append(allocator, parent);
                                }
                            }
                        }

                        if (filter_tags.len == 0) {
                            return current;
                        } else {
                            inline for (filter_tags) |filter| {
                                if (iter.slice.tags[@intFromEnum(current)] == filter) {
                                    return current;
                                }
                            }
                        }
                    }

                    return null;
                }
            };
        }

        /// Iterator which travers the graph from given root tags through their parents, with an optional tag filter.
        /// If new nodes are created while iterating, `list.refresh()` must be called.
        /// The provided `queue` should be reused often, to avoid allocations.
        ///
        /// Only one iterator may be active at a time!
        pub fn traverseIterator(list: *const List, queue: *std.ArrayListUnmanaged(Index), comptime root_tag: Tag, comptime filter_tags: []const Tag) TraverseIterator(root_tag, filter_tags) {
            queue.clearRetainingCapacity();
            for (list.slice.iter_data) |*data| {
                data.visited = false;
            }

            return .{
                .slice = &list.slice,
                .queue = queue,
                .root_index = @intCast(list.graph.len),
            };
        }

        /// Updates slices to node data.
        /// Creating nodes invalidates any active slices
        pub fn refresh(list: *List) void {
            const slice = list.graph.slice();
            list.slice = .{
                .tags = slice.items(.tag),
                .data = slice.items(.data),
                .parents = slice.items(.parents),
                .iter_data = slice.items(.iter_data),
                .sources = slice.items(.source),
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
        pub fn dumpGraph(list: List, writer: std.fs.File.Writer, sema: *const Sema, symbol: Symbol.Index) !void {
            const id = @intFromEnum(symbol);

            try writer.print(
                \\    subgraph {{
                \\        cluster=true
                \\        label="{}"
                \\        bgcolor="#AED9E6"
                \\        # Nodes
                \\
            , .{sema.getSymbolLocation(symbol)});
            for (0..list.graph.len) |idx| {
                const indent = "        ";
                const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));

                switch (index.getTag(list)) {
                    .value => {
                        const int_value = index.getData(list).value;
                        try writer.print(indent ++ "N{d}_{d} [label={},fillcolor=darkorange]\n", .{ idx, id, int_value });
                    },
                    .store_symbol => {
                        const target_symbol = index.getData(list).store_symbol;
                        const target_type = switch (target_symbol.get(@constCast(sema)).*) {
                            .constant => |const_sym| const_sym.type,
                            .variable => |var_sym| var_sym.type,
                            .register => |reg_sym| reg_sym.type,
                            else => unreachable,
                        };
                        try writer.print(indent ++ "N{d}_{d} [label=\"Store '{}' ({})\",shape=box,fillcolor=firebrick1]\n", .{ idx, id, sema.getSymbolLocation(target_symbol), target_type.fmt(sema) });
                    },
                    .@"return" => try writer.print(indent ++ "N{d}_{d} [label=return,shape=diamond,fillcolor=darkorchid2]\n", .{ idx, id }),
                    .merge => {}, // ignore
                    // else => try writer.print(indent ++ "N{d}_{d} [label={s}]\n", .{ idx, id, @tagName(index.getTag(list)) }),
                }
            }

            try writer.writeAll("        # Edges\n");
            for (0..list.graph.len) |idx| {
                const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));
                if (index.getTag(list) == .merge) {
                    continue;
                }

                try list.writeGraphEdges(writer, id, index, .none);
            }

            try writer.writeAll("    }\n");
        }

        fn writeGraphEdges(list: List, writer: std.fs.File.Writer, id: u32, source: Index, target: Index) !void {
            const indent = "        ";
            const style_dependency = "[color=blue]\n";

            const parents = source.getParents(list);

            // Remove `merge` nodes from graph output
            if (source.getTag(list) == .merge) {
                if (parents.left != .none) {
                    try list.writeGraphEdges(writer, id, parents.left, target);
                }
                if (parents.right != .none) {
                    try list.writeGraphEdges(writer, id, parents.right, target);
                }

                const data = list.slice.data[@intFromEnum(source)].merge;
                for (data) |parent| {
                    if (parent != .none) {
                        try list.writeGraphEdges(writer, id, parent, target);
                    }
                }
            } else {
                if (target != .none) {
                    switch (target.getTag(list)) {
                        .@"return" => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} " ++ style_dependency, .{ @intFromEnum(source), id, @intFromEnum(target), id }),
                        else => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=red]\n", .{ @intFromEnum(source), id, @intFromEnum(target), id }),
                    }
                }

                if (parents.left != .none) {
                    try list.writeGraphEdges(writer, id, parents.left, source);
                }
                if (parents.right != .none) {
                    try list.writeGraphEdges(writer, id, parents.right, source);
                }
            }
        }
    };
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

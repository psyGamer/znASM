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

const SemanticIr = @This();

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
    pub fn getParents(index: Index, graph: *const Graph) *Parents {
        return &graph.slice.parents[@intFromEnum(index)];
    }
    pub fn getSourceNode(index: Index, graph: *const Graph) Node.Index {
        return graph.slice.sources[@intFromEnum(index)];
    }

    /// Adds the target node as a dependency to the current node
    pub fn addDependency(index: Index, allocator: std.mem.Allocator, graph: *Graph, dependency: Index, location: enum { left, right, both }) !void {
        var parents = &graph.slice.parents[@intFromEnum(index)];

        if (index.getTag(graph) == .merge) {
            const data = &graph.slice.data[@intFromEnum(index)].merge;

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
            if (parents.left.getTag(graph) == .merge and graph.slice.data[@intFromEnum(parents.left)].merge[data.len - 1] == .none) {
                return parents.left.addDependency(allocator, graph, dependency, undefined);
            }
            if (parents.right.getTag(graph) == .merge and graph.slice.data[@intFromEnum(parents.right)].merge[data.len - 1] == .none) {
                return parents.right.addDependency(allocator, graph, dependency, undefined);
            }
            for (data) |*parent| {
                if (parent.getTag(graph) == .merge and graph.slice.data[@intFromEnum(parent.*)].merge[data.len - 1] == .none) {
                    return parent.addDependency(allocator, graph, dependency, undefined);
                }
            }

            // Create new `merge` node if not already
            if (parents.left.getTag(graph) != .merge) {
                parents.left = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(graph) });
                return;
            }
            if (parents.right.getTag(graph) != .merge) {
                parents.right = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(graph) });
                return;
            }
            for (data) |*parent| {
                if (parent.getTag(graph) != .merge) {
                    parent.* = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parent.*, dependency, .none } }, .source = index.getSourceNode(graph) });
                    return;
                }
            }

            // Everything is occuped -> forward to next `merge`
            std.debug.assert(parents.left.getTag(graph) == .merge);
            return parents.left.addDependency(allocator, graph, dependency, undefined);
        }

        switch (location) {
            .left => {
                if (parents.left == .none) {
                    parents.left = dependency;
                } else if (parents.left.getTag(graph) == .merge) {
                    return parents.left.addDependency(allocator, graph, dependency, undefined);
                } else {
                    parents.left = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(graph) });
                }
            },
            .right => {
                if (parents.right == .none) {
                    parents.right = dependency;
                } else if (parents.right.getTag(graph) == .merge) {
                    return parents.right.addDependency(allocator, graph, dependency, undefined);
                } else {
                    parents.right = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(graph) });
                }
            },
            .both => {
                if (parents.left == .none) {
                    parents.left = dependency;
                } else if (parents.right == .none) {
                    parents.right = dependency;
                } else if (parents.left.getTag(graph) == .merge) {
                    return parents.left.addDependency(allocator, graph, dependency, undefined);
                } else if (parents.right.getTag(graph) == .merge) {
                    return parents.right.addDependency(allocator, graph, dependency, undefined);
                } else if (parents.left.getTag(graph) != .merge) {
                    parents.left = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.left, dependency, .none } }, .source = index.getSourceNode(graph) });
                } else {
                    std.debug.assert(parents.right.getTag(graph) != .merge);
                    parents.right = try graph.createRefresh(allocator, .{ .data = .{ .merge = .{ .none, parents.right, dependency, .none } }, .source = index.getSourceNode(graph) });
                }
            },
        }
    }
};
pub const Data = union(enum) {
    /// Indicates the start of a Single-Static-Assignment (SSA) block
    block_start: void,

    /// Merges up to 6 dependencies into a single dependency
    merge: [4]Index,

    /// Compile-time known immediate value
    /// Cannot have dependencies
    value: std.math.big.int.Const,

    /// Loads value from the target symbol if used as a depdenncy
    /// Stores value to the target symbol if `left != .none`
    /// Depends on the previous assignment to `symbol` with an overlapping range (`right`)
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

    pub fn unpack(packed_data: Packed, extra: Graph.ExtraList) Data {
        switch (packed_data) {
            .value => |info| {
                return .{ .value = .{
                    .positive = info.positive,
                    .limbs = @alignCast(std.mem.bytesAsSlice(std.math.big.Limb, extra.items[info.limbs_index..(info.limbs_index + info.limbs_len * @sizeOf(std.math.big.Limb))])),
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
                // extra.appendUnalignedSlice(allocator, items)
                const index: u32 = @intCast(extra.items.len);
                try extra.appendSlice(allocator, std.mem.sliceAsBytes(info.limbs));
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
        merge: [4]Index,
        value: struct {
            positive: bool,
            limbs_len: u16,
            limbs_index: u32,
        },
        symbol: @TypeOf(@as(Data, undefined).symbol),
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

/// Efficent graph for storing SIR nodes
pub const Graph = b: {
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
        const Self = @This();
        pub const empty: Self = .{
            .list = .empty,
            .extra = .empty,
        };

        /// Additional data for each node, to avoid extra allocators for iterators
        const IteratorData = packed struct(u8) {
            visited: bool = false,
            _: u7 = 0,
        };
        const List = std.MultiArrayList(struct {
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

        list: List,
        extra: ExtraList,
        slice: ComputedSlice = undefined,

        pub fn deinit(graph: *Self, allocator: std.mem.Allocator) void {
            graph.list.deinit(allocator);
            graph.extra.deinit(allocator);
        }

        /// Creates a new node in the graph
        pub fn create(graph: *Self, allocator: std.mem.Allocator, node: SemanticIr) !Index {
            const node_index: Index = @enumFromInt(@as(std.meta.Tag(SemanticIr.Index), @intCast(graph.list.len)));

            const tag = std.meta.activeTag(node.data);
            const packed_data = try node.data.pack(allocator, &graph.extra);

            try graph.list.append(allocator, .{
                .tag = tag,
                .data = switch (tag) {
                    inline else => |t| @unionInit(Bare, @tagName(t), @field(packed_data, @tagName(t))),
                },
                .parents = node.parents,
                .source = node.source,
            });
            graph.slice = undefined;
            return node_index;
        }
        /// Creates a new node in the graph and ensures `graph.slice` is valid
        pub fn createRefresh(graph: *Self, allocator: std.mem.Allocator, node: SemanticIr) !Index {
            const will_reallocate = graph.list.capacity == graph.list.len;
            const index = graph.create(allocator, node);
            if (runtime_safty or will_reallocate) {
                graph.refresh();
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
        /// If new nodes are created while iterating, `graph.refresh()` must be called.
        /// The provided `queue` should be reused often, to avoid allocations.
        ///
        /// Only one iterator may be active at a time!
        pub fn traverseIterator(graph: *const Self, queue: *std.ArrayListUnmanaged(Index), comptime root_tag: Tag, comptime filter_tags: []const Tag) TraverseIterator(root_tag, filter_tags) {
            queue.clearRetainingCapacity();
            for (graph.slice.iter_data) |*data| {
                data.visited = false;
            }

            return .{
                .slice = &graph.slice,
                .queue = queue,
                .root_index = @intCast(graph.list.len),
            };
        }

        /// Updates slices to node data.
        /// Creating nodes invalidates any active slices
        pub fn refresh(graph: *Self) void {
            const slice = graph.list.slice();
            graph.slice = .{
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
        pub fn dumpGraph(graph: *Self, writer: std.fs.File.Writer, sema: *const Sema, symbol: Symbol.Index) !void {
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

            graph.refresh();
            for (0..graph.list.len) |idx| {
                const indent = "        ";
                // const src_template = "\\n{s}:{d}:{d}"; <BR/><FONT COLOR="#191919" POINT-SIZE="11">test3.znasm:48:11: 'src8  = my_const_value:x;'</FONT>
                const src_template = "<BR/><FONT COLOR=\"#191919\" POINT-SIZE=\"11\">{s}:{d}:{d}: '{s}'</FONT>";
                const src_template_small = "<BR/><FONT COLOR=\"#191919\" POINT-SIZE=\"11\">{s}:{d}:{d}</FONT>";

                const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));

                const ast = &symbol.getCommon(@constCast(sema)).module_index.get(@constCast(sema)).ast;
                const token_loc = ast.tokenLoc(ast.nodeToken(index.getSourceNode(graph)));
                const src_loc = std.zig.findLineColumn(ast.source, token_loc.start);
                const src_file = formatting.fmtHtmlEscape(ast.source_path);
                const src_line = formatting.fmtHtmlEscape(std.mem.trim(u8, src_loc.source_line, " "));

                switch (index.getTag(graph)) {
                    .value => {
                        const int_value = index.getData(graph).value;
                        try writer.print(indent ++ "N{d}_{d} [label=<{}>,fillcolor=darkorange]\n", .{ idx, id, int_value });
                    },
                    .symbol => {
                        const target = index.getData(graph).symbol;
                        const target_type = switch (target.symbol.get(@constCast(sema)).*) {
                            .constant => |const_sym| const_sym.type,
                            .variable => |var_sym| var_sym.type,
                            .register => |reg_sym| reg_sym.type,
                            else => unreachable,
                        };
                        try writer.print(indent ++ "N{d}_{d} [label=<{}[{d}..{d}] ({})" ++ src_template ++ ">,shape=box,fillcolor=firebrick1]\n", .{ idx, id, sema.getSymbolLocation(target.symbol), target.byte_start, target.byte_end, target_type.fmt(sema), src_file, src_loc.line + 1, src_loc.column + 1, src_line });
                    },
                    .@"return" => try writer.print(indent ++ "N{d}_{d} [label=<return" ++ src_template_small ++ ">,shape=diamond,fillcolor=darkorchid2]\n", .{ idx, id, src_file, src_loc.line + 1, src_loc.column + 1 }),
                    .merge => {}, // ignore
                    else => try writer.print(indent ++ "N{d}_{d} [label=<{s}" ++ src_template ++ ">,fillcolor=lightgray]\n", .{ idx, id, @tagName(index.getTag(graph)), src_file, src_loc.line + 1, src_loc.column + 1, src_line }),
                }
            }

            try writer.writeAll("        # Edges\n");
            for (0..graph.list.len) |idx| {
                const index: Index = @enumFromInt(@as(std.meta.Tag(Index), @intCast(idx)));
                if (index.getTag(graph) == .merge) {
                    continue;
                }

                try graph.writeGraphEdges(writer, id, index, .none);
            }

            try writer.writeAll("    }\n");
        }

        fn writeGraphEdges(graph: *const Self, writer: std.fs.File.Writer, id: u32, source: Index, target: Index) !void {
            const indent = "        ";
            const style_dependency = "[color=blue]\n";
            _ = style_dependency; // autofix
            const style_return = "[color=darkorchid2]\n";

            const parents = source.getParents(graph);

            // Remove `merge` nodes from graph output
            if (source.getTag(graph) == .merge) {
                if (parents.left != .none) {
                    try graph.writeGraphEdges(writer, id, parents.left, target);
                }
                if (parents.right != .none) {
                    try graph.writeGraphEdges(writer, id, parents.right, target);
                }

                const data = graph.slice.data[@intFromEnum(source)].merge;
                for (data) |parent| {
                    if (parent != .none) {
                        try graph.writeGraphEdges(writer, id, parent, target);
                    }
                }
            } else {
                if (target != .none) {
                    switch (target.getTag(graph)) {
                        .@"return" => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} " ++ style_return, .{ @intFromEnum(source), id, @intFromEnum(target), id }),
                        else => try writer.print(indent ++ "N{d}_{d} -> N{d}_{d} [color=red]\n", .{ @intFromEnum(source), id, @intFromEnum(target), id }),
                    }
                }

                if (parents.left != .none) {
                    try graph.writeGraphEdges(writer, id, parents.left, source);
                }
                if (parents.right != .none) {
                    try graph.writeGraphEdges(writer, id, parents.right, source);
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

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Graph implementation using adjacency list.
pub fn Graph(comptime T: type) type {
    return struct {
        const Self = @This();
        const NeighborSet = std.AutoHashMap(T, void);

        adjacency_list: std.AutoHashMap(T, NeighborSet),
        allocator: Allocator,
        directed: bool,

        pub fn init(allocator: Allocator, directed: bool) Self {
            return .{
                .adjacency_list = std.AutoHashMap(T, NeighborSet).init(allocator),
                .allocator = allocator,
                .directed = directed,
            };
        }

        pub fn deinit(self: *Self) void {
            var it = self.adjacency_list.valueIterator();
            while (it.next()) |neighbors| {
                neighbors.deinit();
            }
            self.adjacency_list.deinit();
        }

        pub fn addVertex(self: *Self, vertex: T) !void {
            if (!self.adjacency_list.contains(vertex)) {
                try self.adjacency_list.put(vertex, NeighborSet.init(self.allocator));
            }
        }

        pub fn addEdge(self: *Self, from: T, to: T) !void {
            try self.addVertex(from);
            try self.addVertex(to);

            var neighbors = self.adjacency_list.getPtr(from).?;
            try neighbors.put(to, {});

            if (!self.directed) {
                var to_neighbors = self.adjacency_list.getPtr(to).?;
                try to_neighbors.put(from, {});
            }
        }

        pub fn hasVertex(self: *const Self, vertex: T) bool {
            return self.adjacency_list.contains(vertex);
        }

        pub fn hasEdge(self: *const Self, from: T, to: T) bool {
            if (self.adjacency_list.get(from)) |neighbors| {
                return neighbors.contains(to);
            }
            return false;
        }

        pub fn neighbors(self: *const Self, vertex: T) ?*const NeighborSet {
            return self.adjacency_list.getPtr(vertex);
        }

        pub fn vertexCount(self: *const Self) usize {
            return self.adjacency_list.count();
        }

        pub fn isDirected(self: *const Self) bool {
            return self.directed;
        }
    };
}

test "Graph operations" {
    const allocator = std.testing.allocator;
    var graph = Graph(i32).init(allocator, false);
    defer graph.deinit();

    try graph.addEdge(1, 2);
    try graph.addEdge(1, 3);

    try std.testing.expect(graph.hasVertex(1));
    try std.testing.expect(graph.hasEdge(1, 2));
    try std.testing.expect(graph.hasEdge(2, 1));
}

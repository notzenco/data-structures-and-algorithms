const std = @import("std");
const Allocator = std.mem.Allocator;
const Graph = @import("graph.zig").Graph;

/// Breadth-first search algorithms.
/// Time: O(V + E)
/// Space: O(V)

pub fn traverse(comptime T: type, graph: *const Graph(T), start: T, allocator: Allocator) !std.ArrayList(T) {
    var result = std.ArrayList(T).init(allocator);
    errdefer result.deinit();

    if (!graph.hasVertex(start)) return result;

    var visited = std.AutoHashMap(T, void).init(allocator);
    defer visited.deinit();

    var queue = std.ArrayList(T).init(allocator);
    defer queue.deinit();

    try queue.append(start);
    try visited.put(start, {});

    while (queue.items.len > 0) {
        const vertex = queue.orderedRemove(0);
        try result.append(vertex);

        if (graph.neighbors(vertex)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |neighbor| {
                if (!visited.contains(neighbor.*)) {
                    try visited.put(neighbor.*, {});
                    try queue.append(neighbor.*);
                }
            }
        }
    }

    return result;
}

pub fn shortestPath(comptime T: type, graph: *const Graph(T), start: T, end: T, allocator: Allocator) !?std.ArrayList(T) {
    if (!graph.hasVertex(start) or !graph.hasVertex(end)) return null;
    if (start == end) {
        var result = std.ArrayList(T).init(allocator);
        try result.append(start);
        return result;
    }

    var visited = std.AutoHashMap(T, void).init(allocator);
    defer visited.deinit();

    var parent = std.AutoHashMap(T, T).init(allocator);
    defer parent.deinit();

    var queue = std.ArrayList(T).init(allocator);
    defer queue.deinit();

    try queue.append(start);
    try visited.put(start, {});

    while (queue.items.len > 0) {
        const vertex = queue.orderedRemove(0);

        if (vertex == end) {
            return try reconstructPath(T, &parent, start, end, allocator);
        }

        if (graph.neighbors(vertex)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |neighbor| {
                if (!visited.contains(neighbor.*)) {
                    try visited.put(neighbor.*, {});
                    try parent.put(neighbor.*, vertex);
                    try queue.append(neighbor.*);
                }
            }
        }
    }

    return null;
}

fn reconstructPath(comptime T: type, parent: *const std.AutoHashMap(T, T), start: T, end: T, allocator: Allocator) !std.ArrayList(T) {
    var path = std.ArrayList(T).init(allocator);
    var current = end;

    while (current != start) {
        try path.append(current);
        current = parent.get(current).?;
    }
    try path.append(start);

    std.mem.reverse(T, path.items);
    return path;
}

test "BFS traverse" {
    const allocator = std.testing.allocator;
    var graph = Graph(i32).init(allocator, false);
    defer graph.deinit();

    try graph.addEdge(1, 2);
    try graph.addEdge(1, 3);
    try graph.addEdge(2, 4);

    var result = try traverse(i32, &graph, 1, allocator);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 4), result.items.len);
    try std.testing.expectEqual(@as(i32, 1), result.items[0]);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const Graph = @import("graph.zig").Graph;

/// Depth-first search algorithms.
/// Time: O(V + E)
/// Space: O(V)

pub fn traverse(comptime T: type, graph: *const Graph(T), start: T, allocator: Allocator) !std.ArrayList(T) {
    var result = std.ArrayList(T).init(allocator);
    errdefer result.deinit();

    if (!graph.hasVertex(start)) return result;

    var visited = std.AutoHashMap(T, void).init(allocator);
    defer visited.deinit();

    var stack = std.ArrayList(T).init(allocator);
    defer stack.deinit();

    try stack.append(start);

    while (stack.items.len > 0) {
        const vertex = stack.pop();

        if (visited.contains(vertex)) continue;

        try visited.put(vertex, {});
        try result.append(vertex);

        if (graph.neighbors(vertex)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |neighbor| {
                if (!visited.contains(neighbor.*)) {
                    try stack.append(neighbor.*);
                }
            }
        }
    }

    return result;
}

pub fn findPath(comptime T: type, graph: *const Graph(T), start: T, end: T, allocator: Allocator) !?std.ArrayList(T) {
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

    var stack = std.ArrayList(T).init(allocator);
    defer stack.deinit();

    try stack.append(start);

    while (stack.items.len > 0) {
        const vertex = stack.pop();

        if (visited.contains(vertex)) continue;
        try visited.put(vertex, {});

        if (vertex == end) {
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

        if (graph.neighbors(vertex)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |neighbor| {
                if (!visited.contains(neighbor.*)) {
                    try parent.put(neighbor.*, vertex);
                    try stack.append(neighbor.*);
                }
            }
        }
    }

    return null;
}

pub fn hasCycle(comptime T: type, graph: *const Graph(T), allocator: Allocator) !bool {
    var visited = std.AutoHashMap(T, void).init(allocator);
    defer visited.deinit();

    var rec_stack = std.AutoHashMap(T, void).init(allocator);
    defer rec_stack.deinit();

    var it = graph.adjacency_list.keyIterator();
    while (it.next()) |vertex| {
        if (!visited.contains(vertex.*)) {
            if (try hasCycleUtil(T, graph, vertex.*, &visited, &rec_stack)) {
                return true;
            }
        }
    }

    return false;
}

fn hasCycleUtil(comptime T: type, graph: *const Graph(T), vertex: T, visited: *std.AutoHashMap(T, void), rec_stack: *std.AutoHashMap(T, void)) !bool {
    try visited.put(vertex, {});
    try rec_stack.put(vertex, {});

    if (graph.neighbors(vertex)) |neighbors| {
        var it = neighbors.keyIterator();
        while (it.next()) |neighbor| {
            if (!visited.contains(neighbor.*)) {
                if (try hasCycleUtil(T, graph, neighbor.*, visited, rec_stack)) {
                    return true;
                }
            } else if (rec_stack.contains(neighbor.*)) {
                return true;
            }
        }
    }

    _ = rec_stack.remove(vertex);
    return false;
}

test "DFS traverse" {
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

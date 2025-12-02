const std = @import("std");
const Allocator = std.mem.Allocator;

/// Disjoint set (Union-Find) with path compression and union by rank.
/// Time: O(α(n)) amortized for all operations
/// Space: O(n)
pub fn DisjointSet(comptime T: type) type {
    return struct {
        const Self = @This();

        parent: std.AutoHashMap(T, T),
        rank: std.AutoHashMap(T, usize),
        allocator: Allocator,

        pub fn init(allocator: Allocator) Self {
            return .{
                .parent = std.AutoHashMap(T, T).init(allocator),
                .rank = std.AutoHashMap(T, usize).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.parent.deinit();
            self.rank.deinit();
        }

        pub fn makeSet(self: *Self, x: T) !void {
            if (!self.parent.contains(x)) {
                try self.parent.put(x, x);
                try self.rank.put(x, 0);
            }
        }

        pub fn find(self: *Self, x: T) ?T {
            const p = self.parent.get(x) orelse return null;
            if (p != x) {
                const root = self.find(p) orelse return null;
                self.parent.put(x, root) catch {};
                return root;
            }
            return x;
        }

        pub fn unionSets(self: *Self, x: T, y: T) bool {
            const root_x = self.find(x) orelse return false;
            const root_y = self.find(y) orelse return false;

            if (root_x == root_y) return true;

            const rank_x = self.rank.get(root_x) orelse 0;
            const rank_y = self.rank.get(root_y) orelse 0;

            if (rank_x < rank_y) {
                self.parent.put(root_x, root_y) catch {};
            } else if (rank_x > rank_y) {
                self.parent.put(root_y, root_x) catch {};
            } else {
                self.parent.put(root_y, root_x) catch {};
                self.rank.put(root_x, rank_x + 1) catch {};
            }

            return true;
        }

        pub fn connected(self: *Self, x: T, y: T) bool {
            const root_x = self.find(x) orelse return false;
            const root_y = self.find(y) orelse return false;
            return root_x == root_y;
        }

        pub fn contains(self: *const Self, x: T) bool {
            return self.parent.contains(x);
        }

        pub fn setCount(self: *Self) usize {
            var count: usize = 0;
            var it = self.parent.iterator();
            while (it.next()) |entry| {
                if (entry.key_ptr.* == entry.value_ptr.*) {
                    count += 1;
                }
            }
            return count;
        }

        pub fn size(self: *const Self) usize {
            return self.parent.count();
        }
    };
}

test "DisjointSet operations" {
    const allocator = std.testing.allocator;
    var ds = DisjointSet(i32).init(allocator);
    defer ds.deinit();

    try ds.makeSet(1);
    try ds.makeSet(2);
    try ds.makeSet(3);

    try std.testing.expect(!ds.connected(1, 2));
    _ = ds.unionSets(1, 2);
    try std.testing.expect(ds.connected(1, 2));
    try std.testing.expect(!ds.connected(1, 3));
}

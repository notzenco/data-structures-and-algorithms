const std = @import("std");
const Allocator = std.mem.Allocator;

/// Hash table using std.HashMap.
/// Time: O(1) average for all operations
/// Space: O(n)
pub fn HashTable(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();
        const Map = std.AutoHashMap(K, V);

        map: Map,

        pub fn init(allocator: Allocator) Self {
            return .{
                .map = Map.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.map.deinit();
        }

        pub fn put(self: *Self, key: K, value: V) !void {
            try self.map.put(key, value);
        }

        pub fn get(self: *const Self, key: K) ?V {
            return self.map.get(key);
        }

        pub fn remove(self: *Self, key: K) ?V {
            const kv = self.map.fetchRemove(key);
            return if (kv) |entry| entry.value else null;
        }

        pub fn contains(self: *const Self, key: K) bool {
            return self.map.contains(key);
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.map.count() == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.map.count();
        }

        pub fn clear(self: *Self) void {
            self.map.clearRetainingCapacity();
        }

        pub fn iterator(self: *const Self) Map.Iterator {
            return self.map.iterator();
        }
    };
}

test "HashTable operations" {
    const allocator = std.testing.allocator;
    var table = HashTable(i32, i32).init(allocator);
    defer table.deinit();

    try table.put(1, 10);
    try table.put(2, 20);
    try table.put(3, 30);

    try std.testing.expectEqual(@as(?i32, 10), table.get(1));
    try std.testing.expectEqual(@as(?i32, 20), table.get(2));
    try std.testing.expectEqual(@as(?i32, null), table.get(4));
}

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Resizable array with automatic capacity management.
/// Time: O(1) amortized for push, O(n) for insert/remove
/// Space: O(n)
pub fn DynamicArray(comptime T: type) type {
    return struct {
        const Self = @This();

        items: std.ArrayList(T),

        pub fn init(allocator: Allocator) Self {
            return .{
                .items = std.ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit();
        }

        pub fn push(self: *Self, value: T) !void {
            try self.items.append(value);
        }

        pub fn pop(self: *Self) ?T {
            return self.items.popOrNull();
        }

        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.items.items.len) return null;
            return self.items.items[index];
        }

        pub fn set(self: *Self, index: usize, value: T) bool {
            if (index >= self.items.items.len) return false;
            self.items.items[index] = value;
            return true;
        }

        pub fn insert(self: *Self, index: usize, value: T) !bool {
            if (index > self.items.items.len) return false;
            try self.items.insert(index, value);
            return true;
        }

        pub fn removeAt(self: *Self, index: usize) ?T {
            if (index >= self.items.items.len) return null;
            return self.items.orderedRemove(index);
        }

        pub fn indexOf(self: *const Self, value: T) ?usize {
            for (self.items.items, 0..) |item, i| {
                if (item == value) return i;
            }
            return null;
        }

        pub fn contains(self: *const Self, value: T) bool {
            return self.indexOf(value) != null;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.items.items.len == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.items.items.len;
        }

        pub fn clear(self: *Self) void {
            self.items.clearRetainingCapacity();
        }

        pub fn toSlice(self: *const Self) []const T {
            return self.items.items;
        }
    };
}

test "DynamicArray operations" {
    const allocator = std.testing.allocator;
    var arr = DynamicArray(i32).init(allocator);
    defer arr.deinit();

    try arr.push(1);
    try arr.push(2);
    try arr.push(3);

    try std.testing.expectEqual(@as(?i32, 2), arr.get(1));
    try std.testing.expect(arr.set(1, 42));
    try std.testing.expectEqual(@as(?i32, 42), arr.get(1));
}

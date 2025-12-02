const std = @import("std");
const Allocator = std.mem.Allocator;

/// FIFO queue implementation using a ring buffer.
/// Time: O(1) amortized for all operations
/// Space: O(n)
pub fn Queue(comptime T: type) type {
    return struct {
        const Self = @This();

        items: std.ArrayList(T),
        head: usize,

        pub fn init(allocator: Allocator) Self {
            return .{
                .items = std.ArrayList(T).init(allocator),
                .head = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit();
        }

        pub fn enqueue(self: *Self, value: T) !void {
            try self.items.append(value);
        }

        pub fn dequeue(self: *Self) ?T {
            if (self.head >= self.items.items.len) return null;
            const value = self.items.items[self.head];
            self.head += 1;
            // Compact when head reaches half the capacity
            if (self.head > self.items.items.len / 2 and self.head > 16) {
                const remaining = self.items.items.len - self.head;
                std.mem.copyForwards(T, self.items.items[0..remaining], self.items.items[self.head..]);
                self.items.shrinkRetainingCapacity(remaining);
                self.head = 0;
            }
            return value;
        }

        pub fn peek(self: *const Self) ?T {
            if (self.head >= self.items.items.len) return null;
            return self.items.items[self.head];
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.head >= self.items.items.len;
        }

        pub fn size(self: *const Self) usize {
            return self.items.items.len - self.head;
        }

        pub fn clear(self: *Self) void {
            self.items.clearRetainingCapacity();
            self.head = 0;
        }
    };
}

test "Queue operations" {
    const allocator = std.testing.allocator;
    var queue = Queue(i32).init(allocator);
    defer queue.deinit();

    try queue.enqueue(1);
    try queue.enqueue(2);
    try queue.enqueue(3);

    try std.testing.expectEqual(@as(?i32, 1), queue.dequeue());
    try std.testing.expectEqual(@as(?i32, 2), queue.dequeue());
    try std.testing.expectEqual(@as(?i32, 3), queue.dequeue());
    try std.testing.expectEqual(@as(?i32, null), queue.dequeue());
}

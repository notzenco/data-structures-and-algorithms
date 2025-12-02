const std = @import("std");
const Allocator = std.mem.Allocator;

/// LIFO stack implementation.
/// Time: O(1) for all operations
/// Space: O(n)
pub fn Stack(comptime T: type) type {
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

        pub fn peek(self: *const Self) ?T {
            if (self.items.items.len == 0) return null;
            return self.items.items[self.items.items.len - 1];
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
    };
}

test "Stack operations" {
    const allocator = std.testing.allocator;
    var stack = Stack(i32).init(allocator);
    defer stack.deinit();

    try stack.push(1);
    try stack.push(2);
    try stack.push(3);

    try std.testing.expectEqual(@as(?i32, 3), stack.pop());
    try std.testing.expectEqual(@as(?i32, 2), stack.pop());
    try std.testing.expectEqual(@as(?i32, 1), stack.pop());
    try std.testing.expectEqual(@as(?i32, null), stack.pop());
}

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Double-ended queue supporting operations at both ends.
/// Time: O(1) for all operations
/// Space: O(n)
pub fn Deque(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            value: T,
            prev: ?*Node,
            next: ?*Node,
        };

        allocator: Allocator,
        head: ?*Node,
        tail: ?*Node,
        count: usize,

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .head = null,
                .tail = null,
                .count = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            var current = self.head;
            while (current) |node| {
                const next = node.next;
                self.allocator.destroy(node);
                current = next;
            }
        }

        pub fn pushFront(self: *Self, value: T) !void {
            const node = try self.allocator.create(Node);
            node.* = .{ .value = value, .prev = null, .next = self.head };

            if (self.head) |head| {
                head.prev = node;
            } else {
                self.tail = node;
            }
            self.head = node;
            self.count += 1;
        }

        pub fn pushBack(self: *Self, value: T) !void {
            const node = try self.allocator.create(Node);
            node.* = .{ .value = value, .prev = self.tail, .next = null };

            if (self.tail) |tail| {
                tail.next = node;
            } else {
                self.head = node;
            }
            self.tail = node;
            self.count += 1;
        }

        pub fn popFront(self: *Self) ?T {
            const head = self.head orelse return null;
            const value = head.value;

            self.head = head.next;
            if (self.head) |new_head| {
                new_head.prev = null;
            } else {
                self.tail = null;
            }

            self.allocator.destroy(head);
            self.count -= 1;
            return value;
        }

        pub fn popBack(self: *Self) ?T {
            const tail = self.tail orelse return null;
            const value = tail.value;

            self.tail = tail.prev;
            if (self.tail) |new_tail| {
                new_tail.next = null;
            } else {
                self.head = null;
            }

            self.allocator.destroy(tail);
            self.count -= 1;
            return value;
        }

        pub fn peekFront(self: *const Self) ?T {
            return if (self.head) |head| head.value else null;
        }

        pub fn peekBack(self: *const Self) ?T {
            return if (self.tail) |tail| tail.value else null;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.count == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.count;
        }
    };
}

test "Deque operations" {
    const allocator = std.testing.allocator;
    var deque = Deque(i32).init(allocator);
    defer deque.deinit();

    try deque.pushFront(2);
    try deque.pushFront(1);
    try deque.pushBack(3);

    try std.testing.expectEqual(@as(?i32, 1), deque.peekFront());
    try std.testing.expectEqual(@as(?i32, 3), deque.peekBack());
}

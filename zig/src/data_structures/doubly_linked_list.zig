const std = @import("std");
const Allocator = std.mem.Allocator;

/// Doubly linked list with bidirectional traversal.
/// Time: O(1) for prepend/append/removeFirst/removeLast
/// Space: O(n)
pub fn DoublyLinkedList(comptime T: type) type {
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

        pub fn prepend(self: *Self, value: T) !void {
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

        pub fn append(self: *Self, value: T) !void {
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

        pub fn removeFirst(self: *Self) ?T {
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

        pub fn removeLast(self: *Self) ?T {
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

        pub fn first(self: *const Self) ?T {
            return if (self.head) |head| head.value else null;
        }

        pub fn last(self: *const Self) ?T {
            return if (self.tail) |tail| tail.value else null;
        }

        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.count) return null;

            var current: ?*Node = undefined;
            if (index < self.count / 2) {
                current = self.head;
                var i: usize = 0;
                while (i < index) : (i += 1) {
                    current = current.?.next;
                }
            } else {
                current = self.tail;
                var i: usize = self.count - 1;
                while (i > index) : (i -= 1) {
                    current = current.?.prev;
                }
            }
            return current.?.value;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.count == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.count;
        }
    };
}

test "DoublyLinkedList operations" {
    const allocator = std.testing.allocator;
    var list = DoublyLinkedList(i32).init(allocator);
    defer list.deinit();

    try list.prepend(2);
    try list.prepend(1);
    try list.append(3);

    try std.testing.expectEqual(@as(?i32, 1), list.first());
    try std.testing.expectEqual(@as(?i32, 3), list.last());
    try std.testing.expectEqual(@as(usize, 3), list.size());
}

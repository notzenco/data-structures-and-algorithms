const std = @import("std");
const Allocator = std.mem.Allocator;

/// Singly linked list with forward traversal.
/// Time: O(1) for prepend, O(n) for other operations
/// Space: O(n)
pub fn SinglyLinkedList(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            value: T,
            next: ?*Node,
        };

        allocator: Allocator,
        head: ?*Node,
        count: usize,

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .head = null,
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
            node.* = .{ .value = value, .next = self.head };
            self.head = node;
            self.count += 1;
        }

        pub fn append(self: *Self, value: T) !void {
            const node = try self.allocator.create(Node);
            node.* = .{ .value = value, .next = null };

            if (self.head == null) {
                self.head = node;
            } else {
                var current = self.head;
                while (current.?.next) |next| {
                    current = next;
                }
                current.?.next = node;
            }
            self.count += 1;
        }

        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.count) return null;
            var current = self.head;
            var i: usize = 0;
            while (current) |node| : (i += 1) {
                if (i == index) return node.value;
                current = node.next;
            }
            return null;
        }

        pub fn removeAt(self: *Self, index: usize) ?T {
            if (index >= self.count) return null;

            if (index == 0) {
                const node = self.head.?;
                const value = node.value;
                self.head = node.next;
                self.allocator.destroy(node);
                self.count -= 1;
                return value;
            }

            var current = self.head;
            var i: usize = 0;
            while (current) |node| : (i += 1) {
                if (i == index - 1) {
                    const to_remove = node.next.?;
                    const value = to_remove.value;
                    node.next = to_remove.next;
                    self.allocator.destroy(to_remove);
                    self.count -= 1;
                    return value;
                }
                current = node.next;
            }
            return null;
        }

        pub fn indexOf(self: *const Self, value: T) ?usize {
            var current = self.head;
            var i: usize = 0;
            while (current) |node| : (i += 1) {
                if (node.value == value) return i;
                current = node.next;
            }
            return null;
        }

        pub fn contains(self: *const Self, value: T) bool {
            return self.indexOf(value) != null;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.count == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.count;
        }
    };
}

test "SinglyLinkedList operations" {
    const allocator = std.testing.allocator;
    var list = SinglyLinkedList(i32).init(allocator);
    defer list.deinit();

    try list.prepend(2);
    try list.prepend(1);
    try list.append(3);

    try std.testing.expectEqual(@as(?i32, 1), list.get(0));
    try std.testing.expectEqual(@as(?i32, 2), list.get(1));
    try std.testing.expectEqual(@as(?i32, 3), list.get(2));
}

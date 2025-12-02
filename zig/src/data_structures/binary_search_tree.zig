const std = @import("std");
const Allocator = std.mem.Allocator;

/// Binary search tree with standard operations.
/// Time: O(log n) average, O(n) worst case
/// Space: O(n)
pub fn BinarySearchTree(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            value: T,
            left: ?*Node,
            right: ?*Node,
        };

        allocator: Allocator,
        root: ?*Node,
        count: usize,

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .root = null,
                .count = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.freeNode(self.root);
        }

        fn freeNode(self: *Self, node: ?*Node) void {
            if (node) |n| {
                self.freeNode(n.left);
                self.freeNode(n.right);
                self.allocator.destroy(n);
            }
        }

        pub fn insert(self: *Self, value: T) !void {
            self.root = try self.insertNode(self.root, value);
        }

        fn insertNode(self: *Self, node: ?*Node, value: T) !?*Node {
            if (node == null) {
                const new_node = try self.allocator.create(Node);
                new_node.* = .{ .value = value, .left = null, .right = null };
                self.count += 1;
                return new_node;
            }

            const n = node.?;
            if (value < n.value) {
                n.left = try self.insertNode(n.left, value);
            } else if (value > n.value) {
                n.right = try self.insertNode(n.right, value);
            } else {
                n.value = value;
            }
            return n;
        }

        pub fn contains(self: *const Self, value: T) bool {
            return self.findNode(self.root, value) != null;
        }

        fn findNode(self: *const Self, node: ?*Node, value: T) ?*Node {
            _ = self;
            if (node == null) return null;
            const n = node.?;
            if (value < n.value) return self.findNode(n.left, value);
            if (value > n.value) return self.findNode(n.right, value);
            return n;
        }

        pub fn findMin(self: *const Self) ?T {
            if (self.root == null) return null;
            var current = self.root;
            while (current.?.left) |left| {
                current = left;
            }
            return current.?.value;
        }

        pub fn findMax(self: *const Self) ?T {
            if (self.root == null) return null;
            var current = self.root;
            while (current.?.right) |right| {
                current = right;
            }
            return current.?.value;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.count == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.count;
        }

        pub fn inorder(self: *const Self, allocator: Allocator) !std.ArrayList(T) {
            var result = std.ArrayList(T).init(allocator);
            try self.inorderTraversal(self.root, &result);
            return result;
        }

        fn inorderTraversal(self: *const Self, node: ?*Node, result: *std.ArrayList(T)) !void {
            _ = self;
            if (node) |n| {
                try self.inorderTraversal(n.left, result);
                try result.append(n.value);
                try self.inorderTraversal(n.right, result);
            }
        }
    };
}

test "BinarySearchTree operations" {
    const allocator = std.testing.allocator;
    var bst = BinarySearchTree(i32).init(allocator);
    defer bst.deinit();

    try bst.insert(5);
    try bst.insert(3);
    try bst.insert(7);
    try bst.insert(1);
    try bst.insert(9);

    try std.testing.expect(bst.contains(5));
    try std.testing.expect(bst.contains(3));
    try std.testing.expect(!bst.contains(10));

    try std.testing.expectEqual(@as(?i32, 1), bst.findMin());
    try std.testing.expectEqual(@as(?i32, 9), bst.findMax());
}

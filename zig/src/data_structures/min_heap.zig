const std = @import("std");
const Allocator = std.mem.Allocator;

/// Min-heap implementation.
/// Time: O(log n) for insert/extract, O(1) for peek
/// Space: O(n)
pub fn MinHeap(comptime T: type) type {
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

        pub fn insert(self: *Self, value: T) !void {
            try self.items.append(value);
            self.siftUp(self.items.items.len - 1);
        }

        pub fn extractMin(self: *Self) ?T {
            if (self.items.items.len == 0) return null;

            const min = self.items.items[0];
            const last = self.items.pop();

            if (self.items.items.len > 0) {
                self.items.items[0] = last;
                self.siftDown(0);
            }

            return min;
        }

        pub fn peek(self: *const Self) ?T {
            if (self.items.items.len == 0) return null;
            return self.items.items[0];
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.items.items.len == 0;
        }

        pub fn size(self: *const Self) usize {
            return self.items.items.len;
        }

        fn siftUp(self: *Self, index: usize) void {
            var i = index;
            while (i > 0) {
                const parent = (i - 1) / 2;
                if (self.items.items[i] < self.items.items[parent]) {
                    const tmp = self.items.items[i];
                    self.items.items[i] = self.items.items[parent];
                    self.items.items[parent] = tmp;
                    i = parent;
                } else {
                    break;
                }
            }
        }

        fn siftDown(self: *Self, index: usize) void {
            var i = index;
            const len = self.items.items.len;

            while (true) {
                const left = 2 * i + 1;
                const right = 2 * i + 2;
                var smallest = i;

                if (left < len and self.items.items[left] < self.items.items[smallest]) {
                    smallest = left;
                }
                if (right < len and self.items.items[right] < self.items.items[smallest]) {
                    smallest = right;
                }

                if (smallest != i) {
                    const tmp = self.items.items[i];
                    self.items.items[i] = self.items.items[smallest];
                    self.items.items[smallest] = tmp;
                    i = smallest;
                } else {
                    break;
                }
            }
        }
    };
}

test "MinHeap operations" {
    const allocator = std.testing.allocator;
    var heap = MinHeap(i32).init(allocator);
    defer heap.deinit();

    try heap.insert(5);
    try heap.insert(3);
    try heap.insert(7);
    try heap.insert(1);
    try heap.insert(9);

    try std.testing.expectEqual(@as(?i32, 1), heap.extractMin());
    try std.testing.expectEqual(@as(?i32, 3), heap.extractMin());
    try std.testing.expectEqual(@as(?i32, 5), heap.extractMin());
}

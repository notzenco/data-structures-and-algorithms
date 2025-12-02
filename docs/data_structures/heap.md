# Binary Min Heap

## Overview

A binary heap is a complete binary tree that satisfies the **heap property**. In a min-heap, every parent is smaller than or equal to its children, making the minimum element always at the root. Heaps are the standard implementation for **priority queues**.

## Intuition

```
Min-Heap:
              1
            /   \
           3     2
          / \   /
         7   4 5

Heap Property: Every parent ≤ children
- 1 ≤ 3 and 1 ≤ 2 ✓
- 3 ≤ 7 and 3 ≤ 4 ✓
- 2 ≤ 5 ✓

Complete Tree: All levels full except possibly last,
               which is filled left-to-right

NOT a heap (violates property):
              1
            /   \
           5     2    ← 5 > 3 (its child)
          / \
         3   4
```

**Key insight**: We only care about parent-child relationships, not left-right sibling order. This weaker constraint (vs. BST) allows O(log n) insert/delete while maintaining O(1) min access.

## How It Works

### Array Representation

A complete binary tree can be stored in an array without pointers:

```
Tree:           1
              /   \
             3     2
            / \   /
           7   4 5

Array: [1, 3, 2, 7, 4, 5]
Index:  0  1  2  3  4  5

Navigation formulas (0-indexed):
- Parent of i: (i - 1) / 2
- Left child of i: 2i + 1
- Right child of i: 2i + 2

Example:
- Node 3 at index 1
- Its parent: (1-1)/2 = 0 (which is 1) ✓
- Its left child: 2(1)+1 = 3 (which is 7) ✓
- Its right child: 2(1)+2 = 4 (which is 4) ✓
```

### Insert (Push)

1. Add element at the end (maintains complete tree)
2. "Bubble up" (sift up) to restore heap property

```
insert(heap, value):
    heap.append(value)
    bubble_up(heap, len(heap) - 1)

bubble_up(heap, index):
    while index > 0:
        parent = (index - 1) / 2
        if heap[parent] <= heap[index]:
            break  // Heap property satisfied
        swap(heap[parent], heap[index])
        index = parent
```

```
Insert 0 into [1, 3, 2, 7, 4, 5]:

Step 1: Append
[1, 3, 2, 7, 4, 5, 0]
              1
            /   \
           3     2
          / \   / \
         7   4 5   0  ← Added

Step 2: Bubble up (0 < 2, swap)
[1, 3, 0, 7, 4, 5, 2]
              1
            /   \
           3     0  ← Moved up
          / \   / \
         7   4 5   2

Step 3: Bubble up (0 < 1, swap)
[0, 3, 1, 7, 4, 5, 2]
              0  ← Now at root
            /   \
           3     1
          / \   / \
         7   4 5   2
```

### Extract Min (Pop)

1. Return root (minimum element)
2. Move last element to root
3. "Bubble down" (sift down) to restore heap property

```
extract_min(heap):
    if heap is empty:
        error
    min_val = heap[0]
    heap[0] = heap[len(heap) - 1]
    heap.pop()  // Remove last
    bubble_down(heap, 0)
    return min_val

bubble_down(heap, index):
    while true:
        smallest = index
        left = 2 * index + 1
        right = 2 * index + 2

        if left < len(heap) and heap[left] < heap[smallest]:
            smallest = left
        if right < len(heap) and heap[right] < heap[smallest]:
            smallest = right

        if smallest == index:
            break  // Heap property restored
        swap(heap[index], heap[smallest])
        index = smallest
```

```
Extract min from [0, 3, 1, 7, 4, 5, 2]:

Step 1: Save root (0), move last to root
[2, 3, 1, 7, 4, 5]
              2  ← Moved from last
            /   \
           3     1
          / \   /
         7   4 5

Step 2: Bubble down (2 > 1, swap with smaller child)
[1, 3, 2, 7, 4, 5]
              1  ← Heap restored
            /   \
           3     2
          / \   /
         7   4 5

Return 0
```

### Peek

```
peek(heap):
    if heap is empty:
        error
    return heap[0]  // O(1)
```

### Heapify (Build Heap)

Build a heap from an unsorted array in O(n) time:

```
heapify(array):
    // Start from last non-leaf and bubble down each
    for i from (len(array) / 2 - 1) down to 0:
        bubble_down(array, i)
```

## Mathematical Analysis

### Why O(log n) for Insert/Extract?

The heap is a complete binary tree:
- Height h = ⌊log₂(n)⌋
- Bubble up/down traverses at most h levels
- Each level: O(1) comparison and swap
- Total: O(log n)

### Why O(n) for Heapify?

Intuition: Most nodes are near the bottom and bubble down only a few levels.

**Detailed analysis:**
- Level k has ≤ 2^k nodes
- Each node at level k bubbles down at most (h - k) levels
- Total work: Σ(k=0 to h) 2^k × (h - k)

Let j = h - k:
= Σ(j=0 to h) 2^(h-j) × j
= 2^h × Σ(j=0 to h) j/2^j
≤ 2^h × 2  (sum converges to 2)
= O(2^h) = O(n)

### Heap Sort Analysis

```
heap_sort(array):
    heapify(array)           // O(n)
    for i from n-1 down to 1:
        swap(array[0], array[i])
        bubble_down(array[0..i-1], 0)  // O(log i)
```

Total: O(n) + Σ(i=1 to n) O(log i) = O(n) + O(n log n) = **O(n log n)**

## Time Complexity

| Operation | Time |
|-----------|------|
| peek (get min) | O(1) |
| insert | O(log n) |
| extract_min | O(log n) |
| heapify | O(n) |
| search | O(n) |
| delete arbitrary | O(n) |

## Space Complexity

- **Storage**: O(n)
- **Array representation**: No pointer overhead
- **Operations**: O(1) extra space (or O(log n) for recursive implementations)

## Use Cases

### Classic Applications

1. **Priority Queue**
   ```
   Task scheduling with priorities:
   - Insert task with priority
   - Always process highest priority (lowest value in min-heap)
   - O(log n) per operation
   ```

2. **Dijkstra's Algorithm**
   ```
   Shortest paths in weighted graphs:
   - Heap stores (distance, vertex) pairs
   - Extract minimum distance vertex
   - Update neighbors, decrease keys
   ```

3. **Heap Sort**
   - In-place O(n log n) sorting
   - Not stable, but guaranteed performance

4. **K Largest/Smallest Elements**
   ```
   Find k smallest in n elements:
   - Build min-heap: O(n)
   - Extract k times: O(k log n)
   - Total: O(n + k log n)

   Alternative with max-heap of size k:
   - For each element: O(log k)
   - Total: O(n log k)
   ```

5. **Median Maintenance**
   ```
   Two heaps:
   - Max-heap for smaller half
   - Min-heap for larger half
   - Median is at one of the roots
   - O(log n) per insertion
   ```

6. **Merge K Sorted Lists**
   ```
   - Heap of size k with one element from each list
   - Extract min, insert next from that list
   - O(n log k) total
   ```

### Real-World Examples

- **Operating systems**: Process scheduling
- **Event-driven simulation**: Next event processing
- **Bandwidth management**: Packet scheduling
- **Huffman coding**: Build optimal prefix codes

## Trade-offs & Comparisons

### Min-Heap vs. Max-Heap

| Need | Use |
|------|-----|
| Smallest first | Min-heap |
| Largest first | Max-heap |
| Convert between | Negate all values, or swap comparisons |

### Heap vs. BST

| Aspect | Heap | BST |
|--------|------|-----|
| Find min/max | O(1) | O(log n)* |
| Insert | O(log n) | O(log n)* |
| Delete min/max | O(log n) | O(log n)* |
| Search | O(n) | O(log n)* |
| k-th element | O(k log n) | O(k)† |
| Memory | Array (compact) | Pointers |
| Implementation | Simple | Moderate |

*Balanced BST; †With augmentation

### Heap vs. Sorted Array

| Aspect | Heap | Sorted Array |
|--------|------|--------------|
| Find min | O(1) | O(1) |
| Insert | O(log n) | O(n) |
| Delete min | O(log n) | O(1) or O(n) |
| Build | O(n) | O(n log n) |

## Common Variations

1. **Max-Heap**
   - Parent ≥ children
   - Maximum at root

2. **D-ary Heap**
   - Each node has d children (not just 2)
   - Shallower tree, but more comparisons per level
   - Decrease-key faster (good for Dijkstra)

3. **Fibonacci Heap**
   - O(1) amortized insert and decrease-key
   - O(log n) amortized extract-min
   - Complex implementation, good for dense graphs

4. **Binomial Heap**
   - O(log n) merge
   - Collection of binomial trees

5. **Pairing Heap**
   - Simpler than Fibonacci, good practical performance

## Common Interview Problems

1. **Kth Largest Element**: Use min-heap of size k
2. **Merge K Sorted Lists**: Heap of k elements
3. **Find Median from Data Stream**: Two heaps
4. **Top K Frequent Elements**: Heap + hash map
5. **Task Scheduler**: Priority queue simulation
6. **Sliding Window Maximum**: Monotonic deque or heap

### Find Kth Largest

```
def kth_largest(nums, k):
    # Min-heap of size k
    heap = []
    for num in nums:
        if len(heap) < k:
            heap_push(heap, num)
        elif num > heap[0]:
            heap_pop(heap)
            heap_push(heap, num)
    return heap[0]  # k-th largest
```

## Implementation Tips

### Index Formulas

For 0-indexed array:
- Parent: `(i - 1) // 2`
- Left child: `2 * i + 1`
- Right child: `2 * i + 2`

For 1-indexed array (some prefer):
- Parent: `i // 2`
- Left child: `2 * i`
- Right child: `2 * i + 1`

### Decrease Key (Update Priority)

```
decrease_key(heap, index, new_value):
    if new_value > heap[index]:
        error "New value must be smaller"
    heap[index] = new_value
    bubble_up(heap, index)
```

For efficient decrease-key, need to track element positions (use hash map).

## Further Reading

- CLRS Chapter 6 (Heapsort) and 19 (Fibonacci Heaps)
- [Binary Heap - Wikipedia](https://en.wikipedia.org/wiki/Binary_heap)
- [Visualgo - Heap](https://visualgo.net/en/heap)
- [Priority Queue - Wikipedia](https://en.wikipedia.org/wiki/Priority_queue)

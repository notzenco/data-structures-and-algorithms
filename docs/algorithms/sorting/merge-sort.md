# Merge Sort

## Overview

Merge sort is a divide-and-conquer algorithm that recursively splits an array in half, sorts each half, and then merges the sorted halves. It guarantees O(n log n) time complexity in all cases and is the algorithm of choice when stability and predictable performance matter.

## Intuition

```
Sort [38, 27, 43, 3, 9, 82, 10]:

DIVIDE phase (split until single elements):
                [38, 27, 43, 3, 9, 82, 10]
                      /              \
            [38, 27, 43, 3]      [9, 82, 10]
              /        \           /      \
        [38, 27]    [43, 3]    [9, 82]   [10]
         /    \      /    \     /    \      |
       [38]  [27]  [43]  [3]  [9]  [82]   [10]

CONQUER phase (merge sorted subarrays):
       [38]  [27]  [43]  [3]  [9]  [82]   [10]
         \    /      \    /     \    /      |
        [27, 38]    [3, 43]    [9, 82]   [10]
              \        /           \      /
          [3, 27, 38, 43]        [9, 10, 82]
                      \              /
              [3, 9, 10, 27, 38, 43, 82]
```

The key insight: Merging two sorted arrays is O(n), and we do O(log n) levels of merging.

## How It Works

### Top-Down (Recursive) Implementation

```
merge_sort(array):
    if len(array) <= 1:
        return array

    mid = len(array) // 2
    left = merge_sort(array[0:mid])
    right = merge_sort(array[mid:])

    return merge(left, right)

merge(left, right):
    result = []
    i = j = 0

    while i < len(left) and j < len(right):
        if left[i] <= right[j]:  // <= for stability
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    // Append remaining elements
    result.extend(left[i:])
    result.extend(right[j:])
    return result
```

### Bottom-Up (Iterative) Implementation

```
merge_sort_bottom_up(array):
    n = len(array)
    width = 1

    while width < n:
        for i from 0 to n by 2*width:
            left = i
            mid = min(i + width, n)
            right = min(i + 2*width, n)
            merge_in_place(array, left, mid, right)
        width *= 2
```

```
Bottom-up on [38, 27, 43, 3, 9, 82, 10]:

width=1: Merge pairs of 1
  [38,27] → [27,38]
  [43,3]  → [3,43]
  [9,82]  → [9,82]
  [10]    → [10]
  Result: [27, 38, 3, 43, 9, 82, 10]

width=2: Merge pairs of 2
  [27,38,3,43] → [3,27,38,43]
  [9,82,10]    → [9,10,82]
  Result: [3, 27, 38, 43, 9, 10, 82]

width=4: Merge pairs of 4
  [3,27,38,43,9,10,82] → [3,9,10,27,38,43,82]

Done!
```

### The Merge Operation (Detailed)

```
Merge [3, 27, 38, 43] and [9, 10, 82]:

left:  [3, 27, 38, 43]    right: [9, 10, 82]
        ↑ i=0                     ↑ j=0

Step 1: 3 < 9, take 3
        [3]
        left: i=1, right: j=0

Step 2: 27 > 9, take 9
        [3, 9]
        left: i=1, right: j=1

Step 3: 27 > 10, take 10
        [3, 9, 10]
        left: i=1, right: j=2

Step 4: 27 < 82, take 27
        [3, 9, 10, 27]
        left: i=2, right: j=2

Step 5: 38 < 82, take 38
        [3, 9, 10, 27, 38]
        left: i=3, right: j=2

Step 6: 43 < 82, take 43
        [3, 9, 10, 27, 38, 43]
        left: i=4 (exhausted), right: j=2

Step 7: Append remaining from right
        [3, 9, 10, 27, 38, 43, 82]
```

## Mathematical Analysis

### Recurrence Relation

T(n) = 2T(n/2) + O(n)

Where:
- 2T(n/2): Two recursive calls on half-sized arrays
- O(n): Merge operation on n elements

### Master Theorem Solution

Using Master Theorem with a=2, b=2, f(n)=n:
- log_b(a) = log_2(2) = 1
- f(n) = n = O(n^1)
- Case 2: f(n) = Θ(n^(log_b(a))) = Θ(n)

**T(n) = O(n log n)**

### Detailed Proof (Recursion Tree)

```
Level 0:           n                    → n work
                 /   \
Level 1:       n/2   n/2                → n work
              / \     / \
Level 2:    n/4 n/4 n/4 n/4             → n work
              ...
Level k:    n/2^k nodes of size 2^(log n - k)  → n work
              ...
Level log n: n nodes of size 1          → n work

Total levels: log n + 1
Work per level: n
Total: O(n log n)
```

### Why Always O(n log n)?

Unlike quicksort, merge sort always divides exactly in half:
- Best case: O(n log n)
- Average case: O(n log n)
- Worst case: O(n log n)

No bad pivots, no degradation to O(n²).

## Time Complexity

| Case | Complexity | Condition |
|------|------------|-----------|
| Best | O(n log n) | Any input |
| Average | O(n log n) | Any input |
| Worst | O(n log n) | Any input |

## Space Complexity

| Implementation | Space |
|----------------|-------|
| Standard (top-down) | O(n) auxiliary |
| Bottom-up | O(n) auxiliary |
| In-place variants | O(1) but complex |
| Call stack (top-down) | O(log n) |

## Use Cases

### When Merge Sort Excels

1. **Guaranteed O(n log n) Required**
   - Safety-critical systems
   - Real-time constraints
   - Can't risk O(n²) worst case

2. **Stable Sorting Required**
   - Equal elements maintain relative order
   - Sorting by multiple keys
   - Database operations

3. **External Sorting (Large Files)**
   - Data doesn't fit in memory
   - Sequential access pattern
   - K-way merge on disk

4. **Linked List Sorting**
   - No random access penalty
   - O(1) extra space possible
   - Merge is pointer manipulation

5. **Parallel Sorting**
   - Independent subproblems
   - Excellent parallelization
   - Used in parallel frameworks

### Real-World Examples

- **Python's Timsort**: Hybrid with merge sort for large runs
- **Java's Arrays.sort()** for objects: Merge sort (stable)
- **External sorting**: Database systems, Unix `sort`
- **Inversion counting**: Modified merge sort
- **Git diff**: Patience diff uses merge concepts

### When NOT to Use

- Memory-constrained environments (needs O(n) space)
- Small arrays (overhead not worth it)
- Cache-sensitive applications (poor locality)
- Need in-place sorting (use heapsort)

## Trade-offs & Comparisons

### vs. Quick Sort

| Aspect | Merge Sort | Quick Sort |
|--------|------------|------------|
| Best case | O(n log n) | O(n log n) |
| Worst case | O(n log n) | O(n²) |
| Average case | O(n log n) | O(n log n) |
| Space | O(n) | O(log n) |
| Stable | Yes | No (typical) |
| Cache | Poor | Better |
| In practice | Slower | Faster (usually) |

### vs. Heap Sort

| Aspect | Merge Sort | Heap Sort |
|--------|------------|-----------|
| Time | O(n log n) | O(n log n) |
| Space | O(n) | O(1) |
| Stable | Yes | No |
| Cache | Poor | Poor |
| Practical speed | Better | Slower |

### vs. Insertion Sort

| Aspect | Merge Sort | Insertion Sort |
|--------|------------|----------------|
| Best case | O(n log n) | O(n) |
| Worst case | O(n log n) | O(n²) |
| Space | O(n) | O(1) |
| Small n | Overhead | Faster |
| Nearly sorted | Same | Much faster |

## Common Variations

### 1. Natural Merge Sort

Exploit existing sorted runs:
```
natural_merge_sort(array):
    // Find natural runs
    runs = find_sorted_runs(array)
    // Merge runs pairwise until one remains
    while len(runs) > 1:
        runs = merge_adjacent_runs(runs)
```

Best case O(n) when already sorted.

### 2. Timsort (Python, Java)

Hybrid algorithm:
1. Find natural runs (or create with insertion sort)
2. Minimum run length ~32-64
3. Merge runs with galloping optimization
4. Maintains merge stack invariants

**Time: O(n) best case, O(n log n) worst case**

### 3. K-Way Merge (External Sorting)

For sorting files larger than memory:
```
external_sort(file, memory_size):
    // Phase 1: Create sorted runs
    runs = []
    while not eof(file):
        chunk = read(file, memory_size)
        sort(chunk)  // In-memory sort
        runs.append(write_to_temp(chunk))

    // Phase 2: K-way merge
    k_way_merge(runs, output_file)
```

### 4. In-Place Merge Sort

Possible but complex, O(n log n) with O(1) space:
- Block-based algorithms
- Rotation-based merging
- Significant constant factor overhead

### 5. Parallel Merge Sort

```
parallel_merge_sort(array, depth=0):
    if len(array) <= threshold or depth > max_depth:
        return sequential_sort(array)

    mid = len(array) // 2
    // Parallel recursive calls
    fork:
        left = parallel_merge_sort(array[0:mid], depth+1)
        right = parallel_merge_sort(array[mid:], depth+1)
    join

    return parallel_merge(left, right)
```

## Common Interview Problems

1. **Sort an Array**: Basic merge sort implementation
2. **Sort Linked List**: O(1) space merge sort
3. **Count Inversions**: Modified merge to count
4. **Merge K Sorted Lists**: K-way merge with heap
5. **Merge Sorted Array**: In-place merge
6. **Count Smaller Numbers After Self**: Modified merge

### Counting Inversions

```
count_inversions(array):
    if len(array) <= 1:
        return array, 0

    mid = len(array) // 2
    left, left_inv = count_inversions(array[0:mid])
    right, right_inv = count_inversions(array[mid:])
    merged, split_inv = merge_count(left, right)

    return merged, left_inv + right_inv + split_inv

merge_count(left, right):
    result = []
    inversions = 0
    i = j = 0

    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            inversions += len(left) - i  // All remaining left elements
            j += 1

    result.extend(left[i:])
    result.extend(right[j:])
    return result, inversions
```

## Implementation Tips

### Stability

To maintain stability, use `<=` not `<` when comparing:
```python
if left[i] <= right[j]:  // Stable: equal elements from left come first
    result.append(left[i])
```

### Optimization Tricks

1. **Switch to insertion sort for small subarrays** (n < 16-32)
2. **Avoid copying**: Alternate between original and auxiliary array
3. **Check if already sorted**: Skip merge if `left[-1] <= right[0]`

### Common Pitfalls

1. **Not handling unequal halves**: Right half may be smaller
2. **Off-by-one in indices**: Carefully define [start, mid) and [mid, end)
3. **Stack overflow**: Very deep recursion on large arrays
4. **Forgetting remaining elements**: Must append leftovers after main loop

## Further Reading

- CLRS Chapter 2.3 (Merge Sort)
- [Merge Sort - Wikipedia](https://en.wikipedia.org/wiki/Merge_sort)
- [Visualgo - Sorting](https://visualgo.net/en/sorting)
- [Timsort - Wikipedia](https://en.wikipedia.org/wiki/Timsort)
- [External Sorting - Wikipedia](https://en.wikipedia.org/wiki/External_sorting)

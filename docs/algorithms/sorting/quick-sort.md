# Quick Sort

## Overview

Quick sort is a divide-and-conquer algorithm that selects a "pivot" element, partitions the array around the pivot (elements less than pivot go left, greater go right), and recursively sorts the partitions. It's typically the fastest general-purpose sorting algorithm in practice.

## Intuition

```
Sort [3, 6, 8, 10, 1, 2, 1]:

Step 1: Choose pivot (last element = 1)
        Partition around 1
        [3, 6, 8, 10, 1, 2, 1]
                          ↑ pivot

        Elements < 1: none
        Elements > 1: [3, 6, 8, 10, 2]
        Result: [1, 3, 6, 8, 10, 2, 1]  (not quite, let's trace properly)

Actually, let's trace Lomuto partition:
        [3, 6, 8, 10, 1, 2, 1]
         ↑                 ↑
         i                pivot=1

        3 > 1: skip
        6 > 1: skip
        8 > 1: skip
        10 > 1: skip
        1 ≤ 1: swap with position after last small element
        2 > 1: skip
        Final: [1, 6, 8, 10, 3, 2, 1] then swap pivot
               [1, 6, 8, 10, 3, 2, 1]
        Wait, let me redo this more carefully...
```

Let me show a cleaner example:

```
Sort [10, 7, 8, 9, 1, 5], pivot = 5 (last)

Partition:
i = -1 (position of last element ≤ pivot)

j=0: 10 > 5, skip            [10, 7, 8, 9, 1, 5], i=-1
j=1: 7 > 5, skip             [10, 7, 8, 9, 1, 5], i=-1
j=2: 8 > 5, skip             [10, 7, 8, 9, 1, 5], i=-1
j=3: 9 > 5, skip             [10, 7, 8, 9, 1, 5], i=-1
j=4: 1 ≤ 5, i++, swap(0,4)   [1, 7, 8, 9, 10, 5], i=0

Swap pivot to position i+1:  [1, 5, 8, 9, 10, 7]
                                 ↑
                           pivot in place

Left partition: [1]         (already sorted)
Right partition: [8, 9, 10, 7]  (recursively sort)
```

## How It Works

### Lomuto Partition Scheme

```
partition(array, low, high):
    pivot = array[high]
    i = low - 1  // Index of smaller element

    for j from low to high - 1:
        if array[j] <= pivot:
            i++
            swap(array[i], array[j])

    swap(array[i + 1], array[high])
    return i + 1

quick_sort(array, low, high):
    if low < high:
        pivot_index = partition(array, low, high)
        quick_sort(array, low, pivot_index - 1)
        quick_sort(array, pivot_index + 1, high)
```

### Hoare Partition Scheme (Original)

```
partition_hoare(array, low, high):
    pivot = array[low]  // or median
    i = low - 1
    j = high + 1

    while true:
        do:
            i++
        while array[i] < pivot

        do:
            j--
        while array[j] > pivot

        if i >= j:
            return j

        swap(array[i], array[j])
```

**Hoare vs Lomuto:**
- Hoare does fewer swaps on average
- Hoare is slightly more complex
- Both achieve same time complexity

### Detailed Trace (Lomuto)

```
Array: [8, 4, 2, 9, 5, 1, 6]
Pivot: 6 (last element)

Initial: i=-1
         [8, 4, 2, 9, 5, 1, 6]
          ↑j

j=0: 8 > 6, skip
j=1: 4 ≤ 6, i=0, swap(0,1): [4, 8, 2, 9, 5, 1, 6]
j=2: 2 ≤ 6, i=1, swap(1,2): [4, 2, 8, 9, 5, 1, 6]
j=3: 9 > 6, skip
j=4: 5 ≤ 6, i=2, swap(2,4): [4, 2, 5, 9, 8, 1, 6]
j=5: 1 ≤ 6, i=3, swap(3,5): [4, 2, 5, 1, 8, 9, 6]

Place pivot: swap(4, 6): [4, 2, 5, 1, 6, 9, 8]
                                  ↑
                            pivot at index 4

Recursively sort [4, 2, 5, 1] and [9, 8]
```

## Mathematical Analysis

### Time Complexity Analysis

**Best/Average Case:**

When pivot splits array roughly in half:
```
T(n) = 2T(n/2) + O(n)
```
By Master Theorem: **T(n) = O(n log n)**

**Worst Case:**

When pivot is always min or max (sorted/reverse sorted input):
```
T(n) = T(n-1) + O(n)
     = T(n-2) + O(n-1) + O(n)
     = ... = O(n) + O(n-1) + ... + O(1)
     = O(n²)
```

### Average Case Analysis (Detailed)

Let T(n) be expected comparisons. Pivot equally likely at any position:

T(n) = n - 1 + (1/n) × Σ(k=0 to n-1) [T(k) + T(n-1-k)]

This solves to: **T(n) ≈ 2n ln n ≈ 1.39n log₂ n**

Quick sort does ~39% more comparisons than optimal (n log n), but fewer data movements.

### Why Fast in Practice?

1. **Cache efficiency**: Sequential memory access during partition
2. **Low overhead**: Simple inner loop
3. **In-place**: No auxiliary array copying
4. **Good constants**: Despite more comparisons than merge sort

## Time Complexity

| Case | Complexity | Condition |
|------|------------|-----------|
| Best | O(n log n) | Balanced partitions |
| Average | O(n log n) | Random pivot selection |
| Worst | O(n²) | Already sorted (with bad pivot) |

## Space Complexity

| Implementation | Space |
|----------------|-------|
| Standard | O(log n) call stack (average) |
| Worst case | O(n) call stack |
| Tail-recursive | O(log n) guaranteed |

## Use Cases

### When Quick Sort Excels

1. **General-purpose sorting**
   - Fastest in practice for random data
   - Default in many standard libraries

2. **In-place sorting required**
   - O(log n) extra space
   - Better than merge sort's O(n)

3. **Cache-sensitive applications**
   - Good locality of reference
   - Outperforms heap sort

4. **Virtual memory environments**
   - Sequential access pattern
   - Fewer page faults

### Real-World Examples

- **C qsort()**: Standard library quicksort
- **Java Arrays.sort()** for primitives: Dual-pivot quicksort
- **C++ std::sort**: Introsort (quicksort + heapsort)
- **Python sorted()** internal: Quick elements of Timsort

### When NOT to Use

- Need guaranteed O(n log n): Use merge sort or heap sort
- Need stable sort: Use merge sort
- Nearly sorted data: Use insertion sort or Timsort
- Adversarial input possible: Use introsort

## Trade-offs & Comparisons

### vs. Merge Sort

| Aspect | Quick Sort | Merge Sort |
|--------|------------|------------|
| Average time | O(n log n) | O(n log n) |
| Worst time | O(n²) | O(n log n) |
| Space | O(log n) | O(n) |
| Stable | No | Yes |
| Cache | Better | Worse |
| In practice | Faster | Slower |

### vs. Heap Sort

| Aspect | Quick Sort | Heap Sort |
|--------|------------|-----------|
| Average time | O(n log n) | O(n log n) |
| Worst time | O(n²) | O(n log n) |
| Space | O(log n) | O(1) |
| Cache | Better | Worse |
| In practice | Faster | Slower |

### vs. Introsort

| Aspect | Quick Sort | Introsort |
|--------|------------|-----------|
| Worst time | O(n²) | O(n log n) |
| Complexity | Simple | Moderate |
| Guarantee | No | Yes |

## Common Variations

### 1. Randomized Quick Sort

Avoid worst case by random pivot:
```
randomized_partition(array, low, high):
    random_index = random(low, high)
    swap(array[random_index], array[high])
    return partition(array, low, high)
```
Expected O(n log n) regardless of input.

### 2. Median-of-Three Pivot

Choose median of first, middle, last:
```
median_of_three(array, low, high):
    mid = (low + high) // 2
    if array[mid] < array[low]:
        swap(array[mid], array[low])
    if array[high] < array[low]:
        swap(array[high], array[low])
    if array[high] < array[mid]:
        swap(array[high], array[mid])
    return array[mid]  // Median is now in middle
```
Reduces probability of worst case.

### 3. Three-Way Partition (Dutch Flag)

Handle duplicates efficiently:
```
three_way_partition(array, low, high):
    pivot = array[high]
    lt = low      // < pivot boundary
    gt = high     // > pivot boundary
    i = low

    while i <= gt:
        if array[i] < pivot:
            swap(array[lt], array[i])
            lt++
            i++
        else if array[i] > pivot:
            swap(array[gt], array[i])
            gt--
        else:
            i++

    return lt, gt  // Elements in [lt, gt] equal to pivot
```

Result: [< pivot | == pivot | > pivot]

**Time with many duplicates**: O(n) vs O(n²) for standard quicksort!

### 4. Dual-Pivot Quick Sort (Java)

Two pivots, three partitions:
```
[< P1 | P1 ≤ x ≤ P2 | > P2]
```
- Used in Java 7+ for primitive arrays
- Slightly better cache performance
- ~5% faster in practice

### 5. Introsort (C++ std::sort)

Hybrid algorithm:
```
introsort(array, depth_limit):
    if len(array) < 16:
        insertion_sort(array)
    else if depth_limit == 0:
        heap_sort(array)  // Avoid O(n²)
    else:
        pivot = partition(array)
        introsort(left, depth_limit - 1)
        introsort(right, depth_limit - 1)
```
- depth_limit = 2 × log₂(n)
- Guarantees O(n log n) worst case

### 6. Tail-Recursive Optimization

Reduce stack space:
```
quick_sort_tail(array, low, high):
    while low < high:
        pivot = partition(array, low, high)

        // Recurse on smaller partition
        if pivot - low < high - pivot:
            quick_sort_tail(array, low, pivot - 1)
            low = pivot + 1  // Tail call elimination
        else:
            quick_sort_tail(array, pivot + 1, high)
            high = pivot - 1
```
Guarantees O(log n) stack depth.

## Common Interview Problems

1. **Sort an Array**: Basic quicksort
2. **Kth Largest Element**: Quick select
3. **Sort Colors**: Dutch National Flag (3-way partition)
4. **Wiggle Sort**: Modified partitioning
5. **Top K Frequent Elements**: Quick select + counting

### Quick Select (Kth Element)

```
quick_select(array, k):
    low, high = 0, len(array) - 1

    while low <= high:
        pivot = partition(array, low, high)

        if pivot == k:
            return array[k]
        else if pivot < k:
            low = pivot + 1
        else:
            high = pivot - 1
```

**Time: O(n) average, O(n²) worst**

## Implementation Tips

### Pivot Selection Strategies

| Strategy | Best For |
|----------|----------|
| Last element | Simple, but vulnerable to sorted input |
| Random | General protection |
| Median-of-three | Good practical choice |
| Ninther | Very large arrays |

### Common Pitfalls

1. **Stack overflow on sorted input**: Use tail recursion or iteration
2. **Quadratic time with duplicates**: Use 3-way partition
3. **Off-by-one errors**: Carefully define partition boundaries
4. **Infinite loop**: Ensure partition always excludes pivot

### Optimization Checklist

1. ✅ Use insertion sort for small subarrays (< 16 elements)
2. ✅ Median-of-three pivot selection
3. ✅ Three-way partition for data with duplicates
4. ✅ Tail recursion on larger partition
5. ✅ Fall back to heapsort if recursion too deep (introsort)

## Further Reading

- CLRS Chapter 7 (Quicksort)
- [Quick Sort - Wikipedia](https://en.wikipedia.org/wiki/Quicksort)
- [Visualgo - Sorting](https://visualgo.net/en/sorting)
- [Dual-Pivot Quicksort](http://codeblab.com/wp-content/uploads/2009/09/DualPivotQuicksort.pdf)
- [Introsort - Wikipedia](https://en.wikipedia.org/wiki/Introsort)

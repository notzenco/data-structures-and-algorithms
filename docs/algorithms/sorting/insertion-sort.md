# Insertion Sort

## Overview

Insertion sort builds a sorted array one element at a time by repeatedly picking the next element and inserting it into its correct position among the previously sorted elements. It's similar to how you might sort playing cards in your hand.

## Intuition

```
Sort [5, 2, 4, 6, 1, 3]:

Initial:     [5, 2, 4, 6, 1, 3]
             ↑ sorted

Step 1:      [5, 2, 4, 6, 1, 3]  Insert 2
                ↑
             [2, 5, 4, 6, 1, 3]  2 < 5, shift 5 right, insert 2
             ↑──↑ sorted

Step 2:      [2, 5, 4, 6, 1, 3]  Insert 4
                   ↑
             [2, 4, 5, 6, 1, 3]  4 < 5, shift 5 right, 4 > 2, insert
             ↑─────↑ sorted

Step 3:      [2, 4, 5, 6, 1, 3]  Insert 6
                      ↑
             [2, 4, 5, 6, 1, 3]  6 > 5, already in place
             ↑────────↑ sorted

Step 4:      [2, 4, 5, 6, 1, 3]  Insert 1
                         ↑
             [1, 2, 4, 5, 6, 3]  1 < all, shift all right, insert at front
             ↑───────────↑ sorted

Step 5:      [1, 2, 4, 5, 6, 3]  Insert 3
                            ↑
             [1, 2, 3, 4, 5, 6]  3 goes between 2 and 4
             ↑──────────────↑ sorted

Done!
```

## How It Works

### Algorithm

```
insertion_sort(array):
    for i from 1 to len(array) - 1:
        key = array[i]
        j = i - 1

        // Shift elements greater than key to the right
        while j >= 0 and array[j] > key:
            array[j + 1] = array[j]
            j = j - 1

        array[j + 1] = key
```

### Detailed Trace

```
Array: [5, 2, 4, 6, 1, 3]

i=1, key=2:
  j=0: array[0]=5 > 2, shift: [5, 5, 4, 6, 1, 3]
  j=-1: stop
  Insert at j+1=0: [2, 5, 4, 6, 1, 3]

i=2, key=4:
  j=1: array[1]=5 > 4, shift: [2, 5, 5, 6, 1, 3]
  j=0: array[0]=2 < 4, stop
  Insert at j+1=1: [2, 4, 5, 6, 1, 3]

i=3, key=6:
  j=2: array[2]=5 < 6, stop immediately
  Insert at j+1=3: [2, 4, 5, 6, 1, 3] (no change)

i=4, key=1:
  j=3: array[3]=6 > 1, shift: [2, 4, 5, 6, 6, 3]
  j=2: array[2]=5 > 1, shift: [2, 4, 5, 5, 6, 3]
  j=1: array[1]=4 > 1, shift: [2, 4, 4, 5, 6, 3]
  j=0: array[0]=2 > 1, shift: [2, 2, 4, 5, 6, 3]
  j=-1: stop
  Insert at j+1=0: [1, 2, 4, 5, 6, 3]

i=5, key=3:
  j=4: array[4]=6 > 3, shift: [1, 2, 4, 5, 6, 6]
  j=3: array[3]=5 > 3, shift: [1, 2, 4, 5, 5, 6]
  j=2: array[2]=4 > 3, shift: [1, 2, 4, 4, 5, 6]
  j=1: array[1]=2 < 3, stop
  Insert at j+1=2: [1, 2, 3, 4, 5, 6]

Final: [1, 2, 3, 4, 5, 6]
```

## Mathematical Analysis

### Time Complexity Analysis

**Best Case (Already Sorted):**
- Inner while loop never executes
- Outer loop runs n-1 times with O(1) work each
- **T(n) = O(n)**

**Worst Case (Reverse Sorted):**
- Each element i requires i comparisons and shifts
- Total comparisons: 1 + 2 + 3 + ... + (n-1) = n(n-1)/2
- **T(n) = O(n²)**

**Average Case:**
- On average, each element needs to move halfway
- Total: (1 + 2 + ... + (n-1)) / 2 = n(n-1)/4
- **T(n) = O(n²)**

### Inversion Count

An **inversion** is a pair (i, j) where i < j but array[i] > array[j].

- Insertion sort performs exactly one swap per inversion
- Number of inversions determines running time
- Sorted array: 0 inversions → O(n)
- Reverse sorted: n(n-1)/2 inversions → O(n²)

### Adaptive Property

Insertion sort is **adaptive**: its running time depends on the initial order.

For an array with k inversions:
- Time complexity: O(n + k)
- Nearly sorted array (k = O(n)): O(n) time!

## Time Complexity

| Case | Complexity | Condition |
|------|------------|-----------|
| Best | O(n) | Already sorted |
| Average | O(n²) | Random order |
| Worst | O(n²) | Reverse sorted |

## Space Complexity

- **In-place**: O(1) extra space
- Only needs one variable for the key being inserted

## Use Cases

### When Insertion Sort Excels

1. **Small Arrays (n < 50)**
   - Low overhead, simple inner loop
   - Often faster than O(n log n) sorts for small n
   - Used as base case in hybrid sorts (Timsort, Introsort)

2. **Nearly Sorted Data**
   - O(n) for k inversions where k is small
   - Excellent for "mostly sorted" inputs
   - Adding few elements to sorted array

3. **Online Sorting**
   - Can sort as data arrives
   - Each new element: O(n) to insert in sorted position
   - Stream processing

4. **Stable Sorting Required**
   - Maintains relative order of equal elements
   - Important when sorting by multiple keys

### Real-World Examples

- **Card sorting by hand**: Natural human algorithm
- **Timsort's base case**: Python/Java use insertion sort for small runs
- **Database insertions**: Maintaining sorted index
- **UI lists**: Inserting items into sorted display

### When NOT to Use

- Large unsorted arrays (use quicksort, mergesort)
- Need guaranteed O(n log n) worst case (use heapsort, mergesort)
- Parallel sorting needed (insertion sort is inherently sequential)

## Trade-offs & Comparisons

### vs. Selection Sort

| Aspect | Insertion Sort | Selection Sort |
|--------|----------------|----------------|
| Best case | O(n) | O(n²) |
| Worst case | O(n²) | O(n²) |
| Adaptive | Yes | No |
| Stable | Yes | No (typical) |
| Writes | O(n²) | O(n) |
| Online | Yes | No |

### vs. Bubble Sort

| Aspect | Insertion Sort | Bubble Sort |
|--------|----------------|-------------|
| Best case | O(n) | O(n) |
| Average case | O(n²) | O(n²) |
| Performance | Better | Worse |
| Practical use | Yes | Rarely |

### vs. Merge Sort / Quick Sort

| Aspect | Insertion Sort | Merge/Quick Sort |
|--------|----------------|------------------|
| Best case | O(n) | O(n log n) |
| Average case | O(n²) | O(n log n) |
| Small n | Faster | Overhead |
| Space | O(1) | O(n) / O(log n) |
| Adaptive | Yes | No |

### Sorting Algorithm Selection Guide

| Condition | Best Choice |
|-----------|-------------|
| Small array (n < 50) | Insertion Sort |
| Nearly sorted | Insertion Sort |
| Large random array | Quick Sort / Merge Sort |
| Guaranteed performance | Merge Sort / Heap Sort |
| Memory constrained | Heap Sort / Quick Sort |

## Common Variations

### 1. Binary Insertion Sort

Use binary search to find insertion position:
```
binary_insertion_sort(array):
    for i from 1 to len(array) - 1:
        key = array[i]
        pos = binary_search_position(array, 0, i, key)
        shift array[pos..i-1] right by 1
        array[pos] = key
```
- Reduces comparisons from O(n²) to O(n log n)
- Shifts still O(n²), so overall still O(n²)
- Better when comparisons are expensive

### 2. Shell Sort

Insertion sort with decreasing gap sequences:
```
shell_sort(array):
    gap = len(array) // 2
    while gap > 0:
        for i from gap to len(array) - 1:
            // Insertion sort with gap
            key = array[i]
            j = i
            while j >= gap and array[j - gap] > key:
                array[j] = array[j - gap]
                j -= gap
            array[j] = key
        gap //= 2
```
- Time complexity depends on gap sequence
- Best known: O(n log² n) with Sedgewick's sequence
- Better cache performance than standard insertion sort

### 3. Insertion Sort with Sentinel

Avoid bounds checking in inner loop:
```
insertion_sort_sentinel(array):
    // Move minimum to front as sentinel
    min_idx = find_minimum_index(array)
    swap(array[0], array[min_idx])

    for i from 2 to len(array) - 1:
        key = array[i]
        j = i - 1
        while array[j] > key:  // No j >= 0 check needed!
            array[j + 1] = array[j]
            j -= 1
        array[j + 1] = key
```

## Common Interview Applications

1. **Sort Colors (Dutch Flag)**: Three-way partitioning variant
2. **Merge Intervals**: Sort then merge
3. **Meeting Rooms**: Sort by start time
4. **Insert Interval**: Binary search for position

## Implementation Tips

### Optimization Opportunities

1. **Early termination**: If no shifts in a pass, array is sorted
2. **Binary search insertion**: Reduces comparisons
3. **Move instead of swap**: More efficient than repeated swaps

### Common Pitfalls

1. **Starting at index 0**: Should start at 1 (first element is trivially sorted)
2. **Off-by-one in shifts**: Carefully track indices
3. **Forgetting to insert key**: After shifting, must place key

## Further Reading

- CLRS Chapter 2.1 (Insertion Sort)
- [Insertion Sort - Wikipedia](https://en.wikipedia.org/wiki/Insertion_sort)
- [Visualgo - Sorting](https://visualgo.net/en/sorting)
- [Shell Sort - Wikipedia](https://en.wikipedia.org/wiki/Shellsort)

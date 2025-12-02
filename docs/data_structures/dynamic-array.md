# Dynamic Array

## Overview

A dynamic array (also known as a growable array, resizable array, or array list) is a random-access, variable-size data structure that allows elements to be added or removed. Unlike static arrays, dynamic arrays can resize themselves when they run out of capacity.

## Intuition

Think of a dynamic array like a notebook with pages:
- You can write on any page directly (random access)
- When the notebook is full, you get a bigger notebook and copy everything over
- The "get a bigger notebook" operation is expensive, but happens rarely

The key insight is that by **doubling the capacity** when full, we ensure the expensive copy operation happens infrequently enough that the average cost per insertion remains constant.

## How It Works

### Structure

```
Logical view:    [4] [7] [2] [9] [1]
                  0   1   2   3   4

Physical memory: [4] [7] [2] [9] [1] [ ] [ ] [ ]
                  0   1   2   3   4   5   6   7
                  └─────── size=5 ───────┘
                  └─────────── capacity=8 ────────────┘
```

### Operations

**Access by Index**
```
get(index):
    if index < 0 or index >= size:
        error "Index out of bounds"
    return array[index]
```

**Append (Push Back)**
```
append(element):
    if size == capacity:
        resize(capacity * 2)
    array[size] = element
    size = size + 1

resize(new_capacity):
    new_array = allocate(new_capacity)
    copy array[0..size-1] to new_array
    array = new_array
    capacity = new_capacity
```

**Insert at Index**
```
insert(index, element):
    if size == capacity:
        resize(capacity * 2)
    shift array[index..size-1] right by 1
    array[index] = element
    size = size + 1
```

**Delete at Index**
```
delete(index):
    shift array[index+1..size-1] left by 1
    size = size - 1
    if size < capacity / 4:
        resize(capacity / 2)  // Optional shrinking
```

## Mathematical Analysis

### Amortized Analysis of Append

The key question: If `append` sometimes takes O(n) time (when resizing), how can we say it's O(1)?

**Aggregate Method:**

Consider n append operations starting from an empty array with initial capacity 1:

Resizes occur at sizes: 1, 2, 4, 8, 16, ..., up to n

Total copy operations:
```
1 + 2 + 4 + 8 + ... + n = 2n - 1 < 2n
```

This is a geometric series: Σ(i=0 to log₂n) 2ⁱ = 2^(log₂n + 1) - 1 = 2n - 1

Total cost for n operations: n (for the n assignments) + 2n (for copies) = 3n

**Amortized cost per operation: 3n/n = O(1)**

**Accounting Method:**

Charge $3 for each append:
- $1 pays for the insertion itself
- $1 pays for moving this element during a future resize
- $1 pays for moving an element that was already present before this one

When we resize from capacity k to 2k:
- We need to move k elements
- The last k/2 elements each contributed $2 (enough to move themselves and one older element)
- Total available: k dollars, exactly what we need

### Why Double? Why Not +10 or +50%?

| Growth Factor | Amortized Append | Space Overhead |
|---------------|------------------|----------------|
| +1 (constant) | O(n)             | O(1)           |
| +n (linear)   | O(1)             | O(n)           |
| ×1.5          | O(1)             | ~50%           |
| ×2            | O(1)             | ~100%          |

Doubling (×2) is common, but ×1.5 (used by some implementations) trades slightly more frequent resizes for less wasted space.

## Time Complexity

| Operation | Average | Worst Case | Notes |
|-----------|---------|------------|-------|
| Access    | O(1)    | O(1)       | Direct index calculation |
| Search    | O(n)    | O(n)       | Linear scan required |
| Append    | O(1)*   | O(n)       | *Amortized |
| Insert    | O(n)    | O(n)       | Shift elements right |
| Delete    | O(n)    | O(n)       | Shift elements left |

## Space Complexity

- **Storage**: O(n) where n is the number of elements
- **Overhead**: Up to 2x due to unused capacity (with doubling strategy)
- **Resize operation**: O(n) temporary space for the new array

## Use Cases

### When to Use Dynamic Arrays

1. **Random access is frequent**: Need O(1) access by index
2. **Iteration is common**: Cache-friendly due to contiguous memory
3. **Mostly appending**: O(1) amortized append at the end
4. **Size is unpredictable**: Can grow/shrink as needed

### Real-World Examples

- **Python's `list`**: General-purpose sequence container
- **Java's `ArrayList`**: Default list implementation
- **JavaScript arrays**: Dynamic by default
- **Buffer management**: Network packets, file I/O
- **Undo history**: Stack of operations

### When NOT to Use

- Frequent insertions/deletions in the middle → Use linked list
- Need guaranteed O(1) insertions → Use linked list
- Memory is very constrained → Fixed-size array
- Need efficient merge/split → Use rope or tree structure

## Trade-offs & Comparisons

### vs. Linked List

| Aspect | Dynamic Array | Linked List |
|--------|---------------|-------------|
| Random access | O(1) | O(n) |
| Append | O(1) amortized | O(1) |
| Insert at front | O(n) | O(1) |
| Insert in middle | O(n) | O(1)* |
| Memory overhead | Low (just capacity) | High (pointer per node) |
| Cache performance | Excellent | Poor |
| Memory allocation | Bulk | Per element |

*O(1) if you have a pointer to the location; O(n) to find the location.

### vs. Static Array

| Aspect | Dynamic Array | Static Array |
|--------|---------------|--------------|
| Size | Variable | Fixed |
| Memory | May waste space | Exact fit |
| Flexibility | High | Low |
| Resize cost | Occasional O(n) | N/A (can't resize) |

## Common Variations

1. **Gap Buffer**: Keeps a gap at the cursor position for efficient local edits (used in text editors)
2. **Circular Buffer**: Fixed-size array with wrap-around (used for queues)
3. **Tiered Vector**: Multiple fixed-size arrays to reduce copy costs
4. **Hashed Array Tree**: Tree of fixed-size arrays for better memory efficiency

## Implementation Notes

### Growth Factor Choices
- **Java ArrayList**: 1.5x (saves memory)
- **Python list**: ~1.125x (overallocates by 12.5%)
- **C++ vector**: Implementation-defined (often 2x)
- **Rust Vec**: 2x

### Shrinking Strategy
Most implementations don't automatically shrink. When they do:
- Shrink when size < capacity/4 (not capacity/2, to avoid thrashing)
- Or provide explicit `shrink_to_fit()` method

## Further Reading

- CLRS Chapter 17 (Amortized Analysis)
- [Dynamic Array - Wikipedia](https://en.wikipedia.org/wiki/Dynamic_array)
- [Visualgo - Array](https://visualgo.net/en/array)

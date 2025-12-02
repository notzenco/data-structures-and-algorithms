# Binary Search

## Overview

Binary search is a divide-and-conquer algorithm that finds a target value in a **sorted** array by repeatedly halving the search space. At each step, it compares the target with the middle element and eliminates half of the remaining elements.

## Intuition

```
Find 23 in sorted array:
[2, 5, 8, 12, 16, 23, 38, 56, 72, 91]

Step 1: Check middle (16)
[2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
              ↑ middle
23 > 16 → Search right half

Step 2: Check middle (56)
                   [23, 38, 56, 72, 91]
                            ↑ middle
23 < 56 → Search left half

Step 3: Check middle (38)
                   [23, 38]
                        ↑ middle
23 < 38 → Search left half

Step 4: Check middle (23)
                   [23]
                    ↑
23 == 23 → Found!

Only 4 comparisons for 10 elements (vs. up to 10 for linear search)
```

## How It Works

### Iterative Implementation

```
binary_search(array, target):
    left = 0
    right = len(array) - 1

    while left <= right:
        mid = left + (right - left) // 2  // Avoids overflow

        if array[mid] == target:
            return mid
        else if array[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1  // Not found
```

### Recursive Implementation

```
binary_search(array, target, left, right):
    if left > right:
        return -1  // Not found

    mid = left + (right - left) // 2

    if array[mid] == target:
        return mid
    else if array[mid] < target:
        return binary_search(array, target, mid + 1, right)
    else:
        return binary_search(array, target, left, mid - 1)
```

### Step-by-Step Trace

```
Array: [1, 3, 5, 7, 9, 11, 13, 15]
Target: 9

Iteration 1:
  left=0, right=7
  mid = 0 + (7-0)//2 = 3
  array[3] = 7
  7 < 9 → left = 4

Iteration 2:
  left=4, right=7
  mid = 4 + (7-4)//2 = 5
  array[5] = 11
  11 > 9 → right = 4

Iteration 3:
  left=4, right=4
  mid = 4 + (4-4)//2 = 4
  array[4] = 9
  9 == 9 → Found at index 4!
```

## Mathematical Analysis

### Time Complexity Derivation

At each step, we halve the search space:
- Start: n elements
- After 1 step: n/2 elements
- After 2 steps: n/4 elements
- After k steps: n/2^k elements

Search ends when n/2^k = 1:
```
n/2^k = 1
n = 2^k
k = log₂(n)
```

**Time Complexity: O(log n)**

### Comparison with Linear Search

| Array Size | Linear Search (worst) | Binary Search (worst) |
|------------|----------------------|----------------------|
| 10         | 10                   | 4                    |
| 100        | 100                  | 7                    |
| 1,000      | 1,000                | 10                   |
| 1,000,000  | 1,000,000            | 20                   |
| 10^9       | 10^9                 | 30                   |

Binary search is exponentially faster.

### Recurrence Relation (Recursive)

T(n) = T(n/2) + O(1)

Using Master Theorem (a=1, b=2, f(n)=O(1)):
- log_b(a) = log_2(1) = 0
- f(n) = O(n^0) = O(1)
- Case 2: T(n) = O(log n)

## Time Complexity

| Case | Complexity | When |
|------|------------|------|
| Best | O(1) | Target at middle |
| Average | O(log n) | Target anywhere |
| Worst | O(log n) | Target at end or missing |

## Space Complexity

| Implementation | Space |
|----------------|-------|
| Iterative | O(1) |
| Recursive | O(log n) - call stack |

## Use Cases

### Direct Applications

1. **Dictionary/Phone book lookup**
   - Words/names sorted alphabetically
   - Binary search to find entry

2. **Version control bisection**
   - `git bisect` to find bug-introducing commit
   - Binary search through commit history

3. **Database indexing**
   - B-trees use binary search within nodes
   - Efficiently locate records

4. **IP routing tables**
   - Longest prefix matching
   - Binary search on sorted prefixes

5. **Library/file systems**
   - Find file in sorted directory listing
   - Locate record in sorted index

### Algorithm Building Block

1. **Lower/Upper Bound**
   - Find first element ≥ target
   - Find last element ≤ target

2. **Search in Rotated Array**
   - Modified binary search
   - Determine which half is sorted

3. **Find Peak Element**
   - Binary search on derivative
   - Climb toward peak

4. **Minimize Maximum / Maximize Minimum**
   - Binary search on answer
   - Check feasibility at each guess

### Real-World Examples

- **Debugging**: Binary search through code changes
- **Testing**: Find threshold where behavior changes
- **Gaming**: Matchmaking rating systems
- **ML**: Hyperparameter tuning (grid search variant)

## Trade-offs & Comparisons

### Binary Search vs. Linear Search

| Aspect | Binary Search | Linear Search |
|--------|---------------|---------------|
| Time | O(log n) | O(n) |
| Prerequisite | Sorted array | None |
| Data structure | Random access | Any |
| Implementation | Moderate | Simple |

### Binary Search vs. Hash Table

| Aspect | Binary Search | Hash Table |
|--------|---------------|------------|
| Search | O(log n) | O(1) average |
| Range queries | Yes | No |
| Ordered iteration | Yes | No |
| Space | O(1) extra | O(n) extra |
| Preprocessing | Sort O(n log n) | Build O(n) |

### When to Use Binary Search

- Data is sorted or can be sorted once
- Many searches on same data
- Need range queries or ordered access
- Memory-constrained (no hash table)

### When NOT to Use

- Data is unsorted and changes frequently
- Only searching once (just linear scan)
- Need O(1) lookup (use hash table)
- Data structure doesn't support random access (linked list)

## Common Variations

### 1. Find First Occurrence

```
find_first(array, target):
    left, right = 0, len(array) - 1
    result = -1
    while left <= right:
        mid = left + (right - left) // 2
        if array[mid] == target:
            result = mid
            right = mid - 1  // Keep searching left
        else if array[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return result
```

### 2. Find Last Occurrence

```
find_last(array, target):
    left, right = 0, len(array) - 1
    result = -1
    while left <= right:
        mid = left + (right - left) // 2
        if array[mid] == target:
            result = mid
            left = mid + 1  // Keep searching right
        else if array[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return result
```

### 3. Lower Bound (First ≥ target)

```
lower_bound(array, target):
    left, right = 0, len(array)
    while left < right:
        mid = left + (right - left) // 2
        if array[mid] < target:
            left = mid + 1
        else:
            right = mid
    return left
```

### 4. Upper Bound (First > target)

```
upper_bound(array, target):
    left, right = 0, len(array)
    while left < right:
        mid = left + (right - left) // 2
        if array[mid] <= target:
            left = mid + 1
        else:
            right = mid
    return left
```

### 5. Search on Answer (Binary Search on Result)

```
// Example: Minimum capacity to ship packages in D days
min_capacity(weights, days):
    left = max(weights)      // At least largest package
    right = sum(weights)     // At most all in one day

    while left < right:
        mid = left + (right - left) // 2
        if can_ship(weights, days, mid):
            right = mid
        else:
            left = mid + 1

    return left
```

## Common Interview Problems

1. **Basic**: Search in sorted array
2. **Rotated Array**: Search in rotated sorted array
3. **First/Last Position**: Find first and last occurrence
4. **Peak Element**: Find any peak in array
5. **Square Root**: Integer square root
6. **Search 2D Matrix**: Treat as 1D or row+column binary search
7. **Median of Two Sorted Arrays**: Binary search on partition
8. **Koko Eating Bananas**: Binary search on answer
9. **Split Array Largest Sum**: Binary search on maximum sum

## Implementation Tips

### Avoiding Integer Overflow

```
// BAD: Can overflow for large left and right
mid = (left + right) / 2

// GOOD: No overflow
mid = left + (right - left) / 2
```

### Loop Invariant

Maintain: If target exists, it's in `[left, right]`

**Exclusive vs. Inclusive bounds:**
- `while left <= right`: Use `right = mid - 1` and `left = mid + 1`
- `while left < right`: Use `right = mid` or `left = mid + 1` (careful with infinite loops)

### Common Pitfalls

1. **Off-by-one errors**: Double-check boundary conditions
2. **Infinite loops**: Ensure search space always shrinks
3. **Wrong comparison**: `<` vs `<=` matters
4. **Unsorted input**: Binary search requires sorted data
5. **Integer overflow**: Use safe mid calculation

### Template Choices

**Find exact match:**
```python
while left <= right:
    mid = left + (right - left) // 2
    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        left = mid + 1
    else:
        right = mid - 1
return -1
```

**Find boundary:**
```python
while left < right:
    mid = left + (right - left) // 2
    if condition(mid):
        right = mid
    else:
        left = mid + 1
return left
```

## Further Reading

- CLRS Chapter 2.3 (mention in merge sort context)
- [Binary Search - Wikipedia](https://en.wikipedia.org/wiki/Binary_search_algorithm)
- [Visualgo - Binary Search](https://visualgo.net/en/bst) (see search operation)
- [Binary Search Problems - LeetCode](https://leetcode.com/tag/binary-search/)

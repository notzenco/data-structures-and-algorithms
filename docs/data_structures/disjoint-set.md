# Disjoint Set (Union-Find)

## Overview

A Disjoint Set (also called Union-Find) is a data structure that tracks a collection of non-overlapping sets. It supports two primary operations: **Find** (determine which set an element belongs to) and **Union** (merge two sets). With path compression and union by rank, both operations achieve nearly O(1) amortized time.

## Intuition

```
Initial: Each element in its own set
{0} {1} {2} {3} {4} {5} {6} {7}

After Union(0,1), Union(2,3), Union(4,5), Union(6,7):
{0,1} {2,3} {4,5} {6,7}

After Union(0,2), Union(4,6):
{0,1,2,3} {4,5,6,7}

After Union(0,4):
{0,1,2,3,4,5,6,7}  ← All connected!

Find(3) and Find(7) now return the same representative.
```

**Key question answered**: "Are X and Y in the same set?" → O(α(n)) ≈ O(1)

## How It Works

### Forest Representation

Each set is represented as a tree, with the root as the set's representative:

```
Sets: {0,1,2,3} and {4,5,6,7}

Tree structure (arrows point to parent):
    0           4
   /|\         /|\
  1 2 3       5 6 7

Find(3) → Follow parent: 3 → 0 (root) → return 0
Find(7) → Follow parent: 7 → 4 (root) → return 4
Find(3) ≠ Find(7) → Different sets
```

### Basic Operations

**Make Set**
```
make_set(x):
    parent[x] = x   // x is its own parent (root)
    rank[x] = 0     // Used for union by rank
```

**Find (with Path Compression)**
```
find(x):
    if parent[x] != x:
        parent[x] = find(parent[x])  // Path compression
    return parent[x]
```

**Union (by Rank)**
```
union(x, y):
    root_x = find(x)
    root_y = find(y)

    if root_x == root_y:
        return  // Already in same set

    // Union by rank: attach smaller tree under larger
    if rank[root_x] < rank[root_y]:
        parent[root_x] = root_y
    else if rank[root_x] > rank[root_y]:
        parent[root_y] = root_x
    else:
        parent[root_y] = root_x
        rank[root_x]++
```

### Path Compression Visualization

```
Before find(7):
        0
        |
        1
        |
        2
        |
        7

After find(7) with path compression:
        0
       /|\
      1 2 7    ← All point directly to root

Future find(7) is O(1)!
```

### Union by Rank Visualization

```
Without union by rank (bad):
Union(0,1), Union(0,2), Union(0,3), Union(0,4)...

Could create:
0-1-2-3-4-5-6-7  ← Linear chain, find is O(n)

With union by rank (good):
Always attach smaller tree under larger:

        0
       /|\
      1 2 4
          |
          5

Tree stays shallow → find stays fast
```

## Mathematical Analysis

### Inverse Ackermann Function α(n)

The amortized time per operation is O(α(n)), where α is the inverse Ackermann function.

**How slow does α(n) grow?**

| n | α(n) |
|---|------|
| 1-2 | 1 |
| 3-4 | 2 |
| 5-16 | 3 |
| 17-65536 | 4 |
| 65537-2^65536 | 5 |

For any practical value of n (even 10^80, atoms in universe), α(n) ≤ 4.

**In practice**: O(α(n)) ≈ O(1)

### Why So Fast?

**Path compression alone**: O(log n) amortized
**Union by rank alone**: O(log n) worst case
**Combined**: O(α(n)) amortized

The combination is more powerful than either alone because:
- Union by rank keeps trees shallow
- Path compression flattens paths as you traverse
- Each operation improves future operations

### Proof Sketch (Tarjan & van Leeuwen)

Using the "iterated log" function and potential method:
- Define potential based on ranks and tree structure
- Show each operation's amortized cost is O(α(n))
- Total cost for m operations on n elements: O(m × α(n))

## Time Complexity

| Operation | Amortized | Notes |
|-----------|-----------|-------|
| make_set  | O(1)      |       |
| find      | O(α(n))   | ≈ O(1) in practice |
| union     | O(α(n))   | ≈ O(1) in practice |

For m operations on n elements: **O(m × α(n)) ≈ O(m)**

## Space Complexity

- **Storage**: O(n) for parent and rank arrays
- **Per element**: 2 integers (parent index + rank)

## Use Cases

### Classic Applications

1. **Kruskal's Minimum Spanning Tree**
   ```
   Sort edges by weight
   For each edge (u, v) in sorted order:
       if find(u) != find(v):
           add edge to MST
           union(u, v)
   ```
   Efficiently detects if adding an edge creates a cycle.

2. **Connected Components in Graph**
   ```
   For each edge (u, v):
       union(u, v)

   Components = unique find(v) values
   ```

3. **Network Connectivity**
   - Dynamic connectivity queries
   - "Is computer A connected to computer B?"
   - Add connections over time

4. **Percolation**
   - Physics simulation
   - Union adjacent open sites
   - Check if top connected to bottom

5. **Image Processing**
   - Connected component labeling
   - Region merging in segmentation

6. **Least Common Ancestor (Offline)**
   - Tarjan's offline LCA algorithm
   - Process queries in DFS order

### Real-World Examples

- **Social networks**: Friend groups
- **Maze generation**: Randomly connect cells
- **Game development**: Pathfinding, region detection
- **Compiler optimization**: Alias analysis

## Trade-offs & Comparisons

### vs. DFS/BFS for Connectivity

| Aspect | Union-Find | DFS/BFS |
|--------|------------|---------|
| Single query | O(n + e) | O(n + e) |
| Many queries | O(α(n)) each | O(n + e) each |
| Dynamic additions | O(α(n)) | Rebuild O(n + e) |
| Dynamic deletions | Not supported | Rebuild O(n + e) |
| Space | O(n) | O(n + e) |

Union-Find excels when you have many connectivity queries and/or dynamic edge additions.

### vs. Other Set Structures

| Operation | Union-Find | Hash Set per group |
|-----------|------------|-------------------|
| Find | O(α(n)) | O(1) |
| Union | O(α(n)) | O(n) (copy elements) |
| List elements | O(n) | O(k) per set |

Union-Find wins when unions are frequent.

## Common Variations

1. **Union by Size** (instead of rank)
   - Track set size instead of rank
   - Attach smaller set to larger
   - Same asymptotic complexity

2. **Weighted Union-Find**
   - Track additional data per element
   - Example: Distance to root
   - Used in some graph algorithms

3. **Persistent Union-Find**
   - Support queries on historical versions
   - Uses persistent data structures

4. **Union-Find with Rollback**
   - Support undoing unions
   - Don't use path compression (use union by rank only)
   - O(log n) per operation

5. **Concurrent Union-Find**
   - Thread-safe implementation
   - Lock-free variants exist

## Common Interview Problems

1. **Number of Connected Components**: Count unique roots
2. **Redundant Connection**: Find edge that creates cycle
3. **Accounts Merge**: Union emails belonging to same person
4. **Longest Consecutive Sequence**: Union consecutive numbers
5. **Number of Islands II**: Dynamic connectivity
6. **Satisfiability of Equality Equations**: Union equal variables

### Example: Number of Components

```
def count_components(n, edges):
    parent = list(range(n))
    rank = [0] * n

    def find(x):
        if parent[x] != x:
            parent[x] = find(parent[x])
        return parent[x]

    def union(x, y):
        px, py = find(x), find(y)
        if px == py:
            return False
        if rank[px] < rank[py]:
            px, py = py, px
        parent[py] = px
        if rank[px] == rank[py]:
            rank[px] += 1
        return True

    components = n
    for u, v in edges:
        if union(u, v):
            components -= 1
    return components
```

## Implementation Tips

### Initialization Patterns

**Array-based (fixed size):**
```python
class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n
```

**Dictionary-based (dynamic):**
```python
class UnionFind:
    def __init__(self):
        self.parent = {}
        self.rank = {}

    def make_set(self, x):
        if x not in self.parent:
            self.parent[x] = x
            self.rank[x] = 0
```

### Common Pitfalls

1. **Forgetting path compression**: Performance degrades to O(log n)
2. **Comparing elements instead of roots**: Use `find(x) == find(y)`, not `x == y`
3. **Not handling already-same-set**: Check before incrementing rank

### Iterative Path Compression

```python
def find(self, x):
    root = x
    while self.parent[root] != root:
        root = self.parent[root]

    # Path compression
    while self.parent[x] != root:
        next_x = self.parent[x]
        self.parent[x] = root
        x = next_x

    return root
```

## Further Reading

- CLRS Chapter 21 (Data Structures for Disjoint Sets)
- [Disjoint-set - Wikipedia](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
- [Visualgo - Union-Find](https://visualgo.net/en/ufds)
- [Tarjan's Paper](https://dl.acm.org/doi/10.1145/321879.321884) (Original analysis)

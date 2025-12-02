# Depth-First Search (DFS)

## Overview

Depth-First Search is a graph traversal algorithm that explores as far as possible along each branch before backtracking. It uses a stack (or recursion) to track the path, making it memory-efficient for deep graphs and ideal for problems involving paths, cycles, and connectivity.

## Intuition

```
Graph:
        1 --- 2
       /|     |
      0 |     5
       \|    /
        3---4

DFS from vertex 0 (recursive, neighbors in order):

Start at 0
  → Go to 1 (first neighbor)
    → Go to 2 (first unvisited neighbor of 1)
      → Go to 5 (first unvisited neighbor of 2)
        → Go to 4 (first unvisited neighbor of 5)
          → Go to 3 (first unvisited neighbor of 4)
            → All neighbors visited, backtrack
          → Backtrack to 4, then 5, then 2, then 1
        → 3 already visited, backtrack
      → Backtrack to 2
    → Backtrack to 1
  → 3 already visited, backtrack
→ Backtrack to 0, done

Visit order: 0 → 1 → 2 → 5 → 4 → 3

Like exploring a maze: go deep until dead end, then backtrack.
```

## How It Works

### Recursive Implementation

```
DFS(graph, vertex, visited):
    visited.add(vertex)
    process(vertex)

    for neighbor in graph.adjacent(vertex):
        if neighbor not in visited:
            DFS(graph, neighbor, visited)

// Initial call:
DFS(graph, start, set())
```

### Iterative Implementation (Explicit Stack)

```
DFS_iterative(graph, start):
    visited = set()
    stack = [start]

    while stack is not empty:
        vertex = stack.pop()

        if vertex in visited:
            continue

        visited.add(vertex)
        process(vertex)

        // Add neighbors in reverse for same order as recursive
        for neighbor in reversed(graph.adjacent(vertex)):
            if neighbor not in visited:
                stack.push(neighbor)

    return visited
```

### Detailed Trace

```
Graph (adjacency list):
0: [1, 3]
1: [0, 2, 3]
2: [1, 5]
3: [0, 1, 4]
4: [3, 5]
5: [2, 4]

DFS from 0 (recursive):

Call DFS(0):
  visited = {0}
  Neighbor 1: not visited
    Call DFS(1):
      visited = {0, 1}
      Neighbor 0: visited, skip
      Neighbor 2: not visited
        Call DFS(2):
          visited = {0, 1, 2}
          Neighbor 1: visited, skip
          Neighbor 5: not visited
            Call DFS(5):
              visited = {0, 1, 2, 5}
              Neighbor 2: visited, skip
              Neighbor 4: not visited
                Call DFS(4):
                  visited = {0, 1, 2, 5, 4}
                  Neighbor 3: not visited
                    Call DFS(3):
                      visited = {0, 1, 2, 5, 4, 3}
                      Neighbor 0: visited, skip
                      Neighbor 1: visited, skip
                      Neighbor 4: visited, skip
                      Return from DFS(3)
                  Neighbor 5: visited, skip
                  Return from DFS(4)
              Return from DFS(5)
          Return from DFS(2)
      Neighbor 3: visited, skip
      Return from DFS(1)
  Neighbor 3: visited, skip
  Return from DFS(0)

Visit order: 0, 1, 2, 5, 4, 3
```

### DFS with Entry/Exit Times

```
time = 0

DFS_with_times(graph, vertex, visited, entry, exit):
    global time
    visited.add(vertex)

    time += 1
    entry[vertex] = time

    for neighbor in graph.adjacent(vertex):
        if neighbor not in visited:
            DFS_with_times(graph, neighbor, visited, entry, exit)

    time += 1
    exit[vertex] = time
```

Entry/exit times enable:
- Detecting back edges (cycles)
- Finding ancestors/descendants
- Topological sorting

## Mathematical Analysis

### Time Complexity

Every vertex visited once: O(V)
Every edge examined once (twice for undirected): O(E)

**Total: O(V + E)**

### Space Complexity

| Implementation | Space |
|----------------|-------|
| Recursive | O(V) - call stack in worst case |
| Iterative | O(V) - explicit stack |

The actual space is O(h) where h is the maximum depth of recursion/stack.
- Tree: O(height)
- Graph: O(V) worst case (long path)

### DFS Tree and Edge Classification

DFS naturally creates a spanning tree (or forest for disconnected graphs):

```
Edge types:
- Tree edge: Part of DFS tree (vertex → first discovery)
- Back edge: Points to ancestor (indicates cycle!)
- Forward edge: Points to descendant (directed graphs only)
- Cross edge: Points to neither (directed graphs only)

For undirected graphs: Only tree edges and back edges exist.
```

## Time Complexity

| Operation | Complexity |
|-----------|------------|
| Visit all vertices | O(V + E) |
| Path finding | O(V + E) |
| Cycle detection | O(V + E) |
| Topological sort | O(V + E) |
| Connected components | O(V + E) |

## Space Complexity

| Component | Space |
|-----------|-------|
| Recursion stack / explicit stack | O(V) |
| Visited set | O(V) |
| Total | O(V) |

## Use Cases

### Classic Applications

1. **Cycle Detection**
   ```
   // Undirected graph
   has_cycle_undirected(graph, vertex, visited, parent):
       visited.add(vertex)
       for neighbor in graph[vertex]:
           if neighbor not in visited:
               if has_cycle(graph, neighbor, visited, vertex):
                   return True
           elif neighbor != parent:
               return True  // Back edge found!
       return False

   // Directed graph (using colors)
   WHITE, GRAY, BLACK = 0, 1, 2

   has_cycle_directed(graph, vertex, color):
       color[vertex] = GRAY  // Currently exploring

       for neighbor in graph[vertex]:
           if color[neighbor] == GRAY:
               return True  // Back edge to ancestor!
           if color[neighbor] == WHITE:
               if has_cycle_directed(graph, neighbor, color):
                   return True

       color[vertex] = BLACK  // Done exploring
       return False
   ```

2. **Topological Sort**
   ```
   topological_sort(graph):
       visited = set()
       result = []

       def dfs(vertex):
           visited.add(vertex)
           for neighbor in graph[vertex]:
               if neighbor not in visited:
                   dfs(neighbor)
           result.append(vertex)  // Add after all descendants

       for vertex in graph:
           if vertex not in visited:
               dfs(vertex)

       return result.reverse()  // Reverse for correct order
   ```

3. **Finding Connected Components**
   ```
   find_components(graph):
       visited = set()
       components = []

       for vertex in graph:
           if vertex not in visited:
               component = []
               dfs(vertex, visited, component)
               components.append(component)

       return components
   ```

4. **Path Finding**
   ```
   find_path(graph, start, end, visited, path):
       visited.add(start)
       path.append(start)

       if start == end:
           return True

       for neighbor in graph[start]:
           if neighbor not in visited:
               if find_path(graph, neighbor, end, visited, path):
                   return True

       path.pop()  // Backtrack
       return False
   ```

5. **Finding Bridges and Articulation Points**
   ```
   // Edges whose removal disconnects graph
   find_bridges(graph):
       visited = set()
       disc = {}  // Discovery time
       low = {}   // Lowest reachable vertex
       bridges = []
       time = [0]

       def dfs(u, parent):
           visited.add(u)
           disc[u] = low[u] = time[0]
           time[0] += 1

           for v in graph[u]:
               if v not in visited:
                   dfs(v, u)
                   low[u] = min(low[u], low[v])
                   if low[v] > disc[u]:
                       bridges.append((u, v))
               elif v != parent:
                   low[u] = min(low[u], disc[v])

       for v in graph:
           if v not in visited:
               dfs(v, -1)

       return bridges
   ```

6. **Maze Solving / Backtracking**
   ```
   solve_maze(maze, row, col, end_row, end_col, path):
       if row == end_row and col == end_col:
           return True

       if not valid(maze, row, col):
           return False

       maze[row][col] = VISITED
       path.append((row, col))

       // Try all directions
       for dr, dc in [(0,1), (1,0), (0,-1), (-1,0)]:
           if solve_maze(maze, row+dr, col+dc, end_row, end_col, path):
               return True

       path.pop()  // Backtrack
       return False
   ```

### Real-World Examples

- **Garbage collection**: Mark-and-sweep algorithm
- **Web crawlers**: Deep exploration of links
- **Compilers**: Syntax tree traversal
- **AI game playing**: Game tree exploration
- **Network analysis**: Finding strongly connected components

## Trade-offs & Comparisons

### DFS vs. BFS

| Aspect | DFS | BFS |
|--------|-----|-----|
| Data structure | Stack / recursion | Queue |
| Memory | O(h) - depth | O(w) - width |
| Shortest path | No | Yes (unweighted) |
| Completeness | Yes (finite) | Yes |
| Path finding | Any path | Shortest path |
| Cycle detection | Natural | Possible but awkward |
| Topological sort | Natural | Kahn's algorithm |

### When to Choose DFS

- Detecting cycles
- Topological sorting
- Path existence (not necessarily shortest)
- Backtracking problems
- When memory is constrained (deep but narrow graph)
- Strongly connected components

### When to Choose BFS

- Shortest path in unweighted graph
- Level-order processing
- When graph is wide but shallow
- Finding nearest neighbor

## Common Variations

### 1. Iterative Deepening DFS (IDDFS)

Combine DFS space efficiency with BFS completeness:
```
IDDFS(graph, start, goal):
    for depth from 0 to infinity:
        result = DLS(graph, start, goal, depth)
        if result is not None:
            return result

DLS(graph, vertex, goal, limit):
    if vertex == goal:
        return vertex
    if limit <= 0:
        return None
    for neighbor in graph[vertex]:
        result = DLS(graph, neighbor, goal, limit - 1)
        if result is not None:
            return result
    return None
```

**Time: O(b^d)** like BFS
**Space: O(d)** like DFS
Where b = branching factor, d = depth of solution.

### 2. DFS for Strongly Connected Components

**Kosaraju's Algorithm:**
```
kosaraju_scc(graph):
    // Step 1: DFS to get finish order
    visited = set()
    stack = []
    for v in graph:
        if v not in visited:
            dfs_finish_order(graph, v, visited, stack)

    // Step 2: Transpose graph
    transposed = transpose(graph)

    // Step 3: DFS in reverse finish order
    visited.clear()
    sccs = []
    while stack:
        v = stack.pop()
        if v not in visited:
            component = []
            dfs_collect(transposed, v, visited, component)
            sccs.append(component)

    return sccs
```

### 3. DFS with Colors (Three-State)

```
WHITE = 0  // Not visited
GRAY = 1   // Currently in stack (being processed)
BLACK = 2  // Completely processed

dfs_colored(graph, vertex, color, result):
    color[vertex] = GRAY

    for neighbor in graph[vertex]:
        if color[neighbor] == WHITE:
            dfs_colored(graph, neighbor, color, result)
        elif color[neighbor] == GRAY:
            // Cycle detected (back edge)
            pass

    color[vertex] = BLACK
    result.append(vertex)  // Post-order
```

### 4. DFS on Implicit Graphs

For problems where graph isn't explicitly given:
```
// N-Queens: Each state is a board configuration
dfs_n_queens(row, cols, diag1, diag2, n):
    if row == n:
        return 1  // Found valid solution

    count = 0
    for col in range(n):
        if col in cols or (row-col) in diag1 or (row+col) in diag2:
            continue
        // Place queen and recurse
        count += dfs_n_queens(row+1,
                              cols | {col},
                              diag1 | {row-col},
                              diag2 | {row+col}, n)
    return count
```

## Common Interview Problems

1. **Number of Islands**: DFS flood fill
2. **Clone Graph**: DFS with hash map
3. **Course Schedule**: Cycle detection
4. **All Paths from Source to Target**: DFS backtracking
5. **Reconstruct Itinerary**: DFS with sorted edges
6. **Word Search**: Grid DFS
7. **Surrounded Regions**: Border DFS
8. **Pacific Atlantic Water Flow**: Multi-source DFS

### Number of Islands

```
def num_islands(grid):
    if not grid:
        return 0

    count = 0
    rows, cols = len(grid), len(grid[0])

    def dfs(r, c):
        if r < 0 or r >= rows or c < 0 or c >= cols:
            return
        if grid[r][c] != '1':
            return

        grid[r][c] = '0'  // Mark visited
        dfs(r+1, c)
        dfs(r-1, c)
        dfs(r, c+1)
        dfs(r, c-1)

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '1':
                dfs(r, c)
                count += 1

    return count
```

## Implementation Tips

### Recursion vs. Iteration

**Use recursion when:**
- Code clarity is priority
- Stack depth is manageable
- Natural recursive structure (trees, backtracking)

**Use iteration when:**
- Risk of stack overflow (deep graphs)
- Need explicit control over stack
- Performance-critical code

### Avoiding Stack Overflow

```python
import sys
sys.setrecursionlimit(10000)  // Python default is ~1000

// Or use iterative DFS for very deep graphs
```

### Common Pitfalls

1. **Forgetting to mark as visited**: Infinite loops
2. **Marking visited too late**: Redundant processing
3. **Not handling disconnected graphs**: Miss some vertices
4. **Wrong backtracking in pathfinding**: Corrupt state

## Further Reading

- CLRS Chapter 22.3 (Depth-First Search)
- [DFS - Wikipedia](https://en.wikipedia.org/wiki/Depth-first_search)
- [Visualgo - Graph Traversal](https://visualgo.net/en/dfsbfs)
- [Tarjan's SCC Algorithm](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm)

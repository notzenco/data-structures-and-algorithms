# Breadth-First Search (BFS)

## Overview

Breadth-First Search is a graph traversal algorithm that explores all vertices at the current depth before moving to vertices at the next depth level. It uses a queue to maintain the frontier of vertices to visit, naturally finding the shortest path (by edge count) in unweighted graphs.

## Intuition

```
Graph:
        1 --- 2
       /|     |
      0 |     5
       \|    /
        3---4

BFS from vertex 0:

Level 0: [0]              Visit: 0
         Queue: [1, 3]

Level 1: [1, 3]           Visit: 1, 3
         Queue: [2, 4]

Level 2: [2, 4]           Visit: 2, 4
         Queue: [5]

Level 3: [5]              Visit: 5
         Queue: []

Visit order: 0 → 1 → 3 → 2 → 4 → 5

Like ripples expanding from a stone dropped in water.
```

## How It Works

### Algorithm

```
BFS(graph, start):
    visited = {start}
    queue = Queue([start])
    result = []

    while queue is not empty:
        vertex = queue.dequeue()
        result.append(vertex)

        for neighbor in graph.adjacent(vertex):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.enqueue(neighbor)

    return result
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

BFS from 0:

Step 1:
  visited = {0}
  queue = [0]
  Dequeue 0, process neighbors [1, 3]
  Enqueue 1, 3
  queue = [1, 3], visited = {0, 1, 3}

Step 2:
  Dequeue 1, process neighbors [0, 2, 3]
  0, 3 already visited
  Enqueue 2
  queue = [3, 2], visited = {0, 1, 3, 2}

Step 3:
  Dequeue 3, process neighbors [0, 1, 4]
  0, 1 already visited
  Enqueue 4
  queue = [2, 4], visited = {0, 1, 3, 2, 4}

Step 4:
  Dequeue 2, process neighbors [1, 5]
  1 already visited
  Enqueue 5
  queue = [4, 5], visited = {0, 1, 3, 2, 4, 5}

Step 5:
  Dequeue 4, process neighbors [3, 5]
  3, 5 already visited
  queue = [5]

Step 6:
  Dequeue 5, process neighbors [2, 4]
  2, 4 already visited
  queue = []

Done! Order: 0, 1, 3, 2, 4, 5
```

### Shortest Path Tracking

```
BFS_shortest_path(graph, start, end):
    visited = {start}
    queue = Queue([(start, [start])])  // (vertex, path)

    while queue is not empty:
        vertex, path = queue.dequeue()

        if vertex == end:
            return path

        for neighbor in graph.adjacent(vertex):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.enqueue((neighbor, path + [neighbor]))

    return None  // No path exists
```

Alternative: Track parent pointers and reconstruct:
```
BFS_with_parents(graph, start):
    visited = {start}
    parent = {start: None}
    queue = Queue([start])

    while queue is not empty:
        vertex = queue.dequeue()

        for neighbor in graph.adjacent(vertex):
            if neighbor not in visited:
                visited.add(neighbor)
                parent[neighbor] = vertex
                queue.enqueue(neighbor)

    return parent

reconstruct_path(parent, start, end):
    path = []
    current = end
    while current is not None:
        path.append(current)
        current = parent[current]
    return path.reverse()
```

## Mathematical Analysis

### Time Complexity

Every vertex enqueued/dequeued once: O(V)
Every edge examined once (twice for undirected): O(E)

**Total: O(V + E)**

### Space Complexity

- Queue: O(V) worst case (all vertices at one level)
- Visited set: O(V)
- Parent map (if used): O(V)

**Total: O(V)**

### Why BFS Finds Shortest Path

**Claim**: BFS visits vertices in order of their distance from the source.

**Proof sketch**:
- Vertices at distance d are all enqueued before any vertex at distance d+1
- Queue maintains FIFO order
- When we first reach a vertex, we've found the shortest path

More formally: At any point, queue contains vertices at distance d followed by vertices at distance d+1 (never d+2 before all d+1).

## Time Complexity

| Operation | Complexity |
|-----------|------------|
| Visit all vertices | O(V + E) |
| Find shortest path | O(V + E) |
| Check connectivity | O(V + E) |

## Space Complexity

| Component | Space |
|-----------|-------|
| Queue | O(V) |
| Visited set | O(V) |
| Total | O(V) |

## Use Cases

### Classic Applications

1. **Shortest Path in Unweighted Graph**
   ```
   Find minimum hops between nodes:
   - Social network: degrees of separation
   - Network routing: minimum hops
   - Puzzle solving: minimum moves
   ```

2. **Level-Order Tree Traversal**
   ```
           1
          / \
         2   3
        / \   \
       4   5   6

   BFS: 1 → 2, 3 → 4, 5, 6
   Outputs level by level
   ```

3. **Connected Components**
   ```
   count_components(graph):
       visited = set()
       count = 0
       for vertex in graph:
           if vertex not in visited:
               BFS(graph, vertex, visited)
               count += 1
       return count
   ```

4. **Bipartite Check (Two-Coloring)**
   ```
   is_bipartite(graph):
       color = {}
       for start in graph:
           if start in color:
               continue
           queue = [start]
           color[start] = 0
           while queue:
               v = queue.pop(0)
               for neighbor in graph[v]:
                   if neighbor not in color:
                       color[neighbor] = 1 - color[v]
                       queue.append(neighbor)
                   elif color[neighbor] == color[v]:
                       return False
       return True
   ```

5. **Web Crawling**
   - Start from seed URL
   - BFS through links
   - Respect depth limits

6. **Garbage Collection**
   - Mark reachable objects from roots
   - BFS traversal of object graph

### Real-World Examples

- **GPS Navigation**: Shortest route by intersections
- **Social Networks**: "People you may know" (2-hop neighbors)
- **Peer-to-peer**: Finding nearby nodes
- **Puzzle Solvers**: Rubik's cube, sliding puzzles
- **Network Broadcasting**: Flood-fill algorithms

## Trade-offs & Comparisons

### BFS vs. DFS

| Aspect | BFS | DFS |
|--------|-----|-----|
| Data structure | Queue | Stack (or recursion) |
| Space | O(V) - can be large | O(h) - height of tree/depth |
| Shortest path | Yes (unweighted) | No |
| Complete | Yes | Yes (finite graphs) |
| Memory | Higher (wide frontier) | Lower (single path) |
| Use case | Shortest path, levels | Topological sort, cycles |

### When to Choose BFS

- Need shortest path in unweighted graph
- Want to explore neighbors first
- Level-by-level processing needed
- Width of graph < depth

### When to Choose DFS

- Need to explore full paths
- Checking for cycles
- Topological sorting
- Memory constrained (graph is deep but narrow)
- Backtracking problems

## Common Variations

### 1. Multi-Source BFS

Start from multiple sources simultaneously:
```
multi_source_bfs(graph, sources):
    visited = set(sources)
    queue = Queue(sources)
    distance = {s: 0 for s in sources}

    while queue:
        vertex = queue.dequeue()
        for neighbor in graph[vertex]:
            if neighbor not in visited:
                visited.add(neighbor)
                distance[neighbor] = distance[vertex] + 1
                queue.enqueue(neighbor)

    return distance
```

Use case: "Rotting Oranges" - spreading from multiple points.

### 2. Bidirectional BFS

Search from both ends, meet in middle:
```
bidirectional_bfs(graph, start, end):
    if start == end:
        return [start]

    front_visited = {start: [start]}
    back_visited = {end: [end]}
    front_queue = [start]
    back_queue = [end]

    while front_queue and back_queue:
        // Expand smaller frontier
        if len(front_queue) <= len(back_queue):
            result = expand(front_queue, front_visited, back_visited)
        else:
            result = expand(back_queue, back_visited, front_visited)

        if result:
            return result

    return None
```

**Time: O(b^(d/2)) instead of O(b^d)** where b is branching factor, d is depth.

### 3. 0-1 BFS

For graphs with edge weights 0 or 1 only:
```
bfs_01(graph, start):
    distance = {v: infinity for v in graph}
    distance[start] = 0
    deque = Deque([start])

    while deque:
        u = deque.pop_front()
        for v, weight in graph[u]:
            if distance[u] + weight < distance[v]:
                distance[v] = distance[u] + weight
                if weight == 0:
                    deque.push_front(v)  // High priority
                else:
                    deque.push_back(v)   // Low priority

    return distance
```

**Time: O(V + E)** - better than Dijkstra's O((V+E) log V).

### 4. BFS with State

For problems where "state" is more than just vertex:
```
// Example: Grid with keys and doors
bfs_with_keys(grid, start):
    // State = (row, col, keys_collected)
    initial_state = (start.row, start.col, frozenset())
    queue = [(initial_state, 0)]  // (state, distance)
    visited = {initial_state}

    while queue:
        state, dist = queue.pop(0)
        row, col, keys = state

        for new_row, new_col in neighbors(row, col):
            cell = grid[new_row][new_col]
            new_keys = keys

            if is_key(cell):
                new_keys = keys | {cell}
            elif is_door(cell) and key_for(cell) not in keys:
                continue  // Can't pass

            new_state = (new_row, new_col, frozenset(new_keys))
            if new_state not in visited:
                visited.add(new_state)
                queue.append((new_state, dist + 1))
```

## Common Interview Problems

1. **Word Ladder**: Transform word to another, one letter at a time
2. **Rotting Oranges**: Multi-source BFS
3. **Shortest Path in Binary Matrix**: Grid BFS
4. **Open the Lock**: State-space BFS
5. **Minimum Knight Moves**: Chess BFS
6. **Shortest Bridge**: Find and expand from island
7. **Bus Routes**: Graph of bus connections

### Word Ladder Example

```
word_ladder(begin, end, word_list):
    word_set = set(word_list)
    if end not in word_set:
        return 0

    queue = [(begin, 1)]
    visited = {begin}

    while queue:
        word, length = queue.pop(0)

        if word == end:
            return length

        for i in range(len(word)):
            for c in 'abcdefghijklmnopqrstuvwxyz':
                new_word = word[:i] + c + word[i+1:]
                if new_word in word_set and new_word not in visited:
                    visited.add(new_word)
                    queue.append((new_word, length + 1))

    return 0
```

## Implementation Tips

### Avoiding Common Mistakes

1. **Mark visited when enqueueing, not when dequeuing**
   ```python
   # WRONG - may enqueue same node multiple times
   while queue:
       node = queue.pop(0)
       if node in visited:
           continue
       visited.add(node)
       ...

   # CORRECT - mark when adding to queue
   while queue:
       node = queue.pop(0)
       for neighbor in graph[node]:
           if neighbor not in visited:
               visited.add(neighbor)  # Mark HERE
               queue.append(neighbor)
   ```

2. **Use collections.deque for O(1) popleft**
   ```python
   from collections import deque
   queue = deque([start])
   # queue.popleft() is O(1)
   # list.pop(0) is O(n)!
   ```

3. **Handle disconnected graphs**
   ```python
   for start in all_vertices:
       if start not in visited:
           bfs(start)
   ```

## Further Reading

- CLRS Chapter 22.2 (Breadth-First Search)
- [BFS - Wikipedia](https://en.wikipedia.org/wiki/Breadth-first_search)
- [Visualgo - Graph Traversal](https://visualgo.net/en/dfsbfs)
- [0-1 BFS - CP Algorithms](https://cp-algorithms.com/graph/01_bfs.html)

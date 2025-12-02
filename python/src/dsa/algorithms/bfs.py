"""Breadth-First Search implementation.

Time: O(V + E)
Space: O(V)
"""

from typing import TypeVar, Optional, Callable
from collections import deque

from .graph import Graph

T = TypeVar("T")


def bfs(graph: Graph[T], start: T, callback: Optional[Callable[[T], None]] = None) -> list[T]:
    """Perform BFS traversal from start vertex. Returns list of visited vertices."""
    if not graph.has_vertex(start):
        return []

    result: list[T] = []
    visited: set[T] = set()
    queue: deque[T] = deque([start])
    visited.add(start)

    while queue:
        current = queue.popleft()
        result.append(current)

        if callback:
            callback(current)

        for neighbor in graph.neighbors(current):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return result


def bfs_path(graph: Graph[T], start: T, end: T) -> Optional[list[T]]:
    """Find the shortest path from start to end using BFS."""
    if not graph.has_vertex(start) or not graph.has_vertex(end):
        return None

    if start == end:
        return [start]

    visited: set[T] = set()
    parent: dict[T, T] = {}
    queue: deque[T] = deque([start])
    visited.add(start)

    while queue:
        current = queue.popleft()

        if current == end:
            path: list[T] = []
            node = end
            while node != start:
                path.append(node)
                node = parent[node]
            path.append(start)
            path.reverse()
            return path

        for neighbor in graph.neighbors(current):
            if neighbor not in visited:
                visited.add(neighbor)
                parent[neighbor] = current
                queue.append(neighbor)

    return None


def bfs_distances(graph: Graph[T], start: T) -> dict[T, int]:
    """Calculate distances from start to all reachable vertices."""
    if not graph.has_vertex(start):
        return {}

    distances: dict[T, int] = {start: 0}
    visited: set[T] = {start}
    queue: deque[tuple[T, int]] = deque([(start, 0)])

    while queue:
        current, dist = queue.popleft()

        for neighbor in graph.neighbors(current):
            if neighbor not in visited:
                visited.add(neighbor)
                distances[neighbor] = dist + 1
                queue.append((neighbor, dist + 1))

    return distances

"""Depth-First Search implementation.

Time: O(V + E)
Space: O(V)
"""

from typing import TypeVar, Optional, Callable

from .graph import Graph

T = TypeVar("T")


def dfs(graph: Graph[T], start: T, callback: Optional[Callable[[T], None]] = None) -> list[T]:
    """Perform iterative DFS traversal from start vertex."""
    if not graph.has_vertex(start):
        return []

    result: list[T] = []
    visited: set[T] = set()
    stack: list[T] = [start]

    while stack:
        current = stack.pop()

        if current in visited:
            continue

        visited.add(current)
        result.append(current)

        if callback:
            callback(current)

        for neighbor in reversed(graph.neighbors(current)):
            if neighbor not in visited:
                stack.append(neighbor)

    return result


def dfs_recursive(
    graph: Graph[T], start: T, callback: Optional[Callable[[T], None]] = None
) -> list[T]:
    """Perform recursive DFS traversal from start vertex."""
    if not graph.has_vertex(start):
        return []

    result: list[T] = []
    visited: set[T] = set()

    def visit(vertex: T) -> None:
        visited.add(vertex)
        result.append(vertex)

        if callback:
            callback(vertex)

        for neighbor in graph.neighbors(vertex):
            if neighbor not in visited:
                visit(neighbor)

    visit(start)
    return result


def dfs_path(graph: Graph[T], start: T, end: T) -> Optional[list[T]]:
    """Find a path from start to end using DFS."""
    if not graph.has_vertex(start) or not graph.has_vertex(end):
        return None

    if start == end:
        return [start]

    visited: set[T] = set()
    path: list[T] = []

    def search(vertex: T) -> bool:
        visited.add(vertex)
        path.append(vertex)

        if vertex == end:
            return True

        for neighbor in graph.neighbors(vertex):
            if neighbor not in visited:
                if search(neighbor):
                    return True

        path.pop()
        return False

    if search(start):
        return path
    return None


def has_cycle(graph: Graph[T], start: T) -> bool:
    """Check if the graph has a cycle starting from the given vertex (for directed graphs)."""
    if not graph.has_vertex(start):
        return False

    visited: set[T] = set()
    rec_stack: set[T] = set()

    def detect(vertex: T) -> bool:
        visited.add(vertex)
        rec_stack.add(vertex)

        for neighbor in graph.neighbors(vertex):
            if neighbor not in visited:
                if detect(neighbor):
                    return True
            elif neighbor in rec_stack:
                return True

        rec_stack.remove(vertex)
        return False

    return detect(start)

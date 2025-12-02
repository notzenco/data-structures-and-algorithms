"""Graph class for BFS and DFS algorithms."""

from typing import TypeVar, Generic, Iterator
from collections import defaultdict

T = TypeVar("T")


class Graph(Generic[T]):
    """A generic graph implementation using adjacency list."""

    def __init__(self, directed: bool = False) -> None:
        self._adj: dict[T, list[T]] = defaultdict(list)
        self._directed = directed

    def add_vertex(self, vertex: T) -> None:
        """Add a vertex to the graph."""
        if vertex not in self._adj:
            self._adj[vertex] = []

    def add_edge(self, from_vertex: T, to_vertex: T) -> None:
        """Add an edge between two vertices."""
        self.add_vertex(from_vertex)
        self.add_vertex(to_vertex)
        self._adj[from_vertex].append(to_vertex)
        if not self._directed:
            self._adj[to_vertex].append(from_vertex)

    def neighbors(self, vertex: T) -> list[T]:
        """Get the neighbors of a vertex."""
        return self._adj.get(vertex, [])

    def has_vertex(self, vertex: T) -> bool:
        """Check if a vertex exists."""
        return vertex in self._adj

    def vertices(self) -> Iterator[T]:
        """Iterate over all vertices."""
        return iter(self._adj.keys())

    def __contains__(self, vertex: T) -> bool:
        """Check if a vertex exists."""
        return self.has_vertex(vertex)

    def __len__(self) -> int:
        """Return the number of vertices."""
        return len(self._adj)

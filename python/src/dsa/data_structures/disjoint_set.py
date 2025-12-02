"""Disjoint set (union-find) implementation with path compression and union by rank.

Time: O(α(n)) amortized for find/union (nearly constant)
Space: O(n)
"""


class DisjointSet:
    """A disjoint set (union-find) implementation."""

    def __init__(self, n: int) -> None:
        self._parent = list(range(n))
        self._rank = [0] * n
        self._size = [1] * n
        self._num_sets = n

    def find(self, x: int) -> int:
        """Find the root of the set containing x with path compression."""
        if self._parent[x] != x:
            self._parent[x] = self.find(self._parent[x])
        return self._parent[x]

    def union(self, x: int, y: int) -> bool:
        """Unite the sets containing x and y. Returns True if they were different sets."""
        root_x = self.find(x)
        root_y = self.find(y)

        if root_x == root_y:
            return False

        # Union by rank
        if self._rank[root_x] < self._rank[root_y]:
            self._parent[root_x] = root_y
            self._size[root_y] += self._size[root_x]
        elif self._rank[root_x] > self._rank[root_y]:
            self._parent[root_y] = root_x
            self._size[root_x] += self._size[root_y]
        else:
            self._parent[root_y] = root_x
            self._size[root_x] += self._size[root_y]
            self._rank[root_x] += 1

        self._num_sets -= 1
        return True

    def connected(self, x: int, y: int) -> bool:
        """Return True if x and y are in the same set."""
        return self.find(x) == self.find(y)

    def set_size(self, x: int) -> int:
        """Return the size of the set containing x."""
        return self._size[self.find(x)]

    @property
    def num_sets(self) -> int:
        """Return the number of disjoint sets."""
        return self._num_sets

    def __len__(self) -> int:
        """Return the total number of elements."""
        return len(self._parent)

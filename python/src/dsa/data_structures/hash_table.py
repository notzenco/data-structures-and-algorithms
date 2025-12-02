"""Hash table implementation using open addressing with linear probing.

Time: O(1) average for insert, get, remove
Space: O(n)
"""

from typing import Generic, TypeVar, Optional, Iterator
from dataclasses import dataclass
from enum import Enum

K = TypeVar("K")
V = TypeVar("V")

INITIAL_CAPACITY = 16
LOAD_FACTOR = 0.75


class _EntryState(Enum):
    EMPTY = 0
    DELETED = 1
    OCCUPIED = 2


@dataclass
class _Entry(Generic[K, V]):
    key: Optional[K]
    value: Optional[V]
    state: _EntryState


class HashTable(Generic[K, V]):
    """A generic hash table implementation with open addressing."""

    def __init__(self, capacity: int = INITIAL_CAPACITY) -> None:
        self._capacity = max(capacity, INITIAL_CAPACITY)
        self._entries: list[_Entry[K, V]] = [
            _Entry(None, None, _EntryState.EMPTY) for _ in range(self._capacity)
        ]
        self._len = 0

    def _hash(self, key: K) -> int:
        return hash(key) % self._capacity

    def _resize(self) -> None:
        old_entries = self._entries
        self._capacity *= 2
        self._entries = [
            _Entry(None, None, _EntryState.EMPTY) for _ in range(self._capacity)
        ]
        self._len = 0

        for entry in old_entries:
            if entry.state == _EntryState.OCCUPIED:
                self.insert(entry.key, entry.value)  # type: ignore

    def insert(self, key: K, value: V) -> Optional[V]:
        """Insert a key-value pair. Returns the old value if key existed."""
        if (self._len + 1) / self._capacity > LOAD_FACTOR:
            self._resize()

        index = self._hash(key)
        start_index = index

        while True:
            entry = self._entries[index]

            if entry.state in (_EntryState.EMPTY, _EntryState.DELETED):
                self._entries[index] = _Entry(key, value, _EntryState.OCCUPIED)
                self._len += 1
                return None

            if entry.state == _EntryState.OCCUPIED and entry.key == key:
                old_value = entry.value
                self._entries[index] = _Entry(key, value, _EntryState.OCCUPIED)
                return old_value

            index = (index + 1) % self._capacity
            if index == start_index:
                raise RuntimeError("Hash table is full")

    def get(self, key: K) -> Optional[V]:
        """Get the value for a key, or None if not found."""
        index = self._hash(key)
        start_index = index

        while True:
            entry = self._entries[index]

            if entry.state == _EntryState.EMPTY:
                return None

            if entry.state == _EntryState.OCCUPIED and entry.key == key:
                return entry.value

            index = (index + 1) % self._capacity
            if index == start_index:
                return None

    def remove(self, key: K) -> Optional[V]:
        """Remove a key and return its value, or None if not found."""
        index = self._hash(key)
        start_index = index

        while True:
            entry = self._entries[index]

            if entry.state == _EntryState.EMPTY:
                return None

            if entry.state == _EntryState.OCCUPIED and entry.key == key:
                value = entry.value
                self._entries[index] = _Entry(None, None, _EntryState.DELETED)
                self._len -= 1
                return value

            index = (index + 1) % self._capacity
            if index == start_index:
                return None

    def contains(self, key: K) -> bool:
        """Return True if the key exists."""
        return self.get(key) is not None

    def is_empty(self) -> bool:
        """Return True if the table is empty."""
        return self._len == 0

    def __len__(self) -> int:
        """Return the number of elements."""
        return self._len

    def __getitem__(self, key: K) -> Optional[V]:
        """Get item by key."""
        return self.get(key)

    def __setitem__(self, key: K, value: V) -> None:
        """Set item by key."""
        self.insert(key, value)

    def __contains__(self, key: K) -> bool:
        """Check if key exists."""
        return self.contains(key)

    def keys(self) -> Iterator[K]:
        """Iterate over keys."""
        for entry in self._entries:
            if entry.state == _EntryState.OCCUPIED:
                yield entry.key  # type: ignore

    def values(self) -> Iterator[V]:
        """Iterate over values."""
        for entry in self._entries:
            if entry.state == _EntryState.OCCUPIED:
                yield entry.value  # type: ignore

    def items(self) -> Iterator[tuple[K, V]]:
        """Iterate over key-value pairs."""
        for entry in self._entries:
            if entry.state == _EntryState.OCCUPIED:
                yield entry.key, entry.value  # type: ignore

    def clear(self) -> None:
        """Remove all elements."""
        self._entries = [
            _Entry(None, None, _EntryState.EMPTY) for _ in range(self._capacity)
        ]
        self._len = 0

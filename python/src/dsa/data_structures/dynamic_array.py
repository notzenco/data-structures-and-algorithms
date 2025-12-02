"""Dynamic array implementation.

Time: O(1) amortized for append, O(n) for insert/remove
Space: O(n)
"""

from typing import Generic, TypeVar, Optional, Iterator

T = TypeVar("T")


class DynamicArray(Generic[T]):
    """A generic dynamic array implementation."""

    def __init__(self) -> None:
        self._data: list[T] = []

    def append(self, value: T) -> None:
        """Append a value to the end."""
        self._data.append(value)

    def pop(self) -> Optional[T]:
        """Remove and return the last value, or None if empty."""
        if self._data:
            return self._data.pop()
        return None

    def insert(self, index: int, value: T) -> None:
        """Insert a value at the given index."""
        if 0 <= index <= len(self._data):
            self._data.insert(index, value)

    def remove(self, index: int) -> Optional[T]:
        """Remove and return the value at the given index, or None if invalid."""
        if 0 <= index < len(self._data):
            return self._data.pop(index)
        return None

    def get(self, index: int) -> Optional[T]:
        """Return the value at the given index, or None if invalid."""
        if 0 <= index < len(self._data):
            return self._data[index]
        return None

    def set(self, index: int, value: T) -> bool:
        """Set the value at the given index. Returns True if successful."""
        if 0 <= index < len(self._data):
            self._data[index] = value
            return True
        return False

    def is_empty(self) -> bool:
        """Return True if the array is empty."""
        return len(self._data) == 0

    def __len__(self) -> int:
        """Return the number of elements."""
        return len(self._data)

    def __getitem__(self, index: int) -> T:
        """Get item at index."""
        return self._data[index]

    def __setitem__(self, index: int, value: T) -> None:
        """Set item at index."""
        self._data[index] = value

    def __iter__(self) -> Iterator[T]:
        """Iterate over elements."""
        return iter(self._data)

    def clear(self) -> None:
        """Remove all elements."""
        self._data.clear()

"""Binary min heap implementation.

Time: O(log n) for push/pop, O(1) for peek
Space: O(n)
"""

from typing import Generic, TypeVar, Optional, Callable, Iterable

T = TypeVar("T")


class MinHeap(Generic[T]):
    """A generic binary min heap implementation."""

    def __init__(self, key: Optional[Callable[[T], any]] = None) -> None:
        self._data: list[T] = []
        self._key = key or (lambda x: x)

    def push(self, value: T) -> None:
        """Add a value to the heap."""
        self._data.append(value)
        self._sift_up(len(self._data) - 1)

    def pop(self) -> Optional[T]:
        """Remove and return the minimum value, or None if empty."""
        if not self._data:
            return None

        self._swap(0, len(self._data) - 1)
        result = self._data.pop()

        if self._data:
            self._sift_down(0)

        return result

    def peek(self) -> Optional[T]:
        """Return the minimum value without removing it, or None if empty."""
        if self._data:
            return self._data[0]
        return None

    def _sift_up(self, index: int) -> None:
        while index > 0:
            parent = (index - 1) // 2
            if self._key(self._data[index]) < self._key(self._data[parent]):
                self._swap(index, parent)
                index = parent
            else:
                break

    def _sift_down(self, index: int) -> None:
        size = len(self._data)
        while True:
            left = 2 * index + 1
            right = 2 * index + 2
            smallest = index

            if left < size and self._key(self._data[left]) < self._key(
                self._data[smallest]
            ):
                smallest = left
            if right < size and self._key(self._data[right]) < self._key(
                self._data[smallest]
            ):
                smallest = right

            if smallest != index:
                self._swap(index, smallest)
                index = smallest
            else:
                break

    def _swap(self, i: int, j: int) -> None:
        self._data[i], self._data[j] = self._data[j], self._data[i]

    def is_empty(self) -> bool:
        """Return True if the heap is empty."""
        return len(self._data) == 0

    def __len__(self) -> int:
        """Return the number of elements."""
        return len(self._data)

    def clear(self) -> None:
        """Remove all elements."""
        self._data.clear()

    @classmethod
    def heapify(cls, items: Iterable[T], key: Optional[Callable[[T], any]] = None) -> "MinHeap[T]":
        """Create a heap from an iterable."""
        heap = cls(key)
        heap._data = list(items)
        for i in range(len(heap._data) // 2 - 1, -1, -1):
            heap._sift_down(i)
        return heap

"""Insertion sort implementation.

Time: O(n^2) worst/average, O(n) best
Space: O(1)
"""

from typing import TypeVar, MutableSequence, Optional, Callable

T = TypeVar("T")


def insertion_sort(
    arr: MutableSequence[T],
    key: Optional[Callable[[T], any]] = None,
    reverse: bool = False,
) -> None:
    """Sort a mutable sequence in place using insertion sort."""
    key_fn = key or (lambda x: x)

    for i in range(1, len(arr)):
        current = arr[i]
        current_key = key_fn(current)
        j = i

        while j > 0:
            prev_key = key_fn(arr[j - 1])
            if reverse:
                should_swap = prev_key < current_key
            else:
                should_swap = prev_key > current_key

            if should_swap:
                arr[j] = arr[j - 1]
                j -= 1
            else:
                break

        arr[j] = current

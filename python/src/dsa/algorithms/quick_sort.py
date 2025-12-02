"""Quick sort implementation with median-of-three pivot selection.

Time: O(n log n) average, O(n^2) worst
Space: O(log n) average
"""

from typing import TypeVar, MutableSequence, Optional, Callable

T = TypeVar("T")


def quick_sort(
    arr: MutableSequence[T],
    key: Optional[Callable[[T], any]] = None,
    reverse: bool = False,
) -> None:
    """Sort a mutable sequence in place using quick sort."""
    if len(arr) < 2:
        return

    key_fn = key or (lambda x: x)
    _quick_sort_impl(arr, 0, len(arr) - 1, key_fn, reverse)


def _quick_sort_impl(
    arr: MutableSequence[T],
    low: int,
    high: int,
    key_fn: Callable[[T], any],
    reverse: bool,
) -> None:
    if low >= high:
        return

    pivot_idx = _partition(arr, low, high, key_fn, reverse)

    if pivot_idx > low:
        _quick_sort_impl(arr, low, pivot_idx - 1, key_fn, reverse)
    if pivot_idx < high:
        _quick_sort_impl(arr, pivot_idx + 1, high, key_fn, reverse)


def _partition(
    arr: MutableSequence[T],
    low: int,
    high: int,
    key_fn: Callable[[T], any],
    reverse: bool,
) -> int:
    # Median of three pivot selection
    mid = low + (high - low) // 2

    if _compare(key_fn(arr[high]), key_fn(arr[low]), reverse):
        arr[low], arr[high] = arr[high], arr[low]
    if _compare(key_fn(arr[mid]), key_fn(arr[low]), reverse):
        arr[low], arr[mid] = arr[mid], arr[low]
    if _compare(key_fn(arr[high]), key_fn(arr[mid]), reverse):
        arr[mid], arr[high] = arr[high], arr[mid]

    arr[mid], arr[high] = arr[high], arr[mid]
    pivot_key = key_fn(arr[high])

    i = low
    for j in range(low, high):
        if _compare(key_fn(arr[j]), pivot_key, reverse):
            arr[i], arr[j] = arr[j], arr[i]
            i += 1

    arr[i], arr[high] = arr[high], arr[i]
    return i


def _compare(a: any, b: any, reverse: bool) -> bool:
    """Compare two values, considering reverse order."""
    if reverse:
        return a > b
    return a < b

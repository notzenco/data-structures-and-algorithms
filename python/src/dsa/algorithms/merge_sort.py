"""Merge sort implementation.

Time: O(n log n) all cases
Space: O(n)
"""

from typing import TypeVar, MutableSequence, Optional, Callable

T = TypeVar("T")


def merge_sort(
    arr: MutableSequence[T],
    key: Optional[Callable[[T], any]] = None,
    reverse: bool = False,
) -> None:
    """Sort a mutable sequence in place using merge sort."""
    if len(arr) < 2:
        return

    key_fn = key or (lambda x: x)
    _merge_sort_impl(arr, 0, len(arr) - 1, key_fn, reverse)


def _merge_sort_impl(
    arr: MutableSequence[T],
    left: int,
    right: int,
    key_fn: Callable[[T], any],
    reverse: bool,
) -> None:
    if left >= right:
        return

    mid = left + (right - left) // 2
    _merge_sort_impl(arr, left, mid, key_fn, reverse)
    _merge_sort_impl(arr, mid + 1, right, key_fn, reverse)
    _merge(arr, left, mid, right, key_fn, reverse)


def _merge(
    arr: MutableSequence[T],
    left: int,
    mid: int,
    right: int,
    key_fn: Callable[[T], any],
    reverse: bool,
) -> None:
    left_arr = list(arr[left : mid + 1])
    right_arr = list(arr[mid + 1 : right + 1])

    i = j = 0
    k = left

    while i < len(left_arr) and j < len(right_arr):
        left_key = key_fn(left_arr[i])
        right_key = key_fn(right_arr[j])

        if reverse:
            take_left = left_key >= right_key
        else:
            take_left = left_key <= right_key

        if take_left:
            arr[k] = left_arr[i]
            i += 1
        else:
            arr[k] = right_arr[j]
            j += 1
        k += 1

    while i < len(left_arr):
        arr[k] = left_arr[i]
        i += 1
        k += 1

    while j < len(right_arr):
        arr[k] = right_arr[j]
        j += 1
        k += 1

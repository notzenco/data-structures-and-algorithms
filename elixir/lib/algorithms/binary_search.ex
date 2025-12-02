defmodule Algorithms.BinarySearch do
  @moduledoc """
  Binary search algorithms for sorted lists.
  Time: O(log n)
  Space: O(1)
  """

  @doc "Search for a value in a sorted list"
  @spec search(list(), any()) :: non_neg_integer() | nil
  def search(list, target) do
    arr = :array.from_list(list)
    do_search(arr, target, 0, :array.size(arr) - 1)
  end

  defp do_search(_arr, _target, left, right) when left > right, do: nil

  defp do_search(arr, target, left, right) do
    mid = left + div(right - left, 2)
    mid_val = :array.get(mid, arr)

    cond do
      mid_val == target -> mid
      mid_val < target -> do_search(arr, target, mid + 1, right)
      mid_val > target -> do_search(arr, target, left, mid - 1)
    end
  end

  @doc "Find index of first element >= target"
  @spec lower_bound(list(), any()) :: non_neg_integer()
  def lower_bound(list, target) do
    arr = :array.from_list(list)
    do_lower_bound(arr, target, 0, :array.size(arr))
  end

  defp do_lower_bound(_arr, _target, left, right) when left >= right, do: left

  defp do_lower_bound(arr, target, left, right) do
    mid = left + div(right - left, 2)
    mid_val = :array.get(mid, arr)

    if mid_val < target do
      do_lower_bound(arr, target, mid + 1, right)
    else
      do_lower_bound(arr, target, left, mid)
    end
  end

  @doc "Find index of first element > target"
  @spec upper_bound(list(), any()) :: non_neg_integer()
  def upper_bound(list, target) do
    arr = :array.from_list(list)
    do_upper_bound(arr, target, 0, :array.size(arr))
  end

  defp do_upper_bound(_arr, _target, left, right) when left >= right, do: left

  defp do_upper_bound(arr, target, left, right) do
    mid = left + div(right - left, 2)
    mid_val = :array.get(mid, arr)

    if mid_val <= target do
      do_upper_bound(arr, target, mid + 1, right)
    else
      do_upper_bound(arr, target, left, mid)
    end
  end

  @doc "Check if value exists in sorted list"
  @spec contains?(list(), any()) :: boolean()
  def contains?(list, target), do: search(list, target) != nil
end

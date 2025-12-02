defmodule Algorithms.QuickSort do
  @moduledoc """
  Quick sort algorithm with median-of-three pivot selection.
  Time: O(n log n) average, O(n²) worst case
  Space: O(n)
  """

  @doc "Sort a list using quick sort"
  @spec sort(list()) :: list()
  def sort([]), do: []
  def sort([x]), do: [x]

  def sort(list) do
    pivot = median_of_three(list)
    {lesser, equal, greater} = partition(list, pivot)
    sort(lesser) ++ equal ++ sort(greater)
  end

  @doc "Sort with custom comparison function"
  @spec sort_by(list(), (any(), any() -> :lt | :eq | :gt)) :: list()
  def sort_by([], _compare), do: []
  def sort_by([x], _compare), do: [x]

  def sort_by(list, compare) do
    pivot = median_of_three_by(list, compare)
    {lesser, equal, greater} = partition_by(list, pivot, compare)
    sort_by(lesser, compare) ++ equal ++ sort_by(greater, compare)
  end

  defp median_of_three([a]), do: a
  defp median_of_three([a, b]), do: min(a, b)

  defp median_of_three(list) do
    first = hd(list)
    last = List.last(list)
    mid = Enum.at(list, div(length(list), 2))

    [first, mid, last]
    |> Enum.sort()
    |> Enum.at(1)
  end

  defp median_of_three_by([a], _compare), do: a
  defp median_of_three_by([a, _b], _compare), do: a

  defp median_of_three_by(list, compare) do
    first = hd(list)
    last = List.last(list)
    mid = Enum.at(list, div(length(list), 2))

    [first, mid, last]
    |> Enum.sort(fn a, b -> compare.(a, b) != :gt end)
    |> Enum.at(1)
  end

  defp partition(list, pivot) do
    {lesser, rest} = Enum.split_with(list, &(&1 < pivot))
    {equal, greater} = Enum.split_with(rest, &(&1 == pivot))
    {lesser, equal, greater}
  end

  defp partition_by(list, pivot, compare) do
    {lesser, rest} = Enum.split_with(list, &(compare.(&1, pivot) == :lt))
    {equal, greater} = Enum.split_with(rest, &(compare.(&1, pivot) == :eq))
    {lesser, equal, greater}
  end
end

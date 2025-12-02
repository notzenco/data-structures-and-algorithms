defmodule Algorithms.InsertionSort do
  @moduledoc """
  Insertion sort algorithm.
  Time: O(n²)
  Space: O(n) - creates new list
  """

  @doc "Sort a list using insertion sort"
  @spec sort(list()) :: list()
  def sort(list) do
    Enum.reduce(list, [], &insert_sorted/2)
  end

  @doc "Sort with custom comparison function"
  @spec sort_by(list(), (any(), any() -> boolean())) :: list()
  def sort_by(list, compare) do
    Enum.reduce(list, [], fn x, acc -> insert_sorted_by(x, acc, compare) end)
  end

  defp insert_sorted(x, []), do: [x]

  defp insert_sorted(x, [head | tail] = sorted) do
    if x <= head do
      [x | sorted]
    else
      [head | insert_sorted(x, tail)]
    end
  end

  defp insert_sorted_by(x, [], _compare), do: [x]

  defp insert_sorted_by(x, [head | tail] = sorted, compare) do
    if compare.(x, head) do
      [x | sorted]
    else
      [head | insert_sorted_by(x, tail, compare)]
    end
  end
end

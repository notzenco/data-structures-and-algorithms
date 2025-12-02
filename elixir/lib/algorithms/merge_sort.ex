defmodule Algorithms.MergeSort do
  @moduledoc """
  Merge sort algorithm.
  Time: O(n log n)
  Space: O(n)
  """

  @doc "Sort a list using merge sort"
  @spec sort(list()) :: list()
  def sort([]), do: []
  def sort([x]), do: [x]

  def sort(list) do
    {left, right} = split(list)
    merge(sort(left), sort(right))
  end

  @doc "Sort with custom comparison function"
  @spec sort_by(list(), (any(), any() -> boolean())) :: list()
  def sort_by([], _compare), do: []
  def sort_by([x], _compare), do: [x]

  def sort_by(list, compare) do
    {left, right} = split(list)
    merge_by(sort_by(left, compare), sort_by(right, compare), compare)
  end

  defp split(list) do
    mid = div(length(list), 2)
    Enum.split(list, mid)
  end

  defp merge([], right), do: right
  defp merge(left, []), do: left

  defp merge([lh | lt] = left, [rh | rt] = right) do
    if lh <= rh do
      [lh | merge(lt, right)]
    else
      [rh | merge(left, rt)]
    end
  end

  defp merge_by([], right, _compare), do: right
  defp merge_by(left, [], _compare), do: left

  defp merge_by([lh | lt] = left, [rh | rt] = right, compare) do
    if compare.(lh, rh) do
      [lh | merge_by(lt, right, compare)]
    else
      [rh | merge_by(left, rt, compare)]
    end
  end
end

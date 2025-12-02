defmodule DataStructures.MinHeap do
  @moduledoc """
  Min-heap implementation using a list.
  Time: O(log n) for insert/extract, O(1) for peek
  Space: O(n)
  """

  defstruct items: [], compare: &<=/2

  @type t :: %__MODULE__{items: list(), compare: (any(), any() -> boolean())}

  @doc "Create an empty min heap"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create with custom comparator"
  @spec new((any(), any() -> boolean())) :: t()
  def new(compare) when is_function(compare, 2) do
    %__MODULE__{compare: compare}
  end

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list) do
    Enum.reduce(list, new(), &insert(&2, &1))
  end

  @doc "Insert a value"
  @spec insert(t(), any()) :: t()
  def insert(%__MODULE__{items: items, compare: compare}, value) do
    new_items = items ++ [value]
    %__MODULE__{items: sift_up(new_items, length(new_items) - 1, compare), compare: compare}
  end

  @doc "Extract minimum value"
  @spec extract_min(t()) :: {:ok, any(), t()} | {:error, :empty}
  def extract_min(%__MODULE__{items: []}), do: {:error, :empty}

  def extract_min(%__MODULE__{items: [min], compare: compare}) do
    {:ok, min, %__MODULE__{items: [], compare: compare}}
  end

  def extract_min(%__MODULE__{items: [min | rest], compare: compare}) do
    [last | init] = Enum.reverse(rest)
    new_items = sift_down([last | Enum.reverse(init)], 0, compare)
    {:ok, min, %__MODULE__{items: new_items, compare: compare}}
  end

  @doc "Peek at minimum value"
  @spec peek(t()) :: {:ok, any()} | {:error, :empty}
  def peek(%__MODULE__{items: []}), do: {:error, :empty}
  def peek(%__MODULE__{items: [min | _]}), do: {:ok, min}

  @doc "Check if heap is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{items: []}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{items: items}), do: length(items)

  @doc "Convert to sorted list"
  @spec to_list(t()) :: list()
  def to_list(%__MODULE__{items: []} = _heap), do: []

  def to_list(heap) do
    case extract_min(heap) do
      {:ok, min, rest} -> [min | to_list(rest)]
      {:error, :empty} -> []
    end
  end

  defp sift_up(items, 0, _compare), do: items

  defp sift_up(items, index, compare) do
    parent_index = div(index - 1, 2)
    current = Enum.at(items, index)
    parent = Enum.at(items, parent_index)

    if compare.(current, parent) do
      items
      |> List.replace_at(index, parent)
      |> List.replace_at(parent_index, current)
      |> sift_up(parent_index, compare)
    else
      items
    end
  end

  defp sift_down(items, index, compare) do
    len = length(items)
    left = 2 * index + 1
    right = 2 * index + 2
    smallest = index

    smallest =
      if left < len and compare.(Enum.at(items, left), Enum.at(items, smallest)) do
        left
      else
        smallest
      end

    smallest =
      if right < len and compare.(Enum.at(items, right), Enum.at(items, smallest)) do
        right
      else
        smallest
      end

    if smallest != index do
      current = Enum.at(items, index)
      smallest_val = Enum.at(items, smallest)

      items
      |> List.replace_at(index, smallest_val)
      |> List.replace_at(smallest, current)
      |> sift_down(smallest, compare)
    else
      items
    end
  end
end

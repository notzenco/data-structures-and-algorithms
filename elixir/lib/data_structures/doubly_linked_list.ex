defmodule DataStructures.DoublyLinkedList do
  @moduledoc """
  Doubly linked list implementation using two lists.
  Time: O(1) for operations at ends, O(n) for arbitrary access
  Space: O(n)
  """

  defstruct front: [], back: [], count: 0

  @type t :: %__MODULE__{front: list(), back: list(), count: non_neg_integer()}

  @doc "Create an empty list"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{front: list, count: length(list)}

  @doc "Add element to the front"
  @spec prepend(t(), any()) :: t()
  def prepend(%__MODULE__{front: front, back: back, count: count}, value) do
    %__MODULE__{front: [value | front], back: back, count: count + 1}
  end

  @doc "Add element to the back"
  @spec append(t(), any()) :: t()
  def append(%__MODULE__{front: front, back: back, count: count}, value) do
    %__MODULE__{front: front, back: [value | back], count: count + 1}
  end

  @doc "Remove first element"
  @spec remove_first(t()) :: {:ok, any(), t()} | {:error, :empty}
  def remove_first(%__MODULE__{front: [], back: [], count: 0}) do
    {:error, :empty}
  end

  def remove_first(%__MODULE__{front: [], back: back, count: count}) do
    remove_first(%__MODULE__{front: Enum.reverse(back), back: [], count: count})
  end

  def remove_first(%__MODULE__{front: [head | tail], back: back, count: count}) do
    {:ok, head, %__MODULE__{front: tail, back: back, count: count - 1}}
  end

  @doc "Remove last element"
  @spec remove_last(t()) :: {:ok, any(), t()} | {:error, :empty}
  def remove_last(%__MODULE__{front: [], back: [], count: 0}) do
    {:error, :empty}
  end

  def remove_last(%__MODULE__{front: front, back: [], count: count}) do
    remove_last(%__MODULE__{front: [], back: Enum.reverse(front), count: count})
  end

  def remove_last(%__MODULE__{front: front, back: [head | tail], count: count}) do
    {:ok, head, %__MODULE__{front: front, back: tail, count: count - 1}}
  end

  @doc "Get first element"
  @spec first(t()) :: {:ok, any()} | {:error, :empty}
  def first(%__MODULE__{front: [], back: []}), do: {:error, :empty}
  def first(%__MODULE__{front: [], back: back}), do: {:ok, List.last(back)}
  def first(%__MODULE__{front: [head | _]}), do: {:ok, head}

  @doc "Get last element"
  @spec last(t()) :: {:ok, any()} | {:error, :empty}
  def last(%__MODULE__{front: [], back: []}), do: {:error, :empty}
  def last(%__MODULE__{front: front, back: []}), do: {:ok, List.last(front)}
  def last(%__MODULE__{back: [head | _]}), do: {:ok, head}

  @doc "Get element at index"
  @spec get(t(), non_neg_integer()) :: {:ok, any()} | {:error, :out_of_bounds}
  def get(%__MODULE__{count: count}, index) when index < 0 or index >= count do
    {:error, :out_of_bounds}
  end

  def get(list, index) do
    {:ok, Enum.at(to_list(list), index)}
  end

  @doc "Find index of first occurrence"
  @spec index_of(t(), any()) :: non_neg_integer() | nil
  def index_of(list, value) do
    Enum.find_index(to_list(list), &(&1 == value))
  end

  @doc "Check if element exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(list, value), do: value in to_list(list)

  @doc "Check if list is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{count: 0}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{count: count}), do: count

  @doc "Convert to list"
  @spec to_list(t()) :: list()
  def to_list(%__MODULE__{front: front, back: back}) do
    front ++ Enum.reverse(back)
  end
end

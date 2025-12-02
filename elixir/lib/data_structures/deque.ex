defmodule DataStructures.Deque do
  @moduledoc """
  Double-ended queue implementation.
  Time: O(1) amortized for all operations
  Space: O(n)
  """

  defstruct front: [], back: [], count: 0

  @type t :: %__MODULE__{front: list(), back: list(), count: non_neg_integer()}

  @doc "Create an empty deque"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{front: list, count: length(list)}

  @doc "Add element to the front"
  @spec push_front(t(), any()) :: t()
  def push_front(%__MODULE__{front: front, back: back, count: count}, value) do
    %__MODULE__{front: [value | front], back: back, count: count + 1}
  end

  @doc "Add element to the back"
  @spec push_back(t(), any()) :: t()
  def push_back(%__MODULE__{front: front, back: back, count: count}, value) do
    %__MODULE__{front: front, back: [value | back], count: count + 1}
  end

  @doc "Remove element from the front"
  @spec pop_front(t()) :: {:ok, any(), t()} | {:error, :empty}
  def pop_front(%__MODULE__{count: 0}), do: {:error, :empty}

  def pop_front(%__MODULE__{front: [], back: back, count: count}) do
    pop_front(%__MODULE__{front: Enum.reverse(back), back: [], count: count})
  end

  def pop_front(%__MODULE__{front: [head | tail], back: back, count: count}) do
    {:ok, head, %__MODULE__{front: tail, back: back, count: count - 1}}
  end

  @doc "Remove element from the back"
  @spec pop_back(t()) :: {:ok, any(), t()} | {:error, :empty}
  def pop_back(%__MODULE__{count: 0}), do: {:error, :empty}

  def pop_back(%__MODULE__{front: front, back: [], count: count}) do
    pop_back(%__MODULE__{front: [], back: Enum.reverse(front), count: count})
  end

  def pop_back(%__MODULE__{front: front, back: [head | tail], count: count}) do
    {:ok, head, %__MODULE__{front: front, back: tail, count: count - 1}}
  end

  @doc "Peek at front element"
  @spec peek_front(t()) :: {:ok, any()} | {:error, :empty}
  def peek_front(%__MODULE__{count: 0}), do: {:error, :empty}
  def peek_front(%__MODULE__{front: [], back: back}), do: {:ok, List.last(back)}
  def peek_front(%__MODULE__{front: [head | _]}), do: {:ok, head}

  @doc "Peek at back element"
  @spec peek_back(t()) :: {:ok, any()} | {:error, :empty}
  def peek_back(%__MODULE__{count: 0}), do: {:error, :empty}
  def peek_back(%__MODULE__{front: front, back: []}), do: {:ok, List.last(front)}
  def peek_back(%__MODULE__{back: [head | _]}), do: {:ok, head}

  @doc "Check if deque is empty"
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

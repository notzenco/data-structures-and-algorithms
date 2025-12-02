defmodule DataStructures.Stack do
  @moduledoc """
  LIFO stack implementation.
  Time: O(1) for all operations
  Space: O(n)
  """

  defstruct items: []

  @type t :: %__MODULE__{items: list()}

  @doc "Create an empty stack"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create a stack from a list (first element becomes top)"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{items: list}

  @doc "Push an element onto the stack"
  @spec push(t(), any()) :: t()
  def push(%__MODULE__{items: items}, value) do
    %__MODULE__{items: [value | items]}
  end

  @doc "Pop an element from the stack"
  @spec pop(t()) :: {:ok, any(), t()} | {:error, :empty}
  def pop(%__MODULE__{items: []}) do
    {:error, :empty}
  end

  def pop(%__MODULE__{items: [head | tail]}) do
    {:ok, head, %__MODULE__{items: tail}}
  end

  @doc "Peek at the top element without removing"
  @spec peek(t()) :: {:ok, any()} | {:error, :empty}
  def peek(%__MODULE__{items: []}), do: {:error, :empty}
  def peek(%__MODULE__{items: [head | _]}), do: {:ok, head}

  @doc "Check if stack is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{items: []}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get the number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{items: items}), do: length(items)

  @doc "Convert stack to list (top element first)"
  @spec to_list(t()) :: list()
  def to_list(%__MODULE__{items: items}), do: items
end

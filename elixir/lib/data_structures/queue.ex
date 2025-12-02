defmodule DataStructures.Queue do
  @moduledoc """
  FIFO queue implementation using two lists.
  Time: O(1) amortized for all operations
  Space: O(n)
  """

  defstruct front: [], back: []

  @type t :: %__MODULE__{front: list(), back: list()}

  @doc "Create an empty queue"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create a queue from a list (first element at front)"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{front: list}

  @doc "Add an element to the back of the queue"
  @spec enqueue(t(), any()) :: t()
  def enqueue(%__MODULE__{front: front, back: back}, value) do
    %__MODULE__{front: front, back: [value | back]}
  end

  @doc "Remove an element from the front of the queue"
  @spec dequeue(t()) :: {:ok, any(), t()} | {:error, :empty}
  def dequeue(%__MODULE__{front: [], back: []}) do
    {:error, :empty}
  end

  def dequeue(%__MODULE__{front: [], back: back}) do
    dequeue(%__MODULE__{front: Enum.reverse(back), back: []})
  end

  def dequeue(%__MODULE__{front: [head | tail], back: back}) do
    {:ok, head, %__MODULE__{front: tail, back: back}}
  end

  @doc "Peek at the front element without removing"
  @spec peek(t()) :: {:ok, any()} | {:error, :empty}
  def peek(%__MODULE__{front: [], back: []}), do: {:error, :empty}
  def peek(%__MODULE__{front: [], back: back}), do: {:ok, List.last(back)}
  def peek(%__MODULE__{front: [head | _]}), do: {:ok, head}

  @doc "Check if queue is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{front: [], back: []}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get the number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{front: front, back: back}) do
    length(front) + length(back)
  end

  @doc "Convert queue to list (front element first)"
  @spec to_list(t()) :: list()
  def to_list(%__MODULE__{front: front, back: back}) do
    front ++ Enum.reverse(back)
  end
end

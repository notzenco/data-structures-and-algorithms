defmodule DataStructures.SinglyLinkedList do
  @moduledoc """
  Singly linked list implementation.
  Time: O(1) for prepend, O(n) for other operations
  Space: O(n)
  """

  defstruct items: []

  @type t :: %__MODULE__{items: list()}

  @doc "Create an empty list"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{items: list}

  @doc "Add element to the front"
  @spec prepend(t(), any()) :: t()
  def prepend(%__MODULE__{items: items}, value) do
    %__MODULE__{items: [value | items]}
  end

  @doc "Add element to the back"
  @spec append(t(), any()) :: t()
  def append(%__MODULE__{items: items}, value) do
    %__MODULE__{items: items ++ [value]}
  end

  @doc "Insert element at index"
  @spec insert_at(t(), non_neg_integer(), any()) :: {:ok, t()} | {:error, :out_of_bounds}
  def insert_at(%__MODULE__{items: items}, index, value) do
    if index >= 0 and index <= length(items) do
      {:ok, %__MODULE__{items: List.insert_at(items, index, value)}}
    else
      {:error, :out_of_bounds}
    end
  end

  @doc "Remove element at index"
  @spec remove_at(t(), non_neg_integer()) :: {:ok, any(), t()} | {:error, :out_of_bounds}
  def remove_at(%__MODULE__{items: items}, index) do
    if index >= 0 and index < length(items) do
      value = Enum.at(items, index)
      {:ok, value, %__MODULE__{items: List.delete_at(items, index)}}
    else
      {:error, :out_of_bounds}
    end
  end

  @doc "Get element at index"
  @spec get(t(), non_neg_integer()) :: {:ok, any()} | {:error, :out_of_bounds}
  def get(%__MODULE__{items: items}, index) do
    if index >= 0 and index < length(items) do
      {:ok, Enum.at(items, index)}
    else
      {:error, :out_of_bounds}
    end
  end

  @doc "Find index of first occurrence"
  @spec index_of(t(), any()) :: non_neg_integer() | nil
  def index_of(%__MODULE__{items: items}, value) do
    Enum.find_index(items, &(&1 == value))
  end

  @doc "Check if element exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(%__MODULE__{items: items}, value), do: value in items

  @doc "Check if list is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{items: []}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{items: items}), do: length(items)

  @doc "Convert to list"
  @spec to_list(t()) :: list()
  def to_list(%__MODULE__{items: items}), do: items
end

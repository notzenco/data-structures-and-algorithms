defmodule DataStructures.DynamicArray do
  @moduledoc """
  Dynamic array implementation using Erlang's :array.
  Time: O(1) for access, O(n) for modifications
  Space: O(n)
  """

  defstruct items: []

  @type t :: %__MODULE__{items: list()}

  @doc "Create an empty dynamic array"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list), do: %__MODULE__{items: list}

  @doc "Add element to the end"
  @spec push(t(), any()) :: t()
  def push(%__MODULE__{items: items}, value) do
    %__MODULE__{items: items ++ [value]}
  end

  @doc "Remove and return the last element"
  @spec pop(t()) :: {:ok, any(), t()} | {:error, :empty}
  def pop(%__MODULE__{items: []}) do
    {:error, :empty}
  end

  def pop(%__MODULE__{items: items}) do
    {init, [last]} = Enum.split(items, -1)
    {:ok, last, %__MODULE__{items: init}}
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

  @doc "Set element at index"
  @spec set(t(), non_neg_integer(), any()) :: {:ok, t()} | {:error, :out_of_bounds}
  def set(%__MODULE__{items: items}, index, value) do
    if index >= 0 and index < length(items) do
      {:ok, %__MODULE__{items: List.replace_at(items, index, value)}}
    else
      {:error, :out_of_bounds}
    end
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

  @doc "Find index of first occurrence"
  @spec index_of(t(), any()) :: non_neg_integer() | nil
  def index_of(%__MODULE__{items: items}, value) do
    Enum.find_index(items, &(&1 == value))
  end

  @doc "Check if element exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(%__MODULE__{items: items}, value) do
    value in items
  end

  @doc "Check if array is empty"
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

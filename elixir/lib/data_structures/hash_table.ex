defmodule DataStructures.HashTable do
  @moduledoc """
  Hash table implementation using Elixir's Map.
  Time: O(1) average for all operations
  Space: O(n)
  """

  defstruct map: %{}

  @type t :: %__MODULE__{map: map()}

  @doc "Create an empty hash table"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from keyword list or map"
  @spec from_list([{any(), any()}]) :: t()
  def from_list(list) when is_list(list) do
    %__MODULE__{map: Map.new(list)}
  end

  @doc "Insert or update a key-value pair"
  @spec put(t(), any(), any()) :: t()
  def put(%__MODULE__{map: map}, key, value) do
    %__MODULE__{map: Map.put(map, key, value)}
  end

  @doc "Get value by key"
  @spec get(t(), any()) :: {:ok, any()} | {:error, :not_found}
  def get(%__MODULE__{map: map}, key) do
    case Map.fetch(map, key) do
      {:ok, value} -> {:ok, value}
      :error -> {:error, :not_found}
    end
  end

  @doc "Get value by key with default"
  @spec get(t(), any(), any()) :: any()
  def get(%__MODULE__{map: map}, key, default) do
    Map.get(map, key, default)
  end

  @doc "Remove a key-value pair"
  @spec remove(t(), any()) :: {any() | nil, t()}
  def remove(%__MODULE__{map: map}, key) do
    {Map.get(map, key), %__MODULE__{map: Map.delete(map, key)}}
  end

  @doc "Check if key exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(%__MODULE__{map: map}, key) do
    Map.has_key?(map, key)
  end

  @doc "Get all keys"
  @spec keys(t()) :: [any()]
  def keys(%__MODULE__{map: map}), do: Map.keys(map)

  @doc "Get all values"
  @spec values(t()) :: [any()]
  def values(%__MODULE__{map: map}), do: Map.values(map)

  @doc "Check if hash table is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{map: map}), do: map_size(map) == 0

  @doc "Get number of key-value pairs"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{map: map}), do: map_size(map)

  @doc "Convert to list of key-value tuples"
  @spec to_list(t()) :: [{any(), any()}]
  def to_list(%__MODULE__{map: map}), do: Map.to_list(map)

  @doc "Clear all entries"
  @spec clear(t()) :: t()
  def clear(%__MODULE__{}), do: %__MODULE__{}
end

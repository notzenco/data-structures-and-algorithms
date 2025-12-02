defmodule DataStructures.DisjointSet do
  @moduledoc """
  Disjoint set (Union-Find) with path compression and union by rank.
  Time: O(α(n)) amortized (nearly constant)
  Space: O(n)
  """

  defstruct parent: %{}, rank: %{}

  @type t :: %__MODULE__{parent: map(), rank: map()}

  @doc "Create an empty disjoint set"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create a new set containing only the given element"
  @spec make_set(t(), any()) :: t()
  def make_set(%__MODULE__{parent: parent, rank: rank} = ds, x) do
    if Map.has_key?(parent, x) do
      ds
    else
      %__MODULE__{
        parent: Map.put(parent, x, x),
        rank: Map.put(rank, x, 0)
      }
    end
  end

  @doc "Find the representative of the set containing x"
  @spec find(t(), any()) :: {:ok, any(), t()} | {:error, :not_found}
  def find(%__MODULE__{parent: parent} = ds, x) do
    case Map.fetch(parent, x) do
      {:ok, p} when p == x ->
        {:ok, x, ds}

      {:ok, p} ->
        {:ok, root, new_ds} = find(ds, p)
        {:ok, root, %{new_ds | parent: Map.put(new_ds.parent, x, root)}}

      :error ->
        {:error, :not_found}
    end
  end

  @doc "Find without path compression (simpler but slower)"
  @spec find_root(t(), any()) :: {:ok, any()} | {:error, :not_found}
  def find_root(%__MODULE__{parent: parent}, x) do
    case Map.fetch(parent, x) do
      {:ok, p} when p == x -> {:ok, x}
      {:ok, p} -> find_root(%__MODULE__{parent: parent}, p)
      :error -> {:error, :not_found}
    end
  end

  @doc "Union two sets"
  @spec union(t(), any(), any()) :: {:ok, t()} | {:error, :not_found}
  def union(ds, x, y) do
    with {:ok, root_x, ds1} <- find(ds, x),
         {:ok, root_y, ds2} <- find(ds1, y) do
      if root_x == root_y do
        {:ok, ds2}
      else
        rank_x = Map.get(ds2.rank, root_x, 0)
        rank_y = Map.get(ds2.rank, root_y, 0)

        cond do
          rank_x < rank_y ->
            {:ok, %{ds2 | parent: Map.put(ds2.parent, root_x, root_y)}}

          rank_x > rank_y ->
            {:ok, %{ds2 | parent: Map.put(ds2.parent, root_y, root_x)}}

          true ->
            {:ok,
             %{
               ds2
               | parent: Map.put(ds2.parent, root_y, root_x),
                 rank: Map.put(ds2.rank, root_x, rank_x + 1)
             }}
        end
      end
    end
  end

  @doc "Check if two elements are in the same set"
  @spec connected?(t(), any(), any()) :: boolean()
  def connected?(ds, x, y) do
    case {find_root(ds, x), find_root(ds, y)} do
      {{:ok, root_x}, {:ok, root_y}} -> root_x == root_y
      _ -> false
    end
  end

  @doc "Check if element exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(%__MODULE__{parent: parent}, x) do
    Map.has_key?(parent, x)
  end

  @doc "Count the number of disjoint sets"
  @spec set_count(t()) :: non_neg_integer()
  def set_count(%__MODULE__{parent: parent}) do
    parent
    |> Map.keys()
    |> Enum.count(fn x -> Map.get(parent, x) == x end)
  end

  @doc "Get total number of elements"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{parent: parent}), do: map_size(parent)

  @doc "Clear all sets"
  @spec clear(t()) :: t()
  def clear(%__MODULE__{}), do: new()
end

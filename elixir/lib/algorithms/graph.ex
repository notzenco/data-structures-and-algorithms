defmodule Algorithms.Graph do
  @moduledoc """
  Graph implementation using adjacency list.
  Supports both directed and undirected graphs.
  """

  defstruct adjacency_list: %{}, directed: false

  @type t :: %__MODULE__{adjacency_list: map(), directed: boolean()}

  @doc "Create an empty undirected graph"
  @spec new() :: t()
  def new, do: %__MODULE__{directed: false}

  @doc "Create a directed graph"
  @spec directed() :: t()
  def directed, do: %__MODULE__{directed: true}

  @doc "Create an undirected graph"
  @spec undirected() :: t()
  def undirected, do: %__MODULE__{directed: false}

  @doc "Add a vertex to the graph"
  @spec add_vertex(t(), any()) :: t()
  def add_vertex(%__MODULE__{adjacency_list: adj} = graph, vertex) do
    if Map.has_key?(adj, vertex) do
      graph
    else
      %{graph | adjacency_list: Map.put(adj, vertex, MapSet.new())}
    end
  end

  @doc "Add an edge to the graph"
  @spec add_edge(t(), any(), any()) :: t()
  def add_edge(graph, from, to) do
    graph
    |> add_vertex(from)
    |> add_vertex(to)
    |> do_add_edge(from, to)
  end

  defp do_add_edge(%__MODULE__{adjacency_list: adj, directed: directed} = graph, from, to) do
    adj = Map.update!(adj, from, &MapSet.put(&1, to))

    adj =
      if directed do
        adj
      else
        Map.update!(adj, to, &MapSet.put(&1, from))
      end

    %{graph | adjacency_list: adj}
  end

  @doc "Remove a vertex and all its edges"
  @spec remove_vertex(t(), any()) :: t()
  def remove_vertex(%__MODULE__{adjacency_list: adj} = graph, vertex) do
    adj =
      adj
      |> Map.delete(vertex)
      |> Map.new(fn {k, v} -> {k, MapSet.delete(v, vertex)} end)

    %{graph | adjacency_list: adj}
  end

  @doc "Remove an edge"
  @spec remove_edge(t(), any(), any()) :: t()
  def remove_edge(%__MODULE__{adjacency_list: adj, directed: directed} = graph, from, to) do
    adj =
      case Map.fetch(adj, from) do
        {:ok, neighbors} -> Map.put(adj, from, MapSet.delete(neighbors, to))
        :error -> adj
      end

    adj =
      if directed do
        adj
      else
        case Map.fetch(adj, to) do
          {:ok, neighbors} -> Map.put(adj, to, MapSet.delete(neighbors, from))
          :error -> adj
        end
      end

    %{graph | adjacency_list: adj}
  end

  @doc "Check if vertex exists"
  @spec has_vertex?(t(), any()) :: boolean()
  def has_vertex?(%__MODULE__{adjacency_list: adj}, vertex) do
    Map.has_key?(adj, vertex)
  end

  @doc "Check if edge exists"
  @spec has_edge?(t(), any(), any()) :: boolean()
  def has_edge?(%__MODULE__{adjacency_list: adj}, from, to) do
    case Map.fetch(adj, from) do
      {:ok, neighbors} -> MapSet.member?(neighbors, to)
      :error -> false
    end
  end

  @doc "Get neighbors of a vertex"
  @spec neighbors(t(), any()) :: MapSet.t()
  def neighbors(%__MODULE__{adjacency_list: adj}, vertex) do
    Map.get(adj, vertex, MapSet.new())
  end

  @doc "Get all vertices"
  @spec vertices(t()) :: [any()]
  def vertices(%__MODULE__{adjacency_list: adj}), do: Map.keys(adj)

  @doc "Get number of vertices"
  @spec vertex_count(t()) :: non_neg_integer()
  def vertex_count(%__MODULE__{adjacency_list: adj}), do: map_size(adj)

  @doc "Get number of edges"
  @spec edge_count(t()) :: non_neg_integer()
  def edge_count(%__MODULE__{adjacency_list: adj, directed: directed}) do
    total =
      adj
      |> Map.values()
      |> Enum.map(&MapSet.size/1)
      |> Enum.sum()

    if directed, do: total, else: div(total, 2)
  end

  @doc "Check if graph is directed"
  @spec directed?(t()) :: boolean()
  def directed?(%__MODULE__{directed: directed}), do: directed
end

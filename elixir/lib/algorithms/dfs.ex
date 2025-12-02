defmodule Algorithms.DFS do
  @moduledoc """
  Depth-first search algorithms.
  Time: O(V + E)
  Space: O(V)
  """

  alias Algorithms.Graph

  @doc "DFS traversal from a starting vertex (iterative)"
  @spec traverse(Graph.t(), any()) :: [any()]
  def traverse(graph, start) do
    if Graph.has_vertex?(graph, start) do
      do_traverse(graph, [start], MapSet.new(), [])
    else
      []
    end
  end

  defp do_traverse(_graph, [], _visited, result), do: Enum.reverse(result)

  defp do_traverse(graph, [vertex | stack], visited, result) do
    if MapSet.member?(visited, vertex) do
      do_traverse(graph, stack, visited, result)
    else
      new_visited = MapSet.put(visited, vertex)

      neighbors =
        graph
        |> Graph.neighbors(vertex)
        |> MapSet.to_list()
        |> Enum.filter(&(not MapSet.member?(new_visited, &1)))

      do_traverse(graph, neighbors ++ stack, new_visited, [vertex | result])
    end
  end

  @doc "DFS traversal from a starting vertex (recursive)"
  @spec traverse_recursive(Graph.t(), any()) :: [any()]
  def traverse_recursive(graph, start) do
    if Graph.has_vertex?(graph, start) do
      {result, _} = do_traverse_recursive(graph, start, MapSet.new())
      result
    else
      []
    end
  end

  defp do_traverse_recursive(graph, vertex, visited) do
    new_visited = MapSet.put(visited, vertex)

    {children_result, final_visited} =
      graph
      |> Graph.neighbors(vertex)
      |> MapSet.to_list()
      |> Enum.reduce({[], new_visited}, fn neighbor, {acc, vis} ->
        if MapSet.member?(vis, neighbor) do
          {acc, vis}
        else
          {result, new_vis} = do_traverse_recursive(graph, neighbor, vis)
          {acc ++ result, new_vis}
        end
      end)

    {[vertex | children_result], final_visited}
  end

  @doc "Find a path between two vertices using DFS"
  @spec find_path(Graph.t(), any(), any()) :: {:ok, [any()]} | {:error, :not_found}
  def find_path(graph, start, target) do
    cond do
      not Graph.has_vertex?(graph, start) -> {:error, :not_found}
      not Graph.has_vertex?(graph, target) -> {:error, :not_found}
      start == target -> {:ok, [start]}
      true -> do_find_path(graph, [{start, [start]}], MapSet.new(), target)
    end
  end

  defp do_find_path(_graph, [], _visited, _target), do: {:error, :not_found}

  defp do_find_path(graph, [{vertex, path} | stack], visited, target) do
    cond do
      vertex == target ->
        {:ok, Enum.reverse(path)}

      MapSet.member?(visited, vertex) ->
        do_find_path(graph, stack, visited, target)

      true ->
        new_visited = MapSet.put(visited, vertex)

        neighbors =
          graph
          |> Graph.neighbors(vertex)
          |> MapSet.to_list()
          |> Enum.filter(&(not MapSet.member?(new_visited, &1)))
          |> Enum.map(&{&1, [&1 | path]})

        do_find_path(graph, neighbors ++ stack, new_visited, target)
    end
  end

  @doc "Check if graph has a cycle (for directed graphs)"
  @spec has_cycle?(Graph.t()) :: boolean()
  def has_cycle?(graph) do
    vertices = Graph.vertices(graph)

    Enum.any?(vertices, fn vertex ->
      has_cycle_from?(graph, vertex, MapSet.new(), MapSet.new())
    end)
  end

  defp has_cycle_from?(graph, vertex, visited, rec_stack) do
    cond do
      MapSet.member?(rec_stack, vertex) ->
        true

      MapSet.member?(visited, vertex) ->
        false

      true ->
        new_visited = MapSet.put(visited, vertex)
        new_rec_stack = MapSet.put(rec_stack, vertex)

        result =
          graph
          |> Graph.neighbors(vertex)
          |> MapSet.to_list()
          |> Enum.any?(&has_cycle_from?(graph, &1, new_visited, new_rec_stack))

        result
    end
  end

  @doc "Topological sort (only for directed acyclic graphs)"
  @spec topological_sort(Graph.t()) :: {:ok, [any()]} | {:error, :has_cycle | :not_directed}
  def topological_sort(graph) do
    cond do
      not Graph.directed?(graph) -> {:error, :not_directed}
      has_cycle?(graph) -> {:error, :has_cycle}
      true -> {:ok, do_topological_sort(graph)}
    end
  end

  defp do_topological_sort(graph) do
    vertices = Graph.vertices(graph)

    {result, _} =
      Enum.reduce(vertices, {[], MapSet.new()}, fn vertex, {acc, visited} ->
        if MapSet.member?(visited, vertex) do
          {acc, visited}
        else
          topo_dfs(graph, vertex, visited, acc)
        end
      end)

    result
  end

  defp topo_dfs(graph, vertex, visited, acc) do
    new_visited = MapSet.put(visited, vertex)

    {new_acc, final_visited} =
      graph
      |> Graph.neighbors(vertex)
      |> MapSet.to_list()
      |> Enum.reduce({acc, new_visited}, fn neighbor, {a, v} ->
        if MapSet.member?(v, neighbor) do
          {a, v}
        else
          topo_dfs(graph, neighbor, v, a)
        end
      end)

    {[vertex | new_acc], final_visited}
  end
end

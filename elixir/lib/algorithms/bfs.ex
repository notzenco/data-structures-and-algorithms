defmodule Algorithms.BFS do
  @moduledoc """
  Breadth-first search algorithms.
  Time: O(V + E)
  Space: O(V)
  """

  alias Algorithms.Graph

  @doc "BFS traversal from a starting vertex"
  @spec traverse(Graph.t(), any()) :: [any()]
  def traverse(graph, start) do
    if Graph.has_vertex?(graph, start) do
      do_traverse(graph, :queue.from_list([start]), MapSet.new([start]), [])
    else
      []
    end
  end

  defp do_traverse(_graph, {[], []}, _visited, result) do
    Enum.reverse(result)
  end

  defp do_traverse(graph, queue, visited, result) do
    {{:value, vertex}, queue} = :queue.out(queue)

    neighbors =
      graph
      |> Graph.neighbors(vertex)
      |> MapSet.to_list()
      |> Enum.filter(&(not MapSet.member?(visited, &1)))

    new_visited = Enum.reduce(neighbors, visited, &MapSet.put(&2, &1))
    new_queue = Enum.reduce(neighbors, queue, &:queue.in(&1, &2))

    do_traverse(graph, new_queue, new_visited, [vertex | result])
  end

  @doc "Find shortest path between two vertices"
  @spec shortest_path(Graph.t(), any(), any()) :: {:ok, [any()]} | {:error, :not_found}
  def shortest_path(graph, start, target) do
    cond do
      not Graph.has_vertex?(graph, start) -> {:error, :not_found}
      not Graph.has_vertex?(graph, target) -> {:error, :not_found}
      start == target -> {:ok, [start]}
      true -> do_shortest_path(graph, :queue.from_list([{start, [start]}]), MapSet.new([start]), target)
    end
  end

  defp do_shortest_path(_graph, {[], []}, _visited, _target) do
    {:error, :not_found}
  end

  defp do_shortest_path(graph, queue, visited, target) do
    {{:value, {vertex, path}}, queue} = :queue.out(queue)

    if vertex == target do
      {:ok, Enum.reverse(path)}
    else
      neighbors =
        graph
        |> Graph.neighbors(vertex)
        |> MapSet.to_list()
        |> Enum.filter(&(not MapSet.member?(visited, &1)))

      new_visited = Enum.reduce(neighbors, visited, &MapSet.put(&2, &1))

      new_queue =
        Enum.reduce(neighbors, queue, fn neighbor, q ->
          :queue.in({neighbor, [neighbor | path]}, q)
        end)

      do_shortest_path(graph, new_queue, new_visited, target)
    end
  end

  @doc "Calculate distances from start vertex to all reachable vertices"
  @spec distances(Graph.t(), any()) :: map()
  def distances(graph, start) do
    if Graph.has_vertex?(graph, start) do
      do_distances(graph, :queue.from_list([{start, 0}]), MapSet.new([start]), %{start => 0})
    else
      %{}
    end
  end

  defp do_distances(_graph, {[], []}, _visited, result), do: result

  defp do_distances(graph, queue, visited, result) do
    {{:value, {vertex, dist}}, queue} = :queue.out(queue)

    neighbors =
      graph
      |> Graph.neighbors(vertex)
      |> MapSet.to_list()
      |> Enum.filter(&(not MapSet.member?(visited, &1)))

    new_visited = Enum.reduce(neighbors, visited, &MapSet.put(&2, &1))
    new_dist = dist + 1

    new_result = Enum.reduce(neighbors, result, &Map.put(&2, &1, new_dist))

    new_queue =
      Enum.reduce(neighbors, queue, fn neighbor, q ->
        :queue.in({neighbor, new_dist}, q)
      end)

    do_distances(graph, new_queue, new_visited, new_result)
  end
end

# frozen_string_literal: true

require_relative '../data_structures/stack'

module DSA
  module Algorithms
    # Depth-First Search algorithm.
    # Time: O(V + E) where V = vertices, E = edges
    # Space: O(V)
    module DFS
      class << self
        # Perform iterative DFS traversal from a starting vertex.
        def traverse(graph, start)
          result = []
          visited = {}

          return result unless graph.vertices.include?(start)

          stack = DataStructures::Stack.new
          stack.push(start)

          until stack.empty?
            vertex = stack.pop

            next if visited[vertex]

            visited[vertex] = true
            result << vertex

            # Push neighbors in reverse order to visit them in natural order
            graph.neighbors(vertex).reverse_each do |neighbor|
              stack.push(neighbor) unless visited[neighbor]
            end
          end

          result
        end

        # Perform recursive DFS traversal from a starting vertex.
        def traverse_recursive(graph, start)
          result = []
          visited = {}

          return result unless graph.vertices.include?(start)

          dfs_recursive(graph, start, visited, result)
          result
        end

        # Find a path between two vertices using DFS.
        def find_path(graph, start, finish)
          vertices = graph.vertices
          return nil unless vertices.include?(start) && vertices.include?(finish)
          return [start] if start == finish

          visited = {}
          parent = { start => nil }
          stack = DataStructures::Stack.new
          stack.push(start)

          until stack.empty?
            vertex = stack.pop

            next if visited[vertex]

            visited[vertex] = true

            return reconstruct_path(parent, start, finish) if vertex == finish

            graph.neighbors(vertex).each do |neighbor|
              unless visited[neighbor]
                parent[neighbor] = vertex
                stack.push(neighbor)
              end
            end
          end

          nil
        end

        # Detect if the graph contains a cycle.
        def has_cycle?(graph)
          visited = {}
          recursion_stack = {}

          graph.vertices.each do |vertex|
            unless visited[vertex]
              return true if has_cycle_util?(graph, vertex, visited, recursion_stack)
            end
          end

          false
        end

        # Perform topological sort on a directed acyclic graph.
        def topological_sort(graph)
          return nil unless graph.directed

          visited = {}
          result = []
          recursion_stack = {}

          graph.vertices.each do |vertex|
            unless visited[vertex]
              return nil unless topological_sort_util(graph, vertex, visited, result, recursion_stack)
            end
          end

          result.reverse
        end

        private

        def dfs_recursive(graph, vertex, visited, result)
          visited[vertex] = true
          result << vertex

          graph.neighbors(vertex).each do |neighbor|
            dfs_recursive(graph, neighbor, visited, result) unless visited[neighbor]
          end
        end

        def has_cycle_util?(graph, vertex, visited, recursion_stack)
          visited[vertex] = true
          recursion_stack[vertex] = true

          graph.neighbors(vertex).each do |neighbor|
            if !visited[neighbor]
              return true if has_cycle_util?(graph, neighbor, visited, recursion_stack)
            elsif recursion_stack[neighbor]
              return true
            end
          end

          recursion_stack.delete(vertex)
          false
        end

        def topological_sort_util(graph, vertex, visited, result, recursion_stack)
          visited[vertex] = true
          recursion_stack[vertex] = true

          graph.neighbors(vertex).each do |neighbor|
            return false if recursion_stack[neighbor] # Cycle detected

            unless visited[neighbor]
              return false unless topological_sort_util(graph, neighbor, visited, result, recursion_stack)
            end
          end

          recursion_stack.delete(vertex)
          result << vertex
          true
        end

        def reconstruct_path(parent, start, finish)
          path = []
          current = finish

          while current
            path.unshift(current)
            current = parent[current]
          end

          path
        end
      end
    end
  end
end

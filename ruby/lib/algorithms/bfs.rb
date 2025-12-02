# frozen_string_literal: true

require_relative '../data_structures/queue'

module DSA
  module Algorithms
    # Breadth-First Search algorithm.
    # Time: O(V + E) where V = vertices, E = edges
    # Space: O(V)
    module BFS
      class << self
        # Perform BFS traversal from a starting vertex.
        def traverse(graph, start)
          result = []
          visited = {}

          return result unless graph.vertices.include?(start)

          queue = DataStructures::Queue.new
          queue.enqueue(start)
          visited[start] = true

          until queue.empty?
            vertex = queue.dequeue
            result << vertex

            graph.neighbors(vertex).each do |neighbor|
              unless visited[neighbor]
                visited[neighbor] = true
                queue.enqueue(neighbor)
              end
            end
          end

          result
        end

        # Find the shortest path between two vertices using BFS.
        # Only works for unweighted graphs.
        def shortest_path(graph, start, finish)
          vertices = graph.vertices
          return nil unless vertices.include?(start) && vertices.include?(finish)
          return [start] if start == finish

          queue = DataStructures::Queue.new
          queue.enqueue(start)
          visited = { start => true }
          parent = { start => nil }

          until queue.empty?
            vertex = queue.dequeue

            graph.neighbors(vertex).each do |neighbor|
              unless visited[neighbor]
                visited[neighbor] = true
                parent[neighbor] = vertex

                return reconstruct_path(parent, start, finish) if neighbor == finish

                queue.enqueue(neighbor)
              end
            end
          end

          nil
        end

        # Find distances from start vertex to all reachable vertices.
        def distances(graph, start)
          distances = {}

          return distances unless graph.vertices.include?(start)

          queue = DataStructures::Queue.new
          queue.enqueue(start)
          distances[start] = 0

          until queue.empty?
            vertex = queue.dequeue
            current_distance = distances[vertex]

            graph.neighbors(vertex).each do |neighbor|
              unless distances.key?(neighbor)
                distances[neighbor] = current_distance + 1
                queue.enqueue(neighbor)
              end
            end
          end

          distances
        end

        private

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

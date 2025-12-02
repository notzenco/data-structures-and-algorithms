# frozen_string_literal: true

module DSA
  module DataStructures
    # Disjoint Set (Union-Find) implementation with path compression and union by rank.
    # Time: O(α(n)) amortized for all operations (nearly constant)
    # Space: O(n)
    class DisjointSet
      def initialize(size)
        @parent = (0...size).to_a
        @rank = Array.new(size, 0)
        @set_count = size
      end

      def find(x)
        @parent[x] = find(@parent[x]) if @parent[x] != x # Path compression
        @parent[x]
      end

      def union(x, y)
        root_x = find(x)
        root_y = find(y)

        return false if root_x == root_y

        # Union by rank
        if @rank[root_x] < @rank[root_y]
          @parent[root_x] = root_y
        elsif @rank[root_x] > @rank[root_y]
          @parent[root_y] = root_x
        else
          @parent[root_y] = root_x
          @rank[root_x] += 1
        end

        @set_count -= 1
        true
      end

      def connected?(x, y)
        find(x) == find(y)
      end

      def count
        @set_count
      end

      def size
        @parent.size
      end
    end
  end
end

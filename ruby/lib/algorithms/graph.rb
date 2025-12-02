# frozen_string_literal: true

module DSA
  module Algorithms
    # Graph representation using adjacency list.
    class Graph
      attr_reader :directed

      def initialize(directed: false)
        @adjacency_list = {}
        @directed = directed
      end

      def add_vertex(vertex)
        @adjacency_list[vertex] ||= []
        self
      end

      def add_edge(from, to)
        add_vertex(from)
        add_vertex(to)

        @adjacency_list[from] << to unless @adjacency_list[from].include?(to)
        @adjacency_list[to] << from unless @directed || @adjacency_list[to].include?(from)
        self
      end

      def remove_edge(from, to)
        @adjacency_list[from]&.delete(to)
        @adjacency_list[to]&.delete(from) unless @directed
        self
      end

      def remove_vertex(vertex)
        @adjacency_list.delete(vertex)
        @adjacency_list.each_value { |neighbors| neighbors.delete(vertex) }
        self
      end

      def has_edge?(from, to)
        @adjacency_list[from]&.include?(to) || false
      end

      def neighbors(vertex)
        @adjacency_list[vertex]&.dup || []
      end

      def vertices
        @adjacency_list.keys
      end

      def vertex_count
        @adjacency_list.size
      end
    end
  end
end

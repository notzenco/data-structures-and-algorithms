# frozen_string_literal: true

module DSA
  module DataStructures
    # LIFO (Last In, First Out) data structure.
    # Time: O(1) for all operations
    # Space: O(n)
    class Stack
      def initialize
        @items = []
      end

      def push(value)
        @items.push(value)
        self
      end

      def pop
        @items.pop
      end

      def peek
        @items.last
      end

      def empty?
        @items.empty?
      end

      def size
        @items.size
      end

      def clear
        @items.clear
        self
      end

      def to_a
        @items.dup
      end
    end
  end
end

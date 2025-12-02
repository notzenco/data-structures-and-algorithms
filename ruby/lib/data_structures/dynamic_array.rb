# frozen_string_literal: true

module DSA
  module DataStructures
    # Resizable array with automatic capacity management.
    # Time: O(1) amortized for push, O(n) for insert/remove
    # Space: O(n)
    class DynamicArray
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

      def get(index)
        return nil if index < 0 || index >= @items.size

        @items[index]
      end

      def set(index, value)
        return false if index < 0 || index >= @items.size

        @items[index] = value
        true
      end

      def insert(index, value)
        return false if index < 0 || index > @items.size

        @items.insert(index, value)
        true
      end

      def remove_at(index)
        return nil if index < 0 || index >= @items.size

        @items.delete_at(index)
      end

      def index_of(value)
        @items.index(value) || -1
      end

      def contains?(value)
        @items.include?(value)
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

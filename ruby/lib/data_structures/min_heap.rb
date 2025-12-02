# frozen_string_literal: true

module DSA
  module DataStructures
    # Binary min-heap implementation.
    # Time: O(log n) insert/extract, O(1) peek
    # Space: O(n)
    class MinHeap
      def initialize(&comparator)
        @heap = []
        @comparator = comparator || ->(a, b) { a <=> b }
      end

      def insert(value)
        @heap << value
        sift_up(@heap.size - 1)
        self
      end

      def extract_min
        return nil if @heap.empty?

        min = @heap[0]
        last = @heap.pop

        unless @heap.empty?
          @heap[0] = last
          sift_down(0)
        end

        min
      end

      def peek
        @heap.first
      end

      def empty?
        @heap.empty?
      end

      def size
        @heap.size
      end

      def clear
        @heap.clear
        self
      end

      def to_a
        @heap.dup
      end

      private

      def sift_up(index)
        while index > 0
          parent_index = (index - 1) / 2
          break if @comparator.call(@heap[index], @heap[parent_index]) >= 0

          swap(index, parent_index)
          index = parent_index
        end
      end

      def sift_down(index)
        loop do
          left_child = 2 * index + 1
          right_child = 2 * index + 2
          smallest = index

          if left_child < @heap.size && @comparator.call(@heap[left_child], @heap[smallest]) < 0
            smallest = left_child
          end

          if right_child < @heap.size && @comparator.call(@heap[right_child], @heap[smallest]) < 0
            smallest = right_child
          end

          break if smallest == index

          swap(index, smallest)
          index = smallest
        end
      end

      def swap(i, j)
        @heap[i], @heap[j] = @heap[j], @heap[i]
      end
    end
  end
end

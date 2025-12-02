# frozen_string_literal: true

module DSA
  module DataStructures
    # FIFO (First In, First Out) data structure using a linked list.
    # Time: O(1) for all operations
    # Space: O(n)
    class Queue
      Node = Struct.new(:value, :next_node)

      def initialize
        @head = nil
        @tail = nil
        @size = 0
      end

      def enqueue(value)
        node = Node.new(value, nil)
        if @tail
          @tail.next_node = node
        else
          @head = node
        end
        @tail = node
        @size += 1
        self
      end

      def dequeue
        return nil unless @head

        value = @head.value
        @head = @head.next_node
        @tail = nil unless @head
        @size -= 1
        value
      end

      def peek
        @head&.value
      end

      def empty?
        @size.zero?
      end

      def size
        @size
      end

      def clear
        @head = nil
        @tail = nil
        @size = 0
        self
      end
    end
  end
end

# frozen_string_literal: true

module DSA
  module DataStructures
    # Double-ended queue supporting operations at both ends.
    # Time: O(1) for all operations
    # Space: O(n)
    class Deque
      Node = Struct.new(:value, :prev_node, :next_node)

      def initialize
        @head = nil
        @tail = nil
        @size = 0
      end

      def push_front(value)
        node = Node.new(value, nil, @head)
        if @head
          @head.prev_node = node
        else
          @tail = node
        end
        @head = node
        @size += 1
        self
      end

      def push_back(value)
        node = Node.new(value, @tail, nil)
        if @tail
          @tail.next_node = node
        else
          @head = node
        end
        @tail = node
        @size += 1
        self
      end

      def pop_front
        return nil unless @head

        value = @head.value
        @head = @head.next_node
        if @head
          @head.prev_node = nil
        else
          @tail = nil
        end
        @size -= 1
        value
      end

      def pop_back
        return nil unless @tail

        value = @tail.value
        @tail = @tail.prev_node
        if @tail
          @tail.next_node = nil
        else
          @head = nil
        end
        @size -= 1
        value
      end

      def peek_front
        @head&.value
      end

      def peek_back
        @tail&.value
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

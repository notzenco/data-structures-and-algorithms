# frozen_string_literal: true

module DSA
  module DataStructures
    # Doubly linked list with bidirectional traversal.
    # Time: O(1) for prepend/append/remove_first/remove_last, O(n) for arbitrary access
    # Space: O(n)
    class DoublyLinkedList
      Node = Struct.new(:value, :prev_node, :next_node)

      def initialize
        @head = nil
        @tail = nil
        @size = 0
      end

      def prepend(value)
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

      def append(value)
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

      def remove_first
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

      def remove_last
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

      def first
        @head&.value
      end

      def last
        @tail&.value
      end

      def get(index)
        return nil if index < 0 || index >= @size

        if index < @size / 2
          current = @head
          index.times { current = current.next_node }
        else
          current = @tail
          (@size - 1 - index).times { current = current.prev_node }
        end
        current.value
      end

      def index_of(value)
        current = @head
        index = 0
        while current
          return index if current.value == value

          current = current.next_node
          index += 1
        end
        -1
      end

      def contains?(value)
        index_of(value) != -1
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

      def to_a
        result = []
        current = @head
        while current
          result << current.value
          current = current.next_node
        end
        result
      end
    end
  end
end

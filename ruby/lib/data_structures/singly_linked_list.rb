# frozen_string_literal: true

module DSA
  module DataStructures
    # Singly linked list with forward traversal.
    # Time: O(1) for prepend, O(n) for other operations
    # Space: O(n)
    class SinglyLinkedList
      Node = Struct.new(:value, :next_node)

      def initialize
        @head = nil
        @size = 0
      end

      def prepend(value)
        @head = Node.new(value, @head)
        @size += 1
        self
      end

      def append(value)
        node = Node.new(value, nil)
        if @head.nil?
          @head = node
        else
          current = @head
          current = current.next_node while current.next_node
          current.next_node = node
        end
        @size += 1
        self
      end

      def insert(index, value)
        return false if index < 0 || index > @size

        if index.zero?
          prepend(value)
          return true
        end

        current = @head
        (index - 1).times { current = current.next_node }
        current.next_node = Node.new(value, current.next_node)
        @size += 1
        true
      end

      def remove_at(index)
        return nil if index < 0 || index >= @size

        if index.zero?
          value = @head.value
          @head = @head.next_node
          @size -= 1
          return value
        end

        current = @head
        (index - 1).times { current = current.next_node }
        value = current.next_node.value
        current.next_node = current.next_node.next_node
        @size -= 1
        value
      end

      def get(index)
        return nil if index < 0 || index >= @size

        current = @head
        index.times { current = current.next_node }
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

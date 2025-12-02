# frozen_string_literal: true

module DSA
  module DataStructures
    # Binary Search Tree implementation.
    # Time: O(log n) average, O(n) worst for all operations
    # Space: O(n)
    class BinarySearchTree
      Node = Struct.new(:value, :left, :right)

      def initialize(&comparator)
        @root = nil
        @size = 0
        @comparator = comparator || ->(a, b) { a <=> b }
      end

      def insert(value)
        @root = insert_node(@root, value)
        @size += 1
        self
      end

      def contains?(value)
        !find_node(@root, value).nil?
      end

      def remove(value)
        size_before = @size
        @root = remove_node(@root, value)
        @size < size_before
      end

      def min
        return nil if @root.nil?

        find_min_node(@root).value
      end

      def max
        return nil if @root.nil?

        find_max_node(@root).value
      end

      def in_order
        result = []
        in_order_traverse(@root, result)
        result
      end

      def empty?
        @size.zero?
      end

      def size
        @size
      end

      def clear
        @root = nil
        @size = 0
        self
      end

      private

      def insert_node(node, value)
        return Node.new(value, nil, nil) if node.nil?

        cmp = @comparator.call(value, node.value)
        if cmp < 0
          node.left = insert_node(node.left, value)
        elsif cmp > 0
          node.right = insert_node(node.right, value)
        end
        node
      end

      def find_node(node, value)
        return nil if node.nil?

        cmp = @comparator.call(value, node.value)
        if cmp < 0
          find_node(node.left, value)
        elsif cmp > 0
          find_node(node.right, value)
        else
          node
        end
      end

      def remove_node(node, value)
        return nil if node.nil?

        cmp = @comparator.call(value, node.value)
        if cmp < 0
          node.left = remove_node(node.left, value)
        elsif cmp > 0
          node.right = remove_node(node.right, value)
        else
          @size -= 1

          return node.right if node.left.nil?
          return node.left if node.right.nil?

          min_right = find_min_node(node.right)
          node.value = min_right.value
          node.right = remove_node(node.right, min_right.value)
          @size += 1 # Compensate for recursive call
        end
        node
      end

      def find_min_node(node)
        current = node
        current = current.left while current.left
        current
      end

      def find_max_node(node)
        current = node
        current = current.right while current.right
        current
      end

      def in_order_traverse(node, result)
        return if node.nil?

        in_order_traverse(node.left, result)
        result << node.value
        in_order_traverse(node.right, result)
      end
    end
  end
end

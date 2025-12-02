# frozen_string_literal: true

require 'minitest/autorun'
require_relative '../lib/data_structures/stack'
require_relative '../lib/data_structures/queue'
require_relative '../lib/data_structures/dynamic_array'
require_relative '../lib/data_structures/singly_linked_list'
require_relative '../lib/data_structures/doubly_linked_list'
require_relative '../lib/data_structures/deque'
require_relative '../lib/data_structures/hash_table'
require_relative '../lib/data_structures/binary_search_tree'
require_relative '../lib/data_structures/min_heap'
require_relative '../lib/data_structures/disjoint_set'

class DataStructuresTest < Minitest::Test
  # Stack Tests
  def test_stack_operations
    stack = DSA::DataStructures::Stack.new
    assert stack.empty?

    stack.push(1).push(2).push(3)

    assert_equal 3, stack.size
    assert_equal 3, stack.peek
    assert_equal 3, stack.pop
    assert_equal 2, stack.pop
    assert_equal 1, stack.size

    stack.clear
    assert stack.empty?
  end

  # Queue Tests
  def test_queue_operations
    queue = DSA::DataStructures::Queue.new
    assert queue.empty?

    queue.enqueue(1).enqueue(2).enqueue(3)

    assert_equal 3, queue.size
    assert_equal 1, queue.peek
    assert_equal 1, queue.dequeue
    assert_equal 2, queue.dequeue
    assert_equal 1, queue.size
  end

  # DynamicArray Tests
  def test_dynamic_array_operations
    arr = DSA::DataStructures::DynamicArray.new
    assert arr.empty?

    arr.push(10).push(20).push(30)

    assert_equal 3, arr.size
    assert_equal 10, arr.get(0)
    assert_equal 30, arr.get(2)

    assert arr.set(1, 25)
    assert_equal 25, arr.get(1)

    assert_equal 30, arr.pop
    assert_equal 2, arr.size

    assert arr.insert(1, 15)
    assert_equal [10, 15, 25], arr.to_a

    assert_equal 15, arr.remove_at(1)
    assert_equal [10, 25], arr.to_a
  end

  # SinglyLinkedList Tests
  def test_singly_linked_list_operations
    list = DSA::DataStructures::SinglyLinkedList.new
    assert list.empty?

    list.append(1).append(2).prepend(0)

    assert_equal 3, list.size
    assert_equal 0, list.get(0)
    assert_equal 2, list.get(2)

    assert list.insert(1, 10)
    assert_equal [0, 10, 1, 2], list.to_a

    assert_equal 10, list.remove_at(1)
    assert_equal [0, 1, 2], list.to_a

    assert list.contains?(1)
    refute list.contains?(100)
  end

  # DoublyLinkedList Tests
  def test_doubly_linked_list_operations
    list = DSA::DataStructures::DoublyLinkedList.new
    assert list.empty?

    list.append(1).append(2).prepend(0)

    assert_equal 3, list.size
    assert_equal 0, list.first
    assert_equal 2, list.last

    assert_equal 0, list.remove_first
    assert_equal 2, list.remove_last
    assert_equal 1, list.size
  end

  # Deque Tests
  def test_deque_operations
    deque = DSA::DataStructures::Deque.new
    assert deque.empty?

    deque.push_back(2).push_front(1).push_back(3)

    assert_equal 3, deque.size
    assert_equal 1, deque.peek_front
    assert_equal 3, deque.peek_back

    assert_equal 1, deque.pop_front
    assert_equal 3, deque.pop_back
    assert_equal 1, deque.size
  end

  # HashTable Tests
  def test_hash_table_operations
    table = DSA::DataStructures::HashTable.new
    assert table.empty?

    table.put('one', 1).put('two', 2).put('three', 3)

    assert_equal 3, table.size
    assert_equal 1, table.get('one')
    assert_equal 2, table.get('two')
    assert_nil table.get('four')

    assert table.contains?('one')
    refute table.contains?('four')

    table.put('one', 100)
    assert_equal 100, table.get('one')

    assert_equal 2, table.remove('two')
    assert_equal 2, table.size
    refute table.contains?('two')
  end

  # BinarySearchTree Tests
  def test_binary_search_tree_operations
    bst = DSA::DataStructures::BinarySearchTree.new
    assert bst.empty?

    bst.insert(5).insert(3).insert(7).insert(1).insert(9)

    assert_equal 5, bst.size
    assert bst.contains?(5)
    assert bst.contains?(3)
    refute bst.contains?(100)

    assert_equal 1, bst.min
    assert_equal 9, bst.max

    assert_equal [1, 3, 5, 7, 9], bst.in_order

    assert bst.remove(3)
    refute bst.contains?(3)
    assert_equal [1, 5, 7, 9], bst.in_order
  end

  # MinHeap Tests
  def test_min_heap_operations
    heap = DSA::DataStructures::MinHeap.new
    assert heap.empty?

    heap.insert(5).insert(3).insert(7).insert(1).insert(9)

    assert_equal 5, heap.size
    assert_equal 1, heap.peek

    assert_equal 1, heap.extract_min
    assert_equal 3, heap.extract_min
    assert_equal 5, heap.extract_min
    assert_equal 7, heap.extract_min
    assert_equal 9, heap.extract_min

    assert heap.empty?
  end

  def test_min_heap_with_custom_comparator
    # Max heap using custom comparator
    heap = DSA::DataStructures::MinHeap.new { |a, b| b <=> a }

    heap.insert(5).insert(3).insert(7)

    assert_equal 7, heap.extract_min
    assert_equal 5, heap.extract_min
    assert_equal 3, heap.extract_min
  end

  # DisjointSet Tests
  def test_disjoint_set_operations
    ds = DSA::DataStructures::DisjointSet.new(5)

    assert_equal 5, ds.count
    refute ds.connected?(0, 1)

    assert ds.union(0, 1)
    assert ds.connected?(0, 1)
    assert_equal 4, ds.count

    assert ds.union(2, 3)
    assert ds.union(0, 2)

    assert ds.connected?(0, 3)
    assert ds.connected?(1, 2)
    assert_equal 2, ds.count

    refute ds.connected?(0, 4)
  end
end

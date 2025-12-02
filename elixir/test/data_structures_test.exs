defmodule DataStructuresTest do
  use ExUnit.Case, async: true

  alias DataStructures.{
    Stack,
    Queue,
    DynamicArray,
    SinglyLinkedList,
    DoublyLinkedList,
    Deque,
    HashTable,
    BinarySearchTree,
    MinHeap,
    DisjointSet
  }

  # Stack Tests
  describe "Stack" do
    test "push and pop work correctly" do
      stack =
        Stack.new()
        |> Stack.push(1)
        |> Stack.push(2)
        |> Stack.push(3)

      assert {:ok, 3, stack} = Stack.pop(stack)
      assert {:ok, 2, stack} = Stack.pop(stack)
      assert {:ok, 1, _stack} = Stack.pop(stack)
    end

    test "peek returns top element without removing" do
      stack = Stack.new() |> Stack.push(42)
      assert {:ok, 42} = Stack.peek(stack)
      assert Stack.size(stack) == 1
    end

    test "empty? returns correct value" do
      assert Stack.empty?(Stack.new())
      refute Stack.empty?(Stack.new() |> Stack.push(1))
    end
  end

  # Queue Tests
  describe "Queue" do
    test "enqueue and dequeue work correctly" do
      queue =
        Queue.new()
        |> Queue.enqueue(1)
        |> Queue.enqueue(2)
        |> Queue.enqueue(3)

      assert {:ok, 1, queue} = Queue.dequeue(queue)
      assert {:ok, 2, queue} = Queue.dequeue(queue)
      assert {:ok, 3, _queue} = Queue.dequeue(queue)
    end

    test "maintains FIFO order" do
      queue =
        Queue.new()
        |> Queue.enqueue(1)
        |> Queue.enqueue(2)
        |> Queue.enqueue(3)

      assert Queue.to_list(queue) == [1, 2, 3]
    end
  end

  # DynamicArray Tests
  describe "DynamicArray" do
    test "push and pop work correctly" do
      arr =
        DynamicArray.new()
        |> DynamicArray.push(1)
        |> DynamicArray.push(2)
        |> DynamicArray.push(3)

      assert {:ok, 3, arr} = DynamicArray.pop(arr)
      assert DynamicArray.size(arr) == 2
    end

    test "get and set work correctly" do
      arr = DynamicArray.from_list([1, 2, 3])
      assert {:ok, 2} = DynamicArray.get(arr, 1)
      assert {:ok, updated} = DynamicArray.set(arr, 1, 42)
      assert {:ok, 42} = DynamicArray.get(updated, 1)
    end
  end

  # SinglyLinkedList Tests
  describe "SinglyLinkedList" do
    test "prepend and append work correctly" do
      list =
        SinglyLinkedList.new()
        |> SinglyLinkedList.prepend(2)
        |> SinglyLinkedList.prepend(1)
        |> SinglyLinkedList.append(3)

      assert SinglyLinkedList.to_list(list) == [1, 2, 3]
    end

    test "index_of finds elements" do
      list = SinglyLinkedList.from_list([10, 20, 30])
      assert SinglyLinkedList.index_of(list, 20) == 1
      assert SinglyLinkedList.index_of(list, 100) == nil
    end
  end

  # DoublyLinkedList Tests
  describe "DoublyLinkedList" do
    test "prepend and append work correctly" do
      list =
        DoublyLinkedList.new()
        |> DoublyLinkedList.prepend(2)
        |> DoublyLinkedList.prepend(1)
        |> DoublyLinkedList.append(3)

      assert DoublyLinkedList.to_list(list) == [1, 2, 3]
    end

    test "first and last work correctly" do
      list = DoublyLinkedList.from_list([1, 2, 3])
      assert {:ok, 1} = DoublyLinkedList.first(list)
      assert {:ok, 3} = DoublyLinkedList.last(list)
    end
  end

  # Deque Tests
  describe "Deque" do
    test "push_front and push_back work correctly" do
      deque =
        Deque.new()
        |> Deque.push_front(2)
        |> Deque.push_front(1)
        |> Deque.push_back(3)

      assert Deque.to_list(deque) == [1, 2, 3]
    end

    test "pop_front and pop_back work correctly" do
      deque = Deque.from_list([1, 2, 3])
      assert {:ok, 1, _} = Deque.pop_front(deque)
      assert {:ok, 3, _} = Deque.pop_back(deque)
    end
  end

  # HashTable Tests
  describe "HashTable" do
    test "put and get work correctly" do
      table =
        HashTable.new()
        |> HashTable.put("one", 1)
        |> HashTable.put("two", 2)

      assert {:ok, 1} = HashTable.get(table, "one")
      assert {:error, :not_found} = HashTable.get(table, "three")
    end

    test "remove works correctly" do
      table = HashTable.new() |> HashTable.put("key", 42)
      {value, updated} = HashTable.remove(table, "key")
      assert value == 42
      assert {:error, :not_found} = HashTable.get(updated, "key")
    end
  end

  # BinarySearchTree Tests
  describe "BinarySearchTree" do
    test "insert and contains? work correctly" do
      bst = BinarySearchTree.from_list([5, 3, 7, 1, 9])
      assert BinarySearchTree.contains?(bst, 5)
      assert BinarySearchTree.contains?(bst, 3)
      refute BinarySearchTree.contains?(bst, 10)
    end

    test "find_min and find_max work correctly" do
      bst = BinarySearchTree.from_list([5, 3, 7, 1, 9])
      assert {:ok, 1} = BinarySearchTree.find_min(bst)
      assert {:ok, 9} = BinarySearchTree.find_max(bst)
    end

    test "inorder traversal returns sorted list" do
      bst = BinarySearchTree.from_list([5, 3, 7, 1, 9])
      assert BinarySearchTree.inorder(bst) == [1, 3, 5, 7, 9]
    end
  end

  # MinHeap Tests
  describe "MinHeap" do
    test "insert and extract_min work correctly" do
      heap = MinHeap.from_list([5, 3, 7, 1, 9])
      assert {:ok, 1, heap} = MinHeap.extract_min(heap)
      assert {:ok, 3, _heap} = MinHeap.extract_min(heap)
    end

    test "to_list returns sorted order" do
      heap = MinHeap.from_list([5, 3, 7, 1, 9])
      assert MinHeap.to_list(heap) == [1, 3, 5, 7, 9]
    end

    test "peek returns minimum without removing" do
      heap = MinHeap.from_list([5, 3, 7])
      assert {:ok, 3} = MinHeap.peek(heap)
      assert MinHeap.size(heap) == 3
    end
  end

  # DisjointSet Tests
  describe "DisjointSet" do
    test "make_set and find work correctly" do
      ds =
        DisjointSet.new()
        |> DisjointSet.make_set(1)
        |> DisjointSet.make_set(2)

      assert {:ok, 1, _} = DisjointSet.find(ds, 1)
      assert {:error, :not_found} = DisjointSet.find(ds, 3)
    end

    test "union and connected? work correctly" do
      ds =
        DisjointSet.new()
        |> DisjointSet.make_set(1)
        |> DisjointSet.make_set(2)
        |> DisjointSet.make_set(3)

      refute DisjointSet.connected?(ds, 1, 2)

      {:ok, ds} = DisjointSet.union(ds, 1, 2)
      assert DisjointSet.connected?(ds, 1, 2)
      refute DisjointSet.connected?(ds, 1, 3)
    end

    test "set_count tracks disjoint sets" do
      ds =
        DisjointSet.new()
        |> DisjointSet.make_set(1)
        |> DisjointSet.make_set(2)
        |> DisjointSet.make_set(3)
        |> DisjointSet.make_set(4)

      assert DisjointSet.set_count(ds) == 4

      {:ok, ds} = DisjointSet.union(ds, 1, 2)
      assert DisjointSet.set_count(ds) == 3

      {:ok, ds} = DisjointSet.union(ds, 3, 4)
      assert DisjointSet.set_count(ds) == 2
    end
  end
end

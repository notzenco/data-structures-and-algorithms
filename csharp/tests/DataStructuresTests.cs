using DSA.DataStructures;
using Xunit;

namespace DSA.Tests;

public class StackTests
{
    [Fact]
    public void BasicOperations()
    {
        var stack = new Stack<int>();
        Assert.True(stack.IsEmpty);
        Assert.Equal(0, stack.Count);

        stack.Push(1);
        stack.Push(2);
        stack.Push(3);

        Assert.Equal(3, stack.Count);
        Assert.False(stack.IsEmpty);
        Assert.Equal(3, stack.Peek());
        Assert.Equal(3, stack.Pop());
        Assert.Equal(2, stack.Pop());
    }
}

public class QueueTests
{
    [Fact]
    public void BasicOperations()
    {
        var queue = new Queue<int>();
        Assert.True(queue.IsEmpty);

        queue.Enqueue(1);
        queue.Enqueue(2);
        queue.Enqueue(3);

        Assert.Equal(3, queue.Count);
        Assert.Equal(1, queue.Peek());
        Assert.Equal(1, queue.Dequeue());
        Assert.Equal(2, queue.Dequeue());
    }
}

public class DynamicArrayTests
{
    [Fact]
    public void BasicOperations()
    {
        var arr = new DynamicArray<int>();
        Assert.True(arr.IsEmpty);

        arr.Push(10);
        arr.Push(20);
        arr.Push(30);

        Assert.Equal(3, arr.Count);
        Assert.Equal(10, arr.Get(0));
        Assert.Equal(30, arr.Get(2));
    }

    [Fact]
    public void Growth()
    {
        var arr = new DynamicArray<int>(2);
        for (int i = 0; i < 100; i++)
            arr.Push(i);

        Assert.Equal(100, arr.Count);
        Assert.Equal(99, arr.Get(99));
    }
}

public class LinkedListTests
{
    [Fact]
    public void SinglyLinkedListBasicOperations()
    {
        var list = new SinglyLinkedList<int>();
        Assert.True(list.IsEmpty);

        list.PushFront(1);
        list.PushFront(2);
        list.PushBack(3);

        Assert.Equal(3, list.Count);
        Assert.Equal(2, list.PeekFront());
        Assert.Equal(3, list.PeekBack());
        Assert.True(list.Contains(1));
    }

    [Fact]
    public void DoublyLinkedListBasicOperations()
    {
        var list = new DoublyLinkedList<int>();
        list.PushFront(1);
        list.PushFront(2);
        list.PushBack(3);

        Assert.Equal(3, list.Count);
        Assert.Equal(2, list.PopFront());
        Assert.Equal(3, list.PopBack());
    }
}

public class DequeTests
{
    [Fact]
    public void BasicOperations()
    {
        var deque = new Deque<int>();
        deque.PushFront(1);
        deque.PushBack(2);
        deque.PushFront(0);

        Assert.Equal(3, deque.Count);
        Assert.Equal(0, deque.PeekFront());
        Assert.Equal(2, deque.PeekBack());
    }
}

public class HashTableTests
{
    [Fact]
    public void BasicOperations()
    {
        var table = new HashTable<string, int>();
        Assert.True(table.IsEmpty);

        table.Put("one", 1);
        table.Put("two", 2);
        table.Put("three", 3);

        Assert.Equal(3, table.Count);
        Assert.Equal(1, table.Get("one"));
        Assert.True(table.ContainsKey("three"));
    }

    [Fact]
    public void CollisionHandling()
    {
        var table = new HashTable<int, string>(4);
        for (int i = 0; i < 20; i++)
            table.Put(i, $"value{i}");

        Assert.Equal(20, table.Count);
        for (int i = 0; i < 20; i++)
            Assert.Equal($"value{i}", table.Get(i));
    }
}

public class BinarySearchTreeTests
{
    [Fact]
    public void BasicOperations()
    {
        var bst = new BinarySearchTree<int>();
        bst.Insert(5);
        bst.Insert(3);
        bst.Insert(7);
        bst.Insert(1);
        bst.Insert(9);

        Assert.Equal(5, bst.Count);
        Assert.True(bst.Contains(3));
        Assert.False(bst.Contains(4));
    }

    [Fact]
    public void InOrderTraversal()
    {
        var bst = new BinarySearchTree<int>();
        bst.Insert(5);
        bst.Insert(3);
        bst.Insert(7);
        bst.Insert(1);
        bst.Insert(9);

        var inorder = bst.InOrder();
        Assert.Equal(new[] { 1, 3, 5, 7, 9 }, inorder);
    }
}

public class MinHeapTests
{
    [Fact]
    public void BasicOperations()
    {
        var heap = new MinHeap<int>();
        heap.Insert(5);
        heap.Insert(3);
        heap.Insert(7);
        heap.Insert(1);

        Assert.Equal(4, heap.Count);
        Assert.Equal(1, heap.Peek());
        Assert.Equal(1, heap.ExtractMin());
        Assert.Equal(3, heap.Peek());
    }

    [Fact]
    public void MaintainsOrder()
    {
        var heap = new MinHeap<int>();
        int[] values = { 9, 4, 7, 1, 8, 2, 6, 3, 5 };
        foreach (var v in values)
            heap.Insert(v);

        int prev = int.MinValue;
        while (!heap.IsEmpty)
        {
            int current = heap.ExtractMin()!.Value;
            Assert.True(current >= prev);
            prev = current;
        }
    }
}

public class DisjointSetTests
{
    [Fact]
    public void BasicOperations()
    {
        var ds = new DisjointSet(5);
        Assert.False(ds.Connected(0, 1));
        ds.Union(0, 1);
        Assert.True(ds.Connected(0, 1));
    }

    [Fact]
    public void ComponentCount()
    {
        var ds = new DisjointSet(5);
        Assert.Equal(5, ds.Count);

        ds.Union(0, 1);
        Assert.Equal(4, ds.Count);

        ds.Union(2, 3);
        Assert.Equal(3, ds.Count);

        ds.Union(0, 2);
        Assert.Equal(2, ds.Count);
    }
}

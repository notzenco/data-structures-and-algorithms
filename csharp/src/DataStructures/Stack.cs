namespace DSA.DataStructures;

/// <summary>
/// Stack implementation using a linked list.
/// Time: O(1) for all operations
/// Space: O(n)
/// </summary>
public class Stack<T>
{
    private class Node
    {
        public T Value { get; }
        public Node? Next { get; set; }

        public Node(T value, Node? next = null)
        {
            Value = value;
            Next = next;
        }
    }

    private Node? _head;
    private int _size;

    public int Count => _size;
    public bool IsEmpty => _size == 0;

    public void Push(T value)
    {
        _head = new Node(value, _head);
        _size++;
    }

    public T? Pop()
    {
        if (_head == null)
            return default;

        var value = _head.Value;
        _head = _head.Next;
        _size--;
        return value;
    }

    public T? Peek()
    {
        return _head == null ? default : _head.Value;
    }

    public void Clear()
    {
        _head = null;
        _size = 0;
    }
}

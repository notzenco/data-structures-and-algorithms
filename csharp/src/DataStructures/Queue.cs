namespace DSA.DataStructures;

/// <summary>
/// Queue implementation using a linked list.
/// Time: O(1) for all operations
/// Space: O(n)
/// </summary>
public class Queue<T>
{
    private class Node
    {
        public T Value { get; }
        public Node? Next { get; set; }

        public Node(T value)
        {
            Value = value;
        }
    }

    private Node? _head;
    private Node? _tail;
    private int _size;

    public int Count => _size;
    public bool IsEmpty => _size == 0;

    public void Enqueue(T value)
    {
        var node = new Node(value);
        if (_tail != null)
        {
            _tail.Next = node;
        }
        else
        {
            _head = node;
        }
        _tail = node;
        _size++;
    }

    public T? Dequeue()
    {
        if (_head == null)
            return default;

        var value = _head.Value;
        _head = _head.Next;
        if (_head == null)
            _tail = null;
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
        _tail = null;
        _size = 0;
    }
}

namespace DSA.DataStructures;

/// <summary>
/// Singly linked list implementation.
/// Time: O(1) front ops, O(n) back ops and search
/// Space: O(n)
/// </summary>
public class SinglyLinkedList<T>
{
    private class Node
    {
        public T Value { get; set; }
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

    public void PushFront(T value)
    {
        var node = new Node(value) { Next = _head };
        _head = node;
        _tail ??= node;
        _size++;
    }

    public void PushBack(T value)
    {
        var node = new Node(value);
        if (_tail != null)
            _tail.Next = node;
        else
            _head = node;
        _tail = node;
        _size++;
    }

    public T? PopFront()
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

    public T? PeekFront() => _head == null ? default : _head.Value;
    public T? PeekBack() => _tail == null ? default : _tail.Value;

    public bool Contains(T value)
    {
        var current = _head;
        while (current != null)
        {
            if (EqualityComparer<T>.Default.Equals(current.Value, value))
                return true;
            current = current.Next;
        }
        return false;
    }

    public bool Remove(T value)
    {
        if (_head == null)
            return false;

        if (EqualityComparer<T>.Default.Equals(_head.Value, value))
        {
            _head = _head.Next;
            if (_head == null)
                _tail = null;
            _size--;
            return true;
        }

        var current = _head;
        while (current.Next != null)
        {
            if (EqualityComparer<T>.Default.Equals(current.Next.Value, value))
            {
                if (current.Next == _tail)
                    _tail = current;
                current.Next = current.Next.Next;
                _size--;
                return true;
            }
            current = current.Next;
        }
        return false;
    }

    public List<T> ToList()
    {
        var result = new List<T>();
        var current = _head;
        while (current != null)
        {
            result.Add(current.Value);
            current = current.Next;
        }
        return result;
    }

    public void Clear()
    {
        _head = null;
        _tail = null;
        _size = 0;
    }
}

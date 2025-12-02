namespace DSA.DataStructures;

/// <summary>
/// Dynamic array implementation with automatic resizing.
/// Time: O(1) amortized push, O(n) insert/remove
/// Space: O(n)
/// </summary>
public class DynamicArray<T>
{
    private T[] _data;
    private int _size;

    public int Count => _size;
    public int Capacity => _data.Length;
    public bool IsEmpty => _size == 0;

    public DynamicArray(int initialCapacity = 8)
    {
        _data = new T[Math.Max(1, initialCapacity)];
        _size = 0;
    }

    public void Push(T value)
    {
        EnsureCapacity(_size + 1);
        _data[_size++] = value;
    }

    public T? Pop()
    {
        if (_size == 0)
            return default;

        var value = _data[--_size];
        _data[_size] = default!;

        if (_size > 0 && _size == _data.Length / 4)
            Resize(_data.Length / 2);

        return value;
    }

    public T? Get(int index)
    {
        if (index < 0 || index >= _size)
            return default;
        return _data[index];
    }

    public bool Set(int index, T value)
    {
        if (index < 0 || index >= _size)
            return false;
        _data[index] = value;
        return true;
    }

    public bool Insert(int index, T value)
    {
        if (index < 0 || index > _size)
            return false;

        EnsureCapacity(_size + 1);

        for (int i = _size; i > index; i--)
            _data[i] = _data[i - 1];

        _data[index] = value;
        _size++;
        return true;
    }

    public T? RemoveAt(int index)
    {
        if (index < 0 || index >= _size)
            return default;

        var value = _data[index];

        for (int i = index; i < _size - 1; i++)
            _data[i] = _data[i + 1];

        _data[--_size] = default!;

        if (_size > 0 && _size == _data.Length / 4)
            Resize(_data.Length / 2);

        return value;
    }

    public int IndexOf(T value)
    {
        for (int i = 0; i < _size; i++)
        {
            if (EqualityComparer<T>.Default.Equals(_data[i], value))
                return i;
        }
        return -1;
    }

    public bool Contains(T value) => IndexOf(value) != -1;

    public T[] ToArray()
    {
        var result = new T[_size];
        Array.Copy(_data, result, _size);
        return result;
    }

    public void Clear()
    {
        Array.Clear(_data, 0, _size);
        _size = 0;
    }

    private void EnsureCapacity(int minCapacity)
    {
        if (minCapacity > _data.Length)
            Resize(Math.Max(_data.Length * 2, minCapacity));
    }

    private void Resize(int newCapacity)
    {
        var newData = new T[newCapacity];
        Array.Copy(_data, newData, _size);
        _data = newData;
    }
}

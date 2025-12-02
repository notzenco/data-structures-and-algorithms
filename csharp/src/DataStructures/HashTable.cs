namespace DSA.DataStructures;

/// <summary>
/// Hash table implementation with open addressing and linear probing.
/// Time: O(1) average for all operations
/// Space: O(n)
/// </summary>
public class HashTable<TKey, TValue> where TKey : notnull
{
    private class Entry
    {
        public TKey Key { get; }
        public TValue Value { get; set; }
        public bool Deleted { get; set; }

        public Entry(TKey key, TValue value)
        {
            Key = key;
            Value = value;
        }
    }

    private Entry?[] _buckets;
    private int _size;
    private const double LoadFactorThreshold = 0.7;

    public int Count => _size;
    public bool IsEmpty => _size == 0;

    public HashTable(int initialCapacity = 16)
    {
        _buckets = new Entry?[Math.Max(1, initialCapacity)];
    }

    public void Put(TKey key, TValue value)
    {
        if (_size >= _buckets.Length * LoadFactorThreshold)
            Resize(_buckets.Length * 2);

        var index = GetIndex(key);
        var firstDeleted = -1;

        for (int i = 0; i < _buckets.Length; i++)
        {
            var entry = _buckets[index];

            if (entry == null)
            {
                var insertIndex = firstDeleted != -1 ? firstDeleted : index;
                _buckets[insertIndex] = new Entry(key, value);
                _size++;
                return;
            }

            if (entry.Deleted && firstDeleted == -1)
            {
                firstDeleted = index;
            }
            else if (!entry.Deleted && EqualityComparer<TKey>.Default.Equals(entry.Key, key))
            {
                entry.Value = value;
                return;
            }

            index = (index + 1) % _buckets.Length;
        }

        if (firstDeleted != -1)
        {
            _buckets[firstDeleted] = new Entry(key, value);
            _size++;
        }
    }

    public TValue? Get(TKey key)
    {
        var index = FindIndex(key);
        return index == -1 ? default : _buckets[index]!.Value;
    }

    public bool TryGet(TKey key, out TValue? value)
    {
        var index = FindIndex(key);
        if (index == -1)
        {
            value = default;
            return false;
        }
        value = _buckets[index]!.Value;
        return true;
    }

    public TValue? Remove(TKey key)
    {
        var index = FindIndex(key);
        if (index == -1)
            return default;

        var entry = _buckets[index]!;
        var value = entry.Value;
        entry.Deleted = true;
        _size--;
        return value;
    }

    public bool ContainsKey(TKey key) => FindIndex(key) != -1;

    public List<TKey> Keys()
    {
        var result = new List<TKey>();
        foreach (var entry in _buckets)
        {
            if (entry != null && !entry.Deleted)
                result.Add(entry.Key);
        }
        return result;
    }

    public List<TValue> Values()
    {
        var result = new List<TValue>();
        foreach (var entry in _buckets)
        {
            if (entry != null && !entry.Deleted)
                result.Add(entry.Value);
        }
        return result;
    }

    public void Clear()
    {
        Array.Clear(_buckets);
        _size = 0;
    }

    private int GetIndex(TKey key) => Math.Abs(key.GetHashCode()) % _buckets.Length;

    private int FindIndex(TKey key)
    {
        var index = GetIndex(key);

        for (int i = 0; i < _buckets.Length; i++)
        {
            var entry = _buckets[index];

            if (entry == null)
                return -1;

            if (!entry.Deleted && EqualityComparer<TKey>.Default.Equals(entry.Key, key))
                return index;

            index = (index + 1) % _buckets.Length;
        }

        return -1;
    }

    private void Resize(int newCapacity)
    {
        var oldBuckets = _buckets;
        _buckets = new Entry?[newCapacity];
        _size = 0;

        foreach (var entry in oldBuckets)
        {
            if (entry != null && !entry.Deleted)
                Put(entry.Key, entry.Value);
        }
    }
}

namespace DSA.DataStructures;

/// <summary>
/// Binary min-heap implementation.
/// Time: O(log n) insert/extract, O(1) peek
/// Space: O(n)
/// </summary>
public class MinHeap<T>
{
    private readonly List<T> _heap = new();
    private readonly IComparer<T> _comparer;

    public int Count => _heap.Count;
    public bool IsEmpty => _heap.Count == 0;

    public MinHeap(IComparer<T>? comparer = null)
    {
        _comparer = comparer ?? Comparer<T>.Default;
    }

    public void Insert(T value)
    {
        _heap.Add(value);
        SiftUp(_heap.Count - 1);
    }

    public T? ExtractMin()
    {
        if (_heap.Count == 0)
            return default;

        var min = _heap[0];
        var last = _heap[^1];
        _heap.RemoveAt(_heap.Count - 1);

        if (_heap.Count > 0)
        {
            _heap[0] = last;
            SiftDown(0);
        }

        return min;
    }

    public T? Peek() => _heap.Count == 0 ? default : _heap[0];

    public void Clear() => _heap.Clear();

    public T[] ToArray() => _heap.ToArray();

    private void SiftUp(int index)
    {
        while (index > 0)
        {
            var parentIndex = (index - 1) / 2;
            if (_comparer.Compare(_heap[index], _heap[parentIndex]) >= 0)
                break;

            Swap(index, parentIndex);
            index = parentIndex;
        }
    }

    private void SiftDown(int index)
    {
        while (true)
        {
            var leftChild = 2 * index + 1;
            var rightChild = 2 * index + 2;
            var smallest = index;

            if (leftChild < _heap.Count && _comparer.Compare(_heap[leftChild], _heap[smallest]) < 0)
                smallest = leftChild;

            if (rightChild < _heap.Count && _comparer.Compare(_heap[rightChild], _heap[smallest]) < 0)
                smallest = rightChild;

            if (smallest == index)
                break;

            Swap(index, smallest);
            index = smallest;
        }
    }

    private void Swap(int i, int j)
    {
        (_heap[i], _heap[j]) = (_heap[j], _heap[i]);
    }
}

namespace DSA.DataStructures;

/// <summary>
/// Binary Search Tree implementation.
/// Time: O(log n) average, O(n) worst for all operations
/// Space: O(n)
/// </summary>
public class BinarySearchTree<T>
{
    private class Node
    {
        public T Value { get; set; }
        public Node? Left { get; set; }
        public Node? Right { get; set; }

        public Node(T value)
        {
            Value = value;
        }
    }

    private Node? _root;
    private int _size;
    private readonly IComparer<T> _comparer;

    public int Count => _size;
    public bool IsEmpty => _size == 0;

    public BinarySearchTree(IComparer<T>? comparer = null)
    {
        _comparer = comparer ?? Comparer<T>.Default;
    }

    public void Insert(T value)
    {
        _root = InsertNode(_root, value);
        _size++;
    }

    private Node InsertNode(Node? node, T value)
    {
        if (node == null)
            return new Node(value);

        var cmp = _comparer.Compare(value, node.Value);
        if (cmp < 0)
            node.Left = InsertNode(node.Left, value);
        else if (cmp > 0)
            node.Right = InsertNode(node.Right, value);

        return node;
    }

    public bool Contains(T value) => FindNode(_root, value) != null;

    private Node? FindNode(Node? node, T value)
    {
        if (node == null)
            return null;

        var cmp = _comparer.Compare(value, node.Value);
        if (cmp < 0)
            return FindNode(node.Left, value);
        if (cmp > 0)
            return FindNode(node.Right, value);
        return node;
    }

    public bool Remove(T value)
    {
        var sizeBefore = _size;
        _root = RemoveNode(_root, value);
        return _size < sizeBefore;
    }

    private Node? RemoveNode(Node? node, T value)
    {
        if (node == null)
            return null;

        var cmp = _comparer.Compare(value, node.Value);
        if (cmp < 0)
        {
            node.Left = RemoveNode(node.Left, value);
        }
        else if (cmp > 0)
        {
            node.Right = RemoveNode(node.Right, value);
        }
        else
        {
            _size--;

            if (node.Left == null)
                return node.Right;
            if (node.Right == null)
                return node.Left;

            var minRight = FindMin(node.Right);
            node.Value = minRight.Value;
            node.Right = RemoveNode(node.Right, minRight.Value);
            _size++; // Compensate for recursive remove
        }

        return node;
    }

    private Node FindMin(Node node)
    {
        while (node.Left != null)
            node = node.Left;
        return node;
    }

    private Node FindMax(Node node)
    {
        while (node.Right != null)
            node = node.Right;
        return node;
    }

    public T? Min() => _root == null ? default : FindMin(_root).Value;
    public T? Max() => _root == null ? default : FindMax(_root).Value;

    public List<T> InOrder()
    {
        var result = new List<T>();
        InOrderTraverse(_root, result);
        return result;
    }

    private void InOrderTraverse(Node? node, List<T> result)
    {
        if (node == null) return;
        InOrderTraverse(node.Left, result);
        result.Add(node.Value);
        InOrderTraverse(node.Right, result);
    }

    public List<T> PreOrder()
    {
        var result = new List<T>();
        PreOrderTraverse(_root, result);
        return result;
    }

    private void PreOrderTraverse(Node? node, List<T> result)
    {
        if (node == null) return;
        result.Add(node.Value);
        PreOrderTraverse(node.Left, result);
        PreOrderTraverse(node.Right, result);
    }

    public List<T> PostOrder()
    {
        var result = new List<T>();
        PostOrderTraverse(_root, result);
        return result;
    }

    private void PostOrderTraverse(Node? node, List<T> result)
    {
        if (node == null) return;
        PostOrderTraverse(node.Left, result);
        PostOrderTraverse(node.Right, result);
        result.Add(node.Value);
    }

    public void Clear()
    {
        _root = null;
        _size = 0;
    }
}

namespace DSA.DataStructures;

/// <summary>
/// Disjoint Set (Union-Find) implementation with path compression and union by rank.
/// Time: O(α(n)) amortized for all operations (nearly constant)
/// Space: O(n)
/// </summary>
public class DisjointSet
{
    private readonly int[] _parent;
    private readonly int[] _rank;
    private int _count;

    public int Count => _count;
    public int Size => _parent.Length;

    public DisjointSet(int size)
    {
        _parent = new int[size];
        _rank = new int[size];
        _count = size;

        for (int i = 0; i < size; i++)
            _parent[i] = i;
    }

    public int Find(int x)
    {
        if (_parent[x] != x)
            _parent[x] = Find(_parent[x]); // Path compression
        return _parent[x];
    }

    public bool Union(int x, int y)
    {
        var rootX = Find(x);
        var rootY = Find(y);

        if (rootX == rootY)
            return false;

        // Union by rank
        if (_rank[rootX] < _rank[rootY])
        {
            _parent[rootX] = rootY;
        }
        else if (_rank[rootX] > _rank[rootY])
        {
            _parent[rootY] = rootX;
        }
        else
        {
            _parent[rootY] = rootX;
            _rank[rootX]++;
        }

        _count--;
        return true;
    }

    public bool Connected(int x, int y) => Find(x) == Find(y);
}

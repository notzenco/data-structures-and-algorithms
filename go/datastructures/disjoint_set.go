package datastructures

// DisjointSet implements Union-Find with path compression and union by rank.
// Time: O(α(n)) amortized for all operations (nearly constant)
// Space: O(n)
type DisjointSet struct {
	parent []int
	rank   []int
	count  int
}

func NewDisjointSet(size int) *DisjointSet {
	parent := make([]int, size)
	rank := make([]int, size)
	for i := 0; i < size; i++ {
		parent[i] = i
	}
	return &DisjointSet{
		parent: parent,
		rank:   rank,
		count:  size,
	}
}

func (d *DisjointSet) Find(x int) int {
	if d.parent[x] != x {
		d.parent[x] = d.Find(d.parent[x]) // Path compression
	}
	return d.parent[x]
}

func (d *DisjointSet) Union(x, y int) bool {
	rootX := d.Find(x)
	rootY := d.Find(y)

	if rootX == rootY {
		return false
	}

	// Union by rank
	if d.rank[rootX] < d.rank[rootY] {
		d.parent[rootX] = rootY
	} else if d.rank[rootX] > d.rank[rootY] {
		d.parent[rootY] = rootX
	} else {
		d.parent[rootY] = rootX
		d.rank[rootX]++
	}

	d.count--
	return true
}

func (d *DisjointSet) Connected(x, y int) bool {
	return d.Find(x) == d.Find(y)
}

func (d *DisjointSet) Count() int {
	return d.count
}

func (d *DisjointSet) Size() int {
	return len(d.parent)
}

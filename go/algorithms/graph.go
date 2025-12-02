package algorithms

// Graph implements a graph using adjacency list.
type Graph[T comparable] struct {
	adjList  map[T][]T
	directed bool
}

// NewGraph creates a new graph.
func NewGraph[T comparable](directed bool) *Graph[T] {
	return &Graph[T]{
		adjList:  make(map[T][]T),
		directed: directed,
	}
}

// AddVertex adds a vertex to the graph.
func (g *Graph[T]) AddVertex(vertex T) {
	if _, exists := g.adjList[vertex]; !exists {
		g.adjList[vertex] = []T{}
	}
}

// AddEdge adds an edge between two vertices.
func (g *Graph[T]) AddEdge(from, to T) {
	g.AddVertex(from)
	g.AddVertex(to)
	g.adjList[from] = append(g.adjList[from], to)
	if !g.directed {
		g.adjList[to] = append(g.adjList[to], from)
	}
}

// Neighbors returns the neighbors of a vertex.
func (g *Graph[T]) Neighbors(vertex T) []T {
	if neighbors, exists := g.adjList[vertex]; exists {
		return neighbors
	}
	return []T{}
}

// HasVertex checks if a vertex exists.
func (g *Graph[T]) HasVertex(vertex T) bool {
	_, exists := g.adjList[vertex]
	return exists
}

// Vertices returns all vertices.
func (g *Graph[T]) Vertices() []T {
	result := make([]T, 0, len(g.adjList))
	for v := range g.adjList {
		result = append(result, v)
	}
	return result
}

// Size returns the number of vertices.
func (g *Graph[T]) Size() int {
	return len(g.adjList)
}

// IsDirected returns whether the graph is directed.
func (g *Graph[T]) IsDirected() bool {
	return g.directed
}

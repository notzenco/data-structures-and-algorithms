package algorithms

// BFS performs breadth-first search traversal.
// Time: O(V + E)
// Space: O(V)
func BFS[T comparable](graph *Graph[T], start T, callback func(T)) []T {
	result := []T{}

	if !graph.HasVertex(start) {
		return result
	}

	visited := make(map[T]bool)
	queue := []T{start}
	visited[start] = true

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		result = append(result, current)

		if callback != nil {
			callback(current)
		}

		for _, neighbor := range graph.Neighbors(current) {
			if !visited[neighbor] {
				visited[neighbor] = true
				queue = append(queue, neighbor)
			}
		}
	}

	return result
}

// BFSFindPath finds the shortest path between two vertices.
func BFSFindPath[T comparable](graph *Graph[T], start, end T) []T {
	if !graph.HasVertex(start) || !graph.HasVertex(end) {
		return nil
	}

	if start == end {
		return []T{start}
	}

	visited := make(map[T]bool)
	parent := make(map[T]T)
	queue := []T{start}
	visited[start] = true

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == end {
			path := []T{}
			node := end
			for node != start {
				path = append([]T{node}, path...)
				node = parent[node]
			}
			path = append([]T{start}, path...)
			return path
		}

		for _, neighbor := range graph.Neighbors(current) {
			if !visited[neighbor] {
				visited[neighbor] = true
				parent[neighbor] = current
				queue = append(queue, neighbor)
			}
		}
	}

	return nil
}

// BFSDistances returns distances from start to all reachable vertices.
func BFSDistances[T comparable](graph *Graph[T], start T) map[T]int {
	distances := make(map[T]int)

	if !graph.HasVertex(start) {
		return distances
	}

	visited := make(map[T]bool)
	queue := []struct {
		vertex   T
		distance int
	}{{start, 0}}
	visited[start] = true
	distances[start] = 0

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		for _, neighbor := range graph.Neighbors(current.vertex) {
			if !visited[neighbor] {
				visited[neighbor] = true
				distances[neighbor] = current.distance + 1
				queue = append(queue, struct {
					vertex   T
					distance int
				}{neighbor, current.distance + 1})
			}
		}
	}

	return distances
}

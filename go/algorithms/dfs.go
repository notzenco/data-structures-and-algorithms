package algorithms

// DFS performs depth-first search traversal.
// Time: O(V + E)
// Space: O(V)
func DFS[T comparable](graph *Graph[T], start T, callback func(T)) []T {
	result := []T{}

	if !graph.HasVertex(start) {
		return result
	}

	visited := make(map[T]bool)
	stack := []T{start}

	for len(stack) > 0 {
		current := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		if visited[current] {
			continue
		}

		visited[current] = true
		result = append(result, current)

		if callback != nil {
			callback(current)
		}

		neighbors := graph.Neighbors(current)
		for i := len(neighbors) - 1; i >= 0; i-- {
			if !visited[neighbors[i]] {
				stack = append(stack, neighbors[i])
			}
		}
	}

	return result
}

// DFSRecursive performs recursive depth-first search traversal.
func DFSRecursive[T comparable](graph *Graph[T], start T, callback func(T)) []T {
	result := []T{}

	if !graph.HasVertex(start) {
		return result
	}

	visited := make(map[T]bool)
	dfsRecursive(graph, start, visited, &result, callback)
	return result
}

func dfsRecursive[T comparable](graph *Graph[T], current T, visited map[T]bool, result *[]T, callback func(T)) {
	visited[current] = true
	*result = append(*result, current)

	if callback != nil {
		callback(current)
	}

	for _, neighbor := range graph.Neighbors(current) {
		if !visited[neighbor] {
			dfsRecursive(graph, neighbor, visited, result, callback)
		}
	}
}

// DFSFindPath finds a path between two vertices using DFS.
func DFSFindPath[T comparable](graph *Graph[T], start, end T) []T {
	if !graph.HasVertex(start) || !graph.HasVertex(end) {
		return nil
	}

	if start == end {
		return []T{start}
	}

	visited := make(map[T]bool)
	path := []T{}

	if dfsPath(graph, start, end, visited, &path) {
		return path
	}
	return nil
}

func dfsPath[T comparable](graph *Graph[T], current, end T, visited map[T]bool, path *[]T) bool {
	visited[current] = true
	*path = append(*path, current)

	if current == end {
		return true
	}

	for _, neighbor := range graph.Neighbors(current) {
		if !visited[neighbor] {
			if dfsPath(graph, neighbor, end, visited, path) {
				return true
			}
		}
	}

	*path = (*path)[:len(*path)-1]
	return false
}

// HasCycle detects if a directed graph has a cycle starting from start.
func HasCycle[T comparable](graph *Graph[T], start T) bool {
	if !graph.HasVertex(start) {
		return false
	}

	visited := make(map[T]bool)
	recStack := make(map[T]bool)

	return detectCycle(graph, start, visited, recStack)
}

func detectCycle[T comparable](graph *Graph[T], current T, visited, recStack map[T]bool) bool {
	visited[current] = true
	recStack[current] = true

	for _, neighbor := range graph.Neighbors(current) {
		if !visited[neighbor] {
			if detectCycle(graph, neighbor, visited, recStack) {
				return true
			}
		} else if recStack[neighbor] {
			return true
		}
	}

	recStack[current] = false
	return false
}

/**
 * @file dfs.h
 * @brief Depth-First Search algorithm interface
 *
 * DFS traverses a graph by exploring as far as possible along each branch
 * before backtracking. Uses recursion or an explicit stack. Ideal for
 * cycle detection, topological sorting, and path finding.
 *
 * Time complexity: O(V + E)
 * Space complexity: O(V)
 */

#ifndef DSA_DFS_H
#define DSA_DFS_H

#include <stddef.h>
#include <stdbool.h>

/**
 * Result codes for DFS operations.
 */
typedef enum {
    DFS_OK = 0,
    DFS_ERR_NULL,
    DFS_ERR_INVALID_VERTEX,
    DFS_ERR_ALLOC,
    DFS_ERR_NO_PATH,
    DFS_ERR_HAS_CYCLE
} DfsResult;

/**
 * Opaque graph type using adjacency list representation.
 */
typedef struct DfsGraph DfsGraph;

/**
 * Create a new graph with the specified number of vertices.
 * Vertices are numbered 0 to num_vertices-1.
 * @param num_vertices Number of vertices in the graph
 * @return New graph or NULL on failure
 */
DfsGraph *dfs_graph_create(size_t num_vertices);

/**
 * Destroy a graph and free all memory.
 * @param graph Graph to destroy
 */
void dfs_graph_destroy(DfsGraph *graph);

/**
 * Add an undirected edge between two vertices.
 * @param graph Graph to modify
 * @param v1 First vertex
 * @param v2 Second vertex
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_graph_add_edge(DfsGraph *graph, size_t v1, size_t v2);

/**
 * Add a directed edge from v1 to v2.
 * @param graph Graph to modify
 * @param from Source vertex
 * @param to Destination vertex
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_graph_add_directed_edge(DfsGraph *graph, size_t from, size_t to);

/**
 * Get the number of vertices in the graph.
 * @param graph Graph to query
 * @return Number of vertices, or 0 if graph is NULL
 */
size_t dfs_graph_vertex_count(const DfsGraph *graph);

/**
 * Perform DFS traversal from a starting vertex (recursive).
 * @param graph Graph to traverse
 * @param start Starting vertex
 * @param result Array to store visited vertices in DFS order (must be at least num_vertices)
 * @param result_size Output: number of vertices visited
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_traverse(const DfsGraph *graph, size_t start, size_t *result, size_t *result_size);

/**
 * Perform DFS traversal using an explicit stack (iterative).
 * @param graph Graph to traverse
 * @param start Starting vertex
 * @param result Array to store visited vertices in DFS order (must be at least num_vertices)
 * @param result_size Output: number of vertices visited
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_traverse_iterative(const DfsGraph *graph, size_t start, size_t *result, size_t *result_size);

/**
 * Find a path between two vertices using DFS.
 * Note: This finds ANY path, not necessarily the shortest.
 * @param graph Graph to search
 * @param start Starting vertex
 * @param end Target vertex
 * @param path Array to store path vertices (must be at least num_vertices)
 * @param path_length Output: number of vertices in path
 * @return DFS_OK on success, DFS_ERR_NO_PATH if no path exists
 */
DfsResult dfs_find_path(const DfsGraph *graph, size_t start, size_t end,
                        size_t *path, size_t *path_length);

/**
 * Check if an undirected graph contains a cycle.
 * @param graph Graph to check
 * @param has_cycle Output: true if cycle exists, false otherwise
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_has_cycle_undirected(const DfsGraph *graph, bool *has_cycle);

/**
 * Check if a directed graph contains a cycle.
 * @param graph Graph to check
 * @param has_cycle Output: true if cycle exists, false otherwise
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_has_cycle_directed(const DfsGraph *graph, bool *has_cycle);

/**
 * Perform topological sort on a directed acyclic graph (DAG).
 * @param graph Graph to sort (must be a DAG)
 * @param result Array to store vertices in topological order (must be at least num_vertices)
 * @param result_size Output: number of vertices
 * @return DFS_OK on success, DFS_ERR_HAS_CYCLE if graph has cycle
 */
DfsResult dfs_topological_sort(const DfsGraph *graph, size_t *result, size_t *result_size);

/**
 * Count connected components in an undirected graph.
 * @param graph Graph to analyze
 * @param count Output: number of connected components
 * @return DFS_OK on success, error code on failure
 */
DfsResult dfs_count_components(const DfsGraph *graph, size_t *count);

#endif /* DSA_DFS_H */

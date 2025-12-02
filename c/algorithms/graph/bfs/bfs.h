/**
 * @file bfs.h
 * @brief Breadth-First Search algorithm interface
 *
 * BFS traverses a graph level by level, visiting all neighbors at the current
 * depth before moving to vertices at the next depth level. Uses a queue to
 * maintain the frontier. Finds shortest path (by edge count) in unweighted graphs.
 *
 * Time complexity: O(V + E)
 * Space complexity: O(V)
 */

#ifndef DSA_BFS_H
#define DSA_BFS_H

#include <stddef.h>
#include <stdbool.h>

/**
 * Result codes for BFS operations.
 */
typedef enum {
    BFS_OK = 0,
    BFS_ERR_NULL,
    BFS_ERR_INVALID_VERTEX,
    BFS_ERR_ALLOC,
    BFS_ERR_NO_PATH
} BfsResult;

/**
 * Opaque graph type using adjacency list representation.
 */
typedef struct Graph Graph;

/**
 * Create a new graph with the specified number of vertices.
 * Vertices are numbered 0 to num_vertices-1.
 * @param num_vertices Number of vertices in the graph
 * @return New graph or NULL on failure
 */
Graph *graph_create(size_t num_vertices);

/**
 * Destroy a graph and free all memory.
 * @param graph Graph to destroy
 */
void graph_destroy(Graph *graph);

/**
 * Add an undirected edge between two vertices.
 * @param graph Graph to modify
 * @param v1 First vertex
 * @param v2 Second vertex
 * @return BFS_OK on success, error code on failure
 */
BfsResult graph_add_edge(Graph *graph, size_t v1, size_t v2);

/**
 * Add a directed edge from v1 to v2.
 * @param graph Graph to modify
 * @param from Source vertex
 * @param to Destination vertex
 * @return BFS_OK on success, error code on failure
 */
BfsResult graph_add_directed_edge(Graph *graph, size_t from, size_t to);

/**
 * Get the number of vertices in the graph.
 * @param graph Graph to query
 * @return Number of vertices, or 0 if graph is NULL
 */
size_t graph_vertex_count(const Graph *graph);

/**
 * Perform BFS traversal from a starting vertex.
 * @param graph Graph to traverse
 * @param start Starting vertex
 * @param result Array to store visited vertices in BFS order (must be at least num_vertices)
 * @param result_size Output: number of vertices visited
 * @return BFS_OK on success, error code on failure
 */
BfsResult bfs_traverse(const Graph *graph, size_t start, size_t *result, size_t *result_size);

/**
 * Find shortest path between two vertices using BFS.
 * @param graph Graph to search
 * @param start Starting vertex
 * @param end Target vertex
 * @param path Array to store path vertices (must be at least num_vertices)
 * @param path_length Output: number of vertices in path
 * @return BFS_OK on success, BFS_ERR_NO_PATH if no path exists
 */
BfsResult bfs_shortest_path(const Graph *graph, size_t start, size_t end,
                            size_t *path, size_t *path_length);

/**
 * Calculate distances from start vertex to all reachable vertices.
 * @param graph Graph to traverse
 * @param start Starting vertex
 * @param distances Array to store distances (must be at least num_vertices)
 *                  Unreachable vertices will have value SIZE_MAX
 * @return BFS_OK on success, error code on failure
 */
BfsResult bfs_distances(const Graph *graph, size_t start, size_t *distances);

/**
 * Check if the graph is connected (for undirected graphs).
 * @param graph Graph to check
 * @param connected Output: true if connected, false otherwise
 * @return BFS_OK on success, error code on failure
 */
BfsResult bfs_is_connected(const Graph *graph, bool *connected);

#endif /* DSA_BFS_H */

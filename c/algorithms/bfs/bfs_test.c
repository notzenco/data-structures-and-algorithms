/**
 * @file bfs_test.c
 * @brief Unit tests for BFS implementation
 */

#include "bfs.h"
#include <assert.h>
#include <stdio.h>
#include <stdint.h>

static void test_graph_create_destroy(void) {
    printf("  test_graph_create_destroy...");

    Graph *graph = graph_create(5);
    assert(graph != NULL);
    assert(graph_vertex_count(graph) == 5);

    graph_destroy(graph);

    /* NULL graph should not crash */
    graph_destroy(NULL);

    /* Zero vertices should return NULL */
    assert(graph_create(0) == NULL);

    printf(" PASSED\n");
}

static void test_add_edges(void) {
    printf("  test_add_edges...");

    Graph *graph = graph_create(5);
    assert(graph != NULL);

    assert(graph_add_edge(graph, 0, 1) == BFS_OK);
    assert(graph_add_edge(graph, 1, 2) == BFS_OK);
    assert(graph_add_edge(graph, 0, 0) == BFS_OK);  /* Self-loop */

    /* Invalid vertices */
    assert(graph_add_edge(graph, 5, 0) == BFS_ERR_INVALID_VERTEX);
    assert(graph_add_edge(graph, 0, 5) == BFS_ERR_INVALID_VERTEX);

    /* Directed edge */
    assert(graph_add_directed_edge(graph, 2, 3) == BFS_OK);
    assert(graph_add_directed_edge(graph, 10, 0) == BFS_ERR_INVALID_VERTEX);

    /* NULL graph */
    assert(graph_add_edge(NULL, 0, 1) == BFS_ERR_NULL);
    assert(graph_add_directed_edge(NULL, 0, 1) == BFS_ERR_NULL);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_traverse_simple(void) {
    printf("  test_bfs_traverse_simple...");

    /*
     * Graph:
     *   0 -- 1 -- 2
     *   |         |
     *   3 ------- 4
     */
    Graph *graph = graph_create(5);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    graph_add_edge(graph, 1, 2);
    graph_add_edge(graph, 0, 3);
    graph_add_edge(graph, 2, 4);
    graph_add_edge(graph, 3, 4);

    size_t result[5];
    size_t result_size;

    assert(bfs_traverse(graph, 0, result, &result_size) == BFS_OK);
    assert(result_size == 5);
    assert(result[0] == 0);  /* Start vertex first */

    /* All vertices should be visited */
    bool visited[5] = {false};
    for (size_t i = 0; i < result_size; i++) {
        visited[result[i]] = true;
    }
    for (size_t i = 0; i < 5; i++) {
        assert(visited[i]);
    }

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_traverse_disconnected(void) {
    printf("  test_bfs_traverse_disconnected...");

    /*
     * Graph:
     *   0 -- 1    2 -- 3
     *   (two components)
     */
    Graph *graph = graph_create(4);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    graph_add_edge(graph, 2, 3);

    size_t result[4];
    size_t result_size;

    /* BFS from 0 should only find 0 and 1 */
    assert(bfs_traverse(graph, 0, result, &result_size) == BFS_OK);
    assert(result_size == 2);

    /* BFS from 2 should only find 2 and 3 */
    assert(bfs_traverse(graph, 2, result, &result_size) == BFS_OK);
    assert(result_size == 2);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_shortest_path(void) {
    printf("  test_bfs_shortest_path...");

    /*
     * Graph:
     *   0 -- 1 -- 2
     *   |         |
     *   3 ------- 4
     *
     * Shortest path from 0 to 4:
     * 0 -> 3 -> 4 (length 3) or 0 -> 1 -> 2 -> 4 (length 4)
     * So shortest is 0 -> 3 -> 4
     */
    Graph *graph = graph_create(5);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    graph_add_edge(graph, 1, 2);
    graph_add_edge(graph, 0, 3);
    graph_add_edge(graph, 2, 4);
    graph_add_edge(graph, 3, 4);

    size_t path[5];
    size_t path_length;

    assert(bfs_shortest_path(graph, 0, 4, path, &path_length) == BFS_OK);
    assert(path_length == 3);  /* 0 -> 3 -> 4 */
    assert(path[0] == 0);
    assert(path[path_length - 1] == 4);

    /* Path to self */
    assert(bfs_shortest_path(graph, 2, 2, path, &path_length) == BFS_OK);
    assert(path_length == 1);
    assert(path[0] == 2);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_no_path(void) {
    printf("  test_bfs_no_path...");

    /* Disconnected graph */
    Graph *graph = graph_create(4);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    graph_add_edge(graph, 2, 3);

    size_t path[4];
    size_t path_length;

    /* No path from 0 to 2 */
    assert(bfs_shortest_path(graph, 0, 2, path, &path_length) == BFS_ERR_NO_PATH);
    assert(bfs_shortest_path(graph, 1, 3, path, &path_length) == BFS_ERR_NO_PATH);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_distances(void) {
    printf("  test_bfs_distances...");

    /*
     * Graph:
     *   0 -- 1 -- 2 -- 5
     *   |    |
     *   3 -- 4
     */
    Graph *graph = graph_create(6);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    graph_add_edge(graph, 1, 2);
    graph_add_edge(graph, 2, 5);
    graph_add_edge(graph, 0, 3);
    graph_add_edge(graph, 3, 4);
    graph_add_edge(graph, 1, 4);

    size_t distances[6];
    assert(bfs_distances(graph, 0, distances) == BFS_OK);

    assert(distances[0] == 0);  /* Distance to self */
    assert(distances[1] == 1);  /* 0 -> 1 */
    assert(distances[2] == 2);  /* 0 -> 1 -> 2 */
    assert(distances[3] == 1);  /* 0 -> 3 */
    assert(distances[4] == 2);  /* 0 -> 1 -> 4 or 0 -> 3 -> 4 */
    assert(distances[5] == 3);  /* 0 -> 1 -> 2 -> 5 */

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_distances_unreachable(void) {
    printf("  test_bfs_distances_unreachable...");

    /* Disconnected graph */
    Graph *graph = graph_create(4);
    assert(graph != NULL);

    graph_add_edge(graph, 0, 1);
    /* 2 and 3 are isolated */

    size_t distances[4];
    assert(bfs_distances(graph, 0, distances) == BFS_OK);

    assert(distances[0] == 0);
    assert(distances[1] == 1);
    assert(distances[2] == SIZE_MAX);  /* Unreachable */
    assert(distances[3] == SIZE_MAX);  /* Unreachable */

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_is_connected(void) {
    printf("  test_bfs_is_connected...");

    bool connected;

    /* Connected graph */
    Graph *graph1 = graph_create(4);
    graph_add_edge(graph1, 0, 1);
    graph_add_edge(graph1, 1, 2);
    graph_add_edge(graph1, 2, 3);

    assert(bfs_is_connected(graph1, &connected) == BFS_OK);
    assert(connected == true);
    graph_destroy(graph1);

    /* Disconnected graph */
    Graph *graph2 = graph_create(4);
    graph_add_edge(graph2, 0, 1);
    graph_add_edge(graph2, 2, 3);

    assert(bfs_is_connected(graph2, &connected) == BFS_OK);
    assert(connected == false);
    graph_destroy(graph2);

    /* Single vertex (connected) */
    Graph *graph3 = graph_create(1);
    assert(bfs_is_connected(graph3, &connected) == BFS_OK);
    assert(connected == true);
    graph_destroy(graph3);

    printf(" PASSED\n");
}

static void test_bfs_directed_graph(void) {
    printf("  test_bfs_directed_graph...");

    /*
     * Directed graph:
     *   0 -> 1 -> 2
     *   |         ^
     *   v         |
     *   3 --------+
     */
    Graph *graph = graph_create(4);
    assert(graph != NULL);

    graph_add_directed_edge(graph, 0, 1);
    graph_add_directed_edge(graph, 1, 2);
    graph_add_directed_edge(graph, 0, 3);
    graph_add_directed_edge(graph, 3, 2);

    size_t path[4];
    size_t path_length;

    /* Path from 0 to 2 exists */
    assert(bfs_shortest_path(graph, 0, 2, path, &path_length) == BFS_OK);
    assert(path_length == 3);  /* 0 -> 1 -> 2 or 0 -> 3 -> 2 */

    /* No path from 2 to 0 (wrong direction) */
    assert(bfs_shortest_path(graph, 2, 0, path, &path_length) == BFS_ERR_NO_PATH);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_null_params(void) {
    printf("  test_bfs_null_params...");

    Graph *graph = graph_create(3);
    size_t result[3];
    size_t result_size;
    size_t distances[3];
    bool connected;

    /* NULL graph */
    assert(bfs_traverse(NULL, 0, result, &result_size) == BFS_ERR_NULL);
    assert(bfs_distances(NULL, 0, distances) == BFS_ERR_NULL);
    assert(bfs_is_connected(NULL, &connected) == BFS_ERR_NULL);

    /* NULL result arrays */
    assert(bfs_traverse(graph, 0, NULL, &result_size) == BFS_ERR_NULL);
    assert(bfs_traverse(graph, 0, result, NULL) == BFS_ERR_NULL);
    assert(bfs_distances(graph, 0, NULL) == BFS_ERR_NULL);
    assert(bfs_is_connected(graph, NULL) == BFS_ERR_NULL);

    /* Invalid start vertex */
    assert(bfs_traverse(graph, 5, result, &result_size) == BFS_ERR_INVALID_VERTEX);
    assert(bfs_distances(graph, 5, distances) == BFS_ERR_INVALID_VERTEX);

    graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_bfs_large_graph(void) {
    printf("  test_bfs_large_graph...");

    /* Create a line graph: 0 - 1 - 2 - ... - 99 */
    size_t n = 100;
    Graph *graph = graph_create(n);
    assert(graph != NULL);

    for (size_t i = 0; i < n - 1; i++) {
        graph_add_edge(graph, i, i + 1);
    }

    size_t *distances = malloc(n * sizeof(size_t));
    assert(distances != NULL);

    assert(bfs_distances(graph, 0, distances) == BFS_OK);

    /* Distance from 0 to i should be i */
    for (size_t i = 0; i < n; i++) {
        assert(distances[i] == i);
    }

    free(distances);
    graph_destroy(graph);
    printf(" PASSED\n");
}

int main(void) {
    printf("Running BFS tests...\n");

    test_graph_create_destroy();
    test_add_edges();
    test_bfs_traverse_simple();
    test_bfs_traverse_disconnected();
    test_bfs_shortest_path();
    test_bfs_no_path();
    test_bfs_distances();
    test_bfs_distances_unreachable();
    test_bfs_is_connected();
    test_bfs_directed_graph();
    test_bfs_null_params();
    test_bfs_large_graph();

    printf("All BFS tests PASSED!\n");
    return 0;
}

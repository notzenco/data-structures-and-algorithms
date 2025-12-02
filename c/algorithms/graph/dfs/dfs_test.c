/**
 * @file dfs_test.c
 * @brief Unit tests for DFS implementation
 */

#include "dfs.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static void test_graph_create_destroy(void) {
    printf("  test_graph_create_destroy...");

    DfsGraph *graph = dfs_graph_create(5);
    assert(graph != NULL);
    assert(dfs_graph_vertex_count(graph) == 5);

    dfs_graph_destroy(graph);

    /* NULL graph should not crash */
    dfs_graph_destroy(NULL);

    /* Zero vertices should return NULL */
    assert(dfs_graph_create(0) == NULL);

    printf(" PASSED\n");
}

static void test_add_edges(void) {
    printf("  test_add_edges...");

    DfsGraph *graph = dfs_graph_create(5);
    assert(graph != NULL);

    assert(dfs_graph_add_edge(graph, 0, 1) == DFS_OK);
    assert(dfs_graph_add_edge(graph, 1, 2) == DFS_OK);
    assert(dfs_graph_add_edge(graph, 0, 0) == DFS_OK);  /* Self-loop */

    /* Invalid vertices */
    assert(dfs_graph_add_edge(graph, 5, 0) == DFS_ERR_INVALID_VERTEX);
    assert(dfs_graph_add_edge(graph, 0, 5) == DFS_ERR_INVALID_VERTEX);

    /* Directed edge */
    assert(dfs_graph_add_directed_edge(graph, 2, 3) == DFS_OK);
    assert(dfs_graph_add_directed_edge(graph, 10, 0) == DFS_ERR_INVALID_VERTEX);

    /* NULL graph */
    assert(dfs_graph_add_edge(NULL, 0, 1) == DFS_ERR_NULL);
    assert(dfs_graph_add_directed_edge(NULL, 0, 1) == DFS_ERR_NULL);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_traverse_simple(void) {
    printf("  test_dfs_traverse_simple...");

    /*
     * Graph:
     *   0 -- 1 -- 2
     *   |         |
     *   3 ------- 4
     */
    DfsGraph *graph = dfs_graph_create(5);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 1, 2);
    dfs_graph_add_edge(graph, 0, 3);
    dfs_graph_add_edge(graph, 2, 4);
    dfs_graph_add_edge(graph, 3, 4);

    size_t result[5];
    size_t result_size;

    assert(dfs_traverse(graph, 0, result, &result_size) == DFS_OK);
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

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_traverse_iterative(void) {
    printf("  test_dfs_traverse_iterative...");

    DfsGraph *graph = dfs_graph_create(5);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 1, 2);
    dfs_graph_add_edge(graph, 0, 3);
    dfs_graph_add_edge(graph, 2, 4);
    dfs_graph_add_edge(graph, 3, 4);

    size_t result[5];
    size_t result_size;

    assert(dfs_traverse_iterative(graph, 0, result, &result_size) == DFS_OK);
    assert(result_size == 5);
    assert(result[0] == 0);

    /* All vertices should be visited */
    bool visited[5] = {false};
    for (size_t i = 0; i < result_size; i++) {
        visited[result[i]] = true;
    }
    for (size_t i = 0; i < 5; i++) {
        assert(visited[i]);
    }

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_traverse_disconnected(void) {
    printf("  test_dfs_traverse_disconnected...");

    /*
     * Graph:
     *   0 -- 1    2 -- 3
     *   (two components)
     */
    DfsGraph *graph = dfs_graph_create(4);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 2, 3);

    size_t result[4];
    size_t result_size;

    /* DFS from 0 should only find 0 and 1 */
    assert(dfs_traverse(graph, 0, result, &result_size) == DFS_OK);
    assert(result_size == 2);

    /* DFS from 2 should only find 2 and 3 */
    assert(dfs_traverse(graph, 2, result, &result_size) == DFS_OK);
    assert(result_size == 2);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_find_path(void) {
    printf("  test_dfs_find_path...");

    /*
     * Graph:
     *   0 -- 1 -- 2
     *   |         |
     *   3 ------- 4
     */
    DfsGraph *graph = dfs_graph_create(5);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 1, 2);
    dfs_graph_add_edge(graph, 0, 3);
    dfs_graph_add_edge(graph, 2, 4);
    dfs_graph_add_edge(graph, 3, 4);

    size_t path[5];
    size_t path_length;

    /* Path from 0 to 4 should exist */
    assert(dfs_find_path(graph, 0, 4, path, &path_length) == DFS_OK);
    assert(path_length >= 2);  /* At least start and end */
    assert(path[0] == 0);
    assert(path[path_length - 1] == 4);

    /* Path to self */
    assert(dfs_find_path(graph, 2, 2, path, &path_length) == DFS_OK);
    assert(path_length == 1);
    assert(path[0] == 2);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_no_path(void) {
    printf("  test_dfs_no_path...");

    /* Disconnected graph */
    DfsGraph *graph = dfs_graph_create(4);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 2, 3);

    size_t path[4];
    size_t path_length;

    /* No path from 0 to 2 */
    assert(dfs_find_path(graph, 0, 2, path, &path_length) == DFS_ERR_NO_PATH);
    assert(dfs_find_path(graph, 1, 3, path, &path_length) == DFS_ERR_NO_PATH);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_cycle_undirected_no_cycle(void) {
    printf("  test_dfs_cycle_undirected_no_cycle...");

    /* Tree (no cycle): 0 - 1 - 2 */
    DfsGraph *graph = dfs_graph_create(3);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 1, 2);

    bool has_cycle;
    assert(dfs_has_cycle_undirected(graph, &has_cycle) == DFS_OK);
    assert(has_cycle == false);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_cycle_undirected_with_cycle(void) {
    printf("  test_dfs_cycle_undirected_with_cycle...");

    /* Triangle: 0 - 1 - 2 - 0 */
    DfsGraph *graph = dfs_graph_create(3);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 1, 2);
    dfs_graph_add_edge(graph, 2, 0);

    bool has_cycle;
    assert(dfs_has_cycle_undirected(graph, &has_cycle) == DFS_OK);
    assert(has_cycle == true);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_cycle_directed_no_cycle(void) {
    printf("  test_dfs_cycle_directed_no_cycle...");

    /* DAG: 0 -> 1 -> 2 */
    DfsGraph *graph = dfs_graph_create(3);
    assert(graph != NULL);

    dfs_graph_add_directed_edge(graph, 0, 1);
    dfs_graph_add_directed_edge(graph, 1, 2);

    bool has_cycle;
    assert(dfs_has_cycle_directed(graph, &has_cycle) == DFS_OK);
    assert(has_cycle == false);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_cycle_directed_with_cycle(void) {
    printf("  test_dfs_cycle_directed_with_cycle...");

    /* Cycle: 0 -> 1 -> 2 -> 0 */
    DfsGraph *graph = dfs_graph_create(3);
    assert(graph != NULL);

    dfs_graph_add_directed_edge(graph, 0, 1);
    dfs_graph_add_directed_edge(graph, 1, 2);
    dfs_graph_add_directed_edge(graph, 2, 0);

    bool has_cycle;
    assert(dfs_has_cycle_directed(graph, &has_cycle) == DFS_OK);
    assert(has_cycle == true);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_topological_sort(void) {
    printf("  test_topological_sort...");

    /*
     * DAG:
     *   0 -> 1 -> 3
     *   |         ^
     *   v         |
     *   2 --------+
     */
    DfsGraph *graph = dfs_graph_create(4);
    assert(graph != NULL);

    dfs_graph_add_directed_edge(graph, 0, 1);
    dfs_graph_add_directed_edge(graph, 0, 2);
    dfs_graph_add_directed_edge(graph, 1, 3);
    dfs_graph_add_directed_edge(graph, 2, 3);

    size_t result[4];
    size_t result_size;

    assert(dfs_topological_sort(graph, result, &result_size) == DFS_OK);
    assert(result_size == 4);

    /* Verify topological order: for each edge (u, v), u comes before v */
    size_t position[4];
    for (size_t i = 0; i < result_size; i++) {
        position[result[i]] = i;
    }

    /* 0 should come before 1, 2 */
    assert(position[0] < position[1]);
    assert(position[0] < position[2]);
    /* 1 and 2 should come before 3 */
    assert(position[1] < position[3]);
    assert(position[2] < position[3]);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_topological_sort_with_cycle(void) {
    printf("  test_topological_sort_with_cycle...");

    /* Graph with cycle */
    DfsGraph *graph = dfs_graph_create(3);
    assert(graph != NULL);

    dfs_graph_add_directed_edge(graph, 0, 1);
    dfs_graph_add_directed_edge(graph, 1, 2);
    dfs_graph_add_directed_edge(graph, 2, 0);

    size_t result[3];
    size_t result_size;

    assert(dfs_topological_sort(graph, result, &result_size) == DFS_ERR_HAS_CYCLE);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_count_components(void) {
    printf("  test_count_components...");

    /*
     * Three components:
     * 0 - 1
     * 2 (isolated)
     * 3 - 4 - 5
     */
    DfsGraph *graph = dfs_graph_create(6);
    assert(graph != NULL);

    dfs_graph_add_edge(graph, 0, 1);
    dfs_graph_add_edge(graph, 3, 4);
    dfs_graph_add_edge(graph, 4, 5);

    size_t count;
    assert(dfs_count_components(graph, &count) == DFS_OK);
    assert(count == 3);

    dfs_graph_destroy(graph);

    /* Single component */
    DfsGraph *graph2 = dfs_graph_create(3);
    dfs_graph_add_edge(graph2, 0, 1);
    dfs_graph_add_edge(graph2, 1, 2);

    assert(dfs_count_components(graph2, &count) == DFS_OK);
    assert(count == 1);

    dfs_graph_destroy(graph2);
    printf(" PASSED\n");
}

static void test_dfs_null_params(void) {
    printf("  test_dfs_null_params...");

    DfsGraph *graph = dfs_graph_create(3);
    size_t result[3];
    size_t result_size;
    size_t count;
    bool has_cycle;

    /* NULL graph */
    assert(dfs_traverse(NULL, 0, result, &result_size) == DFS_ERR_NULL);
    assert(dfs_traverse_iterative(NULL, 0, result, &result_size) == DFS_ERR_NULL);
    assert(dfs_count_components(NULL, &count) == DFS_ERR_NULL);
    assert(dfs_has_cycle_undirected(NULL, &has_cycle) == DFS_ERR_NULL);
    assert(dfs_has_cycle_directed(NULL, &has_cycle) == DFS_ERR_NULL);
    assert(dfs_topological_sort(NULL, result, &result_size) == DFS_ERR_NULL);

    /* NULL result arrays */
    assert(dfs_traverse(graph, 0, NULL, &result_size) == DFS_ERR_NULL);
    assert(dfs_traverse(graph, 0, result, NULL) == DFS_ERR_NULL);
    assert(dfs_count_components(graph, NULL) == DFS_ERR_NULL);
    assert(dfs_has_cycle_undirected(graph, NULL) == DFS_ERR_NULL);

    /* Invalid start vertex */
    assert(dfs_traverse(graph, 5, result, &result_size) == DFS_ERR_INVALID_VERTEX);
    assert(dfs_traverse_iterative(graph, 5, result, &result_size) == DFS_ERR_INVALID_VERTEX);

    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

static void test_dfs_large_graph(void) {
    printf("  test_dfs_large_graph...");

    /* Create a line graph: 0 - 1 - 2 - ... - 99 */
    size_t n = 100;
    DfsGraph *graph = dfs_graph_create(n);
    assert(graph != NULL);

    for (size_t i = 0; i < n - 1; i++) {
        dfs_graph_add_edge(graph, i, i + 1);
    }

    size_t *result = malloc(n * sizeof(size_t));
    assert(result != NULL);
    size_t result_size;

    assert(dfs_traverse(graph, 0, result, &result_size) == DFS_OK);
    assert(result_size == n);

    /* Verify all vertices are visited */
    bool *visited = calloc(n, sizeof(bool));
    for (size_t i = 0; i < result_size; i++) {
        visited[result[i]] = true;
    }
    for (size_t i = 0; i < n; i++) {
        assert(visited[i]);
    }

    free(result);
    free(visited);
    dfs_graph_destroy(graph);
    printf(" PASSED\n");
}

int main(void) {
    printf("Running DFS tests...\n");

    test_graph_create_destroy();
    test_add_edges();
    test_dfs_traverse_simple();
    test_dfs_traverse_iterative();
    test_dfs_traverse_disconnected();
    test_dfs_find_path();
    test_dfs_no_path();
    test_dfs_cycle_undirected_no_cycle();
    test_dfs_cycle_undirected_with_cycle();
    test_dfs_cycle_directed_no_cycle();
    test_dfs_cycle_directed_with_cycle();
    test_topological_sort();
    test_topological_sort_with_cycle();
    test_count_components();
    test_dfs_null_params();
    test_dfs_large_graph();

    printf("All DFS tests PASSED!\n");
    return 0;
}

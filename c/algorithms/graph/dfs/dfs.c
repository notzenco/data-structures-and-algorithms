/**
 * @file dfs.c
 * @brief Depth-First Search algorithm implementation
 */

#include "dfs.h"
#include <stdlib.h>
#include <string.h>

/**
 * Node in adjacency list.
 */
typedef struct DfsAdjNode {
    size_t vertex;
    struct DfsAdjNode *next;
} DfsAdjNode;

/**
 * Graph represented as adjacency list.
 */
struct DfsGraph {
    size_t num_vertices;
    DfsAdjNode **adj_lists;
};

/**
 * Simple stack for iterative DFS.
 */
typedef struct {
    size_t *data;
    size_t top;
    size_t capacity;
} Stack;

static Stack *stack_create(size_t capacity) {
    Stack *s = malloc(sizeof(Stack));
    if (!s) {
        return NULL;
    }

    s->data = malloc(capacity * sizeof(size_t));
    if (!s->data) {
        free(s);
        return NULL;
    }

    s->top = 0;
    s->capacity = capacity;
    return s;
}

static void stack_destroy(Stack *s) {
    if (s) {
        free(s->data);
        free(s);
    }
}

static bool stack_is_empty(const Stack *s) {
    return s->top == 0;
}

static void stack_push(Stack *s, size_t value) {
    s->data[s->top++] = value;
}

static size_t stack_pop(Stack *s) {
    return s->data[--s->top];
}

DfsGraph *dfs_graph_create(size_t num_vertices) {
    if (num_vertices == 0) {
        return NULL;
    }

    DfsGraph *graph = malloc(sizeof(DfsGraph));
    if (!graph) {
        return NULL;
    }

    graph->num_vertices = num_vertices;
    graph->adj_lists = calloc(num_vertices, sizeof(DfsAdjNode *));

    if (!graph->adj_lists) {
        free(graph);
        return NULL;
    }

    return graph;
}

void dfs_graph_destroy(DfsGraph *graph) {
    if (!graph) {
        return;
    }

    for (size_t i = 0; i < graph->num_vertices; i++) {
        DfsAdjNode *current = graph->adj_lists[i];
        while (current) {
            DfsAdjNode *next = current->next;
            free(current);
            current = next;
        }
    }

    free(graph->adj_lists);
    free(graph);
}

static DfsResult add_directed_edge_internal(DfsGraph *graph, size_t from, size_t to) {
    DfsAdjNode *node = malloc(sizeof(DfsAdjNode));
    if (!node) {
        return DFS_ERR_ALLOC;
    }

    node->vertex = to;
    node->next = graph->adj_lists[from];
    graph->adj_lists[from] = node;

    return DFS_OK;
}

DfsResult dfs_graph_add_edge(DfsGraph *graph, size_t v1, size_t v2) {
    if (!graph) {
        return DFS_ERR_NULL;
    }

    if (v1 >= graph->num_vertices || v2 >= graph->num_vertices) {
        return DFS_ERR_INVALID_VERTEX;
    }

    DfsResult result = add_directed_edge_internal(graph, v1, v2);
    if (result != DFS_OK) {
        return result;
    }

    if (v1 != v2) {
        result = add_directed_edge_internal(graph, v2, v1);
        if (result != DFS_OK) {
            return result;
        }
    }

    return DFS_OK;
}

DfsResult dfs_graph_add_directed_edge(DfsGraph *graph, size_t from, size_t to) {
    if (!graph) {
        return DFS_ERR_NULL;
    }

    if (from >= graph->num_vertices || to >= graph->num_vertices) {
        return DFS_ERR_INVALID_VERTEX;
    }

    return add_directed_edge_internal(graph, from, to);
}

size_t dfs_graph_vertex_count(const DfsGraph *graph) {
    if (!graph) {
        return 0;
    }
    return graph->num_vertices;
}

/**
 * Recursive DFS helper.
 */
static void dfs_recursive(const DfsGraph *graph, size_t vertex, bool *visited,
                          size_t *result, size_t *result_size) {
    visited[vertex] = true;
    result[(*result_size)++] = vertex;

    for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
        if (!visited[adj->vertex]) {
            dfs_recursive(graph, adj->vertex, visited, result, result_size);
        }
    }
}

DfsResult dfs_traverse(const DfsGraph *graph, size_t start, size_t *result, size_t *result_size) {
    if (!graph || !result || !result_size) {
        return DFS_ERR_NULL;
    }

    if (start >= graph->num_vertices) {
        return DFS_ERR_INVALID_VERTEX;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return DFS_ERR_ALLOC;
    }

    *result_size = 0;
    dfs_recursive(graph, start, visited, result, result_size);

    free(visited);
    return DFS_OK;
}

DfsResult dfs_traverse_iterative(const DfsGraph *graph, size_t start, size_t *result, size_t *result_size) {
    if (!graph || !result || !result_size) {
        return DFS_ERR_NULL;
    }

    if (start >= graph->num_vertices) {
        return DFS_ERR_INVALID_VERTEX;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return DFS_ERR_ALLOC;
    }

    Stack *stack = stack_create(graph->num_vertices);
    if (!stack) {
        free(visited);
        return DFS_ERR_ALLOC;
    }

    *result_size = 0;
    stack_push(stack, start);

    while (!stack_is_empty(stack)) {
        size_t vertex = stack_pop(stack);

        if (visited[vertex]) {
            continue;
        }

        visited[vertex] = true;
        result[(*result_size)++] = vertex;

        /* Push neighbors in reverse order for consistent traversal */
        /* First, count and collect neighbors */
        size_t neighbor_count = 0;
        for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
            neighbor_count++;
        }

        if (neighbor_count > 0) {
            size_t *neighbors = malloc(neighbor_count * sizeof(size_t));
            if (neighbors) {
                size_t idx = 0;
                for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
                    neighbors[idx++] = adj->vertex;
                }

                /* Push in reverse order */
                for (size_t i = neighbor_count; i > 0; i--) {
                    if (!visited[neighbors[i - 1]]) {
                        stack_push(stack, neighbors[i - 1]);
                    }
                }
                free(neighbors);
            }
        }
    }

    stack_destroy(stack);
    free(visited);
    return DFS_OK;
}

/**
 * Recursive path finding helper.
 */
static bool find_path_recursive(const DfsGraph *graph, size_t current, size_t end,
                                bool *visited, size_t *path, size_t *path_length) {
    visited[current] = true;
    path[(*path_length)++] = current;

    if (current == end) {
        return true;
    }

    for (DfsAdjNode *adj = graph->adj_lists[current]; adj; adj = adj->next) {
        if (!visited[adj->vertex]) {
            if (find_path_recursive(graph, adj->vertex, end, visited, path, path_length)) {
                return true;
            }
        }
    }

    /* Backtrack */
    (*path_length)--;
    return false;
}

DfsResult dfs_find_path(const DfsGraph *graph, size_t start, size_t end,
                        size_t *path, size_t *path_length) {
    if (!graph || !path || !path_length) {
        return DFS_ERR_NULL;
    }

    if (start >= graph->num_vertices || end >= graph->num_vertices) {
        return DFS_ERR_INVALID_VERTEX;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return DFS_ERR_ALLOC;
    }

    *path_length = 0;
    bool found = find_path_recursive(graph, start, end, visited, path, path_length);

    free(visited);
    return found ? DFS_OK : DFS_ERR_NO_PATH;
}

/**
 * Check cycle in undirected graph using DFS.
 */
static bool has_cycle_undirected_dfs(const DfsGraph *graph, size_t vertex,
                                     bool *visited, size_t parent) {
    visited[vertex] = true;

    for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
        if (!visited[adj->vertex]) {
            if (has_cycle_undirected_dfs(graph, adj->vertex, visited, vertex)) {
                return true;
            }
        } else if (adj->vertex != parent) {
            /* Found back edge to non-parent visited vertex */
            return true;
        }
    }

    return false;
}

DfsResult dfs_has_cycle_undirected(const DfsGraph *graph, bool *has_cycle) {
    if (!graph || !has_cycle) {
        return DFS_ERR_NULL;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return DFS_ERR_ALLOC;
    }

    *has_cycle = false;

    for (size_t i = 0; i < graph->num_vertices; i++) {
        if (!visited[i]) {
            if (has_cycle_undirected_dfs(graph, i, visited, SIZE_MAX)) {
                *has_cycle = true;
                break;
            }
        }
    }

    free(visited);
    return DFS_OK;
}

/**
 * Color states for directed cycle detection.
 */
typedef enum {
    WHITE = 0,  /* Not visited */
    GRAY = 1,   /* Currently in stack */
    BLACK = 2   /* Completely processed */
} Color;

/**
 * Check cycle in directed graph using three-color DFS.
 */
static bool has_cycle_directed_dfs(const DfsGraph *graph, size_t vertex, Color *color) {
    color[vertex] = GRAY;

    for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
        if (color[adj->vertex] == GRAY) {
            /* Back edge to vertex in current path */
            return true;
        }
        if (color[adj->vertex] == WHITE) {
            if (has_cycle_directed_dfs(graph, adj->vertex, color)) {
                return true;
            }
        }
    }

    color[vertex] = BLACK;
    return false;
}

DfsResult dfs_has_cycle_directed(const DfsGraph *graph, bool *has_cycle) {
    if (!graph || !has_cycle) {
        return DFS_ERR_NULL;
    }

    Color *color = calloc(graph->num_vertices, sizeof(Color));
    if (!color) {
        return DFS_ERR_ALLOC;
    }

    *has_cycle = false;

    for (size_t i = 0; i < graph->num_vertices; i++) {
        if (color[i] == WHITE) {
            if (has_cycle_directed_dfs(graph, i, color)) {
                *has_cycle = true;
                break;
            }
        }
    }

    free(color);
    return DFS_OK;
}

/**
 * Topological sort helper - DFS that appends to result after processing all neighbors.
 */
static bool topo_sort_dfs(const DfsGraph *graph, size_t vertex, Color *color,
                          size_t *result, size_t *result_idx) {
    color[vertex] = GRAY;

    for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
        if (color[adj->vertex] == GRAY) {
            /* Cycle detected */
            return false;
        }
        if (color[adj->vertex] == WHITE) {
            if (!topo_sort_dfs(graph, adj->vertex, color, result, result_idx)) {
                return false;
            }
        }
    }

    color[vertex] = BLACK;
    result[(*result_idx)++] = vertex;
    return true;
}

DfsResult dfs_topological_sort(const DfsGraph *graph, size_t *result, size_t *result_size) {
    if (!graph || !result || !result_size) {
        return DFS_ERR_NULL;
    }

    Color *color = calloc(graph->num_vertices, sizeof(Color));
    if (!color) {
        return DFS_ERR_ALLOC;
    }

    size_t result_idx = 0;
    bool success = true;

    for (size_t i = 0; i < graph->num_vertices; i++) {
        if (color[i] == WHITE) {
            if (!topo_sort_dfs(graph, i, color, result, &result_idx)) {
                success = false;
                break;
            }
        }
    }

    free(color);

    if (!success) {
        return DFS_ERR_HAS_CYCLE;
    }

    /* Reverse the result for correct topological order */
    *result_size = result_idx;
    for (size_t i = 0; i < result_idx / 2; i++) {
        size_t temp = result[i];
        result[i] = result[result_idx - 1 - i];
        result[result_idx - 1 - i] = temp;
    }

    return DFS_OK;
}

/**
 * DFS helper for counting components.
 */
static void component_dfs(const DfsGraph *graph, size_t vertex, bool *visited) {
    visited[vertex] = true;

    for (DfsAdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
        if (!visited[adj->vertex]) {
            component_dfs(graph, adj->vertex, visited);
        }
    }
}

DfsResult dfs_count_components(const DfsGraph *graph, size_t *count) {
    if (!graph || !count) {
        return DFS_ERR_NULL;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return DFS_ERR_ALLOC;
    }

    *count = 0;

    for (size_t i = 0; i < graph->num_vertices; i++) {
        if (!visited[i]) {
            component_dfs(graph, i, visited);
            (*count)++;
        }
    }

    free(visited);
    return DFS_OK;
}

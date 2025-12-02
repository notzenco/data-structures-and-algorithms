/**
 * @file bfs.c
 * @brief Breadth-First Search algorithm implementation
 */

#include "bfs.h"
#include <stdlib.h>
#include <string.h>

/**
 * Node in adjacency list.
 */
typedef struct AdjNode {
    size_t vertex;
    struct AdjNode *next;
} AdjNode;

/**
 * Graph represented as adjacency list.
 */
struct Graph {
    size_t num_vertices;
    AdjNode **adj_lists;
};

/**
 * Simple queue for BFS.
 */
typedef struct {
    size_t *data;
    size_t front;
    size_t rear;
    size_t capacity;
} Queue;

static Queue *queue_create(size_t capacity) {
    Queue *q = malloc(sizeof(Queue));
    if (!q) {
        return NULL;
    }

    q->data = malloc(capacity * sizeof(size_t));
    if (!q->data) {
        free(q);
        return NULL;
    }

    q->front = 0;
    q->rear = 0;
    q->capacity = capacity;
    return q;
}

static void queue_destroy(Queue *q) {
    if (q) {
        free(q->data);
        free(q);
    }
}

static bool queue_is_empty(const Queue *q) {
    return q->front == q->rear;
}

static void queue_enqueue(Queue *q, size_t value) {
    q->data[q->rear++] = value;
}

static size_t queue_dequeue(Queue *q) {
    return q->data[q->front++];
}

Graph *graph_create(size_t num_vertices) {
    if (num_vertices == 0) {
        return NULL;
    }

    Graph *graph = malloc(sizeof(Graph));
    if (!graph) {
        return NULL;
    }

    graph->num_vertices = num_vertices;
    graph->adj_lists = calloc(num_vertices, sizeof(AdjNode *));

    if (!graph->adj_lists) {
        free(graph);
        return NULL;
    }

    return graph;
}

void graph_destroy(Graph *graph) {
    if (!graph) {
        return;
    }

    for (size_t i = 0; i < graph->num_vertices; i++) {
        AdjNode *current = graph->adj_lists[i];
        while (current) {
            AdjNode *next = current->next;
            free(current);
            current = next;
        }
    }

    free(graph->adj_lists);
    free(graph);
}

static BfsResult add_edge_internal(Graph *graph, size_t from, size_t to) {
    AdjNode *node = malloc(sizeof(AdjNode));
    if (!node) {
        return BFS_ERR_ALLOC;
    }

    node->vertex = to;
    node->next = graph->adj_lists[from];
    graph->adj_lists[from] = node;

    return BFS_OK;
}

BfsResult graph_add_edge(Graph *graph, size_t v1, size_t v2) {
    if (!graph) {
        return BFS_ERR_NULL;
    }

    if (v1 >= graph->num_vertices || v2 >= graph->num_vertices) {
        return BFS_ERR_INVALID_VERTEX;
    }

    BfsResult result = add_edge_internal(graph, v1, v2);
    if (result != BFS_OK) {
        return result;
    }

    if (v1 != v2) {
        result = add_edge_internal(graph, v2, v1);
        if (result != BFS_OK) {
            return result;
        }
    }

    return BFS_OK;
}

BfsResult graph_add_directed_edge(Graph *graph, size_t from, size_t to) {
    if (!graph) {
        return BFS_ERR_NULL;
    }

    if (from >= graph->num_vertices || to >= graph->num_vertices) {
        return BFS_ERR_INVALID_VERTEX;
    }

    return add_edge_internal(graph, from, to);
}

size_t graph_vertex_count(const Graph *graph) {
    if (!graph) {
        return 0;
    }
    return graph->num_vertices;
}

BfsResult bfs_traverse(const Graph *graph, size_t start, size_t *result, size_t *result_size) {
    if (!graph || !result || !result_size) {
        return BFS_ERR_NULL;
    }

    if (start >= graph->num_vertices) {
        return BFS_ERR_INVALID_VERTEX;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return BFS_ERR_ALLOC;
    }

    Queue *queue = queue_create(graph->num_vertices);
    if (!queue) {
        free(visited);
        return BFS_ERR_ALLOC;
    }

    *result_size = 0;
    visited[start] = true;
    queue_enqueue(queue, start);

    while (!queue_is_empty(queue)) {
        size_t vertex = queue_dequeue(queue);
        result[(*result_size)++] = vertex;

        for (AdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
            if (!visited[adj->vertex]) {
                visited[adj->vertex] = true;
                queue_enqueue(queue, adj->vertex);
            }
        }
    }

    queue_destroy(queue);
    free(visited);
    return BFS_OK;
}

BfsResult bfs_shortest_path(const Graph *graph, size_t start, size_t end,
                            size_t *path, size_t *path_length) {
    if (!graph || !path || !path_length) {
        return BFS_ERR_NULL;
    }

    if (start >= graph->num_vertices || end >= graph->num_vertices) {
        return BFS_ERR_INVALID_VERTEX;
    }

    if (start == end) {
        path[0] = start;
        *path_length = 1;
        return BFS_OK;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    size_t *parent = malloc(graph->num_vertices * sizeof(size_t));

    if (!visited || !parent) {
        free(visited);
        free(parent);
        return BFS_ERR_ALLOC;
    }

    for (size_t i = 0; i < graph->num_vertices; i++) {
        parent[i] = SIZE_MAX;
    }

    Queue *queue = queue_create(graph->num_vertices);
    if (!queue) {
        free(visited);
        free(parent);
        return BFS_ERR_ALLOC;
    }

    visited[start] = true;
    queue_enqueue(queue, start);
    bool found = false;

    while (!queue_is_empty(queue) && !found) {
        size_t vertex = queue_dequeue(queue);

        for (AdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
            if (!visited[adj->vertex]) {
                visited[adj->vertex] = true;
                parent[adj->vertex] = vertex;

                if (adj->vertex == end) {
                    found = true;
                    break;
                }

                queue_enqueue(queue, adj->vertex);
            }
        }
    }

    queue_destroy(queue);
    free(visited);

    if (!found) {
        free(parent);
        return BFS_ERR_NO_PATH;
    }

    /* Reconstruct path backwards */
    size_t temp_path[graph->num_vertices];
    size_t temp_len = 0;
    size_t current = end;

    while (current != SIZE_MAX) {
        temp_path[temp_len++] = current;
        current = (current == start) ? SIZE_MAX : parent[current];
    }

    /* Reverse path */
    *path_length = temp_len;
    for (size_t i = 0; i < temp_len; i++) {
        path[i] = temp_path[temp_len - 1 - i];
    }

    free(parent);
    return BFS_OK;
}

BfsResult bfs_distances(const Graph *graph, size_t start, size_t *distances) {
    if (!graph || !distances) {
        return BFS_ERR_NULL;
    }

    if (start >= graph->num_vertices) {
        return BFS_ERR_INVALID_VERTEX;
    }

    bool *visited = calloc(graph->num_vertices, sizeof(bool));
    if (!visited) {
        return BFS_ERR_ALLOC;
    }

    Queue *queue = queue_create(graph->num_vertices);
    if (!queue) {
        free(visited);
        return BFS_ERR_ALLOC;
    }

    for (size_t i = 0; i < graph->num_vertices; i++) {
        distances[i] = SIZE_MAX;
    }

    distances[start] = 0;
    visited[start] = true;
    queue_enqueue(queue, start);

    while (!queue_is_empty(queue)) {
        size_t vertex = queue_dequeue(queue);

        for (AdjNode *adj = graph->adj_lists[vertex]; adj; adj = adj->next) {
            if (!visited[adj->vertex]) {
                visited[adj->vertex] = true;
                distances[adj->vertex] = distances[vertex] + 1;
                queue_enqueue(queue, adj->vertex);
            }
        }
    }

    queue_destroy(queue);
    free(visited);
    return BFS_OK;
}

BfsResult bfs_is_connected(const Graph *graph, bool *connected) {
    if (!graph || !connected) {
        return BFS_ERR_NULL;
    }

    if (graph->num_vertices == 0) {
        *connected = true;
        return BFS_OK;
    }

    size_t *result = malloc(graph->num_vertices * sizeof(size_t));
    if (!result) {
        return BFS_ERR_ALLOC;
    }

    size_t result_size;
    BfsResult res = bfs_traverse(graph, 0, result, &result_size);

    if (res != BFS_OK) {
        free(result);
        return res;
    }

    *connected = (result_size == graph->num_vertices);
    free(result);
    return BFS_OK;
}

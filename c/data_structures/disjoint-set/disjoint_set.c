/**
 * @file disjoint_set.c
 * @brief Disjoint Set (Union-Find) implementation
 *
 * Uses path compression in find and union by rank.
 */

#include "disjoint_set.h"
#include <stdlib.h>

/** Disjoint set internal structure */
struct DisjointSet {
    size_t *parent;     /* Parent of each element */
    size_t *rank;       /* Rank (approximate depth) of each tree */
    size_t *set_size;   /* Size of each set (only valid for roots) */
    size_t n;           /* Number of elements */
    size_t count;       /* Number of disjoint sets */
};

DisjointSet *ds_create(size_t n) {
    DisjointSet *ds = malloc(sizeof(DisjointSet));
    if (!ds) {
        return NULL;
    }

    ds->parent = malloc(n * sizeof(size_t));
    ds->rank = malloc(n * sizeof(size_t));
    ds->set_size = malloc(n * sizeof(size_t));

    if (n > 0 && (!ds->parent || !ds->rank || !ds->set_size)) {
        free(ds->parent);
        free(ds->rank);
        free(ds->set_size);
        free(ds);
        return NULL;
    }

    /* Initialize: each element is its own parent */
    for (size_t i = 0; i < n; i++) {
        ds->parent[i] = i;
        ds->rank[i] = 0;
        ds->set_size[i] = 1;
    }

    ds->n = n;
    ds->count = n;
    return ds;
}

void ds_destroy(DisjointSet *ds) {
    if (ds) {
        free(ds->parent);
        free(ds->rank);
        free(ds->set_size);
        free(ds);
    }
}

/**
 * Internal find with path compression.
 */
static size_t find_internal(DisjointSet *ds, size_t x) {
    if (ds->parent[x] != x) {
        ds->parent[x] = find_internal(ds, ds->parent[x]);  /* Path compression */
    }
    return ds->parent[x];
}

DSResult ds_find(DisjointSet *ds, size_t x, size_t *out) {
    if (!ds) {
        return DS_ERR_NULL;
    }
    if (x >= ds->n) {
        return DS_ERR_INDEX;
    }

    size_t root = find_internal(ds, x);
    if (out) {
        *out = root;
    }
    return DS_OK;
}

DSResult ds_union(DisjointSet *ds, size_t x, size_t y) {
    if (!ds) {
        return DS_ERR_NULL;
    }
    if (x >= ds->n || y >= ds->n) {
        return DS_ERR_INDEX;
    }

    size_t root_x = find_internal(ds, x);
    size_t root_y = find_internal(ds, y);

    /* Already in same set */
    if (root_x == root_y) {
        return DS_OK;
    }

    /* Union by rank: attach smaller tree under larger tree */
    if (ds->rank[root_x] < ds->rank[root_y]) {
        ds->parent[root_x] = root_y;
        ds->set_size[root_y] += ds->set_size[root_x];
    } else if (ds->rank[root_x] > ds->rank[root_y]) {
        ds->parent[root_y] = root_x;
        ds->set_size[root_x] += ds->set_size[root_y];
    } else {
        ds->parent[root_y] = root_x;
        ds->set_size[root_x] += ds->set_size[root_y];
        ds->rank[root_x]++;
    }

    ds->count--;
    return DS_OK;
}

bool ds_connected(DisjointSet *ds, size_t x, size_t y) {
    if (!ds || x >= ds->n || y >= ds->n) {
        return false;
    }
    return find_internal(ds, x) == find_internal(ds, y);
}

size_t ds_count(const DisjointSet *ds) {
    return ds ? ds->count : 0;
}

size_t ds_size(const DisjointSet *ds) {
    return ds ? ds->n : 0;
}

DSResult ds_set_size(DisjointSet *ds, size_t x, size_t *out) {
    if (!ds) {
        return DS_ERR_NULL;
    }
    if (x >= ds->n) {
        return DS_ERR_INDEX;
    }

    size_t root = find_internal(ds, x);
    if (out) {
        *out = ds->set_size[root];
    }
    return DS_OK;
}

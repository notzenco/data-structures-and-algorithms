/**
 * @file disjoint_set.h
 * @brief Disjoint Set (Union-Find) interface
 *
 * A data structure that tracks a set of elements partitioned into disjoint subsets.
 * Uses path compression and union by rank for near O(1) operations.
 */

#ifndef DSA_DISJOINT_SET_H
#define DSA_DISJOINT_SET_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque disjoint set type */
typedef struct DisjointSet DisjointSet;

/** Result codes for disjoint set operations */
typedef enum {
    DS_OK = 0,          /**< Operation successful */
    DS_ERR_NULL,        /**< NULL pointer argument */
    DS_ERR_INDEX,       /**< Index out of bounds */
    DS_ERR_ALLOC        /**< Memory allocation failed */
} DSResult;

/**
 * Create a new disjoint set with n elements.
 * Initially each element is in its own set.
 * @param n Number of elements (0 to n-1)
 * @return New disjoint set or NULL on allocation failure
 */
DisjointSet *ds_create(size_t n);

/**
 * Destroy a disjoint set and free all memory.
 * @param ds Disjoint set to destroy (NULL safe)
 */
void ds_destroy(DisjointSet *ds);

/**
 * Find the representative (root) of the set containing element x.
 * Uses path compression for efficiency.
 * @param ds Target disjoint set
 * @param x Element to find
 * @param out Pointer to store the representative
 * @return DS_OK on success, DS_ERR_INDEX if x out of bounds
 */
DSResult ds_find(DisjointSet *ds, size_t x, size_t *out);

/**
 * Unite the sets containing elements x and y.
 * Uses union by rank for efficiency.
 * @param ds Target disjoint set
 * @param x First element
 * @param y Second element
 * @return DS_OK on success, DS_ERR_INDEX if x or y out of bounds
 */
DSResult ds_union(DisjointSet *ds, size_t x, size_t y);

/**
 * Check if two elements are in the same set.
 * @param ds Target disjoint set
 * @param x First element
 * @param y Second element
 * @return true if same set, false otherwise or on error
 */
bool ds_connected(DisjointSet *ds, size_t x, size_t y);

/**
 * Get the number of disjoint sets.
 * @param ds Target disjoint set
 * @return Number of sets, or 0 if NULL
 */
size_t ds_count(const DisjointSet *ds);

/**
 * Get the total number of elements.
 * @param ds Target disjoint set
 * @return Number of elements, or 0 if NULL
 */
size_t ds_size(const DisjointSet *ds);

/**
 * Get the size of the set containing element x.
 * @param ds Target disjoint set
 * @param x Element
 * @param out Pointer to store set size
 * @return DS_OK on success, DS_ERR_INDEX if x out of bounds
 */
DSResult ds_set_size(DisjointSet *ds, size_t x, size_t *out);

#endif /* DSA_DISJOINT_SET_H */

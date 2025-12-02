/**
 * @file hash_table.h
 * @brief Hash table interface using open addressing with linear probing
 *
 * A hash table that maps integer keys to integer values.
 * Uses open addressing with linear probing for collision resolution.
 */

#ifndef DSA_HASH_TABLE_H
#define DSA_HASH_TABLE_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque hash table type */
typedef struct HashTable HashTable;

/** Result codes for hash table operations */
typedef enum {
    HT_OK = 0,          /**< Operation successful */
    HT_ERR_NULL,        /**< NULL pointer argument */
    HT_ERR_NOT_FOUND,   /**< Key not found */
    HT_ERR_ALLOC        /**< Memory allocation failed */
} HTResult;

/**
 * Create a new hash table.
 * @param capacity Initial capacity (0 for default)
 * @return New hash table or NULL on allocation failure
 */
HashTable *ht_create(size_t capacity);

/**
 * Destroy a hash table and free all memory.
 * @param table Table to destroy (NULL safe)
 */
void ht_destroy(HashTable *table);

/**
 * Insert or update a key-value pair.
 * @param table Target table
 * @param key Key to insert
 * @param value Value to associate with key
 * @return HT_OK on success, error code otherwise
 */
HTResult ht_put(HashTable *table, int key, int value);

/**
 * Get the value associated with a key.
 * @param table Target table
 * @param key Key to look up
 * @param out Pointer to store value
 * @return HT_OK on success, HT_ERR_NOT_FOUND if key not present
 */
HTResult ht_get(const HashTable *table, int key, int *out);

/**
 * Remove a key-value pair.
 * @param table Target table
 * @param key Key to remove
 * @param out Pointer to store removed value (can be NULL)
 * @return HT_OK on success, HT_ERR_NOT_FOUND if key not present
 */
HTResult ht_remove(HashTable *table, int key, int *out);

/**
 * Check if a key exists in the table.
 * @param table Target table
 * @param key Key to check
 * @return true if key exists, false otherwise
 */
bool ht_contains(const HashTable *table, int key);

/**
 * Get the number of key-value pairs in the table.
 * @param table Target table
 * @return Number of pairs, or 0 if NULL
 */
size_t ht_size(const HashTable *table);

/**
 * Check if the table is empty.
 * @param table Target table
 * @return true if empty or NULL, false otherwise
 */
bool ht_is_empty(const HashTable *table);

/**
 * Remove all key-value pairs from the table.
 * @param table Target table
 * @return HT_OK on success, HT_ERR_NULL if NULL
 */
HTResult ht_clear(HashTable *table);

#endif /* DSA_HASH_TABLE_H */

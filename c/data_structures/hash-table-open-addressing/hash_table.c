/**
 * @file hash_table.c
 * @brief Hash table implementation using open addressing with linear probing
 */

#include "hash_table.h"
#include <stdlib.h>

#define DEFAULT_CAPACITY 16
#define LOAD_FACTOR_THRESHOLD 0.75

/** Entry state */
typedef enum {
    ENTRY_EMPTY,
    ENTRY_OCCUPIED,
    ENTRY_DELETED
} EntryState;

/** Hash table entry */
typedef struct {
    int key;
    int value;
    EntryState state;
} Entry;

/** Hash table internal structure */
struct HashTable {
    Entry *entries;
    size_t size;
    size_t capacity;
};

/**
 * Compute hash for a key.
 * @param key Key to hash
 * @param capacity Table capacity
 * @return Hash value in range [0, capacity)
 */
static size_t hash(int key, size_t capacity) {
    /* Handle negative keys */
    unsigned int ukey = (unsigned int)key;
    return ukey % capacity;
}

/**
 * Find slot for a key (for insertion or lookup).
 * @param table Target table
 * @param key Key to find
 * @param for_insert If true, returns first available slot; if false, returns occupied slot only
 * @return Slot index, or capacity if not found
 */
static size_t find_slot(const HashTable *table, int key, bool for_insert) {
    size_t index = hash(key, table->capacity);
    size_t first_deleted = table->capacity;  /* Invalid marker */

    for (size_t i = 0; i < table->capacity; i++) {
        size_t probe = (index + i) % table->capacity;
        Entry *entry = &table->entries[probe];

        if (entry->state == ENTRY_EMPTY) {
            if (for_insert) {
                return (first_deleted < table->capacity) ? first_deleted : probe;
            }
            return table->capacity;  /* Not found */
        }

        if (entry->state == ENTRY_DELETED) {
            if (for_insert && first_deleted == table->capacity) {
                first_deleted = probe;
            }
            continue;
        }

        /* ENTRY_OCCUPIED */
        if (entry->key == key) {
            return probe;
        }
    }

    /* Table is full (shouldn't happen with proper load factor) */
    return for_insert ? first_deleted : table->capacity;
}

/**
 * Resize the hash table.
 * @param table Table to resize
 * @param new_capacity New capacity
 * @return HT_OK on success, HT_ERR_ALLOC on failure
 */
static HTResult ht_resize(HashTable *table, size_t new_capacity) {
    Entry *old_entries = table->entries;
    size_t old_capacity = table->capacity;

    /* Allocate new entries */
    table->entries = calloc(new_capacity, sizeof(Entry));
    if (!table->entries) {
        table->entries = old_entries;
        return HT_ERR_ALLOC;
    }

    table->capacity = new_capacity;
    table->size = 0;

    /* Rehash all existing entries */
    for (size_t i = 0; i < old_capacity; i++) {
        if (old_entries[i].state == ENTRY_OCCUPIED) {
            size_t slot = find_slot(table, old_entries[i].key, true);
            table->entries[slot].key = old_entries[i].key;
            table->entries[slot].value = old_entries[i].value;
            table->entries[slot].state = ENTRY_OCCUPIED;
            table->size++;
        }
    }

    free(old_entries);
    return HT_OK;
}

HashTable *ht_create(size_t capacity) {
    HashTable *table = malloc(sizeof(HashTable));
    if (!table) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    table->entries = calloc(capacity, sizeof(Entry));
    if (!table->entries) {
        free(table);
        return NULL;
    }

    table->size = 0;
    table->capacity = capacity;
    return table;
}

void ht_destroy(HashTable *table) {
    if (table) {
        free(table->entries);
        free(table);
    }
}

HTResult ht_put(HashTable *table, int key, int value) {
    if (!table) {
        return HT_ERR_NULL;
    }

    /* Check load factor and resize if needed */
    if ((double)(table->size + 1) / table->capacity > LOAD_FACTOR_THRESHOLD) {
        HTResult result = ht_resize(table, table->capacity * 2);
        if (result != HT_OK) {
            return result;
        }
    }

    size_t slot = find_slot(table, key, true);
    bool is_new = (table->entries[slot].state != ENTRY_OCCUPIED);

    table->entries[slot].key = key;
    table->entries[slot].value = value;
    table->entries[slot].state = ENTRY_OCCUPIED;

    if (is_new) {
        table->size++;
    }

    return HT_OK;
}

HTResult ht_get(const HashTable *table, int key, int *out) {
    if (!table) {
        return HT_ERR_NULL;
    }

    size_t slot = find_slot(table, key, false);
    if (slot == table->capacity) {
        return HT_ERR_NOT_FOUND;
    }

    if (out) {
        *out = table->entries[slot].value;
    }
    return HT_OK;
}

HTResult ht_remove(HashTable *table, int key, int *out) {
    if (!table) {
        return HT_ERR_NULL;
    }

    size_t slot = find_slot(table, key, false);
    if (slot == table->capacity) {
        return HT_ERR_NOT_FOUND;
    }

    if (out) {
        *out = table->entries[slot].value;
    }

    table->entries[slot].state = ENTRY_DELETED;
    table->size--;
    return HT_OK;
}

bool ht_contains(const HashTable *table, int key) {
    if (!table) {
        return false;
    }
    return find_slot(table, key, false) != table->capacity;
}

size_t ht_size(const HashTable *table) {
    return table ? table->size : 0;
}

bool ht_is_empty(const HashTable *table) {
    return !table || table->size == 0;
}

HTResult ht_clear(HashTable *table) {
    if (!table) {
        return HT_ERR_NULL;
    }

    for (size_t i = 0; i < table->capacity; i++) {
        table->entries[i].state = ENTRY_EMPTY;
    }
    table->size = 0;
    return HT_OK;
}

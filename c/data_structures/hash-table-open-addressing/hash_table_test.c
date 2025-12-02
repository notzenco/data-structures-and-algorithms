/**
 * @file hash_table_test.c
 * @brief Unit tests for hash table implementation
 */

#include "hash_table.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    HashTable *table = ht_create(0);
    assert(table != NULL);
    assert(ht_is_empty(table));
    assert(ht_size(table) == 0);

    ht_destroy(table);
    ht_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_put_get(void) {
    printf("  test_put_get...");

    HashTable *table = ht_create(0);
    int value;

    /* Insert key-value pairs */
    assert(ht_put(table, 1, 100) == HT_OK);
    assert(ht_put(table, 2, 200) == HT_OK);
    assert(ht_put(table, 3, 300) == HT_OK);
    assert(ht_size(table) == 3);

    /* Get values */
    assert(ht_get(table, 1, &value) == HT_OK);
    assert(value == 100);
    assert(ht_get(table, 2, &value) == HT_OK);
    assert(value == 200);
    assert(ht_get(table, 3, &value) == HT_OK);
    assert(value == 300);

    /* Key not found */
    assert(ht_get(table, 99, &value) == HT_ERR_NOT_FOUND);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_update(void) {
    printf("  test_update...");

    HashTable *table = ht_create(0);
    int value;

    ht_put(table, 1, 100);
    assert(ht_size(table) == 1);

    /* Update existing key */
    ht_put(table, 1, 999);
    assert(ht_size(table) == 1);  /* Size unchanged */

    assert(ht_get(table, 1, &value) == HT_OK);
    assert(value == 999);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_remove(void) {
    printf("  test_remove...");

    HashTable *table = ht_create(0);
    int value;

    ht_put(table, 1, 100);
    ht_put(table, 2, 200);
    ht_put(table, 3, 300);

    /* Remove middle key */
    assert(ht_remove(table, 2, &value) == HT_OK);
    assert(value == 200);
    assert(ht_size(table) == 2);
    assert(!ht_contains(table, 2));

    /* Other keys still accessible */
    assert(ht_get(table, 1, &value) == HT_OK && value == 100);
    assert(ht_get(table, 3, &value) == HT_OK && value == 300);

    /* Remove non-existent key */
    assert(ht_remove(table, 99, &value) == HT_ERR_NOT_FOUND);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_contains(void) {
    printf("  test_contains...");

    HashTable *table = ht_create(0);

    ht_put(table, 42, 100);

    assert(ht_contains(table, 42) == true);
    assert(ht_contains(table, 99) == false);

    ht_remove(table, 42, NULL);
    assert(ht_contains(table, 42) == false);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_collision_handling(void) {
    printf("  test_collision_handling...");

    HashTable *table = ht_create(4);  /* Small capacity to force collisions */
    int value;

    /* Insert keys that will likely collide */
    for (int i = 0; i < 10; i++) {
        assert(ht_put(table, i * 4, i * 100) == HT_OK);
    }

    /* Verify all values */
    for (int i = 0; i < 10; i++) {
        assert(ht_get(table, i * 4, &value) == HT_OK);
        assert(value == i * 100);
    }

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    HashTable *table = ht_create(4);  /* Small initial capacity */
    int value;

    /* Insert many items to trigger resize */
    for (int i = 0; i < 100; i++) {
        assert(ht_put(table, i, i * 10) == HT_OK);
    }
    assert(ht_size(table) == 100);

    /* Verify all values after resize */
    for (int i = 0; i < 100; i++) {
        assert(ht_get(table, i, &value) == HT_OK);
        assert(value == i * 10);
    }

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_negative_keys(void) {
    printf("  test_negative_keys...");

    HashTable *table = ht_create(0);
    int value;

    /* Negative keys should work */
    assert(ht_put(table, -1, 100) == HT_OK);
    assert(ht_put(table, -100, 200) == HT_OK);
    assert(ht_put(table, -999, 300) == HT_OK);

    assert(ht_get(table, -1, &value) == HT_OK && value == 100);
    assert(ht_get(table, -100, &value) == HT_OK && value == 200);
    assert(ht_get(table, -999, &value) == HT_OK && value == 300);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_remove_and_reinsert(void) {
    printf("  test_remove_and_reinsert...");

    HashTable *table = ht_create(4);
    int value;

    /* Insert some keys */
    ht_put(table, 1, 100);
    ht_put(table, 5, 500);  /* May collide with key 1 in small table */
    ht_put(table, 9, 900);  /* May collide further */

    /* Remove middle key */
    ht_remove(table, 5, NULL);

    /* Key after deleted slot should still be accessible */
    assert(ht_get(table, 9, &value) == HT_OK && value == 900);

    /* Reinsert removed key */
    ht_put(table, 5, 555);
    assert(ht_get(table, 5, &value) == HT_OK && value == 555);

    /* All keys accessible */
    assert(ht_get(table, 1, &value) == HT_OK && value == 100);
    assert(ht_get(table, 9, &value) == HT_OK && value == 900);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    HashTable *table = ht_create(0);

    ht_put(table, 1, 100);
    ht_put(table, 2, 200);
    ht_put(table, 3, 300);
    assert(ht_size(table) == 3);

    assert(ht_clear(table) == HT_OK);
    assert(ht_is_empty(table));
    assert(ht_size(table) == 0);
    assert(!ht_contains(table, 1));

    /* Can still use after clear */
    ht_put(table, 99, 999);
    assert(ht_size(table) == 1);

    ht_destroy(table);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(ht_put(NULL, 1, 100) == HT_ERR_NULL);
    assert(ht_get(NULL, 1, &value) == HT_ERR_NULL);
    assert(ht_remove(NULL, 1, &value) == HT_ERR_NULL);
    assert(ht_contains(NULL, 1) == false);
    assert(ht_clear(NULL) == HT_ERR_NULL);
    assert(ht_is_empty(NULL) == true);
    assert(ht_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running hash table tests...\n");

    test_create_destroy();
    test_put_get();
    test_update();
    test_remove();
    test_contains();
    test_collision_handling();
    test_resize();
    test_negative_keys();
    test_remove_and_reinsert();
    test_clear();
    test_null_safety();

    printf("All hash table tests PASSED!\n");
    return 0;
}

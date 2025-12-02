/**
 * @file disjoint_set_test.c
 * @brief Unit tests for disjoint set implementation
 */

#include "disjoint_set.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    DisjointSet *ds = ds_create(10);
    assert(ds != NULL);
    assert(ds_size(ds) == 10);
    assert(ds_count(ds) == 10);  /* Initially 10 separate sets */

    ds_destroy(ds);
    ds_destroy(NULL);  /* Should not crash */

    /* Empty disjoint set */
    DisjointSet *empty = ds_create(0);
    assert(empty != NULL);
    assert(ds_size(empty) == 0);
    assert(ds_count(empty) == 0);
    ds_destroy(empty);

    printf(" PASSED\n");
}

static void test_find(void) {
    printf("  test_find...");

    DisjointSet *ds = ds_create(5);
    size_t root;

    /* Initially each element is its own root */
    assert(ds_find(ds, 0, &root) == DS_OK && root == 0);
    assert(ds_find(ds, 1, &root) == DS_OK && root == 1);
    assert(ds_find(ds, 2, &root) == DS_OK && root == 2);
    assert(ds_find(ds, 3, &root) == DS_OK && root == 3);
    assert(ds_find(ds, 4, &root) == DS_OK && root == 4);

    /* Out of bounds */
    assert(ds_find(ds, 5, &root) == DS_ERR_INDEX);
    assert(ds_find(ds, 100, &root) == DS_ERR_INDEX);

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_union(void) {
    printf("  test_union...");

    DisjointSet *ds = ds_create(5);
    size_t root0, root1;

    /* Union 0 and 1 */
    assert(ds_union(ds, 0, 1) == DS_OK);
    assert(ds_count(ds) == 4);

    ds_find(ds, 0, &root0);
    ds_find(ds, 1, &root1);
    assert(root0 == root1);

    /* Union 2 and 3 */
    assert(ds_union(ds, 2, 3) == DS_OK);
    assert(ds_count(ds) == 3);

    /* Union {0,1} and {2,3} */
    assert(ds_union(ds, 0, 2) == DS_OK);
    assert(ds_count(ds) == 2);

    /* All four should have same root */
    size_t r0, r1, r2, r3;
    ds_find(ds, 0, &r0);
    ds_find(ds, 1, &r1);
    ds_find(ds, 2, &r2);
    ds_find(ds, 3, &r3);
    assert(r0 == r1 && r1 == r2 && r2 == r3);

    /* Union already united elements */
    assert(ds_union(ds, 0, 3) == DS_OK);
    assert(ds_count(ds) == 2);  /* Count unchanged */

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_connected(void) {
    printf("  test_connected...");

    DisjointSet *ds = ds_create(6);

    /* Initially nothing connected */
    assert(ds_connected(ds, 0, 1) == false);
    assert(ds_connected(ds, 0, 0) == true);  /* Element connected to itself */

    /* Create some connections */
    ds_union(ds, 0, 1);
    ds_union(ds, 2, 3);
    ds_union(ds, 4, 5);

    assert(ds_connected(ds, 0, 1) == true);
    assert(ds_connected(ds, 2, 3) == true);
    assert(ds_connected(ds, 4, 5) == true);

    assert(ds_connected(ds, 0, 2) == false);
    assert(ds_connected(ds, 1, 3) == false);

    /* Connect two groups */
    ds_union(ds, 1, 2);
    assert(ds_connected(ds, 0, 3) == true);

    /* Out of bounds */
    assert(ds_connected(ds, 0, 100) == false);

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_set_size(void) {
    printf("  test_set_size...");

    DisjointSet *ds = ds_create(5);
    size_t size;

    /* Initially all sets have size 1 */
    assert(ds_set_size(ds, 0, &size) == DS_OK && size == 1);
    assert(ds_set_size(ds, 1, &size) == DS_OK && size == 1);

    ds_union(ds, 0, 1);
    assert(ds_set_size(ds, 0, &size) == DS_OK && size == 2);
    assert(ds_set_size(ds, 1, &size) == DS_OK && size == 2);

    ds_union(ds, 2, 3);
    ds_union(ds, 0, 2);
    assert(ds_set_size(ds, 0, &size) == DS_OK && size == 4);
    assert(ds_set_size(ds, 3, &size) == DS_OK && size == 4);

    /* Element 4 still alone */
    assert(ds_set_size(ds, 4, &size) == DS_OK && size == 1);

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_path_compression(void) {
    printf("  test_path_compression...");

    DisjointSet *ds = ds_create(1000);

    /* Create a long chain: 0 -> 1 -> 2 -> ... -> 999 */
    for (size_t i = 0; i < 999; i++) {
        ds_union(ds, i, i + 1);
    }

    assert(ds_count(ds) == 1);

    /* After find, path should be compressed */
    size_t root;
    ds_find(ds, 0, &root);
    ds_find(ds, 500, &root);
    ds_find(ds, 999, &root);

    /* All elements should quickly find the same root now */
    size_t r0, r500, r999;
    ds_find(ds, 0, &r0);
    ds_find(ds, 500, &r500);
    ds_find(ds, 999, &r999);
    assert(r0 == r500 && r500 == r999);

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_union_by_rank(void) {
    printf("  test_union_by_rank...");

    DisjointSet *ds = ds_create(8);

    /* Create two balanced trees */
    ds_union(ds, 0, 1);
    ds_union(ds, 2, 3);
    ds_union(ds, 0, 2);  /* Tree of 4 elements */

    ds_union(ds, 4, 5);
    ds_union(ds, 6, 7);
    ds_union(ds, 4, 6);  /* Another tree of 4 elements */

    assert(ds_count(ds) == 2);

    /* Union the two trees */
    ds_union(ds, 0, 4);
    assert(ds_count(ds) == 1);

    /* All connected */
    for (size_t i = 0; i < 7; i++) {
        assert(ds_connected(ds, i, i + 1) == true);
    }

    ds_destroy(ds);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    size_t value;

    /* All functions should handle NULL gracefully */
    assert(ds_find(NULL, 0, &value) == DS_ERR_NULL);
    assert(ds_union(NULL, 0, 1) == DS_ERR_NULL);
    assert(ds_connected(NULL, 0, 1) == false);
    assert(ds_set_size(NULL, 0, &value) == DS_ERR_NULL);
    assert(ds_count(NULL) == 0);
    assert(ds_size(NULL) == 0);

    printf(" PASSED\n");
}

static void test_index_bounds(void) {
    printf("  test_index_bounds...");

    DisjointSet *ds = ds_create(5);
    size_t value;

    /* Test boundary conditions */
    assert(ds_find(ds, 4, &value) == DS_OK);  /* Last valid index */
    assert(ds_find(ds, 5, &value) == DS_ERR_INDEX);  /* First invalid */

    assert(ds_union(ds, 0, 4) == DS_OK);
    assert(ds_union(ds, 0, 5) == DS_ERR_INDEX);
    assert(ds_union(ds, 5, 0) == DS_ERR_INDEX);

    assert(ds_set_size(ds, 4, &value) == DS_OK);
    assert(ds_set_size(ds, 5, &value) == DS_ERR_INDEX);

    ds_destroy(ds);
    printf(" PASSED\n");
}

int main(void) {
    printf("Running disjoint set tests...\n");

    test_create_destroy();
    test_find();
    test_union();
    test_connected();
    test_set_size();
    test_path_compression();
    test_union_by_rank();
    test_null_safety();
    test_index_bounds();

    printf("All disjoint set tests PASSED!\n");
    return 0;
}

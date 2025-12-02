/**
 * @file bst_test.c
 * @brief Unit tests for BST implementation
 */

#include "bst.h"
#include <assert.h>
#include <stdio.h>

/* Helper for collecting values during traversal */
typedef struct {
    int *values;
    size_t count;
} CollectContext;

static void collect_visitor(int value, void *user_data) {
    CollectContext *ctx = (CollectContext *)user_data;
    ctx->values[ctx->count++] = value;
}

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    BST *tree = bst_create();
    assert(tree != NULL);
    assert(bst_is_empty(tree));
    assert(bst_size(tree) == 0);
    assert(bst_height(tree) == 0);

    bst_destroy(tree);
    bst_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_insert(void) {
    printf("  test_insert...");

    BST *tree = bst_create();

    assert(bst_insert(tree, 50) == BST_OK);
    assert(bst_insert(tree, 25) == BST_OK);
    assert(bst_insert(tree, 75) == BST_OK);
    assert(bst_insert(tree, 10) == BST_OK);
    assert(bst_insert(tree, 30) == BST_OK);

    assert(bst_size(tree) == 5);
    assert(!bst_is_empty(tree));

    /* Duplicate insert should fail */
    assert(bst_insert(tree, 50) == BST_ERR_DUPLICATE);
    assert(bst_insert(tree, 25) == BST_ERR_DUPLICATE);
    assert(bst_size(tree) == 5);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_contains(void) {
    printf("  test_contains...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);

    assert(bst_contains(tree, 50) == true);
    assert(bst_contains(tree, 25) == true);
    assert(bst_contains(tree, 75) == true);
    assert(bst_contains(tree, 10) == false);
    assert(bst_contains(tree, 100) == false);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_min_max(void) {
    printf("  test_min_max...");

    BST *tree = bst_create();
    int value;

    /* Empty tree */
    assert(bst_min(tree, &value) == BST_ERR_EMPTY);
    assert(bst_max(tree, &value) == BST_ERR_EMPTY);

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);
    bst_insert(tree, 10);
    bst_insert(tree, 90);

    assert(bst_min(tree, &value) == BST_OK);
    assert(value == 10);

    assert(bst_max(tree, &value) == BST_OK);
    assert(value == 90);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_remove_leaf(void) {
    printf("  test_remove_leaf...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);

    /* Remove leaf node */
    assert(bst_remove(tree, 25) == BST_OK);
    assert(bst_size(tree) == 2);
    assert(!bst_contains(tree, 25));
    assert(bst_contains(tree, 50));
    assert(bst_contains(tree, 75));

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_remove_one_child(void) {
    printf("  test_remove_one_child...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 10);  /* 25 has only left child */

    /* Remove node with one child */
    assert(bst_remove(tree, 25) == BST_OK);
    assert(bst_size(tree) == 2);
    assert(!bst_contains(tree, 25));
    assert(bst_contains(tree, 10));
    assert(bst_contains(tree, 50));

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_remove_two_children(void) {
    printf("  test_remove_two_children...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);
    bst_insert(tree, 10);
    bst_insert(tree, 30);
    bst_insert(tree, 60);
    bst_insert(tree, 90);

    /* Remove node with two children */
    assert(bst_remove(tree, 50) == BST_OK);
    assert(bst_size(tree) == 6);
    assert(!bst_contains(tree, 50));

    /* All other nodes still present */
    assert(bst_contains(tree, 25));
    assert(bst_contains(tree, 75));
    assert(bst_contains(tree, 10));
    assert(bst_contains(tree, 30));
    assert(bst_contains(tree, 60));
    assert(bst_contains(tree, 90));

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_remove_not_found(void) {
    printf("  test_remove_not_found...");

    BST *tree = bst_create();

    bst_insert(tree, 50);

    assert(bst_remove(tree, 99) == BST_ERR_NOT_FOUND);
    assert(bst_size(tree) == 1);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_height(void) {
    printf("  test_height...");

    BST *tree = bst_create();

    assert(bst_height(tree) == 0);

    bst_insert(tree, 50);
    assert(bst_height(tree) == 1);

    bst_insert(tree, 25);
    bst_insert(tree, 75);
    assert(bst_height(tree) == 2);

    bst_insert(tree, 10);
    bst_insert(tree, 5);
    assert(bst_height(tree) == 4);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_inorder_traversal(void) {
    printf("  test_inorder_traversal...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);
    bst_insert(tree, 10);
    bst_insert(tree, 30);

    int values[5];
    CollectContext ctx = {values, 0};
    assert(bst_inorder(tree, collect_visitor, &ctx) == BST_OK);

    /* Should be sorted */
    assert(ctx.count == 5);
    assert(values[0] == 10);
    assert(values[1] == 25);
    assert(values[2] == 30);
    assert(values[3] == 50);
    assert(values[4] == 75);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_preorder_traversal(void) {
    printf("  test_preorder_traversal...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);

    int values[3];
    CollectContext ctx = {values, 0};
    assert(bst_preorder(tree, collect_visitor, &ctx) == BST_OK);

    /* Root first */
    assert(ctx.count == 3);
    assert(values[0] == 50);
    assert(values[1] == 25);
    assert(values[2] == 75);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_postorder_traversal(void) {
    printf("  test_postorder_traversal...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);

    int values[3];
    CollectContext ctx = {values, 0};
    assert(bst_postorder(tree, collect_visitor, &ctx) == BST_OK);

    /* Root last */
    assert(ctx.count == 3);
    assert(values[0] == 25);
    assert(values[1] == 75);
    assert(values[2] == 50);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    BST *tree = bst_create();

    bst_insert(tree, 50);
    bst_insert(tree, 25);
    bst_insert(tree, 75);
    assert(bst_size(tree) == 3);

    assert(bst_clear(tree) == BST_OK);
    assert(bst_is_empty(tree));
    assert(bst_size(tree) == 0);
    assert(bst_height(tree) == 0);

    /* Can still use after clear */
    bst_insert(tree, 99);
    assert(bst_size(tree) == 1);

    bst_destroy(tree);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(bst_insert(NULL, 1) == BST_ERR_NULL);
    assert(bst_remove(NULL, 1) == BST_ERR_NULL);
    assert(bst_contains(NULL, 1) == false);
    assert(bst_min(NULL, &value) == BST_ERR_NULL);
    assert(bst_max(NULL, &value) == BST_ERR_NULL);
    assert(bst_clear(NULL) == BST_ERR_NULL);
    assert(bst_inorder(NULL, collect_visitor, NULL) == BST_ERR_NULL);
    assert(bst_preorder(NULL, collect_visitor, NULL) == BST_ERR_NULL);
    assert(bst_postorder(NULL, collect_visitor, NULL) == BST_ERR_NULL);
    assert(bst_is_empty(NULL) == true);
    assert(bst_size(NULL) == 0);
    assert(bst_height(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running BST tests...\n");

    test_create_destroy();
    test_insert();
    test_contains();
    test_min_max();
    test_remove_leaf();
    test_remove_one_child();
    test_remove_two_children();
    test_remove_not_found();
    test_height();
    test_inorder_traversal();
    test_preorder_traversal();
    test_postorder_traversal();
    test_clear();
    test_null_safety();

    printf("All BST tests PASSED!\n");
    return 0;
}

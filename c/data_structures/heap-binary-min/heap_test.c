/**
 * @file heap_test.c
 * @brief Unit tests for binary min heap implementation
 */

#include "heap.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    Heap *heap = heap_create(0);
    assert(heap != NULL);
    assert(heap_is_empty(heap));
    assert(heap_size(heap) == 0);

    heap_destroy(heap);
    heap_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_pop(void) {
    printf("  test_push_pop...");

    Heap *heap = heap_create(4);
    int value;

    /* Push elements */
    assert(heap_push(heap, 30) == HEAP_OK);
    assert(heap_push(heap, 10) == HEAP_OK);
    assert(heap_push(heap, 20) == HEAP_OK);
    assert(heap_size(heap) == 3);

    /* Pop should return in sorted order (min first) */
    assert(heap_pop(heap, &value) == HEAP_OK);
    assert(value == 10);
    assert(heap_pop(heap, &value) == HEAP_OK);
    assert(value == 20);
    assert(heap_pop(heap, &value) == HEAP_OK);
    assert(value == 30);

    assert(heap_is_empty(heap));
    assert(heap_pop(heap, &value) == HEAP_ERR_EMPTY);

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_peek(void) {
    printf("  test_peek...");

    Heap *heap = heap_create(0);
    int value;

    /* Peek on empty */
    assert(heap_peek(heap, &value) == HEAP_ERR_EMPTY);

    heap_push(heap, 50);
    heap_push(heap, 30);
    heap_push(heap, 70);

    /* Peek returns min */
    assert(heap_peek(heap, &value) == HEAP_OK);
    assert(value == 30);
    assert(heap_size(heap) == 3);  /* Size unchanged */

    /* Peek still returns min */
    assert(heap_peek(heap, &value) == HEAP_OK);
    assert(value == 30);

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_heap_property(void) {
    printf("  test_heap_property...");

    Heap *heap = heap_create(0);
    int values[] = {50, 30, 70, 10, 40, 60, 80, 5, 35};
    int n = sizeof(values) / sizeof(values[0]);

    /* Push all values */
    for (int i = 0; i < n; i++) {
        heap_push(heap, values[i]);
    }

    /* Pop should return in sorted order */
    int prev = -999999;
    int value;
    while (heap_pop(heap, &value) == HEAP_OK) {
        assert(value >= prev);  /* Each value >= previous */
        prev = value;
    }

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_duplicates(void) {
    printf("  test_duplicates...");

    Heap *heap = heap_create(0);
    int value;

    /* Duplicates are allowed */
    heap_push(heap, 10);
    heap_push(heap, 10);
    heap_push(heap, 10);
    assert(heap_size(heap) == 3);

    assert(heap_pop(heap, &value) == HEAP_OK && value == 10);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 10);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 10);

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_negative_values(void) {
    printf("  test_negative_values...");

    Heap *heap = heap_create(0);
    int value;

    heap_push(heap, -10);
    heap_push(heap, 5);
    heap_push(heap, -20);
    heap_push(heap, 0);

    assert(heap_pop(heap, &value) == HEAP_OK && value == -20);
    assert(heap_pop(heap, &value) == HEAP_OK && value == -10);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 0);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 5);

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    Heap *heap = heap_create(2);  /* Small initial capacity */
    int value;

    /* Push more than initial capacity */
    for (int i = 100; i >= 1; i--) {
        assert(heap_push(heap, i) == HEAP_OK);
    }
    assert(heap_size(heap) == 100);

    /* Pop should return sorted */
    for (int i = 1; i <= 100; i++) {
        assert(heap_pop(heap, &value) == HEAP_OK);
        assert(value == i);
    }

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_from_array(void) {
    printf("  test_from_array...");

    int values[] = {50, 30, 70, 10, 40, 60, 80};
    int n = sizeof(values) / sizeof(values[0]);

    Heap *heap = heap_from_array(values, n);
    assert(heap != NULL);
    assert(heap_size(heap) == (size_t)n);

    /* Should extract in sorted order */
    int value;
    assert(heap_pop(heap, &value) == HEAP_OK && value == 10);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 30);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 40);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 50);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 60);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 70);
    assert(heap_pop(heap, &value) == HEAP_OK && value == 80);

    heap_destroy(heap);

    /* Empty array */
    Heap *empty = heap_from_array(NULL, 0);
    assert(empty != NULL);
    assert(heap_is_empty(empty));
    heap_destroy(empty);

    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    Heap *heap = heap_create(0);

    heap_push(heap, 10);
    heap_push(heap, 20);
    heap_push(heap, 30);
    assert(heap_size(heap) == 3);

    assert(heap_clear(heap) == HEAP_OK);
    assert(heap_is_empty(heap));
    assert(heap_size(heap) == 0);

    /* Can still use after clear */
    heap_push(heap, 99);
    assert(heap_size(heap) == 1);

    heap_destroy(heap);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(heap_push(NULL, 1) == HEAP_ERR_NULL);
    assert(heap_pop(NULL, &value) == HEAP_ERR_NULL);
    assert(heap_peek(NULL, &value) == HEAP_ERR_NULL);
    assert(heap_clear(NULL) == HEAP_ERR_NULL);
    assert(heap_is_empty(NULL) == true);
    assert(heap_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running heap tests...\n");

    test_create_destroy();
    test_push_pop();
    test_peek();
    test_heap_property();
    test_duplicates();
    test_negative_values();
    test_resize();
    test_from_array();
    test_clear();
    test_null_safety();

    printf("All heap tests PASSED!\n");
    return 0;
}

/**
 * @file dynamic_array_test.c
 * @brief Unit tests for dynamic array implementation
 */

#include "dynamic_array.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    DynamicArray *array = da_create(0);
    assert(array != NULL);
    assert(da_is_empty(array));
    assert(da_size(array) == 0);
    assert(da_capacity(array) > 0);

    da_destroy(array);
    da_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_pop(void) {
    printf("  test_push_pop...");

    DynamicArray *array = da_create(4);
    int value;

    /* Push elements */
    assert(da_push(array, 10) == DA_OK);
    assert(da_push(array, 20) == DA_OK);
    assert(da_push(array, 30) == DA_OK);
    assert(da_size(array) == 3);
    assert(!da_is_empty(array));

    /* Pop elements (LIFO order for push/pop) */
    assert(da_pop(array, &value) == DA_OK);
    assert(value == 30);
    assert(da_pop(array, &value) == DA_OK);
    assert(value == 20);
    assert(da_pop(array, &value) == DA_OK);
    assert(value == 10);

    assert(da_is_empty(array));
    assert(da_pop(array, &value) == DA_ERR_EMPTY);

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_get_set(void) {
    printf("  test_get_set...");

    DynamicArray *array = da_create(0);
    int value;

    da_push(array, 1);
    da_push(array, 2);
    da_push(array, 3);

    /* Get values */
    assert(da_get(array, 0, &value) == DA_OK);
    assert(value == 1);
    assert(da_get(array, 1, &value) == DA_OK);
    assert(value == 2);
    assert(da_get(array, 2, &value) == DA_OK);
    assert(value == 3);

    /* Out of bounds */
    assert(da_get(array, 3, &value) == DA_ERR_INDEX);
    assert(da_get(array, 100, &value) == DA_ERR_INDEX);

    /* Set values */
    assert(da_set(array, 1, 99) == DA_OK);
    assert(da_get(array, 1, &value) == DA_OK);
    assert(value == 99);

    /* Set out of bounds */
    assert(da_set(array, 10, 42) == DA_ERR_INDEX);

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_insert_remove(void) {
    printf("  test_insert_remove...");

    DynamicArray *array = da_create(0);
    int value;

    /* Insert at end */
    assert(da_insert(array, 0, 10) == DA_OK);
    assert(da_insert(array, 1, 30) == DA_OK);

    /* Insert in middle */
    assert(da_insert(array, 1, 20) == DA_OK);

    /* Verify order: 10, 20, 30 */
    assert(da_get(array, 0, &value) == DA_OK && value == 10);
    assert(da_get(array, 1, &value) == DA_OK && value == 20);
    assert(da_get(array, 2, &value) == DA_OK && value == 30);

    /* Insert at beginning */
    assert(da_insert(array, 0, 5) == DA_OK);
    assert(da_get(array, 0, &value) == DA_OK && value == 5);
    assert(da_size(array) == 4);

    /* Remove from middle */
    assert(da_remove(array, 1, &value) == DA_OK);
    assert(value == 10);
    assert(da_size(array) == 3);

    /* Verify order: 5, 20, 30 */
    assert(da_get(array, 0, &value) == DA_OK && value == 5);
    assert(da_get(array, 1, &value) == DA_OK && value == 20);
    assert(da_get(array, 2, &value) == DA_OK && value == 30);

    /* Insert out of bounds */
    assert(da_insert(array, 100, 42) == DA_ERR_INDEX);

    /* Remove out of bounds */
    assert(da_remove(array, 100, &value) == DA_ERR_INDEX);

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    DynamicArray *array = da_create(2);  /* Small initial capacity */

    /* Push more than initial capacity */
    for (int i = 0; i < 100; i++) {
        assert(da_push(array, i) == DA_OK);
    }
    assert(da_size(array) == 100);
    assert(da_capacity(array) >= 100);

    /* Verify values */
    int value;
    for (int i = 0; i < 100; i++) {
        assert(da_get(array, (size_t)i, &value) == DA_OK);
        assert(value == i);
    }

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_shrink_to_fit(void) {
    printf("  test_shrink_to_fit...");

    DynamicArray *array = da_create(100);

    da_push(array, 1);
    da_push(array, 2);
    da_push(array, 3);

    assert(da_capacity(array) == 100);
    assert(da_shrink_to_fit(array) == DA_OK);
    assert(da_capacity(array) == 3);
    assert(da_size(array) == 3);

    /* Verify values still intact */
    int value;
    assert(da_get(array, 0, &value) == DA_OK && value == 1);
    assert(da_get(array, 1, &value) == DA_OK && value == 2);
    assert(da_get(array, 2, &value) == DA_OK && value == 3);

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    DynamicArray *array = da_create(0);

    da_push(array, 1);
    da_push(array, 2);
    da_push(array, 3);
    assert(da_size(array) == 3);

    assert(da_clear(array) == DA_OK);
    assert(da_is_empty(array));
    assert(da_size(array) == 0);

    /* Can still use after clear */
    da_push(array, 99);
    assert(da_size(array) == 1);

    da_destroy(array);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(da_push(NULL, 1) == DA_ERR_NULL);
    assert(da_pop(NULL, &value) == DA_ERR_NULL);
    assert(da_get(NULL, 0, &value) == DA_ERR_NULL);
    assert(da_set(NULL, 0, 1) == DA_ERR_NULL);
    assert(da_insert(NULL, 0, 1) == DA_ERR_NULL);
    assert(da_remove(NULL, 0, &value) == DA_ERR_NULL);
    assert(da_clear(NULL) == DA_ERR_NULL);
    assert(da_shrink_to_fit(NULL) == DA_ERR_NULL);
    assert(da_is_empty(NULL) == true);
    assert(da_size(NULL) == 0);
    assert(da_capacity(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running dynamic array tests...\n");

    test_create_destroy();
    test_push_pop();
    test_get_set();
    test_insert_remove();
    test_resize();
    test_shrink_to_fit();
    test_clear();
    test_null_safety();

    printf("All dynamic array tests PASSED!\n");
    return 0;
}

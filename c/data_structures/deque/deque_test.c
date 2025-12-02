/**
 * @file deque_test.c
 * @brief Unit tests for deque implementation
 */

#include "deque.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    Deque *deque = deque_create(0);
    assert(deque != NULL);
    assert(deque_is_empty(deque));
    assert(deque_size(deque) == 0);

    deque_destroy(deque);
    deque_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_pop_front(void) {
    printf("  test_push_pop_front...");

    Deque *deque = deque_create(4);
    int value;

    /* Push to front */
    assert(deque_push_front(deque, 10) == DEQUE_OK);
    assert(deque_push_front(deque, 20) == DEQUE_OK);
    assert(deque_push_front(deque, 30) == DEQUE_OK);
    assert(deque_size(deque) == 3);

    /* Pop from front (LIFO for front operations) */
    assert(deque_pop_front(deque, &value) == DEQUE_OK);
    assert(value == 30);
    assert(deque_pop_front(deque, &value) == DEQUE_OK);
    assert(value == 20);
    assert(deque_pop_front(deque, &value) == DEQUE_OK);
    assert(value == 10);

    assert(deque_is_empty(deque));
    assert(deque_pop_front(deque, &value) == DEQUE_ERR_EMPTY);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_push_pop_back(void) {
    printf("  test_push_pop_back...");

    Deque *deque = deque_create(4);
    int value;

    /* Push to back */
    assert(deque_push_back(deque, 10) == DEQUE_OK);
    assert(deque_push_back(deque, 20) == DEQUE_OK);
    assert(deque_push_back(deque, 30) == DEQUE_OK);
    assert(deque_size(deque) == 3);

    /* Pop from back (LIFO for back operations) */
    assert(deque_pop_back(deque, &value) == DEQUE_OK);
    assert(value == 30);
    assert(deque_pop_back(deque, &value) == DEQUE_OK);
    assert(value == 20);
    assert(deque_pop_back(deque, &value) == DEQUE_OK);
    assert(value == 10);

    assert(deque_is_empty(deque));
    assert(deque_pop_back(deque, &value) == DEQUE_ERR_EMPTY);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_mixed_operations(void) {
    printf("  test_mixed_operations...");

    Deque *deque = deque_create(4);
    int value;

    /* Push to both ends */
    deque_push_back(deque, 2);   /* [2] */
    deque_push_front(deque, 1); /* [1, 2] */
    deque_push_back(deque, 3);   /* [1, 2, 3] */
    deque_push_front(deque, 0); /* [0, 1, 2, 3] */

    assert(deque_size(deque) == 4);

    /* Verify order */
    assert(deque_get(deque, 0, &value) == DEQUE_OK && value == 0);
    assert(deque_get(deque, 1, &value) == DEQUE_OK && value == 1);
    assert(deque_get(deque, 2, &value) == DEQUE_OK && value == 2);
    assert(deque_get(deque, 3, &value) == DEQUE_OK && value == 3);

    /* Pop from both ends */
    assert(deque_pop_front(deque, &value) == DEQUE_OK && value == 0);
    assert(deque_pop_back(deque, &value) == DEQUE_OK && value == 3);

    assert(deque_size(deque) == 2);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_front_back(void) {
    printf("  test_front_back...");

    Deque *deque = deque_create(0);
    int value;

    /* Empty deque */
    assert(deque_front(deque, &value) == DEQUE_ERR_EMPTY);
    assert(deque_back(deque, &value) == DEQUE_ERR_EMPTY);

    deque_push_back(deque, 10);
    deque_push_back(deque, 20);
    deque_push_back(deque, 30);

    assert(deque_front(deque, &value) == DEQUE_OK);
    assert(value == 10);
    assert(deque_back(deque, &value) == DEQUE_OK);
    assert(value == 30);

    /* Size unchanged */
    assert(deque_size(deque) == 3);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_get_set(void) {
    printf("  test_get_set...");

    Deque *deque = deque_create(0);
    int value;

    deque_push_back(deque, 10);
    deque_push_back(deque, 20);
    deque_push_back(deque, 30);

    /* Get */
    assert(deque_get(deque, 0, &value) == DEQUE_OK && value == 10);
    assert(deque_get(deque, 1, &value) == DEQUE_OK && value == 20);
    assert(deque_get(deque, 2, &value) == DEQUE_OK && value == 30);
    assert(deque_get(deque, 3, &value) == DEQUE_ERR_INDEX);

    /* Set */
    assert(deque_set(deque, 1, 99) == DEQUE_OK);
    assert(deque_get(deque, 1, &value) == DEQUE_OK && value == 99);
    assert(deque_set(deque, 10, 42) == DEQUE_ERR_INDEX);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_circular_wrap(void) {
    printf("  test_circular_wrap...");

    Deque *deque = deque_create(4);
    int value;

    /* Fill deque */
    deque_push_back(deque, 1);
    deque_push_back(deque, 2);
    deque_push_back(deque, 3);
    deque_push_back(deque, 4);

    /* Pop from front to move head forward */
    deque_pop_front(deque, &value);
    assert(value == 1);
    deque_pop_front(deque, &value);
    assert(value == 2);

    /* Push to back (wraps around) */
    deque_push_back(deque, 5);
    deque_push_back(deque, 6);

    /* Verify order: 3, 4, 5, 6 */
    assert(deque_pop_front(deque, &value) == DEQUE_OK && value == 3);
    assert(deque_pop_front(deque, &value) == DEQUE_OK && value == 4);
    assert(deque_pop_front(deque, &value) == DEQUE_OK && value == 5);
    assert(deque_pop_front(deque, &value) == DEQUE_OK && value == 6);

    assert(deque_is_empty(deque));

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_circular_wrap_front(void) {
    printf("  test_circular_wrap_front...");

    Deque *deque = deque_create(4);
    int value;

    /* Push to back first */
    deque_push_back(deque, 3);
    deque_push_back(deque, 4);

    /* Push to front (wraps around to end of array) */
    deque_push_front(deque, 2);
    deque_push_front(deque, 1);

    /* Verify order: 1, 2, 3, 4 */
    assert(deque_get(deque, 0, &value) == DEQUE_OK && value == 1);
    assert(deque_get(deque, 1, &value) == DEQUE_OK && value == 2);
    assert(deque_get(deque, 2, &value) == DEQUE_OK && value == 3);
    assert(deque_get(deque, 3, &value) == DEQUE_OK && value == 4);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    Deque *deque = deque_create(2);  /* Small initial capacity */

    /* Push more than initial capacity */
    for (int i = 0; i < 100; i++) {
        assert(deque_push_back(deque, i) == DEQUE_OK);
    }
    assert(deque_size(deque) == 100);

    /* Verify order */
    int value;
    for (int i = 0; i < 100; i++) {
        assert(deque_get(deque, (size_t)i, &value) == DEQUE_OK);
        assert(value == i);
    }

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_resize_with_wrap(void) {
    printf("  test_resize_with_wrap...");

    Deque *deque = deque_create(4);
    int value;

    /* Create wrapped state before resize */
    deque_push_back(deque, 1);
    deque_push_back(deque, 2);
    deque_pop_front(deque, &value);  /* Move head forward */
    deque_push_back(deque, 3);
    deque_push_back(deque, 4);
    deque_push_back(deque, 5);  /* Should trigger resize */

    /* Verify order: 2, 3, 4, 5 */
    assert(deque_size(deque) == 4);
    assert(deque_get(deque, 0, &value) == DEQUE_OK && value == 2);
    assert(deque_get(deque, 1, &value) == DEQUE_OK && value == 3);
    assert(deque_get(deque, 2, &value) == DEQUE_OK && value == 4);
    assert(deque_get(deque, 3, &value) == DEQUE_OK && value == 5);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    Deque *deque = deque_create(0);

    deque_push_back(deque, 1);
    deque_push_back(deque, 2);
    deque_push_back(deque, 3);
    assert(deque_size(deque) == 3);

    assert(deque_clear(deque) == DEQUE_OK);
    assert(deque_is_empty(deque));
    assert(deque_size(deque) == 0);

    /* Can still use after clear */
    deque_push_back(deque, 99);
    assert(deque_size(deque) == 1);

    deque_destroy(deque);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(deque_push_front(NULL, 1) == DEQUE_ERR_NULL);
    assert(deque_push_back(NULL, 1) == DEQUE_ERR_NULL);
    assert(deque_pop_front(NULL, &value) == DEQUE_ERR_NULL);
    assert(deque_pop_back(NULL, &value) == DEQUE_ERR_NULL);
    assert(deque_front(NULL, &value) == DEQUE_ERR_NULL);
    assert(deque_back(NULL, &value) == DEQUE_ERR_NULL);
    assert(deque_get(NULL, 0, &value) == DEQUE_ERR_NULL);
    assert(deque_set(NULL, 0, 1) == DEQUE_ERR_NULL);
    assert(deque_clear(NULL) == DEQUE_ERR_NULL);
    assert(deque_is_empty(NULL) == true);
    assert(deque_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running deque tests...\n");

    test_create_destroy();
    test_push_pop_front();
    test_push_pop_back();
    test_mixed_operations();
    test_front_back();
    test_get_set();
    test_circular_wrap();
    test_circular_wrap_front();
    test_resize();
    test_resize_with_wrap();
    test_clear();
    test_null_safety();

    printf("All deque tests PASSED!\n");
    return 0;
}

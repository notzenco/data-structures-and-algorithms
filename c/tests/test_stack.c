/**
 * @file test_stack.c
 * @brief Unit tests for stack implementation
 */

#include "dsa/stack.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    Stack *stack = stack_create(0);
    assert(stack != NULL);
    assert(stack_is_empty(stack));
    assert(stack_size(stack) == 0);

    stack_destroy(stack);
    stack_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_pop(void) {
    printf("  test_push_pop...");

    Stack *stack = stack_create(4);
    int value;

    /* Push elements */
    assert(stack_push(stack, 10) == STACK_OK);
    assert(stack_push(stack, 20) == STACK_OK);
    assert(stack_push(stack, 30) == STACK_OK);
    assert(stack_size(stack) == 3);
    assert(!stack_is_empty(stack));

    /* Pop elements (LIFO order) */
    assert(stack_pop(stack, &value) == STACK_OK);
    assert(value == 30);
    assert(stack_pop(stack, &value) == STACK_OK);
    assert(value == 20);
    assert(stack_pop(stack, &value) == STACK_OK);
    assert(value == 10);

    assert(stack_is_empty(stack));
    assert(stack_pop(stack, &value) == STACK_ERR_EMPTY);

    stack_destroy(stack);
    printf(" PASSED\n");
}

static void test_peek(void) {
    printf("  test_peek...");

    Stack *stack = stack_create(0);
    int value;

    /* Peek on empty */
    assert(stack_peek(stack, &value) == STACK_ERR_EMPTY);

    /* Peek after push */
    stack_push(stack, 42);
    assert(stack_peek(stack, &value) == STACK_OK);
    assert(value == 42);
    assert(stack_size(stack) == 1);  /* Size unchanged */

    /* Peek still returns same value */
    assert(stack_peek(stack, &value) == STACK_OK);
    assert(value == 42);

    stack_destroy(stack);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    Stack *stack = stack_create(0);

    stack_push(stack, 1);
    stack_push(stack, 2);
    stack_push(stack, 3);
    assert(stack_size(stack) == 3);

    assert(stack_clear(stack) == STACK_OK);
    assert(stack_is_empty(stack));
    assert(stack_size(stack) == 0);

    /* Can still use after clear */
    stack_push(stack, 99);
    assert(stack_size(stack) == 1);

    stack_destroy(stack);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    Stack *stack = stack_create(2);  /* Small initial capacity */

    /* Push more than initial capacity */
    for (int i = 0; i < 100; i++) {
        assert(stack_push(stack, i) == STACK_OK);
    }
    assert(stack_size(stack) == 100);

    /* Verify LIFO order */
    int value;
    for (int i = 99; i >= 0; i--) {
        assert(stack_pop(stack, &value) == STACK_OK);
        assert(value == i);
    }

    stack_destroy(stack);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    /* All functions should handle NULL gracefully */
    assert(stack_push(NULL, 1) == STACK_ERR_NULL);
    assert(stack_pop(NULL, NULL) == STACK_ERR_NULL);
    assert(stack_peek(NULL, NULL) == STACK_ERR_NULL);
    assert(stack_clear(NULL) == STACK_ERR_NULL);
    assert(stack_is_empty(NULL) == true);
    assert(stack_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running stack tests...\n");

    test_create_destroy();
    test_push_pop();
    test_peek();
    test_clear();
    test_resize();
    test_null_safety();

    printf("All stack tests PASSED!\n");
    return 0;
}

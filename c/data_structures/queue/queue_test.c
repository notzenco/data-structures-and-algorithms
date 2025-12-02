/**
 * @file queue_test.c
 * @brief Unit tests for queue implementation
 */

#include "queue.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    Queue *queue = queue_create(0);
    assert(queue != NULL);
    assert(queue_is_empty(queue));
    assert(queue_size(queue) == 0);

    queue_destroy(queue);
    queue_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_enqueue_dequeue(void) {
    printf("  test_enqueue_dequeue...");

    Queue *queue = queue_create(4);
    int value;

    /* Enqueue elements */
    assert(queue_enqueue(queue, 10) == QUEUE_OK);
    assert(queue_enqueue(queue, 20) == QUEUE_OK);
    assert(queue_enqueue(queue, 30) == QUEUE_OK);
    assert(queue_size(queue) == 3);
    assert(!queue_is_empty(queue));

    /* Dequeue elements (FIFO order) */
    assert(queue_dequeue(queue, &value) == QUEUE_OK);
    assert(value == 10);
    assert(queue_dequeue(queue, &value) == QUEUE_OK);
    assert(value == 20);
    assert(queue_dequeue(queue, &value) == QUEUE_OK);
    assert(value == 30);

    assert(queue_is_empty(queue));
    assert(queue_dequeue(queue, &value) == QUEUE_ERR_EMPTY);

    queue_destroy(queue);
    printf(" PASSED\n");
}

static void test_front(void) {
    printf("  test_front...");

    Queue *queue = queue_create(0);
    int value;

    /* Front on empty */
    assert(queue_front(queue, &value) == QUEUE_ERR_EMPTY);

    /* Front after enqueue */
    queue_enqueue(queue, 42);
    assert(queue_front(queue, &value) == QUEUE_OK);
    assert(value == 42);
    assert(queue_size(queue) == 1);  /* Size unchanged */

    /* Front still returns same value */
    queue_enqueue(queue, 99);
    assert(queue_front(queue, &value) == QUEUE_OK);
    assert(value == 42);  /* Still first element */

    queue_destroy(queue);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    Queue *queue = queue_create(0);

    queue_enqueue(queue, 1);
    queue_enqueue(queue, 2);
    queue_enqueue(queue, 3);
    assert(queue_size(queue) == 3);

    assert(queue_clear(queue) == QUEUE_OK);
    assert(queue_is_empty(queue));
    assert(queue_size(queue) == 0);

    /* Can still use after clear */
    queue_enqueue(queue, 99);
    assert(queue_size(queue) == 1);

    queue_destroy(queue);
    printf(" PASSED\n");
}

static void test_circular_wrap(void) {
    printf("  test_circular_wrap...");

    Queue *queue = queue_create(4);
    int value;

    /* Fill queue */
    queue_enqueue(queue, 1);
    queue_enqueue(queue, 2);
    queue_enqueue(queue, 3);
    queue_enqueue(queue, 4);

    /* Dequeue some to move head forward */
    queue_dequeue(queue, &value);
    assert(value == 1);
    queue_dequeue(queue, &value);
    assert(value == 2);

    /* Enqueue more - these should wrap around */
    queue_enqueue(queue, 5);
    queue_enqueue(queue, 6);

    /* Verify FIFO order maintained through wrap */
    queue_dequeue(queue, &value);
    assert(value == 3);
    queue_dequeue(queue, &value);
    assert(value == 4);
    queue_dequeue(queue, &value);
    assert(value == 5);
    queue_dequeue(queue, &value);
    assert(value == 6);

    assert(queue_is_empty(queue));

    queue_destroy(queue);
    printf(" PASSED\n");
}

static void test_resize(void) {
    printf("  test_resize...");

    Queue *queue = queue_create(2);  /* Small initial capacity */

    /* Push more than initial capacity */
    for (int i = 0; i < 100; i++) {
        assert(queue_enqueue(queue, i) == QUEUE_OK);
    }
    assert(queue_size(queue) == 100);

    /* Verify FIFO order */
    int value;
    for (int i = 0; i < 100; i++) {
        assert(queue_dequeue(queue, &value) == QUEUE_OK);
        assert(value == i);
    }

    queue_destroy(queue);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    /* All functions should handle NULL gracefully */
    assert(queue_enqueue(NULL, 1) == QUEUE_ERR_NULL);
    assert(queue_dequeue(NULL, NULL) == QUEUE_ERR_NULL);
    assert(queue_front(NULL, NULL) == QUEUE_ERR_NULL);
    assert(queue_clear(NULL) == QUEUE_ERR_NULL);
    assert(queue_is_empty(NULL) == true);
    assert(queue_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running queue tests...\n");

    test_create_destroy();
    test_enqueue_dequeue();
    test_front();
    test_clear();
    test_circular_wrap();
    test_resize();
    test_null_safety();

    printf("All queue tests PASSED!\n");
    return 0;
}

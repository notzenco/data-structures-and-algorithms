/**
 * @file singly_linked_list_test.c
 * @brief Unit tests for singly linked list implementation
 */

#include "singly_linked_list.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    SinglyLinkedList *list = sll_create();
    assert(list != NULL);
    assert(sll_is_empty(list));
    assert(sll_size(list) == 0);

    sll_destroy(list);
    sll_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_front(void) {
    printf("  test_push_front...");

    SinglyLinkedList *list = sll_create();
    int value;

    assert(sll_push_front(list, 30) == SLL_OK);
    assert(sll_push_front(list, 20) == SLL_OK);
    assert(sll_push_front(list, 10) == SLL_OK);
    assert(sll_size(list) == 3);

    /* Verify order: 10, 20, 30 */
    assert(sll_get(list, 0, &value) == SLL_OK && value == 10);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 20);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 30);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_push_back(void) {
    printf("  test_push_back...");

    SinglyLinkedList *list = sll_create();
    int value;

    assert(sll_push_back(list, 10) == SLL_OK);
    assert(sll_push_back(list, 20) == SLL_OK);
    assert(sll_push_back(list, 30) == SLL_OK);
    assert(sll_size(list) == 3);

    /* Verify order: 10, 20, 30 */
    assert(sll_get(list, 0, &value) == SLL_OK && value == 10);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 20);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 30);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_pop_front(void) {
    printf("  test_pop_front...");

    SinglyLinkedList *list = sll_create();
    int value;

    sll_push_back(list, 10);
    sll_push_back(list, 20);
    sll_push_back(list, 30);

    assert(sll_pop_front(list, &value) == SLL_OK);
    assert(value == 10);
    assert(sll_pop_front(list, &value) == SLL_OK);
    assert(value == 20);
    assert(sll_pop_front(list, &value) == SLL_OK);
    assert(value == 30);

    assert(sll_is_empty(list));
    assert(sll_pop_front(list, &value) == SLL_ERR_EMPTY);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_front(void) {
    printf("  test_front...");

    SinglyLinkedList *list = sll_create();
    int value;

    assert(sll_front(list, &value) == SLL_ERR_EMPTY);

    sll_push_back(list, 42);
    assert(sll_front(list, &value) == SLL_OK);
    assert(value == 42);
    assert(sll_size(list) == 1);  /* Size unchanged */

    sll_push_front(list, 10);
    assert(sll_front(list, &value) == SLL_OK);
    assert(value == 10);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_get_set(void) {
    printf("  test_get_set...");

    SinglyLinkedList *list = sll_create();
    int value;

    sll_push_back(list, 1);
    sll_push_back(list, 2);
    sll_push_back(list, 3);

    /* Get values */
    assert(sll_get(list, 0, &value) == SLL_OK && value == 1);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 2);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 3);

    /* Out of bounds */
    assert(sll_get(list, 3, &value) == SLL_ERR_INDEX);
    assert(sll_get(list, 100, &value) == SLL_ERR_INDEX);

    /* Set values */
    assert(sll_set(list, 1, 99) == SLL_OK);
    assert(sll_get(list, 1, &value) == SLL_OK);
    assert(value == 99);

    /* Set out of bounds */
    assert(sll_set(list, 10, 42) == SLL_ERR_INDEX);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_insert(void) {
    printf("  test_insert...");

    SinglyLinkedList *list = sll_create();
    int value;

    /* Insert into empty list */
    assert(sll_insert(list, 0, 20) == SLL_OK);

    /* Insert at front */
    assert(sll_insert(list, 0, 10) == SLL_OK);

    /* Insert at back */
    assert(sll_insert(list, 2, 40) == SLL_OK);

    /* Insert in middle */
    assert(sll_insert(list, 2, 30) == SLL_OK);

    /* Verify order: 10, 20, 30, 40 */
    assert(sll_get(list, 0, &value) == SLL_OK && value == 10);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 20);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 30);
    assert(sll_get(list, 3, &value) == SLL_OK && value == 40);

    /* Insert out of bounds */
    assert(sll_insert(list, 100, 42) == SLL_ERR_INDEX);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_remove(void) {
    printf("  test_remove...");

    SinglyLinkedList *list = sll_create();
    int value;

    sll_push_back(list, 10);
    sll_push_back(list, 20);
    sll_push_back(list, 30);
    sll_push_back(list, 40);

    /* Remove from middle */
    assert(sll_remove(list, 1, &value) == SLL_OK);
    assert(value == 20);

    /* Verify: 10, 30, 40 */
    assert(sll_size(list) == 3);
    assert(sll_get(list, 0, &value) == SLL_OK && value == 10);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 30);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 40);

    /* Remove from end */
    assert(sll_remove(list, 2, &value) == SLL_OK);
    assert(value == 40);

    /* Remove from front */
    assert(sll_remove(list, 0, &value) == SLL_OK);
    assert(value == 10);

    assert(sll_size(list) == 1);
    assert(sll_get(list, 0, &value) == SLL_OK && value == 30);

    /* Remove out of bounds */
    assert(sll_remove(list, 100, &value) == SLL_ERR_INDEX);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    SinglyLinkedList *list = sll_create();

    sll_push_back(list, 1);
    sll_push_back(list, 2);
    sll_push_back(list, 3);
    assert(sll_size(list) == 3);

    assert(sll_clear(list) == SLL_OK);
    assert(sll_is_empty(list));
    assert(sll_size(list) == 0);

    /* Can still use after clear */
    sll_push_back(list, 99);
    assert(sll_size(list) == 1);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_reverse(void) {
    printf("  test_reverse...");

    SinglyLinkedList *list = sll_create();
    int value;

    /* Reverse empty list */
    assert(sll_reverse(list) == SLL_OK);

    /* Reverse single element */
    sll_push_back(list, 1);
    assert(sll_reverse(list) == SLL_OK);
    assert(sll_get(list, 0, &value) == SLL_OK && value == 1);

    /* Reverse multiple elements */
    sll_push_back(list, 2);
    sll_push_back(list, 3);
    sll_push_back(list, 4);
    /* List: 1, 2, 3, 4 */

    assert(sll_reverse(list) == SLL_OK);
    /* List: 4, 3, 2, 1 */

    assert(sll_get(list, 0, &value) == SLL_OK && value == 4);
    assert(sll_get(list, 1, &value) == SLL_OK && value == 3);
    assert(sll_get(list, 2, &value) == SLL_OK && value == 2);
    assert(sll_get(list, 3, &value) == SLL_OK && value == 1);

    sll_destroy(list);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(sll_push_front(NULL, 1) == SLL_ERR_NULL);
    assert(sll_push_back(NULL, 1) == SLL_ERR_NULL);
    assert(sll_pop_front(NULL, &value) == SLL_ERR_NULL);
    assert(sll_get(NULL, 0, &value) == SLL_ERR_NULL);
    assert(sll_set(NULL, 0, 1) == SLL_ERR_NULL);
    assert(sll_insert(NULL, 0, 1) == SLL_ERR_NULL);
    assert(sll_remove(NULL, 0, &value) == SLL_ERR_NULL);
    assert(sll_front(NULL, &value) == SLL_ERR_NULL);
    assert(sll_clear(NULL) == SLL_ERR_NULL);
    assert(sll_reverse(NULL) == SLL_ERR_NULL);
    assert(sll_is_empty(NULL) == true);
    assert(sll_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running singly linked list tests...\n");

    test_create_destroy();
    test_push_front();
    test_push_back();
    test_pop_front();
    test_front();
    test_get_set();
    test_insert();
    test_remove();
    test_clear();
    test_reverse();
    test_null_safety();

    printf("All singly linked list tests PASSED!\n");
    return 0;
}

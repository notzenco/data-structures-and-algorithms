/**
 * @file doubly_linked_list_test.c
 * @brief Unit tests for doubly linked list implementation
 */

#include "doubly_linked_list.h"
#include <assert.h>
#include <stdio.h>

static void test_create_destroy(void) {
    printf("  test_create_destroy...");

    DoublyLinkedList *list = dll_create();
    assert(list != NULL);
    assert(dll_is_empty(list));
    assert(dll_size(list) == 0);

    dll_destroy(list);
    dll_destroy(NULL);  /* Should not crash */

    printf(" PASSED\n");
}

static void test_push_front(void) {
    printf("  test_push_front...");

    DoublyLinkedList *list = dll_create();
    int value;

    assert(dll_push_front(list, 30) == DLL_OK);
    assert(dll_push_front(list, 20) == DLL_OK);
    assert(dll_push_front(list, 10) == DLL_OK);
    assert(dll_size(list) == 3);

    /* Verify order: 10, 20, 30 */
    assert(dll_get(list, 0, &value) == DLL_OK && value == 10);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 20);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 30);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_push_back(void) {
    printf("  test_push_back...");

    DoublyLinkedList *list = dll_create();
    int value;

    assert(dll_push_back(list, 10) == DLL_OK);
    assert(dll_push_back(list, 20) == DLL_OK);
    assert(dll_push_back(list, 30) == DLL_OK);
    assert(dll_size(list) == 3);

    /* Verify order: 10, 20, 30 */
    assert(dll_get(list, 0, &value) == DLL_OK && value == 10);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 20);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 30);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_pop_front(void) {
    printf("  test_pop_front...");

    DoublyLinkedList *list = dll_create();
    int value;

    dll_push_back(list, 10);
    dll_push_back(list, 20);
    dll_push_back(list, 30);

    assert(dll_pop_front(list, &value) == DLL_OK);
    assert(value == 10);
    assert(dll_pop_front(list, &value) == DLL_OK);
    assert(value == 20);
    assert(dll_pop_front(list, &value) == DLL_OK);
    assert(value == 30);

    assert(dll_is_empty(list));
    assert(dll_pop_front(list, &value) == DLL_ERR_EMPTY);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_pop_back(void) {
    printf("  test_pop_back...");

    DoublyLinkedList *list = dll_create();
    int value;

    dll_push_back(list, 10);
    dll_push_back(list, 20);
    dll_push_back(list, 30);

    assert(dll_pop_back(list, &value) == DLL_OK);
    assert(value == 30);
    assert(dll_pop_back(list, &value) == DLL_OK);
    assert(value == 20);
    assert(dll_pop_back(list, &value) == DLL_OK);
    assert(value == 10);

    assert(dll_is_empty(list));
    assert(dll_pop_back(list, &value) == DLL_ERR_EMPTY);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_front_back(void) {
    printf("  test_front_back...");

    DoublyLinkedList *list = dll_create();
    int value;

    assert(dll_front(list, &value) == DLL_ERR_EMPTY);
    assert(dll_back(list, &value) == DLL_ERR_EMPTY);

    dll_push_back(list, 10);
    dll_push_back(list, 20);
    dll_push_back(list, 30);

    assert(dll_front(list, &value) == DLL_OK);
    assert(value == 10);
    assert(dll_back(list, &value) == DLL_OK);
    assert(value == 30);

    /* Size unchanged */
    assert(dll_size(list) == 3);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_get_set(void) {
    printf("  test_get_set...");

    DoublyLinkedList *list = dll_create();
    int value;

    dll_push_back(list, 1);
    dll_push_back(list, 2);
    dll_push_back(list, 3);
    dll_push_back(list, 4);
    dll_push_back(list, 5);

    /* Get values (tests both head and tail traversal) */
    assert(dll_get(list, 0, &value) == DLL_OK && value == 1);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 2);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 3);
    assert(dll_get(list, 3, &value) == DLL_OK && value == 4);
    assert(dll_get(list, 4, &value) == DLL_OK && value == 5);

    /* Out of bounds */
    assert(dll_get(list, 5, &value) == DLL_ERR_INDEX);
    assert(dll_get(list, 100, &value) == DLL_ERR_INDEX);

    /* Set values */
    assert(dll_set(list, 2, 99) == DLL_OK);
    assert(dll_get(list, 2, &value) == DLL_OK);
    assert(value == 99);

    /* Set out of bounds */
    assert(dll_set(list, 10, 42) == DLL_ERR_INDEX);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_insert(void) {
    printf("  test_insert...");

    DoublyLinkedList *list = dll_create();
    int value;

    /* Insert into empty list */
    assert(dll_insert(list, 0, 20) == DLL_OK);

    /* Insert at front */
    assert(dll_insert(list, 0, 10) == DLL_OK);

    /* Insert at back */
    assert(dll_insert(list, 2, 40) == DLL_OK);

    /* Insert in middle */
    assert(dll_insert(list, 2, 30) == DLL_OK);

    /* Verify order: 10, 20, 30, 40 */
    assert(dll_get(list, 0, &value) == DLL_OK && value == 10);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 20);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 30);
    assert(dll_get(list, 3, &value) == DLL_OK && value == 40);

    /* Verify front and back */
    assert(dll_front(list, &value) == DLL_OK && value == 10);
    assert(dll_back(list, &value) == DLL_OK && value == 40);

    /* Insert out of bounds */
    assert(dll_insert(list, 100, 42) == DLL_ERR_INDEX);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_remove(void) {
    printf("  test_remove...");

    DoublyLinkedList *list = dll_create();
    int value;

    dll_push_back(list, 10);
    dll_push_back(list, 20);
    dll_push_back(list, 30);
    dll_push_back(list, 40);
    dll_push_back(list, 50);

    /* Remove from middle */
    assert(dll_remove(list, 2, &value) == DLL_OK);
    assert(value == 30);

    /* Verify: 10, 20, 40, 50 */
    assert(dll_size(list) == 4);
    assert(dll_get(list, 0, &value) == DLL_OK && value == 10);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 20);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 40);
    assert(dll_get(list, 3, &value) == DLL_OK && value == 50);

    /* Remove from end */
    assert(dll_remove(list, 3, &value) == DLL_OK);
    assert(value == 50);
    assert(dll_back(list, &value) == DLL_OK && value == 40);

    /* Remove from front */
    assert(dll_remove(list, 0, &value) == DLL_OK);
    assert(value == 10);
    assert(dll_front(list, &value) == DLL_OK && value == 20);

    assert(dll_size(list) == 2);

    /* Remove out of bounds */
    assert(dll_remove(list, 100, &value) == DLL_ERR_INDEX);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_clear(void) {
    printf("  test_clear...");

    DoublyLinkedList *list = dll_create();

    dll_push_back(list, 1);
    dll_push_back(list, 2);
    dll_push_back(list, 3);
    assert(dll_size(list) == 3);

    assert(dll_clear(list) == DLL_OK);
    assert(dll_is_empty(list));
    assert(dll_size(list) == 0);

    /* Can still use after clear */
    dll_push_back(list, 99);
    assert(dll_size(list) == 1);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_reverse(void) {
    printf("  test_reverse...");

    DoublyLinkedList *list = dll_create();
    int value;

    /* Reverse empty list */
    assert(dll_reverse(list) == DLL_OK);

    /* Reverse single element */
    dll_push_back(list, 1);
    assert(dll_reverse(list) == DLL_OK);
    assert(dll_get(list, 0, &value) == DLL_OK && value == 1);

    /* Reverse multiple elements */
    dll_push_back(list, 2);
    dll_push_back(list, 3);
    dll_push_back(list, 4);
    /* List: 1, 2, 3, 4 */

    assert(dll_reverse(list) == DLL_OK);
    /* List: 4, 3, 2, 1 */

    assert(dll_get(list, 0, &value) == DLL_OK && value == 4);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 3);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 2);
    assert(dll_get(list, 3, &value) == DLL_OK && value == 1);

    /* Verify front and back */
    assert(dll_front(list, &value) == DLL_OK && value == 4);
    assert(dll_back(list, &value) == DLL_OK && value == 1);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_bidirectional_traversal(void) {
    printf("  test_bidirectional_traversal...");

    DoublyLinkedList *list = dll_create();
    int value;

    /* Add many elements */
    for (int i = 0; i < 10; i++) {
        dll_push_back(list, i);
    }

    /* Access from front (uses head traversal) */
    assert(dll_get(list, 0, &value) == DLL_OK && value == 0);
    assert(dll_get(list, 1, &value) == DLL_OK && value == 1);
    assert(dll_get(list, 2, &value) == DLL_OK && value == 2);

    /* Access from back (uses tail traversal) */
    assert(dll_get(list, 9, &value) == DLL_OK && value == 9);
    assert(dll_get(list, 8, &value) == DLL_OK && value == 8);
    assert(dll_get(list, 7, &value) == DLL_OK && value == 7);

    /* Access middle */
    assert(dll_get(list, 5, &value) == DLL_OK && value == 5);

    dll_destroy(list);
    printf(" PASSED\n");
}

static void test_null_safety(void) {
    printf("  test_null_safety...");

    int value;

    /* All functions should handle NULL gracefully */
    assert(dll_push_front(NULL, 1) == DLL_ERR_NULL);
    assert(dll_push_back(NULL, 1) == DLL_ERR_NULL);
    assert(dll_pop_front(NULL, &value) == DLL_ERR_NULL);
    assert(dll_pop_back(NULL, &value) == DLL_ERR_NULL);
    assert(dll_get(NULL, 0, &value) == DLL_ERR_NULL);
    assert(dll_set(NULL, 0, 1) == DLL_ERR_NULL);
    assert(dll_insert(NULL, 0, 1) == DLL_ERR_NULL);
    assert(dll_remove(NULL, 0, &value) == DLL_ERR_NULL);
    assert(dll_front(NULL, &value) == DLL_ERR_NULL);
    assert(dll_back(NULL, &value) == DLL_ERR_NULL);
    assert(dll_clear(NULL) == DLL_ERR_NULL);
    assert(dll_reverse(NULL) == DLL_ERR_NULL);
    assert(dll_is_empty(NULL) == true);
    assert(dll_size(NULL) == 0);

    printf(" PASSED\n");
}

int main(void) {
    printf("Running doubly linked list tests...\n");

    test_create_destroy();
    test_push_front();
    test_push_back();
    test_pop_front();
    test_pop_back();
    test_front_back();
    test_get_set();
    test_insert();
    test_remove();
    test_clear();
    test_reverse();
    test_bidirectional_traversal();
    test_null_safety();

    printf("All doubly linked list tests PASSED!\n");
    return 0;
}

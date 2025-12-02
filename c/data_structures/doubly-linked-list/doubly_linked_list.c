/**
 * @file doubly_linked_list.c
 * @brief Doubly linked list implementation
 */

#include "doubly_linked_list.h"
#include <stdlib.h>

/** Node structure for the doubly linked list */
typedef struct Node {
    int value;
    struct Node *prev;
    struct Node *next;
} Node;

/** Doubly linked list internal structure */
struct DoublyLinkedList {
    Node *head;
    Node *tail;
    size_t size;
};

/**
 * Create a new node.
 * @param value Value for the node
 * @return New node or NULL on allocation failure
 */
static Node *node_create(int value) {
    Node *node = malloc(sizeof(Node));
    if (node) {
        node->value = value;
        node->prev = NULL;
        node->next = NULL;
    }
    return node;
}

/**
 * Get node at index (internal helper).
 * Optimizes by starting from head or tail based on index.
 * @param list Target list
 * @param index Index to find
 * @return Node at index or NULL if out of bounds
 */
static Node *get_node_at(const DoublyLinkedList *list, size_t index) {
    if (index >= list->size) {
        return NULL;
    }

    Node *current;
    if (index < list->size / 2) {
        /* Start from head */
        current = list->head;
        for (size_t i = 0; i < index; i++) {
            current = current->next;
        }
    } else {
        /* Start from tail */
        current = list->tail;
        for (size_t i = list->size - 1; i > index; i--) {
            current = current->prev;
        }
    }
    return current;
}

DoublyLinkedList *dll_create(void) {
    DoublyLinkedList *list = malloc(sizeof(DoublyLinkedList));
    if (list) {
        list->head = NULL;
        list->tail = NULL;
        list->size = 0;
    }
    return list;
}

void dll_destroy(DoublyLinkedList *list) {
    if (!list) {
        return;
    }
    Node *current = list->head;
    while (current) {
        Node *next = current->next;
        free(current);
        current = next;
    }
    free(list);
}

DLLResult dll_push_front(DoublyLinkedList *list, int value) {
    if (!list) {
        return DLL_ERR_NULL;
    }

    Node *node = node_create(value);
    if (!node) {
        return DLL_ERR_ALLOC;
    }

    node->next = list->head;
    if (list->head) {
        list->head->prev = node;
    } else {
        list->tail = node;
    }
    list->head = node;
    list->size++;
    return DLL_OK;
}

DLLResult dll_push_back(DoublyLinkedList *list, int value) {
    if (!list) {
        return DLL_ERR_NULL;
    }

    Node *node = node_create(value);
    if (!node) {
        return DLL_ERR_ALLOC;
    }

    node->prev = list->tail;
    if (list->tail) {
        list->tail->next = node;
    } else {
        list->head = node;
    }
    list->tail = node;
    list->size++;
    return DLL_OK;
}

DLLResult dll_pop_front(DoublyLinkedList *list, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (list->size == 0) {
        return DLL_ERR_EMPTY;
    }

    Node *node = list->head;
    if (out) {
        *out = node->value;
    }

    list->head = node->next;
    if (list->head) {
        list->head->prev = NULL;
    } else {
        list->tail = NULL;
    }
    list->size--;
    free(node);
    return DLL_OK;
}

DLLResult dll_pop_back(DoublyLinkedList *list, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (list->size == 0) {
        return DLL_ERR_EMPTY;
    }

    Node *node = list->tail;
    if (out) {
        *out = node->value;
    }

    list->tail = node->prev;
    if (list->tail) {
        list->tail->next = NULL;
    } else {
        list->head = NULL;
    }
    list->size--;
    free(node);
    return DLL_OK;
}

DLLResult dll_get(const DoublyLinkedList *list, size_t index, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }

    Node *node = get_node_at(list, index);
    if (!node) {
        return DLL_ERR_INDEX;
    }

    if (out) {
        *out = node->value;
    }
    return DLL_OK;
}

DLLResult dll_set(DoublyLinkedList *list, size_t index, int value) {
    if (!list) {
        return DLL_ERR_NULL;
    }

    Node *node = get_node_at(list, index);
    if (!node) {
        return DLL_ERR_INDEX;
    }

    node->value = value;
    return DLL_OK;
}

DLLResult dll_insert(DoublyLinkedList *list, size_t index, int value) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (index > list->size) {
        return DLL_ERR_INDEX;
    }

    /* Insert at front */
    if (index == 0) {
        return dll_push_front(list, value);
    }

    /* Insert at back */
    if (index == list->size) {
        return dll_push_back(list, value);
    }

    /* Insert in middle */
    Node *next_node = get_node_at(list, index);
    Node *prev_node = next_node->prev;

    Node *node = node_create(value);
    if (!node) {
        return DLL_ERR_ALLOC;
    }

    node->prev = prev_node;
    node->next = next_node;
    prev_node->next = node;
    next_node->prev = node;
    list->size++;
    return DLL_OK;
}

DLLResult dll_remove(DoublyLinkedList *list, size_t index, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (index >= list->size) {
        return DLL_ERR_INDEX;
    }

    /* Remove from front */
    if (index == 0) {
        return dll_pop_front(list, out);
    }

    /* Remove from back */
    if (index == list->size - 1) {
        return dll_pop_back(list, out);
    }

    /* Remove from middle */
    Node *node = get_node_at(list, index);

    if (out) {
        *out = node->value;
    }

    node->prev->next = node->next;
    node->next->prev = node->prev;
    list->size--;
    free(node);
    return DLL_OK;
}

DLLResult dll_front(const DoublyLinkedList *list, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (list->size == 0) {
        return DLL_ERR_EMPTY;
    }
    if (out) {
        *out = list->head->value;
    }
    return DLL_OK;
}

DLLResult dll_back(const DoublyLinkedList *list, int *out) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (list->size == 0) {
        return DLL_ERR_EMPTY;
    }
    if (out) {
        *out = list->tail->value;
    }
    return DLL_OK;
}

size_t dll_size(const DoublyLinkedList *list) {
    return list ? list->size : 0;
}

bool dll_is_empty(const DoublyLinkedList *list) {
    return !list || list->size == 0;
}

DLLResult dll_clear(DoublyLinkedList *list) {
    if (!list) {
        return DLL_ERR_NULL;
    }

    Node *current = list->head;
    while (current) {
        Node *next = current->next;
        free(current);
        current = next;
    }

    list->head = NULL;
    list->tail = NULL;
    list->size = 0;
    return DLL_OK;
}

DLLResult dll_reverse(DoublyLinkedList *list) {
    if (!list) {
        return DLL_ERR_NULL;
    }
    if (list->size <= 1) {
        return DLL_OK;
    }

    Node *current = list->head;
    Node *temp = NULL;

    /* Swap prev and next for all nodes */
    while (current) {
        temp = current->prev;
        current->prev = current->next;
        current->next = temp;
        current = current->prev;  /* Move to what was next */
    }

    /* Swap head and tail */
    temp = list->head;
    list->head = list->tail;
    list->tail = temp;

    return DLL_OK;
}

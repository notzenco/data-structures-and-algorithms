/**
 * @file singly_linked_list.c
 * @brief Singly linked list implementation
 */

#include "singly_linked_list.h"
#include <stdlib.h>

/** Node structure for the linked list */
typedef struct Node {
    int value;
    struct Node *next;
} Node;

/** Singly linked list internal structure */
struct SinglyLinkedList {
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
        node->next = NULL;
    }
    return node;
}

/**
 * Get node at index (internal helper).
 * @param list Target list
 * @param index Index to find
 * @return Node at index or NULL if out of bounds
 */
static Node *get_node_at(const SinglyLinkedList *list, size_t index) {
    if (index >= list->size) {
        return NULL;
    }
    Node *current = list->head;
    for (size_t i = 0; i < index; i++) {
        current = current->next;
    }
    return current;
}

SinglyLinkedList *sll_create(void) {
    SinglyLinkedList *list = malloc(sizeof(SinglyLinkedList));
    if (list) {
        list->head = NULL;
        list->tail = NULL;
        list->size = 0;
    }
    return list;
}

void sll_destroy(SinglyLinkedList *list) {
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

SLLResult sll_push_front(SinglyLinkedList *list, int value) {
    if (!list) {
        return SLL_ERR_NULL;
    }

    Node *node = node_create(value);
    if (!node) {
        return SLL_ERR_ALLOC;
    }

    node->next = list->head;
    list->head = node;
    if (!list->tail) {
        list->tail = node;
    }
    list->size++;
    return SLL_OK;
}

SLLResult sll_push_back(SinglyLinkedList *list, int value) {
    if (!list) {
        return SLL_ERR_NULL;
    }

    Node *node = node_create(value);
    if (!node) {
        return SLL_ERR_ALLOC;
    }

    if (list->tail) {
        list->tail->next = node;
    } else {
        list->head = node;
    }
    list->tail = node;
    list->size++;
    return SLL_OK;
}

SLLResult sll_pop_front(SinglyLinkedList *list, int *out) {
    if (!list) {
        return SLL_ERR_NULL;
    }
    if (list->size == 0) {
        return SLL_ERR_EMPTY;
    }

    Node *node = list->head;
    if (out) {
        *out = node->value;
    }

    list->head = node->next;
    if (!list->head) {
        list->tail = NULL;
    }
    list->size--;
    free(node);
    return SLL_OK;
}

SLLResult sll_get(const SinglyLinkedList *list, size_t index, int *out) {
    if (!list) {
        return SLL_ERR_NULL;
    }

    Node *node = get_node_at(list, index);
    if (!node) {
        return SLL_ERR_INDEX;
    }

    if (out) {
        *out = node->value;
    }
    return SLL_OK;
}

SLLResult sll_set(SinglyLinkedList *list, size_t index, int value) {
    if (!list) {
        return SLL_ERR_NULL;
    }

    Node *node = get_node_at(list, index);
    if (!node) {
        return SLL_ERR_INDEX;
    }

    node->value = value;
    return SLL_OK;
}

SLLResult sll_insert(SinglyLinkedList *list, size_t index, int value) {
    if (!list) {
        return SLL_ERR_NULL;
    }
    if (index > list->size) {
        return SLL_ERR_INDEX;
    }

    /* Insert at front */
    if (index == 0) {
        return sll_push_front(list, value);
    }

    /* Insert at back */
    if (index == list->size) {
        return sll_push_back(list, value);
    }

    /* Insert in middle */
    Node *prev = get_node_at(list, index - 1);
    Node *node = node_create(value);
    if (!node) {
        return SLL_ERR_ALLOC;
    }

    node->next = prev->next;
    prev->next = node;
    list->size++;
    return SLL_OK;
}

SLLResult sll_remove(SinglyLinkedList *list, size_t index, int *out) {
    if (!list) {
        return SLL_ERR_NULL;
    }
    if (index >= list->size) {
        return SLL_ERR_INDEX;
    }

    /* Remove from front */
    if (index == 0) {
        return sll_pop_front(list, out);
    }

    /* Find node before the one to remove */
    Node *prev = get_node_at(list, index - 1);
    Node *node = prev->next;

    if (out) {
        *out = node->value;
    }

    prev->next = node->next;
    if (node == list->tail) {
        list->tail = prev;
    }
    list->size--;
    free(node);
    return SLL_OK;
}

SLLResult sll_front(const SinglyLinkedList *list, int *out) {
    if (!list) {
        return SLL_ERR_NULL;
    }
    if (list->size == 0) {
        return SLL_ERR_EMPTY;
    }
    if (out) {
        *out = list->head->value;
    }
    return SLL_OK;
}

size_t sll_size(const SinglyLinkedList *list) {
    return list ? list->size : 0;
}

bool sll_is_empty(const SinglyLinkedList *list) {
    return !list || list->size == 0;
}

SLLResult sll_clear(SinglyLinkedList *list) {
    if (!list) {
        return SLL_ERR_NULL;
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
    return SLL_OK;
}

SLLResult sll_reverse(SinglyLinkedList *list) {
    if (!list) {
        return SLL_ERR_NULL;
    }
    if (list->size <= 1) {
        return SLL_OK;
    }

    Node *prev = NULL;
    Node *current = list->head;
    list->tail = list->head;

    while (current) {
        Node *next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }

    list->head = prev;
    return SLL_OK;
}

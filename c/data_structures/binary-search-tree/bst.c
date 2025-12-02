/**
 * @file bst.c
 * @brief Binary Search Tree implementation
 */

#include "bst.h"
#include <stdlib.h>

/** BST node structure */
typedef struct BSTNode {
    int value;
    struct BSTNode *left;
    struct BSTNode *right;
} BSTNode;

/** BST internal structure */
struct BST {
    BSTNode *root;
    size_t size;
};

/**
 * Create a new node.
 * @param value Value for the node
 * @return New node or NULL on allocation failure
 */
static BSTNode *node_create(int value) {
    BSTNode *node = malloc(sizeof(BSTNode));
    if (node) {
        node->value = value;
        node->left = NULL;
        node->right = NULL;
    }
    return node;
}

/**
 * Recursively destroy all nodes.
 * @param node Root of subtree to destroy
 */
static void destroy_subtree(BSTNode *node) {
    if (!node) {
        return;
    }
    destroy_subtree(node->left);
    destroy_subtree(node->right);
    free(node);
}

/**
 * Find a node with the given value.
 * @param node Root of subtree to search
 * @param value Value to find
 * @return Pointer to node, or NULL if not found
 */
static BSTNode *find_node(BSTNode *node, int value) {
    while (node) {
        if (value < node->value) {
            node = node->left;
        } else if (value > node->value) {
            node = node->right;
        } else {
            return node;
        }
    }
    return NULL;
}

/**
 * Find the minimum node in a subtree.
 * @param node Root of subtree
 * @return Leftmost node
 */
static BSTNode *find_min_node(BSTNode *node) {
    while (node && node->left) {
        node = node->left;
    }
    return node;
}

/**
 * Recursively insert a value.
 * @param node Current node
 * @param value Value to insert
 * @param created Output: set to the created node
 * @return Updated subtree root
 */
static BSTNode *insert_recursive(BSTNode *node, int value, BSTNode **created) {
    if (!node) {
        *created = node_create(value);
        return *created;
    }

    if (value < node->value) {
        node->left = insert_recursive(node->left, value, created);
    } else if (value > node->value) {
        node->right = insert_recursive(node->right, value, created);
    } else {
        *created = NULL;  /* Duplicate */
    }

    return node;
}

/**
 * Recursively remove a value.
 * @param node Current node
 * @param value Value to remove
 * @param removed Output: set to true if removed
 * @return Updated subtree root
 */
static BSTNode *remove_recursive(BSTNode *node, int value, bool *removed) {
    if (!node) {
        *removed = false;
        return NULL;
    }

    if (value < node->value) {
        node->left = remove_recursive(node->left, value, removed);
    } else if (value > node->value) {
        node->right = remove_recursive(node->right, value, removed);
    } else {
        /* Found the node to remove */
        *removed = true;

        /* Case 1: No children */
        if (!node->left && !node->right) {
            free(node);
            return NULL;
        }

        /* Case 2: One child */
        if (!node->left) {
            BSTNode *right = node->right;
            free(node);
            return right;
        }
        if (!node->right) {
            BSTNode *left = node->left;
            free(node);
            return left;
        }

        /* Case 3: Two children - replace with in-order successor */
        BSTNode *successor = find_min_node(node->right);
        node->value = successor->value;
        node->right = remove_recursive(node->right, successor->value, removed);
    }

    return node;
}

/**
 * Calculate height of a subtree.
 * @param node Root of subtree
 * @return Height (0 for NULL)
 */
static size_t subtree_height(const BSTNode *node) {
    if (!node) {
        return 0;
    }
    size_t left_height = subtree_height(node->left);
    size_t right_height = subtree_height(node->right);
    return 1 + (left_height > right_height ? left_height : right_height);
}

/**
 * In-order traversal helper.
 */
static void inorder_recursive(const BSTNode *node, BSTVisitor visitor, void *user_data) {
    if (!node) {
        return;
    }
    inorder_recursive(node->left, visitor, user_data);
    visitor(node->value, user_data);
    inorder_recursive(node->right, visitor, user_data);
}

/**
 * Pre-order traversal helper.
 */
static void preorder_recursive(const BSTNode *node, BSTVisitor visitor, void *user_data) {
    if (!node) {
        return;
    }
    visitor(node->value, user_data);
    preorder_recursive(node->left, visitor, user_data);
    preorder_recursive(node->right, visitor, user_data);
}

/**
 * Post-order traversal helper.
 */
static void postorder_recursive(const BSTNode *node, BSTVisitor visitor, void *user_data) {
    if (!node) {
        return;
    }
    postorder_recursive(node->left, visitor, user_data);
    postorder_recursive(node->right, visitor, user_data);
    visitor(node->value, user_data);
}

BST *bst_create(void) {
    BST *tree = malloc(sizeof(BST));
    if (tree) {
        tree->root = NULL;
        tree->size = 0;
    }
    return tree;
}

void bst_destroy(BST *tree) {
    if (!tree) {
        return;
    }
    destroy_subtree(tree->root);
    free(tree);
}

BSTResult bst_insert(BST *tree, int value) {
    if (!tree) {
        return BST_ERR_NULL;
    }

    BSTNode *created = NULL;
    tree->root = insert_recursive(tree->root, value, &created);

    if (!created) {
        return BST_ERR_DUPLICATE;
    }

    tree->size++;
    return BST_OK;
}

BSTResult bst_remove(BST *tree, int value) {
    if (!tree) {
        return BST_ERR_NULL;
    }

    bool removed = false;
    tree->root = remove_recursive(tree->root, value, &removed);

    if (!removed) {
        return BST_ERR_NOT_FOUND;
    }

    tree->size--;
    return BST_OK;
}

bool bst_contains(const BST *tree, int value) {
    if (!tree) {
        return false;
    }
    return find_node(tree->root, value) != NULL;
}

BSTResult bst_min(const BST *tree, int *out) {
    if (!tree) {
        return BST_ERR_NULL;
    }
    if (!tree->root) {
        return BST_ERR_EMPTY;
    }

    BSTNode *min = find_min_node(tree->root);
    if (out) {
        *out = min->value;
    }
    return BST_OK;
}

BSTResult bst_max(const BST *tree, int *out) {
    if (!tree) {
        return BST_ERR_NULL;
    }
    if (!tree->root) {
        return BST_ERR_EMPTY;
    }

    BSTNode *node = tree->root;
    while (node->right) {
        node = node->right;
    }

    if (out) {
        *out = node->value;
    }
    return BST_OK;
}

size_t bst_size(const BST *tree) {
    return tree ? tree->size : 0;
}

bool bst_is_empty(const BST *tree) {
    return !tree || tree->size == 0;
}

size_t bst_height(const BST *tree) {
    if (!tree) {
        return 0;
    }
    return subtree_height(tree->root);
}

BSTResult bst_clear(BST *tree) {
    if (!tree) {
        return BST_ERR_NULL;
    }
    destroy_subtree(tree->root);
    tree->root = NULL;
    tree->size = 0;
    return BST_OK;
}

BSTResult bst_inorder(const BST *tree, BSTVisitor visitor, void *user_data) {
    if (!tree || !visitor) {
        return BST_ERR_NULL;
    }
    inorder_recursive(tree->root, visitor, user_data);
    return BST_OK;
}

BSTResult bst_preorder(const BST *tree, BSTVisitor visitor, void *user_data) {
    if (!tree || !visitor) {
        return BST_ERR_NULL;
    }
    preorder_recursive(tree->root, visitor, user_data);
    return BST_OK;
}

BSTResult bst_postorder(const BST *tree, BSTVisitor visitor, void *user_data) {
    if (!tree || !visitor) {
        return BST_ERR_NULL;
    }
    postorder_recursive(tree->root, visitor, user_data);
    return BST_OK;
}

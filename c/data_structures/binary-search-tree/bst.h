/**
 * @file bst.h
 * @brief Binary Search Tree interface
 *
 * A binary tree where each node's left subtree contains only nodes with
 * smaller values and the right subtree contains only nodes with larger values.
 * Provides O(log n) average-case for search, insert, and delete.
 */

#ifndef DSA_BST_H
#define DSA_BST_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque BST type */
typedef struct BST BST;

/** Result codes for BST operations */
typedef enum {
    BST_OK = 0,         /**< Operation successful */
    BST_ERR_NULL,       /**< NULL pointer argument */
    BST_ERR_NOT_FOUND,  /**< Value not found */
    BST_ERR_DUPLICATE,  /**< Value already exists */
    BST_ERR_EMPTY,      /**< Tree is empty */
    BST_ERR_ALLOC       /**< Memory allocation failed */
} BSTResult;

/**
 * Create a new BST.
 * @return New BST or NULL on allocation failure
 */
BST *bst_create(void);

/**
 * Destroy a BST and free all memory.
 * @param tree Tree to destroy (NULL safe)
 */
void bst_destroy(BST *tree);

/**
 * Insert a value into the tree.
 * @param tree Target tree
 * @param value Value to insert
 * @return BST_OK on success, BST_ERR_DUPLICATE if value exists
 */
BSTResult bst_insert(BST *tree, int value);

/**
 * Remove a value from the tree.
 * @param tree Target tree
 * @param value Value to remove
 * @return BST_OK on success, BST_ERR_NOT_FOUND if value doesn't exist
 */
BSTResult bst_remove(BST *tree, int value);

/**
 * Check if a value exists in the tree.
 * @param tree Target tree
 * @param value Value to search for
 * @return true if value exists, false otherwise
 */
bool bst_contains(const BST *tree, int value);

/**
 * Get the minimum value in the tree.
 * @param tree Target tree
 * @param out Pointer to store minimum value
 * @return BST_OK on success, BST_ERR_EMPTY if tree is empty
 */
BSTResult bst_min(const BST *tree, int *out);

/**
 * Get the maximum value in the tree.
 * @param tree Target tree
 * @param out Pointer to store maximum value
 * @return BST_OK on success, BST_ERR_EMPTY if tree is empty
 */
BSTResult bst_max(const BST *tree, int *out);

/**
 * Get the number of nodes in the tree.
 * @param tree Target tree
 * @return Number of nodes, or 0 if NULL
 */
size_t bst_size(const BST *tree);

/**
 * Check if the tree is empty.
 * @param tree Target tree
 * @return true if empty or NULL, false otherwise
 */
bool bst_is_empty(const BST *tree);

/**
 * Get the height of the tree.
 * @param tree Target tree
 * @return Height (0 for empty tree)
 */
size_t bst_height(const BST *tree);

/**
 * Remove all nodes from the tree.
 * @param tree Target tree
 * @return BST_OK on success, BST_ERR_NULL if NULL
 */
BSTResult bst_clear(BST *tree);

/**
 * Callback function type for tree traversal.
 * @param value Current node's value
 * @param user_data User-provided context
 */
typedef void (*BSTVisitor)(int value, void *user_data);

/**
 * Traverse tree in-order (sorted order).
 * @param tree Target tree
 * @param visitor Callback function for each node
 * @param user_data User context passed to callback
 * @return BST_OK on success, BST_ERR_NULL if NULL
 */
BSTResult bst_inorder(const BST *tree, BSTVisitor visitor, void *user_data);

/**
 * Traverse tree pre-order (root, left, right).
 * @param tree Target tree
 * @param visitor Callback function for each node
 * @param user_data User context passed to callback
 * @return BST_OK on success, BST_ERR_NULL if NULL
 */
BSTResult bst_preorder(const BST *tree, BSTVisitor visitor, void *user_data);

/**
 * Traverse tree post-order (left, right, root).
 * @param tree Target tree
 * @param visitor Callback function for each node
 * @param user_data User context passed to callback
 * @return BST_OK on success, BST_ERR_NULL if NULL
 */
BSTResult bst_postorder(const BST *tree, BSTVisitor visitor, void *user_data);

#endif /* DSA_BST_H */

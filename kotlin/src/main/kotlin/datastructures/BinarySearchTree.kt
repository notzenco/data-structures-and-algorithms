package datastructures

/**
 * Binary Search Tree implementation.
 * Time: O(log n) average, O(n) worst for all operations
 * Space: O(n)
 */
class BinarySearchTree<T : Comparable<T>>(
    private val comparator: Comparator<T>? = null
) {
    private class Node<T>(var value: T, var left: Node<T>? = null, var right: Node<T>? = null)

    private var root: Node<T>? = null
    private var count = 0

    fun insert(value: T) {
        root = insertNode(root, value)
        count++
    }

    private fun insertNode(node: Node<T>?, value: T): Node<T> {
        if (node == null) return Node(value)

        val cmp = compare(value, node.value)
        when {
            cmp < 0 -> node.left = insertNode(node.left, value)
            cmp > 0 -> node.right = insertNode(node.right, value)
        }
        return node
    }

    fun contains(value: T): Boolean = findNode(root, value) != null

    private fun findNode(node: Node<T>?, value: T): Node<T>? {
        if (node == null) return null

        val cmp = compare(value, node.value)
        return when {
            cmp < 0 -> findNode(node.left, value)
            cmp > 0 -> findNode(node.right, value)
            else -> node
        }
    }

    fun remove(value: T): Boolean {
        val sizeBefore = count
        root = removeNode(root, value)
        return count < sizeBefore
    }

    private fun removeNode(node: Node<T>?, value: T): Node<T>? {
        if (node == null) return null

        val cmp = compare(value, node.value)
        when {
            cmp < 0 -> node.left = removeNode(node.left, value)
            cmp > 0 -> node.right = removeNode(node.right, value)
            else -> {
                count--
                when {
                    node.left == null -> return node.right
                    node.right == null -> return node.left
                    else -> {
                        val minRight = findMinNode(node.right!!)
                        node.value = minRight.value
                        node.right = removeNode(node.right, minRight.value)
                        count++ // Compensate for the recursive call decrement
                    }
                }
            }
        }
        return node
    }

    private fun findMinNode(node: Node<T>): Node<T> {
        var current = node
        while (current.left != null) {
            current = current.left!!
        }
        return current
    }

    private fun findMaxNode(node: Node<T>): Node<T> {
        var current = node
        while (current.right != null) {
            current = current.right!!
        }
        return current
    }

    fun min(): T? = root?.let { findMinNode(it).value }

    fun max(): T? = root?.let { findMaxNode(it).value }

    fun inOrder(): List<T> {
        val result = mutableListOf<T>()
        inOrderTraverse(root, result)
        return result
    }

    private fun inOrderTraverse(node: Node<T>?, result: MutableList<T>) {
        if (node == null) return
        inOrderTraverse(node.left, result)
        result.add(node.value)
        inOrderTraverse(node.right, result)
    }

    fun preOrder(): List<T> {
        val result = mutableListOf<T>()
        preOrderTraverse(root, result)
        return result
    }

    private fun preOrderTraverse(node: Node<T>?, result: MutableList<T>) {
        if (node == null) return
        result.add(node.value)
        preOrderTraverse(node.left, result)
        preOrderTraverse(node.right, result)
    }

    fun postOrder(): List<T> {
        val result = mutableListOf<T>()
        postOrderTraverse(root, result)
        return result
    }

    private fun postOrderTraverse(node: Node<T>?, result: MutableList<T>) {
        if (node == null) return
        postOrderTraverse(node.left, result)
        postOrderTraverse(node.right, result)
        result.add(node.value)
    }

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun clear() {
        root = null
        count = 0
    }

    private fun compare(a: T, b: T): Int = comparator?.compare(a, b) ?: a.compareTo(b)
}

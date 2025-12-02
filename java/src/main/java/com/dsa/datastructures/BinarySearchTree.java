package com.dsa.datastructures;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * A binary search tree implementation.
 * Time: O(log n) average, O(n) worst for insert, search, delete
 * Space: O(n)
 */
public class BinarySearchTree<T> {
    private static class Node<T> {
        T value;
        Node<T> left;
        Node<T> right;

        Node(T value) {
            this.value = value;
        }
    }

    private Node<T> root;
    private int size;
    private final Comparator<T> comparator;

    @SuppressWarnings("unchecked")
    public BinarySearchTree() {
        this((a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public BinarySearchTree(Comparator<T> comparator) {
        this.root = null;
        this.size = 0;
        this.comparator = comparator;
    }

    public boolean insert(T value) {
        if (root == null) {
            root = new Node<>(value);
            size++;
            return true;
        }
        return insertNode(root, value);
    }

    private boolean insertNode(Node<T> node, T value) {
        int cmp = comparator.compare(value, node.value);

        if (cmp < 0) {
            if (node.left == null) {
                node.left = new Node<>(value);
                size++;
                return true;
            }
            return insertNode(node.left, value);
        } else if (cmp > 0) {
            if (node.right == null) {
                node.right = new Node<>(value);
                size++;
                return true;
            }
            return insertNode(node.right, value);
        }
        return false; // Duplicate
    }

    public boolean contains(T value) {
        return searchNode(root, value) != null;
    }

    private Node<T> searchNode(Node<T> node, T value) {
        if (node == null) {
            return null;
        }

        int cmp = comparator.compare(value, node.value);

        if (cmp < 0) {
            return searchNode(node.left, value);
        } else if (cmp > 0) {
            return searchNode(node.right, value);
        }
        return node;
    }

    public boolean remove(T value) {
        int oldSize = size;
        root = removeNode(root, value);
        return size < oldSize;
    }

    private Node<T> removeNode(Node<T> node, T value) {
        if (node == null) {
            return null;
        }

        int cmp = comparator.compare(value, node.value);

        if (cmp < 0) {
            node.left = removeNode(node.left, value);
        } else if (cmp > 0) {
            node.right = removeNode(node.right, value);
        } else {
            size--;
            if (node.left == null) {
                return node.right;
            }
            if (node.right == null) {
                return node.left;
            }

            Node<T> minNode = findMin(node.right);
            node.value = minNode.value;
            node.right = removeNode(node.right, minNode.value);
            size++; // Compensate for the recursive remove
        }
        return node;
    }

    private Node<T> findMin(Node<T> node) {
        while (node.left != null) {
            node = node.left;
        }
        return node;
    }

    public Optional<T> min() {
        if (root == null) {
            return Optional.empty();
        }
        return Optional.of(findMin(root).value);
    }

    public Optional<T> max() {
        if (root == null) {
            return Optional.empty();
        }
        Node<T> node = root;
        while (node.right != null) {
            node = node.right;
        }
        return Optional.of(node.value);
    }

    public List<T> inorder() {
        List<T> result = new ArrayList<>();
        inorderTraverse(root, result);
        return result;
    }

    private void inorderTraverse(Node<T> node, List<T> result) {
        if (node != null) {
            inorderTraverse(node.left, result);
            result.add(node.value);
            inorderTraverse(node.right, result);
        }
    }

    public boolean isEmpty() {
        return root == null;
    }

    public int size() {
        return size;
    }
}

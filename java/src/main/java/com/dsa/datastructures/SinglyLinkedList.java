package com.dsa.datastructures;

import java.util.Optional;

/**
 * A singly linked list implementation.
 * Time: O(1) for pushFront, O(n) for pushBack, search
 * Space: O(n)
 */
public class SinglyLinkedList<T> {
    private static class Node<T> {
        T value;
        Node<T> next;

        Node(T value) {
            this.value = value;
            this.next = null;
        }
    }

    private Node<T> head;
    private int size;

    public SinglyLinkedList() {
        this.head = null;
        this.size = 0;
    }

    public void pushFront(T value) {
        Node<T> node = new Node<>(value);
        node.next = head;
        head = node;
        size++;
    }

    public Optional<T> popFront() {
        if (head == null) {
            return Optional.empty();
        }
        T value = head.value;
        head = head.next;
        size--;
        return Optional.of(value);
    }

    public void pushBack(T value) {
        Node<T> node = new Node<>(value);
        if (head == null) {
            head = node;
        } else {
            Node<T> current = head;
            while (current.next != null) {
                current = current.next;
            }
            current.next = node;
        }
        size++;
    }

    public Optional<T> peekFront() {
        if (head == null) {
            return Optional.empty();
        }
        return Optional.of(head.value);
    }

    public boolean contains(T value) {
        Node<T> current = head;
        while (current != null) {
            if (current.value.equals(value)) {
                return true;
            }
            current = current.next;
        }
        return false;
    }

    public boolean isEmpty() {
        return head == null;
    }

    public int size() {
        return size;
    }

    public void clear() {
        head = null;
        size = 0;
    }
}

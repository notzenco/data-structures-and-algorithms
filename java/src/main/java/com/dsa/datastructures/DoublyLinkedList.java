package com.dsa.datastructures;

import java.util.Optional;

/**
 * A doubly linked list implementation.
 * Time: O(1) for push/pop front/back, O(n) for search
 * Space: O(n)
 */
public class DoublyLinkedList<T> {
    private static class Node<T> {
        T value;
        Node<T> prev;
        Node<T> next;

        Node(T value) {
            this.value = value;
            this.prev = null;
            this.next = null;
        }
    }

    private Node<T> head;
    private Node<T> tail;
    private int size;

    public DoublyLinkedList() {
        this.head = null;
        this.tail = null;
        this.size = 0;
    }

    public void pushFront(T value) {
        Node<T> node = new Node<>(value);
        if (head == null) {
            head = tail = node;
        } else {
            node.next = head;
            head.prev = node;
            head = node;
        }
        size++;
    }

    public void pushBack(T value) {
        Node<T> node = new Node<>(value);
        if (tail == null) {
            head = tail = node;
        } else {
            node.prev = tail;
            tail.next = node;
            tail = node;
        }
        size++;
    }

    public Optional<T> popFront() {
        if (head == null) {
            return Optional.empty();
        }
        T value = head.value;
        head = head.next;
        if (head == null) {
            tail = null;
        } else {
            head.prev = null;
        }
        size--;
        return Optional.of(value);
    }

    public Optional<T> popBack() {
        if (tail == null) {
            return Optional.empty();
        }
        T value = tail.value;
        tail = tail.prev;
        if (tail == null) {
            head = null;
        } else {
            tail.next = null;
        }
        size--;
        return Optional.of(value);
    }

    public Optional<T> peekFront() {
        if (head == null) {
            return Optional.empty();
        }
        return Optional.of(head.value);
    }

    public Optional<T> peekBack() {
        if (tail == null) {
            return Optional.empty();
        }
        return Optional.of(tail.value);
    }

    public boolean isEmpty() {
        return head == null;
    }

    public int size() {
        return size;
    }

    public void clear() {
        head = null;
        tail = null;
        size = 0;
    }
}

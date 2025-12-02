package com.dsa.datastructures;

import java.util.LinkedList;
import java.util.Optional;

/**
 * A generic queue implementation using LinkedList.
 * Time: O(1) for enqueue, dequeue, peek
 * Space: O(n)
 */
public class Queue<T> {
    private final LinkedList<T> data;

    public Queue() {
        this.data = new LinkedList<>();
    }

    public void enqueue(T value) {
        data.addLast(value);
    }

    public Optional<T> dequeue() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.removeFirst());
    }

    public Optional<T> peek() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.getFirst());
    }

    public boolean isEmpty() {
        return data.isEmpty();
    }

    public int size() {
        return data.size();
    }

    public void clear() {
        data.clear();
    }
}

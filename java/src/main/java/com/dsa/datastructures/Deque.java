package com.dsa.datastructures;

import java.util.ArrayDeque;
import java.util.Optional;

/**
 * A double-ended queue implementation.
 * Time: O(1) for push/pop front/back
 * Space: O(n)
 */
public class Deque<T> {
    private final ArrayDeque<T> data;

    public Deque() {
        this.data = new ArrayDeque<>();
    }

    public void pushFront(T value) {
        data.addFirst(value);
    }

    public void pushBack(T value) {
        data.addLast(value);
    }

    public Optional<T> popFront() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.removeFirst());
    }

    public Optional<T> popBack() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.removeLast());
    }

    public Optional<T> peekFront() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.getFirst());
    }

    public Optional<T> peekBack() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.getLast());
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

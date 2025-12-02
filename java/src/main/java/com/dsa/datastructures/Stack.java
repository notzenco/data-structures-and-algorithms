package com.dsa.datastructures;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * A generic stack implementation using ArrayList.
 * Time: O(1) for push, pop, peek
 * Space: O(n)
 */
public class Stack<T> {
    private final List<T> data;

    public Stack() {
        this.data = new ArrayList<>();
    }

    public void push(T value) {
        data.add(value);
    }

    public Optional<T> pop() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.remove(data.size() - 1));
    }

    public Optional<T> peek() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.get(data.size() - 1));
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

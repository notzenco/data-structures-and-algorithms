package com.dsa.datastructures;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

/**
 * A generic dynamic array implementation.
 * Time: O(1) amortized for add, O(n) for insert/remove
 * Space: O(n)
 */
public class DynamicArray<T> implements Iterable<T> {
    private final List<T> data;

    public DynamicArray() {
        this.data = new ArrayList<>();
    }

    public void add(T value) {
        data.add(value);
    }

    public Optional<T> removeLast() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.remove(data.size() - 1));
    }

    public void insert(int index, T value) {
        if (index >= 0 && index <= data.size()) {
            data.add(index, value);
        }
    }

    public Optional<T> remove(int index) {
        if (index >= 0 && index < data.size()) {
            return Optional.of(data.remove(index));
        }
        return Optional.empty();
    }

    public Optional<T> get(int index) {
        if (index >= 0 && index < data.size()) {
            return Optional.of(data.get(index));
        }
        return Optional.empty();
    }

    public boolean set(int index, T value) {
        if (index >= 0 && index < data.size()) {
            data.set(index, value);
            return true;
        }
        return false;
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

    @Override
    public Iterator<T> iterator() {
        return data.iterator();
    }
}

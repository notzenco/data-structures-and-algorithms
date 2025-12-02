package com.dsa.datastructures;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * A binary min heap implementation.
 * Time: O(log n) for push/pop, O(1) for peek
 * Space: O(n)
 */
public class MinHeap<T> {
    private final List<T> data;
    private final Comparator<T> comparator;

    @SuppressWarnings("unchecked")
    public MinHeap() {
        this((a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public MinHeap(Comparator<T> comparator) {
        this.data = new ArrayList<>();
        this.comparator = comparator;
    }

    public void push(T value) {
        data.add(value);
        siftUp(data.size() - 1);
    }

    public Optional<T> pop() {
        if (data.isEmpty()) {
            return Optional.empty();
        }

        swap(0, data.size() - 1);
        T result = data.remove(data.size() - 1);

        if (!data.isEmpty()) {
            siftDown(0);
        }

        return Optional.of(result);
    }

    public Optional<T> peek() {
        if (data.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(data.get(0));
    }

    private void siftUp(int index) {
        while (index > 0) {
            int parent = (index - 1) / 2;
            if (comparator.compare(data.get(index), data.get(parent)) < 0) {
                swap(index, parent);
                index = parent;
            } else {
                break;
            }
        }
    }

    private void siftDown(int index) {
        int size = data.size();
        while (true) {
            int left = 2 * index + 1;
            int right = 2 * index + 2;
            int smallest = index;

            if (left < size && comparator.compare(data.get(left), data.get(smallest)) < 0) {
                smallest = left;
            }
            if (right < size && comparator.compare(data.get(right), data.get(smallest)) < 0) {
                smallest = right;
            }

            if (smallest != index) {
                swap(index, smallest);
                index = smallest;
            } else {
                break;
            }
        }
    }

    private void swap(int i, int j) {
        T temp = data.get(i);
        data.set(i, data.get(j));
        data.set(j, temp);
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

    public static <T> MinHeap<T> heapify(List<T> items, Comparator<T> comparator) {
        MinHeap<T> heap = new MinHeap<>(comparator);
        heap.data.addAll(items);
        for (int i = heap.data.size() / 2 - 1; i >= 0; i--) {
            heap.siftDown(i);
        }
        return heap;
    }
}

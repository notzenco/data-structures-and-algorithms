package com.dsa.datastructures;

import java.util.*;

/**
 * A hash table implementation using open addressing with linear probing.
 * Time: O(1) average for insert, get, remove
 * Space: O(n)
 */
public class HashTable<K, V> {
    private static final int INITIAL_CAPACITY = 16;
    private static final double LOAD_FACTOR = 0.75;

    private enum EntryState { EMPTY, DELETED, OCCUPIED }

    private static class Entry<K, V> {
        K key;
        V value;
        EntryState state;

        Entry() {
            this.state = EntryState.EMPTY;
        }

        void set(K key, V value) {
            this.key = key;
            this.value = value;
            this.state = EntryState.OCCUPIED;
        }

        void delete() {
            this.key = null;
            this.value = null;
            this.state = EntryState.DELETED;
        }
    }

    private Entry<K, V>[] entries;
    private int size;
    private int capacity;

    @SuppressWarnings("unchecked")
    public HashTable() {
        this.capacity = INITIAL_CAPACITY;
        this.entries = new Entry[capacity];
        for (int i = 0; i < capacity; i++) {
            entries[i] = new Entry<>();
        }
        this.size = 0;
    }

    private int hash(K key) {
        return Math.abs(key.hashCode()) % capacity;
    }

    @SuppressWarnings("unchecked")
    private void resize() {
        Entry<K, V>[] oldEntries = entries;
        capacity *= 2;
        entries = new Entry[capacity];
        for (int i = 0; i < capacity; i++) {
            entries[i] = new Entry<>();
        }
        size = 0;

        for (Entry<K, V> entry : oldEntries) {
            if (entry.state == EntryState.OCCUPIED) {
                put(entry.key, entry.value);
            }
        }
    }

    public Optional<V> put(K key, V value) {
        if ((size + 1.0) / capacity > LOAD_FACTOR) {
            resize();
        }

        int index = hash(key);
        int startIndex = index;

        while (true) {
            Entry<K, V> entry = entries[index];

            if (entry.state == EntryState.EMPTY || entry.state == EntryState.DELETED) {
                entry.set(key, value);
                size++;
                return Optional.empty();
            }

            if (entry.state == EntryState.OCCUPIED && entry.key.equals(key)) {
                V oldValue = entry.value;
                entry.value = value;
                return Optional.of(oldValue);
            }

            index = (index + 1) % capacity;
            if (index == startIndex) {
                throw new RuntimeException("Hash table is full");
            }
        }
    }

    public Optional<V> get(K key) {
        int index = hash(key);
        int startIndex = index;

        while (true) {
            Entry<K, V> entry = entries[index];

            if (entry.state == EntryState.EMPTY) {
                return Optional.empty();
            }

            if (entry.state == EntryState.OCCUPIED && entry.key.equals(key)) {
                return Optional.of(entry.value);
            }

            index = (index + 1) % capacity;
            if (index == startIndex) {
                return Optional.empty();
            }
        }
    }

    public Optional<V> remove(K key) {
        int index = hash(key);
        int startIndex = index;

        while (true) {
            Entry<K, V> entry = entries[index];

            if (entry.state == EntryState.EMPTY) {
                return Optional.empty();
            }

            if (entry.state == EntryState.OCCUPIED && entry.key.equals(key)) {
                V value = entry.value;
                entry.delete();
                size--;
                return Optional.of(value);
            }

            index = (index + 1) % capacity;
            if (index == startIndex) {
                return Optional.empty();
            }
        }
    }

    public boolean containsKey(K key) {
        return get(key).isPresent();
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public void clear() {
        for (int i = 0; i < capacity; i++) {
            entries[i] = new Entry<>();
        }
        size = 0;
    }
}

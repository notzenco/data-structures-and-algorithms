package com.dsa.algorithms;

import java.util.Comparator;
import java.util.List;

/**
 * Insertion sort implementation.
 * Time: O(n^2) worst/average, O(n) best
 * Space: O(1)
 */
public class InsertionSort {

    @SuppressWarnings("unchecked")
    public static <T> void sort(List<T> arr) {
        sort(arr, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> void sort(List<T> arr, Comparator<T> comparator) {
        for (int i = 1; i < arr.size(); i++) {
            T current = arr.get(i);
            int j = i;

            while (j > 0 && comparator.compare(arr.get(j - 1), current) > 0) {
                arr.set(j, arr.get(j - 1));
                j--;
            }

            arr.set(j, current);
        }
    }

    public static <T> void sortDescending(List<T> arr, Comparator<T> comparator) {
        sort(arr, comparator.reversed());
    }
}

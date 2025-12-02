package com.dsa.algorithms;

import java.util.Comparator;
import java.util.List;

/**
 * Quick sort implementation with median-of-three pivot selection.
 * Time: O(n log n) average, O(n^2) worst
 * Space: O(log n) average
 */
public class QuickSort {

    @SuppressWarnings("unchecked")
    public static <T> void sort(List<T> arr) {
        sort(arr, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> void sort(List<T> arr, Comparator<T> comparator) {
        if (arr.size() < 2) {
            return;
        }
        quickSort(arr, 0, arr.size() - 1, comparator);
    }

    private static <T> void quickSort(List<T> arr, int low, int high, Comparator<T> comparator) {
        if (low >= high) {
            return;
        }

        int pivotIdx = partition(arr, low, high, comparator);

        if (pivotIdx > low) {
            quickSort(arr, low, pivotIdx - 1, comparator);
        }
        if (pivotIdx < high) {
            quickSort(arr, pivotIdx + 1, high, comparator);
        }
    }

    private static <T> int partition(List<T> arr, int low, int high, Comparator<T> comparator) {
        // Median of three pivot selection
        int mid = low + (high - low) / 2;

        if (comparator.compare(arr.get(high), arr.get(low)) < 0) {
            swap(arr, low, high);
        }
        if (comparator.compare(arr.get(mid), arr.get(low)) < 0) {
            swap(arr, low, mid);
        }
        if (comparator.compare(arr.get(high), arr.get(mid)) < 0) {
            swap(arr, mid, high);
        }

        swap(arr, mid, high);
        T pivot = arr.get(high);

        int i = low;
        for (int j = low; j < high; j++) {
            if (comparator.compare(arr.get(j), pivot) < 0) {
                swap(arr, i, j);
                i++;
            }
        }

        swap(arr, i, high);
        return i;
    }

    private static <T> void swap(List<T> arr, int i, int j) {
        T temp = arr.get(i);
        arr.set(i, arr.get(j));
        arr.set(j, temp);
    }

    public static <T> void sortDescending(List<T> arr, Comparator<T> comparator) {
        sort(arr, comparator.reversed());
    }
}

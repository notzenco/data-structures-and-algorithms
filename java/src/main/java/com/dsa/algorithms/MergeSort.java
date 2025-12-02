package com.dsa.algorithms;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Merge sort implementation.
 * Time: O(n log n) all cases
 * Space: O(n)
 */
public class MergeSort {

    @SuppressWarnings("unchecked")
    public static <T> void sort(List<T> arr) {
        sort(arr, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> void sort(List<T> arr, Comparator<T> comparator) {
        if (arr.size() < 2) {
            return;
        }
        mergeSort(arr, 0, arr.size() - 1, comparator);
    }

    private static <T> void mergeSort(List<T> arr, int left, int right, Comparator<T> comparator) {
        if (left >= right) {
            return;
        }

        int mid = left + (right - left) / 2;
        mergeSort(arr, left, mid, comparator);
        mergeSort(arr, mid + 1, right, comparator);
        merge(arr, left, mid, right, comparator);
    }

    private static <T> void merge(List<T> arr, int left, int mid, int right, Comparator<T> comparator) {
        List<T> leftArr = new ArrayList<>(arr.subList(left, mid + 1));
        List<T> rightArr = new ArrayList<>(arr.subList(mid + 1, right + 1));

        int i = 0, j = 0, k = left;

        while (i < leftArr.size() && j < rightArr.size()) {
            if (comparator.compare(leftArr.get(i), rightArr.get(j)) <= 0) {
                arr.set(k++, leftArr.get(i++));
            } else {
                arr.set(k++, rightArr.get(j++));
            }
        }

        while (i < leftArr.size()) {
            arr.set(k++, leftArr.get(i++));
        }

        while (j < rightArr.size()) {
            arr.set(k++, rightArr.get(j++));
        }
    }

    public static <T> void sortDescending(List<T> arr, Comparator<T> comparator) {
        sort(arr, comparator.reversed());
    }
}

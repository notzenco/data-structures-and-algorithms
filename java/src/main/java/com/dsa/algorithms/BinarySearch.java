package com.dsa.algorithms;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Binary search implementations.
 * Time: O(log n)
 * Space: O(1) iterative, O(log n) recursive
 */
public class BinarySearch {

    @SuppressWarnings("unchecked")
    public static <T> Optional<Integer> search(List<T> arr, T target) {
        return search(arr, target, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> Optional<Integer> search(List<T> arr, T target, Comparator<T> comparator) {
        if (arr.isEmpty()) {
            return Optional.empty();
        }

        int left = 0;
        int right = arr.size() - 1;

        while (left <= right) {
            int mid = left + (right - left) / 2;
            int cmp = comparator.compare(arr.get(mid), target);

            if (cmp == 0) {
                return Optional.of(mid);
            } else if (cmp < 0) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }

        return Optional.empty();
    }

    @SuppressWarnings("unchecked")
    public static <T> Optional<Integer> searchRecursive(List<T> arr, T target) {
        return searchRecursive(arr, target, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> Optional<Integer> searchRecursive(List<T> arr, T target, Comparator<T> comparator) {
        if (arr.isEmpty()) {
            return Optional.empty();
        }
        return searchRecursiveImpl(arr, target, 0, arr.size() - 1, comparator);
    }

    private static <T> Optional<Integer> searchRecursiveImpl(
            List<T> arr, T target, int left, int right, Comparator<T> comparator) {
        if (left > right) {
            return Optional.empty();
        }

        int mid = left + (right - left) / 2;
        int cmp = comparator.compare(arr.get(mid), target);

        if (cmp == 0) {
            return Optional.of(mid);
        } else if (cmp < 0) {
            return searchRecursiveImpl(arr, target, mid + 1, right, comparator);
        } else {
            return searchRecursiveImpl(arr, target, left, mid - 1, comparator);
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> int lowerBound(List<T> arr, T target) {
        return lowerBound(arr, target, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> int lowerBound(List<T> arr, T target, Comparator<T> comparator) {
        int left = 0;
        int right = arr.size();

        while (left < right) {
            int mid = left + (right - left) / 2;
            if (comparator.compare(arr.get(mid), target) < 0) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        return left;
    }

    @SuppressWarnings("unchecked")
    public static <T> int upperBound(List<T> arr, T target) {
        return upperBound(arr, target, (a, b) -> ((Comparable<T>) a).compareTo(b));
    }

    public static <T> int upperBound(List<T> arr, T target, Comparator<T> comparator) {
        int left = 0;
        int right = arr.size();

        while (left < right) {
            int mid = left + (right - left) / 2;
            if (comparator.compare(arr.get(mid), target) <= 0) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        return left;
    }
}

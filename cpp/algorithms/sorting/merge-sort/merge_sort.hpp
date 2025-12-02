/**
 * @file merge_sort.hpp
 * @brief Merge sort algorithm implementation
 *
 * Divide-and-conquer sorting algorithm that splits the array,
 * recursively sorts each half, then merges them.
 * Time: O(n log n) all cases
 * Space: O(n)
 */

#ifndef DSA_MERGE_SORT_HPP
#define DSA_MERGE_SORT_HPP

#include <cstddef>
#include <functional>
#include <vector>

namespace dsa {

namespace detail {

template <typename T, typename Compare>
void merge(std::vector<T>& arr, std::size_t left, std::size_t mid,
           std::size_t right, Compare comp) {
    std::vector<T> left_arr(arr.begin() + left, arr.begin() + mid + 1);
    std::vector<T> right_arr(arr.begin() + mid + 1, arr.begin() + right + 1);

    std::size_t i = 0, j = 0, k = left;

    while (i < left_arr.size() && j < right_arr.size()) {
        if (comp(left_arr[i], right_arr[j]) ||
            (!comp(right_arr[j], left_arr[i]) && i <= j)) {
            arr[k++] = std::move(left_arr[i++]);
        } else {
            arr[k++] = std::move(right_arr[j++]);
        }
    }

    while (i < left_arr.size()) {
        arr[k++] = std::move(left_arr[i++]);
    }

    while (j < right_arr.size()) {
        arr[k++] = std::move(right_arr[j++]);
    }
}

template <typename T, typename Compare>
void merge_sort_impl(std::vector<T>& arr, std::size_t left, std::size_t right,
                     Compare comp) {
    if (left >= right) return;

    std::size_t mid = left + (right - left) / 2;
    merge_sort_impl(arr, left, mid, comp);
    merge_sort_impl(arr, mid + 1, right, comp);
    merge(arr, left, mid, right, comp);
}

} // namespace detail

template <typename T, typename Compare = std::less<T>>
void merge_sort(std::vector<T>& arr, Compare comp = Compare{}) {
    if (arr.size() < 2) return;
    detail::merge_sort_impl(arr, 0, arr.size() - 1, comp);
}

template <typename T, typename Compare = std::less<T>>
void merge_sort(T* arr, std::size_t size, Compare comp = Compare{}) {
    if (!arr || size < 2) return;
    std::vector<T> vec(arr, arr + size);
    merge_sort(vec, comp);
    for (std::size_t i = 0; i < size; ++i) {
        arr[i] = std::move(vec[i]);
    }
}

template <typename T>
void merge_sort_desc(std::vector<T>& arr) {
    merge_sort(arr, std::greater<T>{});
}

} // namespace dsa

#endif // DSA_MERGE_SORT_HPP

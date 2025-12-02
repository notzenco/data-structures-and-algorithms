/**
 * @file insertion_sort.hpp
 * @brief Insertion sort algorithm implementation
 *
 * Builds sorted array one element at a time by inserting each element
 * into its correct position. Stable and adaptive.
 * Time: O(n^2) worst/average, O(n) best
 * Space: O(1)
 */

#ifndef DSA_INSERTION_SORT_HPP
#define DSA_INSERTION_SORT_HPP

#include <cstddef>
#include <functional>
#include <utility>
#include <vector>

namespace dsa {

template <typename T, typename Compare = std::less<T>>
void insertion_sort(std::vector<T>& arr, Compare comp = Compare{}) {
    for (std::size_t i = 1; i < arr.size(); ++i) {
        T key = std::move(arr[i]);
        std::size_t j = i;
        while (j > 0 && comp(key, arr[j - 1])) {
            arr[j] = std::move(arr[j - 1]);
            --j;
        }
        arr[j] = std::move(key);
    }
}

template <typename T, typename Compare = std::less<T>>
void insertion_sort(T* arr, std::size_t size, Compare comp = Compare{}) {
    if (!arr || size < 2) return;
    for (std::size_t i = 1; i < size; ++i) {
        T key = std::move(arr[i]);
        std::size_t j = i;
        while (j > 0 && comp(key, arr[j - 1])) {
            arr[j] = std::move(arr[j - 1]);
            --j;
        }
        arr[j] = std::move(key);
    }
}

template <typename T>
void insertion_sort_desc(std::vector<T>& arr) {
    insertion_sort(arr, std::greater<T>{});
}

} // namespace dsa

#endif // DSA_INSERTION_SORT_HPP

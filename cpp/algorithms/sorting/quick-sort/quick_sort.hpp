/**
 * @file quick_sort.hpp
 * @brief Quick sort algorithm implementation
 *
 * Divide-and-conquer sorting using partitioning around a pivot.
 * Time: O(n log n) average, O(n^2) worst
 * Space: O(log n) average
 */

#ifndef DSA_QUICK_SORT_HPP
#define DSA_QUICK_SORT_HPP

#include <cstddef>
#include <functional>
#include <utility>
#include <vector>

namespace dsa {

namespace detail {

template <typename T, typename Compare>
std::size_t partition(std::vector<T>& arr, std::size_t low, std::size_t high,
                      Compare comp) {
    T pivot = arr[high];
    std::size_t i = low;

    for (std::size_t j = low; j < high; ++j) {
        if (comp(arr[j], pivot)) {
            std::swap(arr[i], arr[j]);
            ++i;
        }
    }
    std::swap(arr[i], arr[high]);
    return i;
}

template <typename T, typename Compare>
std::size_t median_of_three(std::vector<T>& arr, std::size_t low,
                            std::size_t high, Compare comp) {
    std::size_t mid = low + (high - low) / 2;

    if (comp(arr[high], arr[low])) std::swap(arr[low], arr[high]);
    if (comp(arr[mid], arr[low])) std::swap(arr[low], arr[mid]);
    if (comp(arr[high], arr[mid])) std::swap(arr[mid], arr[high]);

    std::swap(arr[mid], arr[high]);
    return partition(arr, low, high, comp);
}

template <typename T, typename Compare>
void quick_sort_impl(std::vector<T>& arr, std::size_t low, std::size_t high,
                     Compare comp) {
    if (low >= high) return;

    std::size_t pivot_idx = median_of_three(arr, low, high, comp);

    if (pivot_idx > low) {
        quick_sort_impl(arr, low, pivot_idx - 1, comp);
    }
    if (pivot_idx < high) {
        quick_sort_impl(arr, pivot_idx + 1, high, comp);
    }
}

} // namespace detail

template <typename T, typename Compare = std::less<T>>
void quick_sort(std::vector<T>& arr, Compare comp = Compare{}) {
    if (arr.size() < 2) return;
    detail::quick_sort_impl(arr, 0, arr.size() - 1, comp);
}

template <typename T, typename Compare = std::less<T>>
void quick_sort(T* arr, std::size_t size, Compare comp = Compare{}) {
    if (!arr || size < 2) return;
    std::vector<T> vec(arr, arr + size);
    quick_sort(vec, comp);
    for (std::size_t i = 0; i < size; ++i) {
        arr[i] = std::move(vec[i]);
    }
}

template <typename T>
void quick_sort_desc(std::vector<T>& arr) {
    quick_sort(arr, std::greater<T>{});
}

} // namespace dsa

#endif // DSA_QUICK_SORT_HPP

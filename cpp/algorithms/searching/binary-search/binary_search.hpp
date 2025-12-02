/**
 * @file binary_search.hpp
 * @brief Binary search algorithm implementation
 *
 * Efficiently searches a sorted array by repeatedly dividing the search space in half.
 * Time complexity: O(log n)
 * Space complexity: O(1) iterative, O(log n) recursive
 */

#ifndef DSA_BINARY_SEARCH_HPP
#define DSA_BINARY_SEARCH_HPP

#include <cstddef>
#include <functional>
#include <optional>
#include <vector>

namespace dsa {

/**
 * Binary search in a sorted array (iterative).
 * @param arr Sorted array to search
 * @param target Value to find
 * @return Index of target if found, std::nullopt otherwise
 */
template <typename T>
std::optional<std::size_t> binary_search(const std::vector<T>& arr, const T& target) {
    if (arr.empty()) return std::nullopt;

    std::size_t left = 0;
    std::size_t right = arr.size() - 1;

    while (left <= right) {
        std::size_t mid = left + (right - left) / 2;

        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            if (mid == 0) break;
            right = mid - 1;
        }
    }

    return std::nullopt;
}

/**
 * Binary search using raw pointers.
 * @param arr Pointer to sorted array
 * @param size Number of elements
 * @param target Value to find
 * @return Index of target if found, std::nullopt otherwise
 */
template <typename T>
std::optional<std::size_t> binary_search(const T* arr, std::size_t size, const T& target) {
    if (!arr || size == 0) return std::nullopt;

    std::size_t left = 0;
    std::size_t right = size - 1;

    while (left <= right) {
        std::size_t mid = left + (right - left) / 2;

        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            if (mid == 0) break;
            right = mid - 1;
        }
    }

    return std::nullopt;
}

/**
 * Find the lower bound (first element >= target).
 * @param arr Sorted array
 * @param target Value to find
 * @return Index of first element >= target, or arr.size() if none
 */
template <typename T>
std::size_t lower_bound(const std::vector<T>& arr, const T& target) {
    std::size_t left = 0;
    std::size_t right = arr.size();

    while (left < right) {
        std::size_t mid = left + (right - left) / 2;
        if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    return left;
}

/**
 * Find the upper bound (first element > target).
 * @param arr Sorted array
 * @param target Value to find
 * @return Index of first element > target, or arr.size() if none
 */
template <typename T>
std::size_t upper_bound(const std::vector<T>& arr, const T& target) {
    std::size_t left = 0;
    std::size_t right = arr.size();

    while (left < right) {
        std::size_t mid = left + (right - left) / 2;
        if (arr[mid] <= target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    return left;
}

/**
 * Binary search (recursive version).
 * @param arr Sorted array to search
 * @param target Value to find
 * @return Index of target if found, std::nullopt otherwise
 */
template <typename T>
std::optional<std::size_t> binary_search_recursive(const std::vector<T>& arr, const T& target) {
    if (arr.empty()) return std::nullopt;

    std::function<std::optional<std::size_t>(std::size_t, std::size_t)> search =
        [&](std::size_t left, std::size_t right) -> std::optional<std::size_t> {
        if (left > right) return std::nullopt;

        std::size_t mid = left + (right - left) / 2;

        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            return search(mid + 1, right);
        } else {
            if (mid == 0) return std::nullopt;
            return search(left, mid - 1);
        }
    };

    return search(0, arr.size() - 1);
}

} // namespace dsa

#endif // DSA_BINARY_SEARCH_HPP

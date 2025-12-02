# frozen_string_literal: true

module DSA
  module Algorithms
    # Quick sort algorithm with median-of-three pivot selection.
    # Time: O(n log n) average, O(n²) worst case
    # Space: O(log n) average for recursion stack
    # Stable: No
    module QuickSort
      class << self
        # Sort an array in-place.
        def sort!(arr, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          quick_sort(arr, 0, arr.size - 1, comparator)
          arr
        end

        # Return a sorted copy of the array.
        def sort(arr, &comparator)
          sort!(arr.dup, &comparator)
        end

        private

        def quick_sort(arr, low, high, comparator)
          return if low >= high

          pivot_index = partition(arr, low, high, comparator)
          quick_sort(arr, low, pivot_index - 1, comparator)
          quick_sort(arr, pivot_index + 1, high, comparator)
        end

        def partition(arr, low, high, comparator)
          # Median-of-three pivot selection
          mid = low + (high - low) / 2

          arr[low], arr[mid] = arr[mid], arr[low] if comparator.call(arr[mid], arr[low]) < 0
          arr[low], arr[high] = arr[high], arr[low] if comparator.call(arr[high], arr[low]) < 0
          arr[mid], arr[high] = arr[high], arr[mid] if comparator.call(arr[mid], arr[high]) < 0

          pivot = arr[high]
          i = low - 1

          (low...high).each do |j|
            if comparator.call(arr[j], pivot) <= 0
              i += 1
              arr[i], arr[j] = arr[j], arr[i]
            end
          end

          arr[i + 1], arr[high] = arr[high], arr[i + 1]
          i + 1
        end
      end
    end
  end
end

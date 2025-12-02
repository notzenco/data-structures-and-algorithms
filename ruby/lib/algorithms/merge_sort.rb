# frozen_string_literal: true

module DSA
  module Algorithms
    # Merge sort algorithm.
    # Time: O(n log n) all cases
    # Space: O(n)
    # Stable: Yes
    module MergeSort
      class << self
        # Sort an array in-place.
        def sort!(arr, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          sorted = merge_sort(arr, comparator)
          arr.replace(sorted)
        end

        # Return a sorted copy of the array.
        def sort(arr, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          merge_sort(arr, comparator)
        end

        private

        def merge_sort(arr, comparator)
          return arr if arr.size <= 1

          mid = arr.size / 2
          left = merge_sort(arr[0...mid], comparator)
          right = merge_sort(arr[mid..], comparator)

          merge(left, right, comparator)
        end

        def merge(left, right, comparator)
          result = []
          i = 0
          j = 0

          while i < left.size && j < right.size
            if comparator.call(left[i], right[j]) <= 0
              result << left[i]
              i += 1
            else
              result << right[j]
              j += 1
            end
          end

          result.concat(left[i..])
          result.concat(right[j..])
          result
        end
      end
    end
  end
end

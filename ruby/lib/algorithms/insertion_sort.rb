# frozen_string_literal: true

module DSA
  module Algorithms
    # Insertion sort algorithm.
    # Time: O(n²) worst/average, O(n) best (already sorted)
    # Space: O(1)
    # Stable: Yes
    module InsertionSort
      class << self
        # Sort an array in-place.
        def sort!(arr, &comparator)
          comparator ||= ->(a, b) { a <=> b }

          (1...arr.size).each do |i|
            key = arr[i]
            j = i - 1

            while j >= 0 && comparator.call(arr[j], key) > 0
              arr[j + 1] = arr[j]
              j -= 1
            end
            arr[j + 1] = key
          end

          arr
        end

        # Return a sorted copy of the array.
        def sort(arr, &comparator)
          sort!(arr.dup, &comparator)
        end
      end
    end
  end
end

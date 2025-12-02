# frozen_string_literal: true

module DSA
  module Algorithms
    # Binary search algorithms.
    # Time: O(log n)
    # Space: O(1)
    module BinarySearch
      class << self
        # Find the index of target in a sorted array.
        # Returns -1 if not found.
        def search(arr, target, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          left = 0
          right = arr.size - 1

          while left <= right
            mid = left + (right - left) / 2
            cmp = comparator.call(arr[mid], target)

            if cmp.zero?
              return mid
            elsif cmp < 0
              left = mid + 1
            else
              right = mid - 1
            end
          end

          -1
        end

        # Find the leftmost index where target could be inserted to maintain sorted order.
        # Returns the index of the first element >= target.
        def lower_bound(arr, target, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          left = 0
          right = arr.size

          while left < right
            mid = left + (right - left) / 2
            if comparator.call(arr[mid], target) < 0
              left = mid + 1
            else
              right = mid
            end
          end

          left
        end

        # Find the rightmost index where target could be inserted to maintain sorted order.
        # Returns the index of the first element > target.
        def upper_bound(arr, target, &comparator)
          comparator ||= ->(a, b) { a <=> b }
          left = 0
          right = arr.size

          while left < right
            mid = left + (right - left) / 2
            if comparator.call(arr[mid], target) <= 0
              left = mid + 1
            else
              right = mid
            end
          end

          left
        end
      end
    end
  end
end

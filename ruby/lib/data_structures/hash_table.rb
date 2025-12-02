# frozen_string_literal: true

module DSA
  module DataStructures
    # Hash table implementation with open addressing and linear probing.
    # Time: O(1) average for all operations
    # Space: O(n)
    class HashTable
      Entry = Struct.new(:key, :value, :deleted)

      LOAD_FACTOR = 0.7
      DEFAULT_CAPACITY = 16

      def initialize(capacity = DEFAULT_CAPACITY)
        @capacity = [1, capacity].max
        @buckets = Array.new(@capacity)
        @size = 0
      end

      def put(key, value)
        resize(@capacity * 2) if @size >= @capacity * LOAD_FACTOR

        index = hash(key)
        first_deleted = -1

        @capacity.times do
          entry = @buckets[index]

          if entry.nil?
            insert_index = first_deleted != -1 ? first_deleted : index
            @buckets[insert_index] = Entry.new(key, value, false)
            @size += 1
            return self
          end

          if entry.deleted && first_deleted == -1
            first_deleted = index
          elsif !entry.deleted && entry.key == key
            entry.value = value
            return self
          end

          index = (index + 1) % @capacity
        end

        if first_deleted != -1
          @buckets[first_deleted] = Entry.new(key, value, false)
          @size += 1
        end

        self
      end

      def get(key)
        index = find_index(key)
        index == -1 ? nil : @buckets[index].value
      end

      def remove(key)
        index = find_index(key)
        return nil if index == -1

        entry = @buckets[index]
        value = entry.value
        entry.deleted = true
        @size -= 1
        value
      end

      def contains?(key)
        find_index(key) != -1
      end

      def empty?
        @size.zero?
      end

      def size
        @size
      end

      def keys
        @buckets.compact.reject(&:deleted).map(&:key)
      end

      def values
        @buckets.compact.reject(&:deleted).map(&:value)
      end

      def clear
        @buckets = Array.new(@capacity)
        @size = 0
        self
      end

      private

      def hash(key)
        key.hash.abs % @capacity
      end

      def find_index(key)
        index = hash(key)

        @capacity.times do
          entry = @buckets[index]
          return -1 if entry.nil?
          return index if !entry.deleted && entry.key == key

          index = (index + 1) % @capacity
        end

        -1
      end

      def resize(new_capacity)
        old_buckets = @buckets
        @capacity = new_capacity
        @buckets = Array.new(@capacity)
        @size = 0

        old_buckets.each do |entry|
          put(entry.key, entry.value) if entry && !entry.deleted
        end
      end
    end
  end
end

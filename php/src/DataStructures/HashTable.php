<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Hash table implementation with open addressing and linear probing.
 * Time: O(1) average for all operations
 * Space: O(n)
 *
 * @template K
 * @template V
 */
class HashTable
{
    private array $buckets;
    private int $size = 0;
    private int $capacity;
    private const LOAD_FACTOR_THRESHOLD = 0.7;

    public function __construct(int $initialCapacity = 16)
    {
        $this->capacity = max(1, $initialCapacity);
        $this->buckets = array_fill(0, $this->capacity, null);
    }

    public function put(mixed $key, mixed $value): void
    {
        if ($this->size >= $this->capacity * self::LOAD_FACTOR_THRESHOLD) {
            $this->resize($this->capacity * 2);
        }

        $index = $this->hash($key);
        $firstDeleted = -1;

        for ($i = 0; $i < $this->capacity; $i++) {
            $entry = $this->buckets[$index];

            if ($entry === null) {
                $insertIndex = $firstDeleted !== -1 ? $firstDeleted : $index;
                $this->buckets[$insertIndex] = new HashEntry($key, $value);
                $this->size++;
                return;
            }

            if ($entry->deleted && $firstDeleted === -1) {
                $firstDeleted = $index;
            } elseif (!$entry->deleted && $entry->key === $key) {
                $entry->value = $value;
                return;
            }

            $index = ($index + 1) % $this->capacity;
        }

        if ($firstDeleted !== -1) {
            $this->buckets[$firstDeleted] = new HashEntry($key, $value);
            $this->size++;
        }
    }

    public function get(mixed $key): mixed
    {
        $index = $this->findIndex($key);
        return $index === -1 ? null : $this->buckets[$index]->value;
    }

    public function remove(mixed $key): mixed
    {
        $index = $this->findIndex($key);
        if ($index === -1) {
            return null;
        }
        $entry = $this->buckets[$index];
        $value = $entry->value;
        $entry->deleted = true;
        $this->size--;
        return $value;
    }

    public function contains(mixed $key): bool
    {
        return $this->findIndex($key) !== -1;
    }

    public function isEmpty(): bool
    {
        return $this->size === 0;
    }

    public function size(): int
    {
        return $this->size;
    }

    public function keys(): array
    {
        $result = [];
        foreach ($this->buckets as $entry) {
            if ($entry !== null && !$entry->deleted) {
                $result[] = $entry->key;
            }
        }
        return $result;
    }

    public function values(): array
    {
        $result = [];
        foreach ($this->buckets as $entry) {
            if ($entry !== null && !$entry->deleted) {
                $result[] = $entry->value;
            }
        }
        return $result;
    }

    public function clear(): void
    {
        $this->buckets = array_fill(0, $this->capacity, null);
        $this->size = 0;
    }

    private function hash(mixed $key): int
    {
        return abs(crc32((string)$key)) % $this->capacity;
    }

    private function findIndex(mixed $key): int
    {
        $index = $this->hash($key);

        for ($i = 0; $i < $this->capacity; $i++) {
            $entry = $this->buckets[$index];

            if ($entry === null) {
                return -1;
            }

            if (!$entry->deleted && $entry->key === $key) {
                return $index;
            }

            $index = ($index + 1) % $this->capacity;
        }

        return -1;
    }

    private function resize(int $newCapacity): void
    {
        $oldBuckets = $this->buckets;
        $this->capacity = $newCapacity;
        $this->buckets = array_fill(0, $this->capacity, null);
        $this->size = 0;

        foreach ($oldBuckets as $entry) {
            if ($entry !== null && !$entry->deleted) {
                $this->put($entry->key, $entry->value);
            }
        }
    }
}

/**
 * @internal
 */
class HashEntry
{
    public function __construct(
        public mixed $key,
        public mixed $value,
        public bool $deleted = false
    ) {}
}

<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Dynamic array implementation with automatic resizing.
 * Time: O(1) amortized push, O(n) insert/remove
 * Space: O(n)
 *
 * @template T
 */
class DynamicArray
{
    private array $data;
    private int $size = 0;
    private int $capacity;

    public function __construct(int $initialCapacity = 8)
    {
        $this->capacity = max(1, $initialCapacity);
        $this->data = array_fill(0, $this->capacity, null);
    }

    public function push(mixed $value): void
    {
        if ($this->size === $this->capacity) {
            $this->resize($this->capacity * 2);
        }
        $this->data[$this->size++] = $value;
    }

    public function pop(): mixed
    {
        if ($this->size === 0) {
            return null;
        }
        $value = $this->data[--$this->size];
        $this->data[$this->size] = null;
        if ($this->size > 0 && $this->size === intdiv($this->capacity, 4)) {
            $this->resize(intdiv($this->capacity, 2));
        }
        return $value;
    }

    public function get(int $index): mixed
    {
        if ($index < 0 || $index >= $this->size) {
            return null;
        }
        return $this->data[$index];
    }

    public function set(int $index, mixed $value): bool
    {
        if ($index < 0 || $index >= $this->size) {
            return false;
        }
        $this->data[$index] = $value;
        return true;
    }

    public function insert(int $index, mixed $value): bool
    {
        if ($index < 0 || $index > $this->size) {
            return false;
        }
        if ($this->size === $this->capacity) {
            $this->resize($this->capacity * 2);
        }
        for ($i = $this->size; $i > $index; $i--) {
            $this->data[$i] = $this->data[$i - 1];
        }
        $this->data[$index] = $value;
        $this->size++;
        return true;
    }

    public function remove(int $index): mixed
    {
        if ($index < 0 || $index >= $this->size) {
            return null;
        }
        $value = $this->data[$index];
        for ($i = $index; $i < $this->size - 1; $i++) {
            $this->data[$i] = $this->data[$i + 1];
        }
        $this->data[--$this->size] = null;
        if ($this->size > 0 && $this->size === intdiv($this->capacity, 4)) {
            $this->resize(intdiv($this->capacity, 2));
        }
        return $value;
    }

    public function indexOf(mixed $value): int
    {
        for ($i = 0; $i < $this->size; $i++) {
            if ($this->data[$i] === $value) {
                return $i;
            }
        }
        return -1;
    }

    public function contains(mixed $value): bool
    {
        return $this->indexOf($value) !== -1;
    }

    public function isEmpty(): bool
    {
        return $this->size === 0;
    }

    public function size(): int
    {
        return $this->size;
    }

    public function capacity(): int
    {
        return $this->capacity;
    }

    public function toArray(): array
    {
        return array_slice($this->data, 0, $this->size);
    }

    public function clear(): void
    {
        $this->data = array_fill(0, $this->capacity, null);
        $this->size = 0;
    }

    private function resize(int $newCapacity): void
    {
        $newData = array_fill(0, $newCapacity, null);
        for ($i = 0; $i < $this->size; $i++) {
            $newData[$i] = $this->data[$i];
        }
        $this->data = $newData;
        $this->capacity = $newCapacity;
    }
}

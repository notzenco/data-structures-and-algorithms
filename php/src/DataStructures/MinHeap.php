<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Binary min-heap implementation.
 * Time: O(log n) insert/extract, O(1) peek
 * Space: O(n)
 *
 * @template T
 */
class MinHeap
{
    private array $heap = [];
    /** @var callable */
    private $comparator;

    public function __construct(?callable $comparator = null)
    {
        $this->comparator = $comparator ?? fn($a, $b) => $a <=> $b;
    }

    public function insert(mixed $value): void
    {
        $this->heap[] = $value;
        $this->siftUp(count($this->heap) - 1);
    }

    public function extractMin(): mixed
    {
        if (empty($this->heap)) {
            return null;
        }

        $min = $this->heap[0];
        $last = array_pop($this->heap);

        if (!empty($this->heap)) {
            $this->heap[0] = $last;
            $this->siftDown(0);
        }

        return $min;
    }

    public function peek(): mixed
    {
        return $this->heap[0] ?? null;
    }

    public function isEmpty(): bool
    {
        return empty($this->heap);
    }

    public function size(): int
    {
        return count($this->heap);
    }

    public function clear(): void
    {
        $this->heap = [];
    }

    public function toArray(): array
    {
        return $this->heap;
    }

    private function siftUp(int $index): void
    {
        while ($index > 0) {
            $parentIndex = intdiv($index - 1, 2);
            if (($this->comparator)($this->heap[$index], $this->heap[$parentIndex]) >= 0) {
                break;
            }
            $this->swap($index, $parentIndex);
            $index = $parentIndex;
        }
    }

    private function siftDown(int $index): void
    {
        $length = count($this->heap);

        while (true) {
            $leftChild = 2 * $index + 1;
            $rightChild = 2 * $index + 2;
            $smallest = $index;

            if ($leftChild < $length &&
                ($this->comparator)($this->heap[$leftChild], $this->heap[$smallest]) < 0) {
                $smallest = $leftChild;
            }

            if ($rightChild < $length &&
                ($this->comparator)($this->heap[$rightChild], $this->heap[$smallest]) < 0) {
                $smallest = $rightChild;
            }

            if ($smallest === $index) {
                break;
            }

            $this->swap($index, $smallest);
            $index = $smallest;
        }
    }

    private function swap(int $i, int $j): void
    {
        [$this->heap[$i], $this->heap[$j]] = [$this->heap[$j], $this->heap[$i]];
    }
}

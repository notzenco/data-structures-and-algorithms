<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Queue implementation using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 *
 * @template T
 */
class Queue
{
    private ?QueueNode $head = null;
    private ?QueueNode $tail = null;
    private int $size = 0;

    public function enqueue(mixed $value): void
    {
        $node = new QueueNode($value);
        if ($this->tail !== null) {
            $this->tail->next = $node;
        } else {
            $this->head = $node;
        }
        $this->tail = $node;
        $this->size++;
    }

    public function dequeue(): mixed
    {
        if ($this->head === null) {
            return null;
        }
        $value = $this->head->value;
        $this->head = $this->head->next;
        if ($this->head === null) {
            $this->tail = null;
        }
        $this->size--;
        return $value;
    }

    public function peek(): mixed
    {
        return $this->head?->value;
    }

    public function isEmpty(): bool
    {
        return $this->size === 0;
    }

    public function size(): int
    {
        return $this->size;
    }

    public function clear(): void
    {
        $this->head = null;
        $this->tail = null;
        $this->size = 0;
    }
}

/**
 * @internal
 */
class QueueNode
{
    public function __construct(
        public mixed $value,
        public ?QueueNode $next = null
    ) {}
}

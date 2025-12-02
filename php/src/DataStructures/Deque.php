<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Double-ended queue implementation using a doubly linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 *
 * @template T
 */
class Deque
{
    private ?DequeNode $head = null;
    private ?DequeNode $tail = null;
    private int $size = 0;

    public function pushFront(mixed $value): void
    {
        $node = new DequeNode($value, next: $this->head);
        if ($this->head !== null) {
            $this->head->prev = $node;
        } else {
            $this->tail = $node;
        }
        $this->head = $node;
        $this->size++;
    }

    public function pushBack(mixed $value): void
    {
        $node = new DequeNode($value, prev: $this->tail);
        if ($this->tail !== null) {
            $this->tail->next = $node;
        } else {
            $this->head = $node;
        }
        $this->tail = $node;
        $this->size++;
    }

    public function popFront(): mixed
    {
        if ($this->head === null) {
            return null;
        }
        $value = $this->head->value;
        $this->head = $this->head->next;
        if ($this->head !== null) {
            $this->head->prev = null;
        } else {
            $this->tail = null;
        }
        $this->size--;
        return $value;
    }

    public function popBack(): mixed
    {
        if ($this->tail === null) {
            return null;
        }
        $value = $this->tail->value;
        $this->tail = $this->tail->prev;
        if ($this->tail !== null) {
            $this->tail->next = null;
        } else {
            $this->head = null;
        }
        $this->size--;
        return $value;
    }

    public function peekFront(): mixed
    {
        return $this->head?->value;
    }

    public function peekBack(): mixed
    {
        return $this->tail?->value;
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
class DequeNode
{
    public function __construct(
        public mixed $value,
        public ?DequeNode $prev = null,
        public ?DequeNode $next = null
    ) {}
}

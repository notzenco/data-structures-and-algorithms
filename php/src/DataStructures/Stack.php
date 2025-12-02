<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Stack implementation using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 *
 * @template T
 */
class Stack
{
    private ?StackNode $head = null;
    private int $size = 0;

    public function push(mixed $value): void
    {
        $this->head = new StackNode($value, $this->head);
        $this->size++;
    }

    public function pop(): mixed
    {
        if ($this->head === null) {
            return null;
        }
        $value = $this->head->value;
        $this->head = $this->head->next;
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
        $this->size = 0;
    }
}

/**
 * @internal
 */
class StackNode
{
    public function __construct(
        public mixed $value,
        public ?StackNode $next = null
    ) {}
}

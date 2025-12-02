<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Doubly linked list implementation.
 * Time: O(1) for front/back ops, O(n) for search
 * Space: O(n)
 *
 * @template T
 */
class DoublyLinkedList
{
    private ?DoublyNode $head = null;
    private ?DoublyNode $tail = null;
    private int $size = 0;

    public function pushFront(mixed $value): void
    {
        $node = new DoublyNode($value, next: $this->head);
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
        $node = new DoublyNode($value, prev: $this->tail);
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

    public function contains(mixed $value): bool
    {
        $current = $this->head;
        while ($current !== null) {
            if ($current->value === $value) {
                return true;
            }
            $current = $current->next;
        }
        return false;
    }

    public function remove(mixed $value): bool
    {
        $current = $this->head;
        while ($current !== null) {
            if ($current->value === $value) {
                if ($current->prev !== null) {
                    $current->prev->next = $current->next;
                } else {
                    $this->head = $current->next;
                }
                if ($current->next !== null) {
                    $current->next->prev = $current->prev;
                } else {
                    $this->tail = $current->prev;
                }
                $this->size--;
                return true;
            }
            $current = $current->next;
        }
        return false;
    }

    public function isEmpty(): bool
    {
        return $this->size === 0;
    }

    public function size(): int
    {
        return $this->size;
    }

    public function toArray(): array
    {
        $result = [];
        $current = $this->head;
        while ($current !== null) {
            $result[] = $current->value;
            $current = $current->next;
        }
        return $result;
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
class DoublyNode
{
    public function __construct(
        public mixed $value,
        public ?DoublyNode $prev = null,
        public ?DoublyNode $next = null
    ) {}
}

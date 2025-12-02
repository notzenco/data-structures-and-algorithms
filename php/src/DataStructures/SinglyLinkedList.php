<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Singly linked list implementation.
 * Time: O(1) front ops, O(n) back ops and search
 * Space: O(n)
 *
 * @template T
 */
class SinglyLinkedList
{
    private ?SinglyNode $head = null;
    private ?SinglyNode $tail = null;
    private int $size = 0;

    public function pushFront(mixed $value): void
    {
        $node = new SinglyNode($value, $this->head);
        $this->head = $node;
        $this->tail ??= $node;
        $this->size++;
    }

    public function pushBack(mixed $value): void
    {
        $node = new SinglyNode($value);
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
        if ($this->head === null) {
            $this->tail = null;
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
        if ($this->head === null) {
            return false;
        }
        if ($this->head->value === $value) {
            $this->head = $this->head->next;
            if ($this->head === null) {
                $this->tail = null;
            }
            $this->size--;
            return true;
        }
        $current = $this->head;
        while ($current->next !== null) {
            if ($current->next->value === $value) {
                if ($current->next === $this->tail) {
                    $this->tail = $current;
                }
                $current->next = $current->next->next;
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
class SinglyNode
{
    public function __construct(
        public mixed $value,
        public ?SinglyNode $next = null
    ) {}
}

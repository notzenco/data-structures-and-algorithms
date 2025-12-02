<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Binary Search Tree implementation.
 * Time: O(log n) average, O(n) worst for all operations
 * Space: O(n)
 *
 * @template T
 */
class BinarySearchTree
{
    private ?BSTNode $root = null;
    private int $size = 0;
    /** @var callable|null */
    private $comparator;

    public function __construct(?callable $comparator = null)
    {
        $this->comparator = $comparator ?? fn($a, $b) => $a <=> $b;
    }

    public function insert(mixed $value): void
    {
        $this->root = $this->insertNode($this->root, $value);
        $this->size++;
    }

    private function insertNode(?BSTNode $node, mixed $value): BSTNode
    {
        if ($node === null) {
            return new BSTNode($value);
        }

        $cmp = ($this->comparator)($value, $node->value);
        if ($cmp < 0) {
            $node->left = $this->insertNode($node->left, $value);
        } elseif ($cmp > 0) {
            $node->right = $this->insertNode($node->right, $value);
        }
        return $node;
    }

    public function contains(mixed $value): bool
    {
        return $this->findNode($this->root, $value) !== null;
    }

    private function findNode(?BSTNode $node, mixed $value): ?BSTNode
    {
        if ($node === null) {
            return null;
        }

        $cmp = ($this->comparator)($value, $node->value);
        if ($cmp < 0) {
            return $this->findNode($node->left, $value);
        } elseif ($cmp > 0) {
            return $this->findNode($node->right, $value);
        }
        return $node;
    }

    public function remove(mixed $value): bool
    {
        $sizeBefore = $this->size;
        $this->root = $this->removeNode($this->root, $value);
        return $this->size < $sizeBefore;
    }

    private function removeNode(?BSTNode $node, mixed $value): ?BSTNode
    {
        if ($node === null) {
            return null;
        }

        $cmp = ($this->comparator)($value, $node->value);
        if ($cmp < 0) {
            $node->left = $this->removeNode($node->left, $value);
        } elseif ($cmp > 0) {
            $node->right = $this->removeNode($node->right, $value);
        } else {
            $this->size--;

            if ($node->left === null) {
                return $node->right;
            }
            if ($node->right === null) {
                return $node->left;
            }

            $minRight = $this->findMinNode($node->right);
            $node->value = $minRight->value;
            $node->right = $this->removeNode($node->right, $minRight->value);
            $this->size++;
        }
        return $node;
    }

    private function findMinNode(BSTNode $node): BSTNode
    {
        while ($node->left !== null) {
            $node = $node->left;
        }
        return $node;
    }

    private function findMaxNode(BSTNode $node): BSTNode
    {
        while ($node->right !== null) {
            $node = $node->right;
        }
        return $node;
    }

    public function min(): mixed
    {
        return $this->root === null ? null : $this->findMinNode($this->root)->value;
    }

    public function max(): mixed
    {
        return $this->root === null ? null : $this->findMaxNode($this->root)->value;
    }

    public function inOrder(): array
    {
        $result = [];
        $this->inOrderTraverse($this->root, $result);
        return $result;
    }

    private function inOrderTraverse(?BSTNode $node, array &$result): void
    {
        if ($node === null) return;
        $this->inOrderTraverse($node->left, $result);
        $result[] = $node->value;
        $this->inOrderTraverse($node->right, $result);
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
        $this->root = null;
        $this->size = 0;
    }
}

/**
 * @internal
 */
class BSTNode
{
    public function __construct(
        public mixed $value,
        public ?BSTNode $left = null,
        public ?BSTNode $right = null
    ) {}
}

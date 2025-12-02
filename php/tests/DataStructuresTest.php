<?php

declare(strict_types=1);

namespace DSA\Tests;

use PHPUnit\Framework\TestCase;
use DSA\DataStructures\Stack;
use DSA\DataStructures\Queue;
use DSA\DataStructures\DynamicArray;
use DSA\DataStructures\SinglyLinkedList;
use DSA\DataStructures\DoublyLinkedList;
use DSA\DataStructures\Deque;
use DSA\DataStructures\HashTable;
use DSA\DataStructures\BinarySearchTree;
use DSA\DataStructures\MinHeap;
use DSA\DataStructures\DisjointSet;

class DataStructuresTest extends TestCase
{
    // Stack Tests
    public function testStackOperations(): void
    {
        $stack = new Stack();
        $this->assertTrue($stack->isEmpty());

        $stack->push(1);
        $stack->push(2);
        $stack->push(3);

        $this->assertEquals(3, $stack->size());
        $this->assertEquals(3, $stack->peek());
        $this->assertEquals(3, $stack->pop());
        $this->assertEquals(2, $stack->pop());
        $this->assertEquals(1, $stack->size());

        $stack->clear();
        $this->assertTrue($stack->isEmpty());
    }

    // Queue Tests
    public function testQueueOperations(): void
    {
        $queue = new Queue();
        $this->assertTrue($queue->isEmpty());

        $queue->enqueue(1);
        $queue->enqueue(2);
        $queue->enqueue(3);

        $this->assertEquals(3, $queue->size());
        $this->assertEquals(1, $queue->peek());
        $this->assertEquals(1, $queue->dequeue());
        $this->assertEquals(2, $queue->dequeue());
        $this->assertEquals(1, $queue->size());
    }

    // DynamicArray Tests
    public function testDynamicArrayOperations(): void
    {
        $arr = new DynamicArray();
        $this->assertTrue($arr->isEmpty());

        $arr->push(10);
        $arr->push(20);
        $arr->push(30);

        $this->assertEquals(3, $arr->size());
        $this->assertEquals(10, $arr->get(0));
        $this->assertEquals(30, $arr->get(2));

        $arr->set(1, 25);
        $this->assertEquals(25, $arr->get(1));

        $this->assertEquals(30, $arr->pop());
        $this->assertEquals(2, $arr->size());

        $arr->insert(1, 15);
        $this->assertEquals([10, 15, 25], $arr->toArray());

        $arr->removeAt(1);
        $this->assertEquals([10, 25], $arr->toArray());
    }

    // SinglyLinkedList Tests
    public function testSinglyLinkedListOperations(): void
    {
        $list = new SinglyLinkedList();
        $this->assertTrue($list->isEmpty());

        $list->append(1);
        $list->append(2);
        $list->prepend(0);

        $this->assertEquals(3, $list->size());
        $this->assertEquals(0, $list->get(0));
        $this->assertEquals(2, $list->get(2));

        $list->insert(1, 10);
        $this->assertEquals([0, 10, 1, 2], $list->toArray());

        $list->removeAt(1);
        $this->assertEquals([0, 1, 2], $list->toArray());

        $this->assertTrue($list->contains(1));
        $this->assertFalse($list->contains(100));
    }

    // DoublyLinkedList Tests
    public function testDoublyLinkedListOperations(): void
    {
        $list = new DoublyLinkedList();
        $this->assertTrue($list->isEmpty());

        $list->append(1);
        $list->append(2);
        $list->prepend(0);

        $this->assertEquals(3, $list->size());
        $this->assertEquals(0, $list->getFirst());
        $this->assertEquals(2, $list->getLast());

        $this->assertEquals(0, $list->removeFirst());
        $this->assertEquals(2, $list->removeLast());
        $this->assertEquals(1, $list->size());
    }

    // Deque Tests
    public function testDequeOperations(): void
    {
        $deque = new Deque();
        $this->assertTrue($deque->isEmpty());

        $deque->pushBack(2);
        $deque->pushFront(1);
        $deque->pushBack(3);

        $this->assertEquals(3, $deque->size());
        $this->assertEquals(1, $deque->peekFront());
        $this->assertEquals(3, $deque->peekBack());

        $this->assertEquals(1, $deque->popFront());
        $this->assertEquals(3, $deque->popBack());
        $this->assertEquals(1, $deque->size());
    }

    // HashTable Tests
    public function testHashTableOperations(): void
    {
        $table = new HashTable();
        $this->assertTrue($table->isEmpty());

        $table->put('one', 1);
        $table->put('two', 2);
        $table->put('three', 3);

        $this->assertEquals(3, $table->size());
        $this->assertEquals(1, $table->get('one'));
        $this->assertEquals(2, $table->get('two'));
        $this->assertNull($table->get('four'));

        $this->assertTrue($table->contains('one'));
        $this->assertFalse($table->contains('four'));

        $table->put('one', 100);
        $this->assertEquals(100, $table->get('one'));

        $this->assertEquals(2, $table->remove('two'));
        $this->assertEquals(2, $table->size());
        $this->assertFalse($table->contains('two'));
    }

    // BinarySearchTree Tests
    public function testBinarySearchTreeOperations(): void
    {
        $bst = new BinarySearchTree();
        $this->assertTrue($bst->isEmpty());

        $bst->insert(5);
        $bst->insert(3);
        $bst->insert(7);
        $bst->insert(1);
        $bst->insert(9);

        $this->assertEquals(5, $bst->size());
        $this->assertTrue($bst->contains(5));
        $this->assertTrue($bst->contains(3));
        $this->assertFalse($bst->contains(100));

        $this->assertEquals(1, $bst->min());
        $this->assertEquals(9, $bst->max());

        $this->assertEquals([1, 3, 5, 7, 9], $bst->inOrder());

        $this->assertTrue($bst->remove(3));
        $this->assertFalse($bst->contains(3));
        $this->assertEquals([1, 5, 7, 9], $bst->inOrder());
    }

    // MinHeap Tests
    public function testMinHeapOperations(): void
    {
        $heap = new MinHeap();
        $this->assertTrue($heap->isEmpty());

        $heap->insert(5);
        $heap->insert(3);
        $heap->insert(7);
        $heap->insert(1);
        $heap->insert(9);

        $this->assertEquals(5, $heap->size());
        $this->assertEquals(1, $heap->peek());

        $this->assertEquals(1, $heap->extractMin());
        $this->assertEquals(3, $heap->extractMin());
        $this->assertEquals(5, $heap->extractMin());
        $this->assertEquals(7, $heap->extractMin());
        $this->assertEquals(9, $heap->extractMin());

        $this->assertTrue($heap->isEmpty());
    }

    public function testMinHeapWithCustomComparator(): void
    {
        // Max heap using custom comparator
        $heap = new MinHeap(fn($a, $b) => $b <=> $a);

        $heap->insert(5);
        $heap->insert(3);
        $heap->insert(7);

        $this->assertEquals(7, $heap->extractMin());
        $this->assertEquals(5, $heap->extractMin());
        $this->assertEquals(3, $heap->extractMin());
    }

    // DisjointSet Tests
    public function testDisjointSetOperations(): void
    {
        $ds = new DisjointSet(5);

        $this->assertEquals(5, $ds->count());
        $this->assertFalse($ds->connected(0, 1));

        $ds->union(0, 1);
        $this->assertTrue($ds->connected(0, 1));
        $this->assertEquals(4, $ds->count());

        $ds->union(2, 3);
        $ds->union(0, 2);

        $this->assertTrue($ds->connected(0, 3));
        $this->assertTrue($ds->connected(1, 2));
        $this->assertEquals(2, $ds->count());

        $this->assertFalse($ds->connected(0, 4));
    }
}

import {
  Stack,
  Queue,
  DynamicArray,
  SinglyLinkedList,
  DoublyLinkedList,
  Deque,
  HashTable,
  BinarySearchTree,
  MinHeap,
  DisjointSet,
} from '../src/data-structures';

describe('Stack', () => {
  test('basic operations', () => {
    const stack = new Stack<number>();
    expect(stack.isEmpty()).toBe(true);
    expect(stack.size()).toBe(0);

    stack.push(1);
    stack.push(2);
    stack.push(3);

    expect(stack.size()).toBe(3);
    expect(stack.isEmpty()).toBe(false);
    expect(stack.peek()).toBe(3);
    expect(stack.pop()).toBe(3);
    expect(stack.pop()).toBe(2);
    expect(stack.size()).toBe(1);
  });

  test('empty operations', () => {
    const stack = new Stack<string>();
    expect(stack.pop()).toBeUndefined();
    expect(stack.peek()).toBeUndefined();
  });
});

describe('Queue', () => {
  test('basic operations', () => {
    const queue = new Queue<number>();
    expect(queue.isEmpty()).toBe(true);

    queue.enqueue(1);
    queue.enqueue(2);
    queue.enqueue(3);

    expect(queue.size()).toBe(3);
    expect(queue.peek()).toBe(1);
    expect(queue.dequeue()).toBe(1);
    expect(queue.dequeue()).toBe(2);
  });

  test('empty operations', () => {
    const queue = new Queue<string>();
    expect(queue.dequeue()).toBeUndefined();
    expect(queue.peek()).toBeUndefined();
  });
});

describe('DynamicArray', () => {
  test('basic operations', () => {
    const arr = new DynamicArray<number>();
    expect(arr.isEmpty()).toBe(true);

    arr.push(10);
    arr.push(20);
    arr.push(30);

    expect(arr.size()).toBe(3);
    expect(arr.get(0)).toBe(10);
    expect(arr.get(2)).toBe(30);
  });

  test('set and remove', () => {
    const arr = new DynamicArray<string>();
    arr.push('a');
    arr.push('b');
    arr.push('c');

    arr.set(1, 'x');
    expect(arr.get(1)).toBe('x');

    expect(arr.remove(1)).toBe('x');
    expect(arr.size()).toBe(2);
    expect(arr.get(1)).toBe('c');
  });

  test('growth', () => {
    const arr = new DynamicArray<number>(2);
    for (let i = 0; i < 100; i++) {
      arr.push(i);
    }
    expect(arr.size()).toBe(100);
    expect(arr.get(99)).toBe(99);
  });
});

describe('SinglyLinkedList', () => {
  test('basic operations', () => {
    const list = new SinglyLinkedList<number>();
    expect(list.isEmpty()).toBe(true);

    list.pushFront(1);
    list.pushFront(2);
    list.pushBack(3);

    expect(list.size()).toBe(3);
    expect(list.peekFront()).toBe(2);
    expect(list.peekBack()).toBe(3);
  });

  test('find', () => {
    const list = new SinglyLinkedList<string>();
    list.pushBack('a');
    list.pushBack('b');
    list.pushBack('c');

    expect(list.contains('b')).toBe(true);
    expect(list.contains('d')).toBe(false);
  });
});

describe('DoublyLinkedList', () => {
  test('basic operations', () => {
    const list = new DoublyLinkedList<number>();
    expect(list.isEmpty()).toBe(true);

    list.pushFront(1);
    list.pushFront(2);
    list.pushBack(3);

    expect(list.size()).toBe(3);
    expect(list.popFront()).toBe(2);
    expect(list.popBack()).toBe(3);
  });

  test('remove', () => {
    const list = new DoublyLinkedList<string>();
    list.pushBack('a');
    list.pushBack('b');
    list.pushBack('c');

    expect(list.remove('b')).toBe(true);
    expect(list.size()).toBe(2);
    expect(list.contains('b')).toBe(false);
  });
});

describe('Deque', () => {
  test('basic operations', () => {
    const deque = new Deque<number>();
    expect(deque.isEmpty()).toBe(true);

    deque.pushFront(1);
    deque.pushBack(2);
    deque.pushFront(0);

    expect(deque.size()).toBe(3);
    expect(deque.peekFront()).toBe(0);
    expect(deque.peekBack()).toBe(2);
  });

  test('mixed operations', () => {
    const deque = new Deque<string>();
    deque.pushBack('a');
    deque.pushBack('b');
    deque.pushFront('x');

    expect(deque.popFront()).toBe('x');
    expect(deque.popBack()).toBe('b');
    expect(deque.popFront()).toBe('a');
    expect(deque.isEmpty()).toBe(true);
  });
});

describe('HashTable', () => {
  test('basic operations', () => {
    const table = new HashTable<string, number>();
    expect(table.isEmpty()).toBe(true);

    table.put('one', 1);
    table.put('two', 2);
    table.put('three', 3);

    expect(table.size()).toBe(3);
    expect(table.get('one')).toBe(1);
    expect(table.get('two')).toBe(2);
    expect(table.has('three')).toBe(true);
  });

  test('update and remove', () => {
    const table = new HashTable<string, string>();
    table.put('key', 'value1');
    expect(table.get('key')).toBe('value1');

    table.put('key', 'value2');
    expect(table.get('key')).toBe('value2');
    expect(table.size()).toBe(1);

    expect(table.remove('key')).toBe('value2');
    expect(table.has('key')).toBe(false);
  });

  test('collision handling', () => {
    const table = new HashTable<number, string>(4);
    for (let i = 0; i < 20; i++) {
      table.put(i, `value${i}`);
    }
    expect(table.size()).toBe(20);
    for (let i = 0; i < 20; i++) {
      expect(table.get(i)).toBe(`value${i}`);
    }
  });
});

describe('BinarySearchTree', () => {
  test('basic operations', () => {
    const bst = new BinarySearchTree<number>();
    expect(bst.isEmpty()).toBe(true);

    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    expect(bst.size()).toBe(5);
    expect(bst.contains(3)).toBe(true);
    expect(bst.contains(7)).toBe(true);
    expect(bst.contains(4)).toBe(false);
  });

  test('min and max', () => {
    const bst = new BinarySearchTree<number>();
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    expect(bst.min()).toBe(1);
    expect(bst.max()).toBe(9);
  });

  test('inorder traversal', () => {
    const bst = new BinarySearchTree<number>();
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    const inorder = bst.inorder();
    expect(inorder).toEqual([1, 3, 5, 7, 9]);
  });

  test('remove', () => {
    const bst = new BinarySearchTree<number>();
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);

    expect(bst.remove(3)).toBe(true);
    expect(bst.contains(3)).toBe(false);
    expect(bst.size()).toBe(2);
  });
});

describe('MinHeap', () => {
  test('basic operations', () => {
    const heap = new MinHeap<number>();
    expect(heap.isEmpty()).toBe(true);

    heap.insert(5);
    heap.insert(3);
    heap.insert(7);
    heap.insert(1);

    expect(heap.size()).toBe(4);
    expect(heap.peek()).toBe(1);
    expect(heap.extractMin()).toBe(1);
    expect(heap.peek()).toBe(3);
  });

  test('maintains order', () => {
    const heap = new MinHeap<number>();
    const values = [9, 4, 7, 1, 8, 2, 6, 3, 5];
    for (const v of values) {
      heap.insert(v);
    }

    let prev = Number.MIN_SAFE_INTEGER;
    while (!heap.isEmpty()) {
      const current = heap.extractMin()!;
      expect(current).toBeGreaterThanOrEqual(prev);
      prev = current;
    }
  });
});

describe('DisjointSet', () => {
  test('basic operations', () => {
    const ds = new DisjointSet(5);

    expect(ds.connected(0, 1)).toBe(false);
    ds.union(0, 1);
    expect(ds.connected(0, 1)).toBe(true);
  });

  test('multiple unions', () => {
    const ds = new DisjointSet(6);

    ds.union(0, 1);
    ds.union(2, 3);
    ds.union(4, 5);

    expect(ds.connected(0, 1)).toBe(true);
    expect(ds.connected(2, 3)).toBe(true);
    expect(ds.connected(0, 2)).toBe(false);

    ds.union(1, 2);
    expect(ds.connected(0, 3)).toBe(true);
  });

  test('component count', () => {
    const ds = new DisjointSet(5);
    expect(ds.count()).toBe(5);

    ds.union(0, 1);
    expect(ds.count()).toBe(4);

    ds.union(2, 3);
    expect(ds.count()).toBe(3);

    ds.union(0, 2);
    expect(ds.count()).toBe(2);
  });
});

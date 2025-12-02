/**
 * Double-ended queue implementation using a doubly linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 */

interface DequeNode<T> {
  value: T;
  prev: DequeNode<T> | null;
  next: DequeNode<T> | null;
}

export class Deque<T> {
  private head: DequeNode<T> | null = null;
  private tail: DequeNode<T> | null = null;
  private _size: number = 0;

  pushFront(value: T): void {
    const node: DequeNode<T> = { value, prev: null, next: this.head };
    if (this.head) {
      this.head.prev = node;
    } else {
      this.tail = node;
    }
    this.head = node;
    this._size++;
  }

  pushBack(value: T): void {
    const node: DequeNode<T> = { value, prev: this.tail, next: null };
    if (this.tail) {
      this.tail.next = node;
    } else {
      this.head = node;
    }
    this.tail = node;
    this._size++;
  }

  popFront(): T | undefined {
    if (!this.head) {
      return undefined;
    }
    const value = this.head.value;
    this.head = this.head.next;
    if (this.head) {
      this.head.prev = null;
    } else {
      this.tail = null;
    }
    this._size--;
    return value;
  }

  popBack(): T | undefined {
    if (!this.tail) {
      return undefined;
    }
    const value = this.tail.value;
    this.tail = this.tail.prev;
    if (this.tail) {
      this.tail.next = null;
    } else {
      this.head = null;
    }
    this._size--;
    return value;
  }

  peekFront(): T | undefined {
    return this.head?.value;
  }

  peekBack(): T | undefined {
    return this.tail?.value;
  }

  isEmpty(): boolean {
    return this._size === 0;
  }

  size(): number {
    return this._size;
  }

  clear(): void {
    this.head = null;
    this.tail = null;
    this._size = 0;
  }
}

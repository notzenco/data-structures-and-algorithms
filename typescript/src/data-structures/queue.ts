/**
 * Queue implementation using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 */

interface QueueNode<T> {
  value: T;
  next: QueueNode<T> | null;
}

export class Queue<T> {
  private head: QueueNode<T> | null = null;
  private tail: QueueNode<T> | null = null;
  private _size: number = 0;

  enqueue(value: T): void {
    const node: QueueNode<T> = { value, next: null };
    if (this.tail) {
      this.tail.next = node;
    } else {
      this.head = node;
    }
    this.tail = node;
    this._size++;
  }

  dequeue(): T | undefined {
    if (!this.head) {
      return undefined;
    }
    const value = this.head.value;
    this.head = this.head.next;
    if (!this.head) {
      this.tail = null;
    }
    this._size--;
    return value;
  }

  peek(): T | undefined {
    return this.head?.value;
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

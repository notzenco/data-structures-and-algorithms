/**
 * Stack implementation using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 */

interface StackNode<T> {
  value: T;
  next: StackNode<T> | null;
}

export class Stack<T> {
  private head: StackNode<T> | null = null;
  private _size: number = 0;

  push(value: T): void {
    const node: StackNode<T> = { value, next: this.head };
    this.head = node;
    this._size++;
  }

  pop(): T | undefined {
    if (!this.head) {
      return undefined;
    }
    const value = this.head.value;
    this.head = this.head.next;
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
    this._size = 0;
  }
}

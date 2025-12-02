/**
 * Singly linked list implementation.
 * Time: O(1) front ops, O(n) back ops and search
 * Space: O(n)
 */

interface SinglyNode<T> {
  value: T;
  next: SinglyNode<T> | null;
}

export class SinglyLinkedList<T> {
  private head: SinglyNode<T> | null = null;
  private tail: SinglyNode<T> | null = null;
  private _size: number = 0;

  pushFront(value: T): void {
    const node: SinglyNode<T> = { value, next: this.head };
    this.head = node;
    if (!this.tail) {
      this.tail = node;
    }
    this._size++;
  }

  pushBack(value: T): void {
    const node: SinglyNode<T> = { value, next: null };
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
    if (!this.head) {
      this.tail = null;
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

  contains(value: T): boolean {
    let current = this.head;
    while (current) {
      if (current.value === value) {
        return true;
      }
      current = current.next;
    }
    return false;
  }

  remove(value: T): boolean {
    if (!this.head) {
      return false;
    }

    if (this.head.value === value) {
      this.head = this.head.next;
      if (!this.head) {
        this.tail = null;
      }
      this._size--;
      return true;
    }

    let current = this.head;
    while (current.next) {
      if (current.next.value === value) {
        if (current.next === this.tail) {
          this.tail = current;
        }
        current.next = current.next.next;
        this._size--;
        return true;
      }
      current = current.next;
    }
    return false;
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

  toArray(): T[] {
    const result: T[] = [];
    let current = this.head;
    while (current) {
      result.push(current.value);
      current = current.next;
    }
    return result;
  }
}

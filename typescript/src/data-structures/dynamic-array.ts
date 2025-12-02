/**
 * Dynamic array implementation with automatic resizing.
 * Time: O(1) amortized push, O(n) insert/remove
 * Space: O(n)
 */

export class DynamicArray<T> {
  private data: T[];
  private _size: number = 0;
  private _capacity: number;

  constructor(initialCapacity: number = 8) {
    this._capacity = Math.max(1, initialCapacity);
    this.data = new Array(this._capacity);
  }

  push(value: T): void {
    if (this._size === this._capacity) {
      this.resize(this._capacity * 2);
    }
    this.data[this._size++] = value;
  }

  pop(): T | undefined {
    if (this._size === 0) {
      return undefined;
    }
    const value = this.data[--this._size];
    if (this._size > 0 && this._size === Math.floor(this._capacity / 4)) {
      this.resize(Math.floor(this._capacity / 2));
    }
    return value;
  }

  get(index: number): T | undefined {
    if (index < 0 || index >= this._size) {
      return undefined;
    }
    return this.data[index];
  }

  set(index: number, value: T): boolean {
    if (index < 0 || index >= this._size) {
      return false;
    }
    this.data[index] = value;
    return true;
  }

  insert(index: number, value: T): boolean {
    if (index < 0 || index > this._size) {
      return false;
    }
    if (this._size === this._capacity) {
      this.resize(this._capacity * 2);
    }
    for (let i = this._size; i > index; i--) {
      this.data[i] = this.data[i - 1];
    }
    this.data[index] = value;
    this._size++;
    return true;
  }

  remove(index: number): T | undefined {
    if (index < 0 || index >= this._size) {
      return undefined;
    }
    const value = this.data[index];
    for (let i = index; i < this._size - 1; i++) {
      this.data[i] = this.data[i + 1];
    }
    this._size--;
    if (this._size > 0 && this._size === Math.floor(this._capacity / 4)) {
      this.resize(Math.floor(this._capacity / 2));
    }
    return value;
  }

  indexOf(value: T): number {
    for (let i = 0; i < this._size; i++) {
      if (this.data[i] === value) {
        return i;
      }
    }
    return -1;
  }

  contains(value: T): boolean {
    return this.indexOf(value) !== -1;
  }

  isEmpty(): boolean {
    return this._size === 0;
  }

  size(): number {
    return this._size;
  }

  capacity(): number {
    return this._capacity;
  }

  clear(): void {
    this.data = new Array(this._capacity);
    this._size = 0;
  }

  toArray(): T[] {
    return this.data.slice(0, this._size);
  }

  private resize(newCapacity: number): void {
    const newData = new Array(newCapacity);
    for (let i = 0; i < this._size; i++) {
      newData[i] = this.data[i];
    }
    this.data = newData;
    this._capacity = newCapacity;
  }
}

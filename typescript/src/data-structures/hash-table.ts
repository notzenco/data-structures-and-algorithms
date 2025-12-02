/**
 * Hash table implementation with open addressing and linear probing.
 * Time: O(1) average for all operations
 * Space: O(n)
 */

interface HashEntry<K, V> {
  key: K;
  value: V;
  deleted: boolean;
}

export class HashTable<K, V> {
  private buckets: (HashEntry<K, V> | null)[];
  private _size: number = 0;
  private _capacity: number;
  private readonly loadFactorThreshold = 0.7;

  constructor(initialCapacity: number = 16) {
    this._capacity = Math.max(1, initialCapacity);
    this.buckets = new Array(this._capacity).fill(null);
  }

  put(key: K, value: V): void {
    if (this._size >= this._capacity * this.loadFactorThreshold) {
      this.resize(this._capacity * 2);
    }

    let index = this.hash(key);
    let firstDeleted = -1;

    for (let i = 0; i < this._capacity; i++) {
      const entry = this.buckets[index];

      if (entry === null) {
        const insertIndex = firstDeleted !== -1 ? firstDeleted : index;
        this.buckets[insertIndex] = { key, value, deleted: false };
        this._size++;
        return;
      }

      if (entry.deleted && firstDeleted === -1) {
        firstDeleted = index;
      } else if (!entry.deleted && this.keysEqual(entry.key, key)) {
        entry.value = value;
        return;
      }

      index = (index + 1) % this._capacity;
    }

    if (firstDeleted !== -1) {
      this.buckets[firstDeleted] = { key, value, deleted: false };
      this._size++;
    }
  }

  get(key: K): V | undefined {
    const index = this.findIndex(key);
    if (index === -1) {
      return undefined;
    }
    return this.buckets[index]!.value;
  }

  remove(key: K): V | undefined {
    const index = this.findIndex(key);
    if (index === -1) {
      return undefined;
    }
    const entry = this.buckets[index]!;
    const value = entry.value;
    entry.deleted = true;
    this._size--;
    return value;
  }

  has(key: K): boolean {
    return this.findIndex(key) !== -1;
  }

  isEmpty(): boolean {
    return this._size === 0;
  }

  size(): number {
    return this._size;
  }

  clear(): void {
    this.buckets = new Array(this._capacity).fill(null);
    this._size = 0;
  }

  keys(): K[] {
    const result: K[] = [];
    for (const entry of this.buckets) {
      if (entry && !entry.deleted) {
        result.push(entry.key);
      }
    }
    return result;
  }

  values(): V[] {
    const result: V[] = [];
    for (const entry of this.buckets) {
      if (entry && !entry.deleted) {
        result.push(entry.value);
      }
    }
    return result;
  }

  private hash(key: K): number {
    const str = String(key);
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      hash = (hash * 31 + str.charCodeAt(i)) >>> 0;
    }
    return hash % this._capacity;
  }

  private keysEqual(a: K, b: K): boolean {
    return a === b;
  }

  private findIndex(key: K): number {
    let index = this.hash(key);

    for (let i = 0; i < this._capacity; i++) {
      const entry = this.buckets[index];

      if (entry === null) {
        return -1;
      }

      if (!entry.deleted && this.keysEqual(entry.key, key)) {
        return index;
      }

      index = (index + 1) % this._capacity;
    }

    return -1;
  }

  private resize(newCapacity: number): void {
    const oldBuckets = this.buckets;
    this._capacity = newCapacity;
    this.buckets = new Array(this._capacity).fill(null);
    this._size = 0;

    for (const entry of oldBuckets) {
      if (entry && !entry.deleted) {
        this.put(entry.key, entry.value);
      }
    }
  }
}

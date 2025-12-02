/**
 * Disjoint Set (Union-Find) implementation with path compression and union by rank.
 * Time: O(α(n)) amortized for all operations (nearly constant)
 * Space: O(n)
 */

export class DisjointSet {
  private parent: number[];
  private rank: number[];
  private _count: number;

  constructor(size: number) {
    this.parent = new Array(size);
    this.rank = new Array(size).fill(0);
    this._count = size;

    for (let i = 0; i < size; i++) {
      this.parent[i] = i;
    }
  }

  find(x: number): number {
    if (this.parent[x] !== x) {
      this.parent[x] = this.find(this.parent[x]); // Path compression
    }
    return this.parent[x];
  }

  union(x: number, y: number): boolean {
    const rootX = this.find(x);
    const rootY = this.find(y);

    if (rootX === rootY) {
      return false;
    }

    // Union by rank
    if (this.rank[rootX] < this.rank[rootY]) {
      this.parent[rootX] = rootY;
    } else if (this.rank[rootX] > this.rank[rootY]) {
      this.parent[rootY] = rootX;
    } else {
      this.parent[rootY] = rootX;
      this.rank[rootX]++;
    }

    this._count--;
    return true;
  }

  connected(x: number, y: number): boolean {
    return this.find(x) === this.find(y);
  }

  count(): number {
    return this._count;
  }

  size(): number {
    return this.parent.length;
  }
}

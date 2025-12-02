/**
 * Graph implementation using adjacency list.
 */

export class Graph<T> {
  private adjList: Map<T, T[]> = new Map();
  private readonly directed: boolean;

  constructor(directed: boolean = false) {
    this.directed = directed;
  }

  addVertex(vertex: T): void {
    if (!this.adjList.has(vertex)) {
      this.adjList.set(vertex, []);
    }
  }

  addEdge(from: T, to: T): void {
    this.addVertex(from);
    this.addVertex(to);
    this.adjList.get(from)!.push(to);
    if (!this.directed) {
      this.adjList.get(to)!.push(from);
    }
  }

  neighbors(vertex: T): T[] {
    return this.adjList.get(vertex) || [];
  }

  hasVertex(vertex: T): boolean {
    return this.adjList.has(vertex);
  }

  vertices(): T[] {
    return Array.from(this.adjList.keys());
  }

  size(): number {
    return this.adjList.size;
  }

  isDirected(): boolean {
    return this.directed;
  }
}

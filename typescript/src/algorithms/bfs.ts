/**
 * Breadth-First Search implementation.
 * Time: O(V + E)
 * Space: O(V)
 */

import { Graph } from './graph';

export function bfs<T>(graph: Graph<T>, start: T, callback?: (vertex: T) => void): T[] {
  const result: T[] = [];

  if (!graph.hasVertex(start)) {
    return result;
  }

  const visited = new Set<T>();
  const queue: T[] = [start];
  visited.add(start);

  while (queue.length > 0) {
    const current = queue.shift()!;
    result.push(current);

    if (callback) {
      callback(current);
    }

    for (const neighbor of graph.neighbors(current)) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor);
        queue.push(neighbor);
      }
    }
  }

  return result;
}

export function bfsFindPath<T>(graph: Graph<T>, start: T, end: T): T[] | undefined {
  if (!graph.hasVertex(start) || !graph.hasVertex(end)) {
    return undefined;
  }

  if (start === end) {
    return [start];
  }

  const visited = new Set<T>();
  const parent = new Map<T, T>();
  const queue: T[] = [start];
  visited.add(start);

  while (queue.length > 0) {
    const current = queue.shift()!;

    if (current === end) {
      const path: T[] = [];
      let node: T | undefined = end;
      while (node !== undefined && node !== start) {
        path.push(node);
        node = parent.get(node);
      }
      path.push(start);
      return path.reverse();
    }

    for (const neighbor of graph.neighbors(current)) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor);
        parent.set(neighbor, current);
        queue.push(neighbor);
      }
    }
  }

  return undefined;
}

export function bfsDistances<T>(graph: Graph<T>, start: T): Map<T, number> {
  const distances = new Map<T, number>();

  if (!graph.hasVertex(start)) {
    return distances;
  }

  const visited = new Set<T>();
  const queue: Array<{ vertex: T; distance: number }> = [{ vertex: start, distance: 0 }];
  visited.add(start);
  distances.set(start, 0);

  while (queue.length > 0) {
    const { vertex: current, distance: dist } = queue.shift()!;

    for (const neighbor of graph.neighbors(current)) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor);
        distances.set(neighbor, dist + 1);
        queue.push({ vertex: neighbor, distance: dist + 1 });
      }
    }
  }

  return distances;
}

/**
 * Depth-First Search implementation.
 * Time: O(V + E)
 * Space: O(V)
 */

import { Graph } from './graph';

export function dfs<T>(graph: Graph<T>, start: T, callback?: (vertex: T) => void): T[] {
  const result: T[] = [];

  if (!graph.hasVertex(start)) {
    return result;
  }

  const visited = new Set<T>();
  const stack: T[] = [start];

  while (stack.length > 0) {
    const current = stack.pop()!;

    if (visited.has(current)) {
      continue;
    }

    visited.add(current);
    result.push(current);

    if (callback) {
      callback(current);
    }

    const neighbors = graph.neighbors(current);
    for (let i = neighbors.length - 1; i >= 0; i--) {
      if (!visited.has(neighbors[i])) {
        stack.push(neighbors[i]);
      }
    }
  }

  return result;
}

export function dfsRecursive<T>(graph: Graph<T>, start: T, callback?: (vertex: T) => void): T[] {
  const result: T[] = [];

  if (!graph.hasVertex(start)) {
    return result;
  }

  const visited = new Set<T>();
  dfsRecursiveHelper(graph, start, visited, result, callback);
  return result;
}

function dfsRecursiveHelper<T>(
  graph: Graph<T>,
  current: T,
  visited: Set<T>,
  result: T[],
  callback?: (vertex: T) => void
): void {
  visited.add(current);
  result.push(current);

  if (callback) {
    callback(current);
  }

  for (const neighbor of graph.neighbors(current)) {
    if (!visited.has(neighbor)) {
      dfsRecursiveHelper(graph, neighbor, visited, result, callback);
    }
  }
}

export function dfsFindPath<T>(graph: Graph<T>, start: T, end: T): T[] | undefined {
  if (!graph.hasVertex(start) || !graph.hasVertex(end)) {
    return undefined;
  }

  if (start === end) {
    return [start];
  }

  const visited = new Set<T>();
  const path: T[] = [];

  if (dfsPathHelper(graph, start, end, visited, path)) {
    return path;
  }
  return undefined;
}

function dfsPathHelper<T>(
  graph: Graph<T>,
  current: T,
  end: T,
  visited: Set<T>,
  path: T[]
): boolean {
  visited.add(current);
  path.push(current);

  if (current === end) {
    return true;
  }

  for (const neighbor of graph.neighbors(current)) {
    if (!visited.has(neighbor)) {
      if (dfsPathHelper(graph, neighbor, end, visited, path)) {
        return true;
      }
    }
  }

  path.pop();
  return false;
}

export function hasCycle<T>(graph: Graph<T>, start: T): boolean {
  if (!graph.hasVertex(start)) {
    return false;
  }

  const visited = new Set<T>();
  const recStack = new Set<T>();

  return detectCycle(graph, start, visited, recStack);
}

function detectCycle<T>(
  graph: Graph<T>,
  current: T,
  visited: Set<T>,
  recStack: Set<T>
): boolean {
  visited.add(current);
  recStack.add(current);

  for (const neighbor of graph.neighbors(current)) {
    if (!visited.has(neighbor)) {
      if (detectCycle(graph, neighbor, visited, recStack)) {
        return true;
      }
    } else if (recStack.has(neighbor)) {
      return true;
    }
  }

  recStack.delete(current);
  return false;
}

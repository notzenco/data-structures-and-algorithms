import {
  binarySearch,
  lowerBound,
  upperBound,
  insertionSort,
  mergeSort,
  quickSort,
  Graph,
  bfs,
  bfsFindPath,
  bfsDistances,
  dfs,
  dfsRecursive,
  dfsFindPath,
  hasCycle,
} from '../src/algorithms';

describe('Binary Search', () => {
  test('finds element', () => {
    const arr = [1, 3, 5, 7, 9, 11, 13];

    expect(binarySearch(arr, 1)).toBe(0);
    expect(binarySearch(arr, 7)).toBe(3);
    expect(binarySearch(arr, 13)).toBe(6);
  });

  test('element not found', () => {
    const arr = [1, 3, 5, 7, 9];

    expect(binarySearch(arr, 0)).toBeUndefined();
    expect(binarySearch(arr, 4)).toBeUndefined();
    expect(binarySearch(arr, 10)).toBeUndefined();
  });

  test('empty array', () => {
    expect(binarySearch([], 5)).toBeUndefined();
  });

  test('with comparator', () => {
    const arr = ['apple', 'banana', 'cherry', 'date'];
    const cmp = (a: string, b: string) => a.localeCompare(b);

    expect(binarySearch(arr, 'banana', cmp)).toBe(1);
    expect(binarySearch(arr, 'fig', cmp)).toBeUndefined();
  });

  test('lower bound', () => {
    const arr = [1, 2, 2, 2, 3, 4, 5];
    expect(lowerBound(arr, 2)).toBe(1);
    expect(lowerBound(arr, 3)).toBe(4);
    expect(lowerBound(arr, 0)).toBe(0);
  });

  test('upper bound', () => {
    const arr = [1, 2, 2, 2, 3, 4, 5];
    expect(upperBound(arr, 2)).toBe(4);
    expect(upperBound(arr, 3)).toBe(5);
    expect(upperBound(arr, 5)).toBe(7);
  });
});

describe('Insertion Sort', () => {
  test('basic', () => {
    const arr = [5, 2, 8, 1, 9, 3];
    insertionSort(arr);
    expect(arr).toEqual([1, 2, 3, 5, 8, 9]);
  });

  test('already sorted', () => {
    const arr = [1, 2, 3, 4, 5];
    insertionSort(arr);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test('reversed', () => {
    const arr = [5, 4, 3, 2, 1];
    insertionSort(arr);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test('empty', () => {
    const arr: number[] = [];
    insertionSort(arr);
    expect(arr).toEqual([]);
  });

  test('single element', () => {
    const arr = [42];
    insertionSort(arr);
    expect(arr).toEqual([42]);
  });
});

describe('Merge Sort', () => {
  test('basic', () => {
    const arr = [5, 2, 8, 1, 9, 3];
    mergeSort(arr);
    expect(arr).toEqual([1, 2, 3, 5, 8, 9]);
  });

  test('with duplicates', () => {
    const arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
    mergeSort(arr);
    expect(arr).toEqual([1, 1, 2, 3, 3, 4, 5, 5, 6, 9]);
  });

  test('strings', () => {
    const arr = ['banana', 'apple', 'cherry'];
    mergeSort(arr, (a, b) => a.localeCompare(b));
    expect(arr).toEqual(['apple', 'banana', 'cherry']);
  });
});

describe('Quick Sort', () => {
  test('basic', () => {
    const arr = [5, 2, 8, 1, 9, 3];
    quickSort(arr);
    expect(arr).toEqual([1, 2, 3, 5, 8, 9]);
  });

  test('with duplicates', () => {
    const arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
    quickSort(arr);
    expect(arr).toEqual([1, 1, 2, 3, 3, 4, 5, 5, 6, 9]);
  });

  test('large array', () => {
    const arr: number[] = [];
    for (let i = 0; i < 1000; i++) {
      arr.push(Math.floor(Math.random() * 1000));
    }
    const expected = [...arr].sort((a, b) => a - b);
    quickSort(arr);
    expect(arr).toEqual(expected);
  });

  test('strings', () => {
    const arr = ['banana', 'apple', 'cherry'];
    quickSort(arr, (a, b) => a.localeCompare(b));
    expect(arr).toEqual(['apple', 'banana', 'cherry']);
  });
});

describe('BFS', () => {
  test('traversal', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('A', 'C');
    graph.addEdge('B', 'D');
    graph.addEdge('C', 'D');

    const result = bfs(graph, 'A');
    expect(result[0]).toBe('A');
    expect(result).toContain('B');
    expect(result).toContain('C');
    expect(result).toContain('D');
    expect(result.length).toBe(4);
  });

  test('with callback', () => {
    const graph = new Graph<number>();
    graph.addEdge(1, 2);
    graph.addEdge(1, 3);
    graph.addEdge(2, 4);

    const visited: number[] = [];
    bfs(graph, 1, (v) => visited.push(v));

    expect(visited[0]).toBe(1);
    expect(visited.length).toBe(4);
  });

  test('find path', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('B', 'C');
    graph.addEdge('C', 'D');
    graph.addEdge('A', 'D');

    const path = bfsFindPath(graph, 'A', 'D');
    expect(path).toBeDefined();
    expect(path![0]).toBe('A');
    expect(path![path!.length - 1]).toBe('D');
    expect(path!.length).toBe(2); // Shortest path A -> D
  });

  test('no path', () => {
    const graph = new Graph<string>();
    graph.addVertex('A');
    graph.addVertex('B');

    const path = bfsFindPath(graph, 'A', 'B');
    expect(path).toBeUndefined();
  });

  test('distances', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('A', 'C');
    graph.addEdge('B', 'D');
    graph.addEdge('C', 'D');
    graph.addEdge('D', 'E');

    const distances = bfsDistances(graph, 'A');

    expect(distances.get('A')).toBe(0);
    expect(distances.get('B')).toBe(1);
    expect(distances.get('C')).toBe(1);
    expect(distances.get('D')).toBe(2);
    expect(distances.get('E')).toBe(3);
  });
});

describe('DFS', () => {
  test('traversal', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('A', 'C');
    graph.addEdge('B', 'D');
    graph.addEdge('C', 'D');

    const result = dfs(graph, 'A');
    expect(result[0]).toBe('A');
    expect(result).toContain('B');
    expect(result).toContain('C');
    expect(result).toContain('D');
    expect(result.length).toBe(4);
  });

  test('recursive traversal', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('A', 'C');
    graph.addEdge('B', 'D');

    const iterative = dfs(graph, 'A');
    const recursive = dfsRecursive(graph, 'A');

    expect(iterative.length).toBe(recursive.length);
    expect(iterative[0]).toBe(recursive[0]); // Both start at A
  });

  test('find path', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');
    graph.addEdge('B', 'C');
    graph.addEdge('C', 'D');

    const path = dfsFindPath(graph, 'A', 'D');
    expect(path).toBeDefined();
    expect(path![0]).toBe('A');
    expect(path![path!.length - 1]).toBe('D');
  });

  test('cycle detection', () => {
    const directedGraph = new Graph<string>(true);
    directedGraph.addEdge('A', 'B');
    directedGraph.addEdge('B', 'C');
    directedGraph.addEdge('C', 'A');

    expect(hasCycle(directedGraph, 'A')).toBe(true);
  });

  test('no cycle', () => {
    const directedGraph = new Graph<string>(true);
    directedGraph.addEdge('A', 'B');
    directedGraph.addEdge('B', 'C');
    directedGraph.addEdge('A', 'C');

    expect(hasCycle(directedGraph, 'A')).toBe(false);
  });

  test('invalid start', () => {
    const graph = new Graph<string>();
    graph.addEdge('A', 'B');

    const result = dfs(graph, 'X');
    expect(result.length).toBe(0);
  });
});

describe('Graph', () => {
  test('basic operations', () => {
    const graph = new Graph<number>();
    graph.addVertex(1);
    graph.addVertex(2);

    expect(graph.hasVertex(1)).toBe(true);
    expect(graph.hasVertex(2)).toBe(true);
    expect(graph.hasVertex(3)).toBe(false);
    expect(graph.size()).toBe(2);
  });

  test('undirected', () => {
    const graph = new Graph<string>(false);
    graph.addEdge('A', 'B');

    expect(graph.neighbors('A')).toContain('B');
    expect(graph.neighbors('B')).toContain('A');
  });

  test('directed', () => {
    const graph = new Graph<string>(true);
    graph.addEdge('A', 'B');

    expect(graph.neighbors('A')).toContain('B');
    expect(graph.neighbors('B')).not.toContain('A');
  });
});

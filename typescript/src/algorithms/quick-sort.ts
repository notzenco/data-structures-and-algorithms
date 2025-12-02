/**
 * Quick sort implementation with median-of-three pivot selection.
 * Time: O(n log n) average, O(n^2) worst
 * Space: O(log n) average
 */

type Comparator<T> = (a: T, b: T) => number;

export function quickSort<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  if (arr.length < 2) {
    return;
  }

  quickSortHelper(arr, 0, arr.length - 1, compare);
}

function quickSortHelper<T>(arr: T[], low: number, high: number, compare: Comparator<T>): void {
  if (low >= high) {
    return;
  }

  const pivotIdx = partition(arr, low, high, compare);

  if (pivotIdx > low) {
    quickSortHelper(arr, low, pivotIdx - 1, compare);
  }
  if (pivotIdx < high) {
    quickSortHelper(arr, pivotIdx + 1, high, compare);
  }
}

function partition<T>(arr: T[], low: number, high: number, compare: Comparator<T>): number {
  // Median of three pivot selection
  const mid = low + Math.floor((high - low) / 2);

  if (compare(arr[high], arr[low]) < 0) {
    swap(arr, low, high);
  }
  if (compare(arr[mid], arr[low]) < 0) {
    swap(arr, low, mid);
  }
  if (compare(arr[high], arr[mid]) < 0) {
    swap(arr, mid, high);
  }

  swap(arr, mid, high);
  const pivot = arr[high];

  let i = low;
  for (let j = low; j < high; j++) {
    if (compare(arr[j], pivot) < 0) {
      swap(arr, i, j);
      i++;
    }
  }

  swap(arr, i, high);
  return i;
}

function swap<T>(arr: T[], i: number, j: number): void {
  [arr[i], arr[j]] = [arr[j], arr[i]];
}

export function quickSortDescending<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  quickSort(arr, (a, b) => -compare(a, b));
}

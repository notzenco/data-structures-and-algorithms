/**
 * Merge sort implementation.
 * Time: O(n log n) all cases
 * Space: O(n)
 */

type Comparator<T> = (a: T, b: T) => number;

export function mergeSort<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  if (arr.length < 2) {
    return;
  }

  mergeSortHelper(arr, 0, arr.length - 1, compare);
}

function mergeSortHelper<T>(arr: T[], left: number, right: number, compare: Comparator<T>): void {
  if (left >= right) {
    return;
  }

  const mid = left + Math.floor((right - left) / 2);
  mergeSortHelper(arr, left, mid, compare);
  mergeSortHelper(arr, mid + 1, right, compare);
  merge(arr, left, mid, right, compare);
}

function merge<T>(arr: T[], left: number, mid: number, right: number, compare: Comparator<T>): void {
  const leftArr = arr.slice(left, mid + 1);
  const rightArr = arr.slice(mid + 1, right + 1);

  let i = 0;
  let j = 0;
  let k = left;

  while (i < leftArr.length && j < rightArr.length) {
    if (compare(leftArr[i], rightArr[j]) <= 0) {
      arr[k++] = leftArr[i++];
    } else {
      arr[k++] = rightArr[j++];
    }
  }

  while (i < leftArr.length) {
    arr[k++] = leftArr[i++];
  }

  while (j < rightArr.length) {
    arr[k++] = rightArr[j++];
  }
}

export function mergeSortDescending<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  mergeSort(arr, (a, b) => -compare(a, b));
}

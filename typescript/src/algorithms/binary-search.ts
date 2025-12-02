/**
 * Binary search implementation.
 * Time: O(log n)
 * Space: O(1)
 */

type Comparator<T> = (a: T, b: T) => number;

export function binarySearch<T>(arr: T[], target: T, comparator?: Comparator<T>): number | undefined {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  let left = 0;
  let right = arr.length - 1;

  while (left <= right) {
    const mid = left + Math.floor((right - left) / 2);
    const cmp = compare(arr[mid], target);

    if (cmp === 0) {
      return mid;
    } else if (cmp < 0) {
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }

  return undefined;
}

export function lowerBound<T>(arr: T[], target: T, comparator?: Comparator<T>): number {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  let left = 0;
  let right = arr.length;

  while (left < right) {
    const mid = left + Math.floor((right - left) / 2);
    if (compare(arr[mid], target) < 0) {
      left = mid + 1;
    } else {
      right = mid;
    }
  }

  return left;
}

export function upperBound<T>(arr: T[], target: T, comparator?: Comparator<T>): number {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  let left = 0;
  let right = arr.length;

  while (left < right) {
    const mid = left + Math.floor((right - left) / 2);
    if (compare(arr[mid], target) <= 0) {
      left = mid + 1;
    } else {
      right = mid;
    }
  }

  return left;
}

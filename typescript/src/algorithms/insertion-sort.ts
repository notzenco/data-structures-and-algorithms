/**
 * Insertion sort implementation.
 * Time: O(n^2) average, O(n) best (nearly sorted)
 * Space: O(1)
 */

type Comparator<T> = (a: T, b: T) => number;

export function insertionSort<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  for (let i = 1; i < arr.length; i++) {
    const key = arr[i];
    let j = i - 1;

    while (j >= 0 && compare(arr[j], key) > 0) {
      arr[j + 1] = arr[j];
      j--;
    }
    arr[j + 1] = key;
  }
}

export function insertionSortDescending<T>(arr: T[], comparator?: Comparator<T>): void {
  const compare = comparator || ((a: T, b: T) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  insertionSort(arr, (a, b) => -compare(a, b));
}

namespace DSA.Algorithms;

/// <summary>
/// Merge sort implementation.
/// Time: O(n log n) all cases
/// Space: O(n)
/// </summary>
public static class MergeSort
{
    public static void Sort<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        if (arr.Count < 2)
            return;

        MergeSortHelper(arr, 0, arr.Count - 1, comparer);
    }

    private static void MergeSortHelper<T>(IList<T> arr, int left, int right, IComparer<T> comparer)
    {
        if (left >= right)
            return;

        int mid = left + (right - left) / 2;
        MergeSortHelper(arr, left, mid, comparer);
        MergeSortHelper(arr, mid + 1, right, comparer);
        Merge(arr, left, mid, right, comparer);
    }

    private static void Merge<T>(IList<T> arr, int left, int mid, int right, IComparer<T> comparer)
    {
        var leftArr = new T[mid - left + 1];
        var rightArr = new T[right - mid];

        for (int x = 0; x < leftArr.Length; x++)
            leftArr[x] = arr[left + x];
        for (int x = 0; x < rightArr.Length; x++)
            rightArr[x] = arr[mid + 1 + x];

        int i = 0, j = 0, k = left;

        while (i < leftArr.Length && j < rightArr.Length)
        {
            if (comparer.Compare(leftArr[i], rightArr[j]) <= 0)
                arr[k++] = leftArr[i++];
            else
                arr[k++] = rightArr[j++];
        }

        while (i < leftArr.Length)
            arr[k++] = leftArr[i++];

        while (j < rightArr.Length)
            arr[k++] = rightArr[j++];
    }

    public static void SortDescending<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;
        Sort(arr, Comparer<T>.Create((a, b) => comparer.Compare(b, a)));
    }
}

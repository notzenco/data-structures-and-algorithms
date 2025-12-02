namespace DSA.Algorithms;

/// <summary>
/// Quick sort implementation with median-of-three pivot selection.
/// Time: O(n log n) average, O(n^2) worst
/// Space: O(log n) average
/// </summary>
public static class QuickSort
{
    public static void Sort<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        if (arr.Count < 2)
            return;

        QuickSortHelper(arr, 0, arr.Count - 1, comparer);
    }

    private static void QuickSortHelper<T>(IList<T> arr, int low, int high, IComparer<T> comparer)
    {
        if (low >= high)
            return;

        int pivotIdx = Partition(arr, low, high, comparer);

        if (pivotIdx > low)
            QuickSortHelper(arr, low, pivotIdx - 1, comparer);
        if (pivotIdx < high)
            QuickSortHelper(arr, pivotIdx + 1, high, comparer);
    }

    private static int Partition<T>(IList<T> arr, int low, int high, IComparer<T> comparer)
    {
        // Median of three pivot selection
        int mid = low + (high - low) / 2;

        if (comparer.Compare(arr[high], arr[low]) < 0)
            Swap(arr, low, high);
        if (comparer.Compare(arr[mid], arr[low]) < 0)
            Swap(arr, low, mid);
        if (comparer.Compare(arr[high], arr[mid]) < 0)
            Swap(arr, mid, high);

        Swap(arr, mid, high);
        var pivot = arr[high];

        int i = low;
        for (int j = low; j < high; j++)
        {
            if (comparer.Compare(arr[j], pivot) < 0)
            {
                Swap(arr, i, j);
                i++;
            }
        }

        Swap(arr, i, high);
        return i;
    }

    private static void Swap<T>(IList<T> arr, int i, int j)
    {
        (arr[i], arr[j]) = (arr[j], arr[i]);
    }

    public static void SortDescending<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;
        Sort(arr, Comparer<T>.Create((a, b) => comparer.Compare(b, a)));
    }
}

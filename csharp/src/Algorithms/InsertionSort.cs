namespace DSA.Algorithms;

/// <summary>
/// Insertion sort implementation.
/// Time: O(n^2) average, O(n) best (nearly sorted)
/// Space: O(1)
/// </summary>
public static class InsertionSort
{
    public static void Sort<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        for (int i = 1; i < arr.Count; i++)
        {
            var key = arr[i];
            int j = i - 1;

            while (j >= 0 && comparer.Compare(arr[j], key) > 0)
            {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    }

    public static void SortDescending<T>(IList<T> arr, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;
        Sort(arr, Comparer<T>.Create((a, b) => comparer.Compare(b, a)));
    }
}

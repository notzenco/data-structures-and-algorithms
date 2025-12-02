namespace DSA.Algorithms;

/// <summary>
/// Binary search implementation.
/// Time: O(log n)
/// Space: O(1)
/// </summary>
public static class BinarySearch
{
    public static int? Search<T>(IList<T> arr, T target, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        int left = 0;
        int right = arr.Count - 1;

        while (left <= right)
        {
            int mid = left + (right - left) / 2;
            int cmp = comparer.Compare(arr[mid], target);

            if (cmp == 0)
                return mid;
            if (cmp < 0)
                left = mid + 1;
            else
                right = mid - 1;
        }

        return null;
    }

    public static int LowerBound<T>(IList<T> arr, T target, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        int left = 0;
        int right = arr.Count;

        while (left < right)
        {
            int mid = left + (right - left) / 2;
            if (comparer.Compare(arr[mid], target) < 0)
                left = mid + 1;
            else
                right = mid;
        }

        return left;
    }

    public static int UpperBound<T>(IList<T> arr, T target, IComparer<T>? comparer = null)
    {
        comparer ??= Comparer<T>.Default;

        int left = 0;
        int right = arr.Count;

        while (left < right)
        {
            int mid = left + (right - left) / 2;
            if (comparer.Compare(arr[mid], target) <= 0)
                left = mid + 1;
            else
                right = mid;
        }

        return left;
    }
}

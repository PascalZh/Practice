#include <functional>
#include "test.h"
#include "utils.h"

using namespace std;
namespace i2a {

inline void exchange(int& x, int& y)
{
    if (x == y) return;
    x = x + y;
    y = x - y;
    x = x - y;
}

inline int parent(int i)
{
    return (i + 1) / 2 - 1;
}

inline int left(int i)
{
    return 2 * i + 1;
}

inline int right(int i)
{
    return 2 * i + 2;
}

void max_heapify(vector<int>& A, int heap_size, int p)
{
    int l = left(p);
    int r = right(p);
    int largest;
    if (l < heap_size && A[l] > A[p]) {
        largest = l;
    } else {
        largest = p;
    }
    if (r < heap_size && A[r] > A[largest]) {
        largest = r;
    }
    if (largest != p) {
        exchange(A[largest], A[p]);
        max_heapify(A, heap_size, largest);
    }
}

void build_max_heap(vector<int>& A)
{
    int heap_size = A.size();
    for (int i = parent(heap_size - 1); i >= 0; --i)
        max_heapify(A, heap_size, i);
}

void heap_sort(vector<int>& A)
{
    build_max_heap(A);
    int heap_size = A.size();
    for (int i = A.size() - 1; i > 0; --i) {
        exchange(A[i], A[0]);
        --heap_size;
        max_heapify(A, heap_size, 0);
    }
}

// quick sort
int partition(vector<int>& A, int p, int r)
{
    int x = A[r];
    int i = p;
    for (int k = p; k < r; ++k) {
        if (A[k] < x) {
            exchange(A[k], A[i]);
            ++i;
        }
    }
    exchange(A[r], A[i]);
    return i;
}

void quick_sort_(vector<int>& A, int p, int r)
{
    int n = r - p + 1;
    if (n > 1) {
        int q = partition(A, p, r);
        quick_sort_(A, p, q - 1);
        quick_sort_(A, q + 1, r);
    }
}

void quick_sort(vector<int>& A)
{
    quick_sort_(A, 0, A.size() - 1);
}

void insert_sort_(Array<Key>& A, int a, int b)
{
    for (int j = a + 1; j < b + 1; ++j) {
        Key key = A[j];
        int i = j - 1;
        while (i >= a && A[i] > key) {
            A[i+1] = A[i];
            --i;
        }
        A[i+1] = key;
    }
}

void quick_insert_sort_(vector<int>& A, int p, int r, const int k)
{
    int n = r - p + 1;
    if (n > k) {
        int q  = partition(A, p, r);
        quick_insert_sort_(A, p, q - 1, k);
        quick_insert_sort_(A, q + 1, r, k);
    }
}

void quick_insert_sort(vector<int>& A, const int k)
{
    quick_insert_sort_(A, 0, A.size() - 1, k);
    insert_sort_(A, 0, A.size() - 1);
}

} /* namespace i2a */

int main(int argc, char *argv[])
{
    using namespace i2a;

    test(heap_sort, {10, 100, 1000, 10000, 100000}, 100);
    test(quick_sort, {10, 100, 1000, 10000, 100000}, 100);
    for (int k : {1, 2, 3, 4, 5, 10, 20, 30, 50, 100}) {
        cout << "k = " << k << "\t";
        auto quick_insert_sort_k = bind(quick_insert_sort, placeholders::_1, k);
        test(quick_insert_sort_k, {10, 100, 1000, 10000, 200000}, 100);
    }
    return 0;
}

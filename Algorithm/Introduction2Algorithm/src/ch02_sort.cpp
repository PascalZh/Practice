#include <functional>
#include "test.h"
#include "utils.h"

using namespace std;
namespace i2a {

inline void exchange(int* x, int* y)
{
    if (x == y) return;
    *x = *x + *y;
    *y = *x - *y;
    *x = *x - *y;
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
        exchange(&A[largest], &A[p]);
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
        exchange(&A[i], &A[0]);
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
            exchange(&A[k], &A[i]);
            ++i;
        }
    }
    exchange(&A[r], &A[i]);
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

void insert_sort_(vector<int>& A, int a, int b)
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

int rand_partition(vector<int>& A, int p, int r, int k)
{
    if (r - p + 1 > 10) {
        vector<int> ind;
        for (int i = 0; i < k; ++i)
            ind.push_back(randint(p, r));
        insert_sort_(ind, 0, ind.size() - 1);
        exchange(&A[r], &A[ind[k / 2]]);
    }
    int x = A[r];
    int i = p;
    for (int m = p; m < r; ++m) {
        if (A[m] < x) {
            exchange(&A[m], &A[i]);
            ++i;
        }
    }
    exchange(&A[r], &A[i]);
    return i;
}

int rand_quicksort_(vector<int>& A, int p, int r, int k)
{
    int n = r - p + 1;
    if (n > 1) {
        int q  = rand_partition(A, p, r, k);
        rand_quicksort_(A, p, q - 1, k);
        rand_quicksort_(A, q + 1, r, k);
    }
}

int rand_quicksort(vector<int>& A, int k)
{
    rand_quicksort_(A, 0, A.size() - 1, k);
}

int hoare_partition(vector<int>& A, int p, int r)
{
    int x = A[p];
    int i = p - 1;
    int j = r + 1;
    while (true) {
        do --j; while (A[j] > x);
        do ++i; while (A[i] < x);
        if (i < j) exchange(&A[i], &A[j]);
        else return j;
    }
}

int hoare_quicksort_(vector<int>& A, int p, int r)
{
    int n = r - p + 1;
    if (n > 1) {
        int q  = hoare_partition(A, p, r);
        hoare_quicksort_(A, p, q - 1);
        hoare_quicksort_(A, q + 1, r);
    }
}

int hoare_quicksort(vector<int>& A)
{
    hoare_quicksort_(A, 0, A.size() - 1);
}

} /* namespace i2a */

int main(int argc, char *argv[])
{
    using namespace i2a;
    namespace ph = placeholders;
    srand(time(NULL));

    vector<int> A = {3, 2, 1, 7, 8, 9, 5, 4, 4};
    hoare_quicksort(A);
    print_list(A);

    TEST(heap_sort);

    TEST(quick_sort);

    TEST(hoare_quicksort);

    cout << "rand_quick_sort(k = 3):" << endl;
    test(bind(rand_quicksort, ph::_1, 3));
    cout << endl;

    cout << "rand_quick_sort(k = 1):" << endl;
    test(bind(rand_quicksort, ph::_1, 1));
    cout << endl;


    cout << "quick_insert_sort:" << endl;
    for (int k : {1, 2, 3, 4, 5, 10, 20, 30, 50, 100}) {
        cout << "k = " << k << "\t";
        auto fun = bind(quick_insert_sort, placeholders::_1, k);
        test(fun, {10, 100, 1000, 10000, 100000}, 100);
    }
    return 0;
}

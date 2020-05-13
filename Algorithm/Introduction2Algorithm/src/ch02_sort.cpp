#include <functional>
#include <array>
#include <algorithm>
#include <cassert>
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

void quicksort_(vector<int>& A, int p, int r)
{
    int n = r - p + 1;
    if (n > 1) {
        int q = partition(A, p, r);
        quicksort_(A, p, q - 1);
        quicksort_(A, q + 1, r);
    }
}

void quicksort(vector<int>& A)
{
    quicksort_(A, 0, A.size() - 1);
}

template<class T>
void insert_sort_(T& A, int a, int b)
{
    for (int j = a + 1; j < b + 1; ++j) {
        int key = A[j];
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

template <int k>
int rand_partition(vector<int>& A, int p, int r)
{
    if (r - p + 1 > 10 * k) {
        int ind[k];
        for (int i = 0; i < k; ++i)
            ind[i] = randint(p, r);
        insert_sort_(ind, 0, k - 1);
        exchange(&A[r], &A[ind[k / 2]]);
    }
    return partition(A, p, r);
}

template <int k>
int rand_quicksort_(vector<int>& A, int p, int r)
{
    int n = r - p + 1;
    if (n > 1) {
        int q  = rand_partition<k>(A, p, r);
        rand_quicksort_<k>(A, p, q - 1);
        rand_quicksort_<k>(A, q + 1, r);
    }
}

template <int k>
int rand_quicksort(vector<int>& A)
{
    rand_quicksort_<k>(A, 0, A.size() - 1);
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

auto partition_1(vector<int>& A, int p, int r)
{
    int x = A[r];
    int i = p;
    for (int k = p; k < r; ++k) {
        if (A[k] < x) {
            exchange(&A[k], &A[i]);
            ++i;
        }
    }
    int q = i;
    for (int k = q; k < r; ++k) {
        if (A[k] == x) {
            exchange(&A[k], &A[i]);
            ++i;
        }
    }
    exchange(&A[r], &A[i]);
    return make_tuple(q, i);
}

void quicksort_1_(vector<int>& A, int p, int r)
{
    int n = r - p + 1;
    if (n > 1) {
        auto[q, t] = partition_1(A, p, r);
        quicksort_1_(A, p, q - 1);
        quicksort_1_(A, t + 1, r);
    }
}

void quicksort_1(vector<int>& A)
{
    quicksort_1_(A, 0, A.size() - 1);
}

// sorting in linear time
void counting_sort(vector<int>& A, vector<int>& B, int k)
{
    vector<int> C(k + 1);
    for (int j = 0; j < A.size(); ++j)
        C[A[j]] = C[A[j]] + 1;
    for (int i = 0; i < k + 1; ++i)
        C[i] += C[i - 1];
    for (int j = A.size() - 1; j >= 0; --j) {
        B[C[A[j]]] = A[j];
        C[A[j]] = C[A[j]] - 1;
    }
}

// medians and order statistics
void do_nothing() { for (int i = 0; i < 500; ++i) ; }

bool gt(int a, int b) { do_nothing(); return a > b; }

bool lt(int a, int b) { do_nothing(); return a < b; }

auto min_max(vector<int>& A)
{
    int min = A[0];
    int max = A[0];
    for (int i = 1; i < A.size(); ++i) {
        if (gt(min, A[i])) {
            min = A[i];
        }
        if (lt(max, A[i])) {
            max = A[i];
        }
    }
    assert(all_of(A.cbegin(), A.cend(), [max](int x) { return x <= max; }));
    assert(all_of(A.cbegin(), A.cend(), [min](int x) { return x >= min; }));
    return make_tuple(min, max);
}

// This function only performs 1.5*n comparisons, while `min_max` performs 2*n
// comparisons. However, `min_max_` is not faster than `min_max` due to some
// implementation reasons. So, to show the difference, we change <, > to
// `lt`, `gt`, and run `do_nothing` in both comparison function.
auto min_max_(vector<int>& A)
{
    int min, max, offset;
    if (A.size() % 2 == 1) {
        min = max = A[0];
        offset = 1;
    } else {
        offset = 0;
        min = A[0] < A[1] ? A[0] : A[1];
        max = A[0] < A[1] ? A[1] : A[0];
    }
    for (int i = 0; i < (A.size() - offset) / 2; ++i) {
        int a1 = A[offset + 2 * i];
        int a2 = A[offset + 2 * i + 1];
        if (lt(a1, a2)) {
            if (gt(min, a1))
                min = a1;
            if (lt(max, a2))
                max = a2;
        } else {
            if (gt(min, a2))
                min = a2;
            if (lt(max, a1))
                max = a1;
        }
    }
    assert(all_of(A.cbegin(), A.cend(), [max](int x) { return x <= max; }));
    assert(all_of(A.cbegin(), A.cend(), [min](int x) { return x >= min; }));
    return make_tuple(min, max);
}

} /* namespace i2a */

int main(int argc, char *argv[])
{
    using namespace i2a;
    namespace ph = placeholders;
    srand(time(NULL));

    TEST(min_max);
    TEST(min_max_);

    vector<int> A = {3, 2, 1, 7, 8, 9, 5, 4, 4};
    quicksort_1(A);
    print_list(A);

    TEST(heap_sort);

    TEST(quicksort);

    cout << "quicksort(all values are equal):" << endl;
    for (int i : {10, 100, 1000, 2000}) {
        unique_ptr<Profile> p = make_unique<Profile>("", "\t");
        cout << i << ":";
        for (int j = 0; j < 100; ++j) {
            vector<int> arr(i, 7);
            quicksort(arr);
        }
    } cout << endl << endl;

    TEST(quicksort_1);

    cout << "quicksort_1(all values are equal):" << endl;
    for (int i : {10, 100, 1000, 10000, 100000}) {
        unique_ptr<Profile> p = make_unique<Profile>("", "\t");
        cout << i << ":";
        for (int j = 0; j < 100; ++j) {
            vector<int> arr(i, 7);
            quicksort_1(arr);
        }
    } cout << endl << endl;

    TEST(hoare_quicksort);

    TEST(rand_quicksort<1>);

    TEST(rand_quicksort<3>);

    TEST(rand_quicksort<5>);

    TEST(rand_quicksort<7>);

    cout << "quick_insert_sort:" << endl;
    for (int k : {1, 2, 3, 4, 5, 10, 20, 30, 50, 100}) {
        cout << "k = " << k << "\t";
        auto fun = bind(quick_insert_sort, placeholders::_1, k);
        TestConfig cfg;
        cfg.times = 20;
        test(fun, cfg);
    } cout << endl;

    vector<int> B(A.size());
    counting_sort(A, B, 9);
    print_list(A);

    return 0;
}

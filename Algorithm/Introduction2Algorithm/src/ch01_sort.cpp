#include <functional>
#include <vector>
#include <iostream>
#include <limits>

#include "ch01_sort.h"
#include "utils.h"

namespace i2a {

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

void insert_sort(Array<Key>& A)
{
    for (int j = 1; j < int(A.size()); ++j) {
        Key key = A[j];
        int i = j - 1;
        while (i >= 0 && A[i] > key) {
            A[i+1] = A[i];
            --i;
        }
        A[i+1] = key;
    }
}

void merge_sort(Array<Key>& A)
{
    merge_sort_(A, 0, A.size() - 1);
}

void merge_sort_(Array<Key>& A, int p, int r)
{
    if (p < r) {
        int q = (p + r) / 2;
        merge_sort_(A, p, q);
        merge_sort_(A, q + 1, r);
        merge(A, p, q, r);
    }
}

void merge(Array<Key>& A, int p, int q, int r)
{
    int n1 = q - p + 1;
    int n2 = r - q;
    
    Array<Key> L(n1+1), R(n2+1);
    for (int i = 0; i < n1; ++i)
        L[i] = A[p + i];
    for (int i = 0; i < n2; ++i)
        R[i] = A[q + i + 1];
    L[n1] = std::numeric_limits<Key>::max();
    R[n2] = std::numeric_limits<Key>::max();
    
    for (int k = p, i = 0, j = 0; k < r + 1; ++k) {
        if (L[i] < R[j]) {
            A[k] = L[i];
            ++i;
        } else {
            A[k] = R[j];
            ++j;
        }
    }
}

void merge_insert_sort_(Array<Key>& A, int p, int r, int k)
{
    const int n = r - p + 1;
    if (n <= k) {
        insert_sort_(A, p, r);
    } else {
        int q = (p + r) / 2;
        merge_insert_sort_(A, p, q, k);
        merge_insert_sort_(A, q + 1, r, k);
        merge(A, p, q, r);
    }
}

void merge_insert_sort(Array<Key>& A, int k = 10)
{
    merge_insert_sort_(A, 0, A.size() - 1, k);
}

} /* namespace i2a */

int main()
{
    using namespace i2a;
    using namespace std;
    srand(time(NULL));

    Array<Key> v{11, 3, 2, 9, 22, 5};
    insert_sort_(v, 0, v.size() - 1);
    print_list(v);

    TestConfig cfg1;
    cfg1.sizes = {10, 100, 1000, 10000};
    test(merge_sort, cfg1);
    test(insert_sort, cfg1);

    // find best k value of merge insert sort.
    for (int k : {1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100}) {
        cout << "k = " << k << "\t";
        auto fun = bind(merge_insert_sort, placeholders::_1, k);
        test(fun, cfg1);
    }
    return 0;
}

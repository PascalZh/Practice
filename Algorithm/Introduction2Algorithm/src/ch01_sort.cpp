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
    L[n1] = numeric_limits<Key>::max();
    R[n2] = numeric_limits<Key>::max();
    
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

    Array<Key> v{11, 3, 2, 9, 22, 5};
    insert_sort_(v, 0, v.size() - 1);
    print_list(v);

    // compare insert sort and merge sort
    //for (int i : {10, 100, 1000, 10000}) {
        //Profile p;
        //for (auto& arr : test_gen_uniform(i, 100)) {
            //insert_sort(arr);
        //}
    //}
    for (int i : {10, 100, 1000, 10000}) {
        Profile p;
        for (auto& arr : test_gen_uniform(i, 100)) {
            merge_sort(arr);
        }
    }

    // find best k value of merge insert sort.
    for (int k : {1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100}) {
        cout << k << ": ";
        for (int i : {10, 20, 30, 40, 90, 100, 1000, 10000, 20000}) {
            Profile p("", " ");
            for (auto& arr : test_gen_uniform(i, 100)) {
                merge_insert_sort(arr, k);
            }
        }
        cout << endl;
    }
    // test result for k = 1 ~ 100    
    //1: 0.677977ms 1.1607ms 1.64804ms 2.78161ms 6.9275ms 7.24838ms 77.0135ms 827.238ms 1749.2ms 
    //2: 0.526851ms 1.06343ms 1.27652ms 1.74764ms 4.39856ms 5.10841ms 51.6682ms 663.641ms 1373.52ms 
    //3: 0.564514ms 1.01859ms 1.58367ms 1.7012ms 3.3602ms 3.42092ms 53.5128ms 588.99ms 1126.28ms 
    //4: 0.493301ms 0.706526ms 0.838548ms 1.42754ms 3.05ms 3.21701ms 38.093ms 510.882ms 1071.51ms 
    //5: 0.355262ms 0.519077ms 0.886529ms 1.05724ms 2.78817ms 3.35138ms 38.8024ms 444.065ms 940.045ms 
    //10: 0.309127ms 0.430353ms 0.720537ms 0.881184ms 2.37378ms 2.68184ms 32.1019ms 393.882ms 836.562ms 
    //
    //20: 0.357421ms 0.392288ms 0.671891ms 0.796435ms 2.07287ms 2.17798ms 29.9522ms 368.07ms 788.646ms 
    //
    //30: 0.291483ms 0.459292ms 0.547436ms 0.822448ms 1.85669ms 2.05591ms 29.3816ms 370.007ms 808.077ms 
    //40: 0.385863ms 0.393641ms 0.638909ms 0.753904ms 1.84962ms 2.10996ms 29.793ms 373.064ms 797.955ms 
    //50: 0.309797ms 0.445814ms 0.540068ms 0.826586ms 1.93114ms 2.17632ms 29.0874ms 379.69ms 794.395ms 
    //100: 0.292589ms 0.486418ms 0.540207ms 0.84623ms 2.55024ms 3.01943ms 33.4556ms 424.587ms 905.405ms 
    // we can know the best k is arround 20.
    return 0;
}

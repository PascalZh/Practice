#include "test.h"
#include "utils.h"

namespace i2a {

inline void exchange(int& x, int& y)
{
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

} /* namespace i2a */

int main(int argc, char *argv[])
{
    using namespace std;
    using namespace i2a;

    test(heap_sort, {10, 100, 1000, 10000, 100000}, 100);
    return 0;
}

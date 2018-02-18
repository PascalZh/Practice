// Test code: 
#include "algorithm.h"
#include <iostream>
using namespace std;
int main()
{
    items[0].w=10; items[0].val=3;
    items[1].w=11; items[1].val=3;
    items[2].w=12; items[2].val=4;
    items[3].w=13; items[3].val=3;
    items[4].w=14; items[4].val=5;
    items[5].w=15; items[5].val=3;
    items[6].w=16; items[6].val=3;
    items[7].w=17; items[7].val=2;
    items[8].w=18; items[8].val=3;
    items[9].w=19; items[9].val=3;
    int solution[10]={0};
    int max_val=BestPlan(solution, 30, items);
    cout << "max_val: " << max_val << endl;
    for ( int i = 0 ; i < 10 ; i++ )
        cout << solution[i];
    return 0;
}

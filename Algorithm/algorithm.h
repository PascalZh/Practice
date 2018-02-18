#pragma once
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
using namespace std;

// {{{ 分治法解决背包问题
/*2017.12.13 This is a complete failure. 
struct CBagItem {
	string name;
	int weight;
	int value;
	CBagItem(string n, int w, int v) {
		name = n;
		weight = w;
		value = v;
	}
	CBagItem() :name(" "), weight(0), value(0) {}
};
struct CBag {
	const int volume;//DEBUG: 这里的volume原来是用来表示剩余能装的空间，但递归过程的两个分支都会对它修改，所以应该用一个int型参数传递，\
	那么每个分支的计算不会影响了。\
	现在的volume表示背包的最大容量，单位是重量，不可修改
	vector<string> items;
	CBag(int v) :volume(v), items() {}
};
int GetBestSchemeOfBagProblem(CBag &bag, const CBagItem items[],int size)
{// int 返回值表示背包中的价值
	int max_value1,max_value2;//前者表示不放当前物品，后者放入当前物品
	if (size == 0 || bag.volume < items->weight)
		return 0;

	// max_value1
	int tmpvol = bag.volume;
	max_value1 = GetBestSchemeOfBagProblem(bag,  items + 1, size - 1);
	bag.volume = tmpvol;

	//max_value2
	if (bag.volume < items->weight)
		max_value2=0;
	else {
		max_value2 = items->value + GetBestSchemeOfBagProblem(bag, items + 1, size - 1);
	}

	if (max_value1 > max_value2) {
		return max_value1;
	}
	else {
		bag.volume -= items->weight;
		if (bag.items.end() == find(bag.items.begin(), bag.items.end(), items->name))//not find
			bag.items.push_back(items->name);
		return max_value2;
	}
		

}

ostream &operator<<(ostream &out, const CBag &bag)
{
	vector<string>::const_iterator itr = bag.items.cbegin();
	for (; itr != bag.items.cend(); itr++)
		out << *itr << endl;
	return out;
}
*/
/*New Code:*/
// TODO: 2017.12.13 
struct Item{
    int val;// value
    int w;// weight
};
Item items[10]={};
int BestPlan(int solution[], int rm_vol, Item rm_items[], int count=0)// rm_vol : remained volume(weight) ; rm_items : remained items
{
    int val1,val2;// val1: put in ; val2: not put in
    int * solu1,* solu2;
    if(count==10)return 0;
    solu1 = new int[10];
    solu2 = new int[10];
    for(int i=0;i<10;i++){
        solu1[i]=solution[i];
        solu2[i]=solution[i];
    }
    if(rm_items->w>rm_vol){
        solu1[count]=0;
        val1 = BestPlan(solu1,rm_vol,rm_items+1,count+1);
    }
    else{//put in the item
        solu1[count]=1;
        val1 = rm_items->val + BestPlan(solu1, rm_vol-rm_items->w, rm_items+1, count+1);
    }
    solu2[count]=0;
    val2=BestPlan(solu2,rm_vol,rm_items+1,count+1);

    if (val1>val2){
        for(int i=0;i<10;i++)
            solution[i]=solu1[i];
        delete [] solu1;
        delete [] solu2;
        return val1;
    }
    else{
        for(int i=0;i<10;i++)
            solution[i]=solu2[i];
        delete [] solu1;
        delete [] solu2;
        return val2;
    }
}
// Test code: 
//#include "algorithm.h"
//#include <iostream>
//using namespace std;
//int main()
//{
//    items[0].w=10; items[0].val=3;
//    items[1].w=11; items[1].val=3;
//    items[2].w=12; items[2].val=3;
//    items[3].w=13; items[3].val=3;
//    items[4].w=14; items[4].val=3;
//    items[5].w=15; items[5].val=3;
//    items[6].w=16; items[6].val=3;
//    items[7].w=17; items[7].val=3;
//    items[8].w=18; items[8].val=3;
//    items[9].w=19; items[9].val=3;
//    int solution[10]={0};
//    int max_val=BestPlan(solution, 10, items);
//    cout << "max_val: " << max_value << endl;
//    for ( int i = 0 ; i < 10 ; i++ )
//        cout << solution[i];
//
//    return 0;
//}
//// }}}

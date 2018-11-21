#include<iostream>
#include<set>
#include<vector>
#include<algorithm>

template<class Iter> void output(Iter begin, Iter end)
{
	while (begin != end) {
		std::cout << *begin;
		++begin;
		if (begin == end)
			;
		else
			std::cout << " ";
	}
}
int main()
{

	using namespace std;
	int size = 0;
	cin >> size;
	
	set<int> t, f, label;

	int ** m = new int *[size];
	for (int i = 0; i < size; i++) {
		m[i] =  new int[size];
		for (int j = 0; j < size; j++)
			cin >> m[i][j];
	}

	for (int j = 0; j < size; j++) {
		for (int i = 0; i < size; i++) {
			if (m[i][j])
				t.insert(i+1);
			else
				f.insert(i+1);
		}
		set<int> temp;
		vector<int> result;
		if (t.size() > f.size())
			temp = t;
		else
			temp = f;
		if (!label.empty()) {
			set_intersection(label.begin(),label.end(),temp.begin(),temp.end(),back_inserter(result));
			label = set<int>(result.begin(), result.end());
		}
		else
			label = temp;
		t.clear(); f.clear();
	}
	for (int j = 0; j < size; j++) {
		for (int i = 0; i < size; i++) {
			if ( j == i && m[i][j] == 0)
				label.erase(i+1);
		}
		delete[] m[j];
	}
	output(label.begin(), label.end());


	return 0;	
}

#include<iostream>
#include<vector>
using namespace std;
int noc(vector<int> *c, int cur, long T)
{
	int ret = 1;
	if (T == 0)
		return 1;

	for (auto iter = c[cur].begin(); iter != c[cur].end(); iter++) {
		ret += noc(c, *iter, T-1);
	}


	return ret % 2018;

}
int main()
{
	int n, m;
	cin >> n; cin >> m;
	int *c = new int[m+1];
	for (int i = 1; i <= m; i++) {
		c[i] = 0;
		int ind;
		cin >> ind;
		cin >> c[ind];
	}
	auto *a = new vector<int>[n+1];
	for (int i = 1; i <= n; i++) {
		a[i].push_back(i);
		a[i].push_back(c[i]);
		for (int j = 1; j <= m; j++) {
			if (c[j] == i)
				a[i].push_back(j);
		}
	}


	long T;
	cin >> T;
	cout << noc(a, 1, T);
	

	delete [] c;

	delete [] a;
	return 0;
}

#include<iostream>

int max(int x, int y)
{
	return x > y?x:y;
}
int beibao(int *m, int size, int w)
{
	int ret;
	if (size == 0)
		return 0;
	if (m[0] <= w)
		ret = max(beibao(m+1, size-1, w), beibao(m+1, size-1, w-m[0]) + m[0]);
	else
		ret = beibao(m+1, size-1, w);
	return ret;
}

int main()
{
	int w, n;
	using namespace std;
	cin >> w;
	cin >> n;
	int *m = new int[n];
	for (int i = 0; i < n; i++)
		cin >> m[i];
	 
	cout << w - beibao(m, n, w);
	delete[] m;

}

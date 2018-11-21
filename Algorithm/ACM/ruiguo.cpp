#include<iostream>
using namespace std;
void print(int **m, int n)
{
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			cout << m[i][j] << " ";
		}
		cout <<endl;
	}
	cout << endl;
}
int search_min(int begin, int end, int **juzhen, int n)
{
	int ret = -1;

	if (begin == end) {
		return 65535;
	}

	for (int i = 0; i < n; i++) {
		if (juzhen[begin][i] != 0) {

			// 标记begin为已访问
			for (int j = 0; j <n ; j++)
				juzhen[j][begin] = 0;

			int temp = min(juzhen[begin][i], search_min(i, end, juzhen, n));

			// 恢复标记
			for (int j = 0; j <n ; j++)
				juzhen[j][begin] = juzhen[begin][j];

			if ( ret < temp )
				ret = temp;
		}
	}
	return ret;
}
int main()
{

	int n, m;
	cin >> n; cin >> m;

	int ** juzhen = new int *[n];
	for (int i = 0; i < n; i++) {
		juzhen[i] = new int[n];
		for (int j = 0; j <n ; j++)
			juzhen[i][j] = 0;
	}

	for (int i = 0; i < m; i++) {
		int id1, id2, y;
		cin >> id1; cin >> id2; cin >> y;
		juzhen[id1-1][id2-1] = y;
		juzhen[id2-1][id1-1] = y;
	}

	int q_num;
	cin >> q_num;
	int *ans = new int[q_num];

	for (int i = 0; i < q_num; i++) {
		int begin, end;
		cin >> begin; cin >> end;
		ans[i] = search_min(begin - 1, end - 1, juzhen, n);
	}

	for (int i = 0; i < q_num; i++) {
		// cout << (ans[i] == 65535 ? -1 : ans[i]) << endl;
		cout << ans[i] << endl;
	}



	for (int i = 0; i < n; i++) {
		delete[] juzhen[i];
	}
	delete [] juzhen;
	delete [] ans;
	return 0;
}

#include<iostream>
using namespace std;
int main()
{

	int n, m;
	cin >> n; cin >> m;

	int ** juzhen = new int *[m];
	for (int i = 0; i < m; i++) {
		juzhen[i] = new int[3];
		cin >> juzhen[i][0];
		cin >> juzhen[i][1];
		cin >> juzhen[i][2];
	}

	int q_num;
	cin >> q_num;
	int *ans = new int[q_num];

	for (int i = 0; i < q_num; i++) {
		int begin, end;
		cin >> begin; cin >> end;
	}

	for (int i = 0; i < q_num; i++) {
		// cout << (ans[i] == 65535 ? -1 : ans[i]) << endl;
		cout << ans[i] << endl;
	}



	for (int i = 0; i < m; i++) {
		delete[] juzhen[i];
	}
	delete [] juzhen;
	delete [] ans;
	return 0;
}

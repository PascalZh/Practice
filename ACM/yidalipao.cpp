#include<iostream>
#include<string>

int main()
{
	using namespace std;
	int n, begin, end;
	cin >> n;
	cin >> begin;
	cin >> end;
	int *dp = new int[n];
	int **m = new int *[n];
	for (int i = 0; i < n; i++) {
		m[i] = new int[n];
		for (int j = 0; j < n; j++)
			m[i][j] = n;
		cin >> dp[i];
	}
	if (n == 1) {
		cout << 0;
		return 0;
	}
	for (int i = 0; i < n; i++) {
		if (i + dp[i] <= n - 1) {
			m[i][i + dp[i]] = 1;
		}
		if (i - dp[i] >= 0) {
			m[i][i - dp[i]] = 1;
		}
	}
	for(int k=0;k<n;k++)
		for(int i=0;i<n;i++)
		    for(int j=0;j<n;j++){
			if((m[i][j]>m[i][k]+m[k][j]))
			    m[i][j]=m[i][k]+m[k][j];
		    }
	if (m[begin-1][end-1] == n)
		cout << string("-1");
	else
		cout << m[begin-1][end-1];
	delete[] dp;
	for (int i = 0 ; i <n ; i++)
		delete[] m[i];
	return 0;

}

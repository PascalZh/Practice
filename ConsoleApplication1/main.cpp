#include "stdafx.h"
// stdafx.h must be put in the first line of the file.
// Ԥ����ͷ�ļ���Ŀ����Ϊ�˽�ʡ����ʱ�䡣ֻ����stdafx.h�м��빤���г��õ�ͷ�ļ����ɣ�����<windows.h>��<iostream>�ȣ��Լ�д��ͷ�ļ��Ͳ�Ҫ���������
#include "big_integer.h"


using namespace std;
int main()
{
	BigInteger a;
	cin >> a;
	BigInteger b;
	cin >> b;
	cout << a << b<<endl;
	cout << a + b << endl;

	cin.get();
	return 0;
}
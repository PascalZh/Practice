/*
class A {
public:
	A() { cout << "construct A" << endl; }
	A(const A & b) { cout << "copy construct A" << endl; }
	A(A &&b) { cout << "move construct A" << endl; }
	A & operator=(A && rvl) { cout << "move copy A" << endl; return *this; }
	~A() { cout << "destroy A" << endl; }
};

A returnA(A c)
{
	A d(c);
	return d;// return ʱ�����������move constructor������һ����ʱ�����������ʱ����������c = returnA(b)����ֵ
			//returnʱ�����move constructor��ԭ������ǣ�d���������Ϊ���������٣����Կ��Կ�����һ����ֵ��
}
int main()
{
	A c;
	A b;
	c = returnA(b);// ����ֵ�ֻ����move copy����

	return 0;
}
*/
//last-edit:2017/11/28
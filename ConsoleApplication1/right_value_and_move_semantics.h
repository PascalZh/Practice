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
	return d;// return 时编译器会调用move constructor，生成一个临时变量，这个临时变量即下面c = returnA(b)的右值
			//return时会调用move constructor的原因可能是，d这个变量因为即将被销毁，所以可以看做是一个右值。
}
int main()
{
	A c;
	A b;
	c = returnA(b);// 该右值又会调用move copy函数

	return 0;
}
*/
//last-edit:2017/11/28
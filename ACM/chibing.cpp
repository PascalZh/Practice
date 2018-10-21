#include<iostream>
#include<string>
#include<vector>
#include<sstream>

using namespace std;
class LLL {
private:
	struct Node { int d; Node *next; Node(int n) :d(n), next(nullptr){};
	Node() :d(0), next(nullptr) {}} *h;
public:
	LLL(int n) {
		if (n >= 10000)
			throw;
		this->h = new Node(n);}
	LLL(Node *n) :h(n) {}
	Node *getNode() {return h;}
	~LLL() { this-> destroy();}
	void destroy() { 
		auto head = this -> h;
		while (head != nullptr) {
			auto tmp = head;
			head = tmp -> next;
			delete tmp;
		}
	}
	const LLL operator+(const LLL &r) {
		Node *res = new Node();
		Node *head = res;
		auto rNode = r.h;
		auto lNode = this->getNode();
		int carry = 0;
		while (rNode != nullptr && lNode != nullptr) {
			int sum = rNode->d + lNode->d + carry;
			head -> d = sum % 10000;
			carry = sum / 10000;

			if (rNode -> next == nullptr || lNode -> next == nullptr) {
				rNode = rNode -> next;
				lNode = lNode -> next;
			}
			else {
				head -> next = new Node();
				head = head -> next;
				rNode = rNode -> next;
				lNode = lNode -> next;
			}
		}
		Node *remaining = nullptr;
		if (rNode != nullptr)
			remaining = rNode;
		else if (lNode != nullptr)
			remaining = lNode;
		while (remaining != nullptr) {
			int sum = remaining->d + carry;
			head -> next = new Node(sum % 10000);
			carry = sum / 10000;

			remaining = remaining -> next;
			head = head -> next;
		}
		//cout << "carry: " << carry << endl;
		if (carry != 0)
			head -> next = new Node(carry);
		return LLL(res);
	}
	LLL& operator =(const LLL &r)
	{
		this -> destroy();
		return copy(r);
	}
	LLL &copy(const LLL &r) {
		auto ln = r.h;

		this->h = new Node(ln->d);
		auto head = h;
		ln = ln -> next;
		while(ln != nullptr) {
			head->next = new Node(ln->d);
			head = head->next;
			ln = ln -> next;
		}
		return *this;
	}
	LLL(const LLL &r) { copy(r);}
	friend ostream &operator<<(ostream &o, LLL &);

};

ostream &operator<<(ostream &o, LLL &num)
{
	auto head = num.getNode();
	string str;
	while (head != nullptr) {
		str = to_string(head->d) + str;
		head = head->next;
	}
	cout << str;

}




LLL rec(int n)
{
	LLL a = 0; LLL b = 1;
	// cout << a;
	for (int i = 0; i < n; i++){
		LLL tmp = b;
		b = a + b;
		a = tmp;
	}
	return b;
}
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
	int x;
	vector<LLL> res;
	string str;
	getline(cin, str);
	stringstream ss;
	ss << str;
	while (ss >> x) {
		res.push_back(rec(x));
	}
	output(res.begin(), res.end());



	return 0;
}

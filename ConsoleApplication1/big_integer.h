#ifndef _BIG_INTEGER_H
#define _BIG_INTEGER_H

/*
@date:2017/11/5
*/
class BigInteger {
private:
	struct _node {
		short number;
		_node *next;
		_node(const _node *rvl) :number(rvl->number), next(rvl->next) {}
		_node(const short &n, _node *ne = NULL) :number(n), next(ne) {}
		_node():next(NULL){}
	};
	_node *head;
	void clear();
public:
	BigInteger() { head = NULL; }
	BigInteger(const short &n) { head = new _node(n); }
	BigInteger(const BigInteger &);

	~BigInteger() { clear(); }

	BigInteger &operator=(const BigInteger&);
	BigInteger operator+(const BigInteger&);
	
	friend std::ostream & operator<<(std::ostream &, const BigInteger &);
	friend std::istream & operator>>(std::istream &, BigInteger &);
};
void BigInteger::clear() {
	_node *p;
	while (head != NULL) {
		p = head;
		head = head->next;
		delete p;
	}
}
BigInteger::BigInteger(const BigInteger & rvl) {

	head = new _node(rvl.head);
	_node *p_this = head;
	_node *p_rvl = rvl.head;
	while (p_rvl->next != NULL) {
		p_this->next = new _node(p_rvl->next);

		p_this = p_this->next;
		p_rvl = p_rvl->next;
	}
}
BigInteger & BigInteger::operator=(const BigInteger & rvl) {
	this->clear();
	
	this->head = new _node(rvl.head);
	_node *p_this = head;
	_node *p_rvl = rvl.head;
	while (p_rvl->next != NULL) {
		p_this->next = new _node(p_rvl->next);

		p_this = p_this->next;
		p_rvl = p_rvl->next;
	}
	return *this;
}
BigInteger BigInteger::operator+(const BigInteger & rvl) {
	BigInteger tmp;//used to return
	short carry; // 'carry' is used to store the carry of every iteration.
	_node *p_rvl = rvl.head;
	_node *p_lvl = this->head;
	_node *p_tmp;

	tmp.head = new _node((p_rvl->number + p_lvl->number) % 10); // initialize the head.
	carry = (p_rvl->number + p_lvl->number - (p_rvl->number + p_lvl->number) % 10)/10;
	p_tmp = tmp.head; // initialize p_tmp

	while (p_rvl->next != NULL && p_lvl->next != NULL) {
		p_tmp->next = new _node((p_rvl->next->number + p_lvl->next->number + carry) % 10);
		carry = ((p_rvl->next->number + p_lvl->next->number + carry) - (p_rvl->next->number + p_lvl->next->number + carry) % 10)/10;

		p_tmp = p_tmp->next;
		p_rvl = p_rvl->next;
		p_lvl = p_lvl->next;
	}
	if (p_rvl->next != NULL) {//if right value has higher bit
		while (p_rvl->next != NULL) {
			p_tmp->next = new _node((p_rvl->next->number + carry) % 10);
			carry = (p_rvl->next->number + carry - (p_rvl->next->number + carry) % 10)/10;

			p_tmp = p_tmp->next;
			p_rvl = p_rvl->next;
		}
	}
	if (p_lvl->next != NULL) {//if left value has higher bit
		while (p_lvl->next != NULL) {
			p_tmp->next = new _node((p_lvl->next->number + carry) % 10);
			carry = (p_lvl->next->number + carry - (p_lvl->next->number + carry) % 10)/10;
			//more smart writing:
			//carry = p_tmp->next->number / 10;

			p_tmp = p_tmp->next;
			p_lvl = p_lvl->next;
		}
	}
	if (carry != 0) { p_tmp->next = new _node(carry); }//!!!Don't forget it

	return tmp;
}
std::ostream &operator<<(std::ostream &output, const BigInteger & x)
{// x is what you want to output
	std::string tmp; //used to invert the number and store it as a string
	BigInteger::_node *p = x.head;
	while (p != NULL) {
		tmp = char(p->number + '0') + tmp;
		p = p->next;
	}
	for (int i = 0; i < tmp.size(); i++)output << tmp[i];
	return output;

}
std::istream & operator>>(std::istream &is, BigInteger &x)
{
	char ch;

	x.clear();

	while ((ch = is.get()) != '\n') { // these codes are amazing!
		x.head = new BigInteger::_node(ch - '0', x.head);
	}
	return is;
}
#endif // !_BIG_INTEGER_H

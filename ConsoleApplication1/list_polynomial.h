
/*
@date:2017/11/5
@author:PascalZh
*/
#ifndef _LIST_POLYNOMIAL_H
#define _LIST_POLYNOMIAL_H

#include <iostream>

using namespace std;
class Polynomial
{//I'll show you the way to use this class
    //First, initiating a object like 'Polynomial a;'
    //call member function inputPolynomial() like 'a.inputPolynomial();'
    //print the result using printPolynomial() like 'a.printPolynomial();'

    //here are two other operations
    //= and + (for example, you could add two polynomials and assign it to another class:'a=b+c;'.
private:
    struct _node
    {
        int exponent,coefficient;
        _node *next;
        _node(int exp, int coeff):exponent(exp),coefficient(coeff),next(NULL){}
		_node(_node *node) {
			if (node == NULL) {
				cout << "can't construct a note with a nullptr!";
				return;
			}
			this->coefficient = node->coefficient;
			this->exponent = node->exponent;
			this->next = NULL;
		}
    };
    _node *head;
	void clear()
	{
		_node *p;
		while (head != NULL)
		{
			p = head->next;
			delete head;
			head = p;
		}
	}
public:
    Polynomial(const Polynomial & tmp);//copy constructor.
    Polynomial(){head=NULL;}
    Polynomial &operator=(const Polynomial &other)//the class with pointer must have special operator= function
    {
        if(this==&other)return *this;//in case of 'x=x'
//deleted:        this->~Polynomial();//destroy the left value
//add:
		this->clear();

        _node *p_left, *p_right;//point respectively to two values for ITERATING.
        
        
		p_right = other.head;

		this->head = new _node(p_right);
		p_left=this->head;	//construct the head
		
        while(p_right->next!=NULL){ //DEBUGLOG: These big error cost me a lot of time...
			p_left->next = new _node(p_right->next);
			
			p_left = p_left->next;
			p_right = p_right->next;
        }
        return *this;
    }
    Polynomial operator+(const Polynomial &right)
    {
        Polynomial tmp=*this;
		if (tmp.head == NULL || right.head == NULL) {
			cout << "the operands of + mustn't be NULL" << endl;
			return Polynomial();
		}
            
		_node *p_tmp = tmp.head;
		_node *p_right = right.head;
        while(p_right!=NULL)// add two polynomials
        {
            p_tmp=tmp.head;// find if has the same coeffient in the tmp
            while(p_tmp!=NULL)
            {
                if(p_tmp->exponent==p_right->exponent){
                    p_tmp->coefficient+=p_right->coefficient;
                    
                    break;//find! add them, and break.
                }
				if (p_tmp->next == NULL) {
					p_tmp->next = new _node(p_right);
					break;//add: don't forget to break when insert new _node to tmp.
				}
                p_tmp=p_tmp->next;
            }
            p_right=p_right->next;
        }
        return tmp;
    }
    void inputPolynomial();
    void printPolynomial();
    ~Polynomial();
};
Polynomial::~Polynomial()
{
	clear();
}
void Polynomial::inputPolynomial()
{
    cout<<"Please input the coefficient and the exponent:"<<endl;
    char tag='1';
    int tmp_exp, tmp_coeff;
    _node *p;

    cin>>tmp_coeff>>tmp_exp;
    head=new _node(tmp_exp, tmp_coeff);
    p=head;
    while(1)
    {
        cout<<"Continue?(1 for true, 0 for false):";
        cin>>tag;
        if(tag=='0')
            break;
        else if(tag=='1')
        {
            cin>>tmp_coeff>>tmp_exp;
            p->next=new _node(tmp_exp,tmp_coeff);
            p=p->next;
        }else{
            cout<<"Wrong input!"<<endl;
        }
        
    }
}
void Polynomial::printPolynomial()
{
    _node *p=this->head;
    cout<<"Here are they!"<<endl;
    while(p!=NULL)
    {
		cout << p->coefficient << "*x^" << p->exponent << endl;
        p=p->next;
    }
    
}
Polynomial::Polynomial(const Polynomial & tmp)
{
    _node *right=tmp.head;
    head = new _node(right);
    
    for(_node *left=head;right->next!=NULL;right=right->next)
    {
        left->next=new _node(right->next);
//add:
		left = left->next;
    }
}

/*
@reflection:
I spend a lot of time to write down this short simple program,
however,unfortunately I spend more than five times work to debug it.
And here are some stupid mistakes I've made:
1.
	```c++
	_node *p=head;
	p=new _node(other);
	```
	I thought it will work at first, however the value of head never change, so you can't add any node in your list.
2.
	WARNING:Never call destructor explicitly!!!
3.
	Figure out how to iterate your list, how to write your 'for' circle and 'while' circle!
	This is the most time-consuming and frequent mistake I've made.
*/
//@finished-date:2017/11/7
#endif
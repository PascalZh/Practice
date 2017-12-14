#pragma once
#include<iostream>
#include<memory>
#include<queue>
#include"tree.h"
//TODO: wait to finish![--
using namespace std;

template <typename Type>
class AVLTree{
private:
	struct Node {
		Type key;
		int h;
		Node *left;
		Node *right;
		Node(const Type &pkey, Node *pleft=NULL, Node *pright=NULL, int ph=1):key(pkey),h(ph),left(pleft),right(pright){}
	};
	Node *root;

public:
	AVLTree():root(NULL){}
	~AVLTree() {
		queue<Node *> node_queue;
		node_queue.push(root);
		Node *cur, *pleft, *pright;
		while (!node_queue.empty()) {
			cur = node_queue.front();
			pleft = cur->left;
			pright = cur->right;

			node_queue.pop();
			delete cur;

			if (pleft != NULL)node_queue.push(pleft);
			if (pright != NULL)node_queue.push(pright);
		}
	}

	bool find(const Type &x) {
		Node *p;
		p = root;
		while (p != NULL) {
			if (x < p->key)
				p = p->left;
			else if (x > p->key)
				p = p->right;
			else
				return true;
		}
		return false;
	}
	void insert(const Type &x) { insert(x, root); }
	void remove(const Type &x) { 
		if (find(x))remove(x, root);
		else cout << "Cant't delete a node that doesn't exist!" << endl;
	}
	void print() { printTree(root); cout << endl; }

private:
	inline int height(Node * &t)const {
		return t == NULL ? 0 : t->h;
	}// this function is for the convenience of get the height of the node when iterating the tree using pointer

	void insert(const Type &x, Node * &t);
	bool remove(const Type &x, Node * &t);
	inline int max(int a, int b) { return (a > b) ? a : b; }

	void LL(Node * &t);
	void RR(Node * &t);
	void RL(Node * &t) {
		LL(t->right); RR(t);
	}
	void LR(Node * &t) {
		RR(t->left); LL(t);
	}
	template<class Node>// 声明模板友元函数的格式
	friend void printTree(Node *);

};

template <typename Type>
void AVLTree<Type>::insert(const Type &x, Node * &t)
{
	if (t == NULL) {//insert if is NULL
		t = new Node(x);
	}
	else if (x > t->key) {//insert in the right tree
		insert(x, t->right);
		if(height(t->right)-height(t->left)==2)//right subtree is 2 higher than left
			if (x > t->right->key)
				RR(t); else RL(t);
	}
	else if (x < t->key) {//insert in the left tree
		insert(x, t->left);
		if(height(t->right)-height(t->left)==-2)//left subtree is 2 higher than left
			if (x < t->left->key)
				LL(t); else LR(t);
	}
	else {
		cout << "Can't insert node that exists already!" << endl;
	}

	t->h = max(height(t->left), height(t->right)) + 1; // recalculate the height of the node.
}

template <typename Type>
bool AVLTree<Type>::remove(const Type &x, Node * &t)
{
	enum enumSubTree { LeftTree, RightTree }subtree;
	bool stop = false;

	if (x > t->key) {
		subtree = RightTree;
		stop = remove(x, t->right);
	}
	else if (x < t->key) {
		subtree = LeftTree;
		stop = remove(x, t->left);
	}
	else {// x is just the node you want to delete.
		if (t->left != NULL && t->right != NULL) {
			Node *tmp = t->right;//find the min node in the right tree.
			while (tmp->left != NULL)tmp = tmp->left;
			t->key = tmp->key;//move this min node to the t node.
			subtree = RightTree;
			stop = remove(t->key, t->right);
		}
		else {
			Node *oldnode = t;
			t = (t->left != NULL) ? t->left : t->right;
			delete oldnode;
			return false;// spanning of the recursion ends at hear.
		}
	}
	if (stop)return true; // if stop is true, than return it

	// if not, check it.
	int bf = height(t->left) - height(t->right);//balance factor 	
	switch (subtree) {
	case RightTree:
		if (bf == 0) return true;
		if (bf == -1) { t->h = max(height(t->left), height(t->right)); return false; }//remove doesn't violate the balance requirement, than modify the height.
		if (bf == 1) {
			int left_sub_bf = height(t->left->left) - height(t->left->right);
			switch (left_sub_bf) {
			case 0:LL(t); return true;//LL,LR... will change the height.
			case 1:LL(t); return false;
			case -1:LR(t); return false;
			default:cout << "Wrong!" << endl; return false;
			}
		}
	case LeftTree:
		if (bf == 0)return true;
		if (bf == 1) { t->h = max(height(t->left), height(t->right)); return false; }
		if (bf == -1) {
			int right_sub_bf = height(t->right->left) - height(t->right->right);
			switch (right_sub_bf) {
			case 0:RR(t); return true;//LL,LR... will change the height.
			case -1:RR(t); return false;
			case 1:RL(t); return false;
			default:cout << "Wrong!" << endl; return false;
			}
		}

	}
}

template <typename Type>
void AVLTree<Type>::LL(Node * &t)
{
	Node *tmp = t;
	t = t->left;
	tmp->left = t->right;
	t->right = tmp;
	
	t->right->h = max(height(t->right->left), height(t->right->right)) + 1;
	t->h = max(height(t->left), height(t->right)) + 1;
}
template <typename Type>
void AVLTree<Type>::RR(Node * &t)
{
	Node *tmp = t;
	t = t->right;
	tmp->right = t->left;
	t->left = tmp;
	
	t->left->h = max(height(t->left->left), height(t->left->right)) + 1;
	t->h = max(height(t->left), height(t->right)) + 1;
}

/* test program
int main()
{
	AVLTree<int> tree;
	int i = 1;
	ifstream fin("input.txt");
	while (true) {
		fin >> i;
		if (i == 0)break;
		tree.insert(i);
	}
	cout << "tree.print():";
	tree.print();
	while (true) {
		fin>>i;
		cout << "find";
		cout << i<<": ";
		if (i == 0)break;
		cout<<(tree.find(i)?"true":"false")<<endl;
	}
	fin.close();
	tree.remove(1);
	tree.remove(2);
	cout << "After removing 1 and 2:" << endl;
	tree.print();

	return 0;
}

*/
#pragma once
#include <iostream>
//There are some common munipulation of the tree.

template<class Node>
void printTree(Node *root_node)// Node must be structure, and have left(Node *), right(Node *), key(Type) tree members.
{
	using namespace std;
	if (root_node != NULL) {
		cout.width(4);
		cout << root_node->key;//��������ڵ����
		printTree(root_node->left);
		printTree(root_node->right);
	}
	else {
		cout.width(4);
		cout << "NULL";//����ƽ���������������������NULL����ô�϶���ĳ���ڵ�����������ӽڵ�ΪNULL
	}
}
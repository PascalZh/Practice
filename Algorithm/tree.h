#pragma once
#include <iostream>
//There are some common munipulation of the tree.

template<class Node>
void printTree(Node *root_node)// Node must be structure, and have left(Node *), right(Node *), key(Type) tree members.
{
	using namespace std;
	if (root_node != NULL) {
		cout.width(4);
		cout << root_node->key;//先序遍历节点输出
		printTree(root_node->left);
		printTree(root_node->right);
	}
	else {
		cout.width(4);
		cout << "NULL";//对于平衡树，如果连续出现两个NULL，那么肯定是某个节点的左右两个子节点为NULL
	}
}
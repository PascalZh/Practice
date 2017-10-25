#include<stdio.h>
#include<string>
#include<iostream>
using namespace std;
#define M 4
typedef struct{
	char ch;
	int weight;
	int left;
	int right;
	int parent;
}HuffmanTreeNode;
typedef struct{
	string code;
	char ch;
}HuffmanCode;

void CreateHuffmanTree(HuffmanTreeNode node[]){

	int left, right;
	int min1, min2;


	for(int j = M;j < 2*M-1;j++){
		min1 = min2 = 3000;
		left = right = 0;
		for(int k = 0;k < j; k++){
			if(node[k].parent == 0){
				if(node[k].weight < min1){
					min2 = min1;
					right = left;
					min1 = node[k].weight;
					left = k;
				}else if(node[k].weight < min2){
					min2 = node[k].weight;
					right = k;
				}
			}
		}	
		node[left].parent = j;
		node[right].parent = j;
		
		node[j].weight = node[left].weight + node[right].weight;
		node[j].left = left; node[j].right = right;
	}
}
void CreateHuffmanCode(HuffmanTreeNode node[], HuffmanCode cd[]) {
	int p; //used to iterate the tree
	for (int i = 0; i < M; i++) {
		printf_s("%c", node[i].ch);
		cd[i].ch = node[i].ch;
		p = node[i].parent;
		while (p != 0) {
			if (node[p].left == i) {
				string temp = "0";
				cd[i].code += temp;
			}
			else {
				string temp2 = "1";
				cd[i].code += temp2;
			}
			p = node[p].parent;
		}
		cout << cd[i].code << endl;
	}
}
int main() {
	HuffmanTreeNode node[2 * M - 1]; HuffmanCode code[M];
	
	printf("Please input the character and the frequency:");
	for(int i = 0;i < M;i++){
		cin >> node[i].ch;
		cin >> node[i].weight;
	}
	
	for(int j = 0;j < 2*M-1;j++){
		node[j].parent = 0;
		node[j].left = 0;
		node[j].right = 0;
	}
	CreateHuffmanTree(node);
	CreateHuffmanCode(node,code);
	return 0;
}

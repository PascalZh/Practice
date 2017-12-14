//@finished-time:2017/11/27
#pragma once
#include <memory>
#include <cmath>
#include <iostream>
#include <string>

class CPriorityQueue {
private:
	std::unique_ptr<int[]> _node; // 堆中每个节点都为一个int类型的数
	int size; // 节点的个数
	int array_size; // 数组的大小
	int _level(int p);
	void _doubleSpace() { 
		std::unique_ptr<int[]> tmp(new int[array_size*2]);
		for (int i = 0; i < array_size; i++) {
			tmp[i] = _node[i];
		}
		array_size *= 2;
		_node.reset();
		_node = std::move(tmp);
	}
	void _findInMaxLayer(int n, int x);
	void _findInMinLayer(int n, int x);
public:
	CPriorityQueue() :_node(std::unique_ptr<int[]>(new int[8])), size(0), array_size(8){};

	int GetMin() const;
	int GetMax() const;
	void DeleteMin();
	void DeleteMax();
	void insert(int n);                                                       
	void PrintHeap();

};
int CPriorityQueue::_level(int x) 
{
	return log(x + 1) / log(2) + 1;
}
int CPriorityQueue::GetMin() const
{
	return _node[0];
}
int CPriorityQueue::GetMax() const
{
	return _node[1] > _node[2] ? _node[1] : _node[2];
}
void CPriorityQueue::insert(int n)
{
	if (size > array_size) //如果数组空间不够
		_doubleSpace();
	if (size == 0) {
		_node[size] = n;
		size++;
		return;
	}

	int parent = (size+1) / 2 - 1; // 双亲节点的标号
	int minormax = _level(size) % 2; // 为0则在最大层，为1则在最小层
	switch (minormax)
	{
	case 0://最大层
		if (_node[parent] > n) {
			_node[size] = _node[parent];
			_findInMinLayer(n, parent);
		}
		else {
			_findInMaxLayer(n, size);
		}
		break;
	case 1://最小层
		if (_node[parent] < n) {
			_node[size] = _node[parent];
			_findInMaxLayer(n, parent);
		}
		else
			_findInMinLayer(n, size);
		break;

	}
	size++;
}
void CPriorityQueue::_findInMinLayer(int n, int x)
{
	int parent;
	int current = x;
	parent = (current + 1) / 2 - 1;
	parent = (parent + 1) / 2 - 1;
	for (; parent >= 0;){
		if (_node[parent] > n) {
			_node[current] = _node[parent];
			current = (current + 1) / 2 - 1, current = (current + 1) / 2 - 1;
			parent = (current + 1) / 2 - 1;
			parent = (parent + 1) / 2 - 1;
		}
		else
			break;
	}
	_node[current] = n;
}
void CPriorityQueue::_findInMaxLayer(int n, int x)
{
	int parent;
	int current = x;
	parent = (current + 1) / 2 - 1;
	parent = (parent + 1) / 2 - 1;
	for (; parent >= 0;){
		if (_node[parent] < n) {
			_node[current] = _node[parent];
			current = (current + 1) / 2 - 1, current = (current + 1) / 2 - 1;
			parent = (current + 1) / 2 - 1;
			parent = (parent + 1) / 2 - 1;
		}
		else
			break;
	}
	_node[current] = n;
}
void CPriorityQueue::DeleteMin()
{
	int cur = 0;// 用于标志待交换元素的位置，初始把最后一个元素删除并放入堆的顶部
	int x = _node[--size];// 储存待交换元素
	int son, grandson, minpos, min;
	
#define LEFT_SON(i) ((i+1)*2-1)
#define RIGHT_SON(i) ((i+1)*2)
	while (_level(cur) < _level(size)) {
		if (LEFT_SON(cur) < size) {
			min = _node[LEFT_SON(cur)];// 找到儿子和孙子中最小值
			minpos = LEFT_SON(cur);
		}
		else //没有儿子了，那么退出
			break;

		if (_node[RIGHT_SON(cur)] < min && RIGHT_SON(cur) < size) {
			min = _node[RIGHT_SON(cur)];
			minpos = RIGHT_SON(cur);
		}
		for (int i = 0; i < 4; i++) {
			if (_node[LEFT_SON(LEFT_SON(cur)) + i] < min && LEFT_SON(LEFT_SON(cur)) + i < size) {
				min = _node[LEFT_SON(LEFT_SON(cur)) + i];
				minpos = LEFT_SON(LEFT_SON(cur));
			}
		}
		if (x < min)break;

		_node[cur] = min;
		cur = minpos; //交换最小的

		if (x > _node[(cur + 1) / 2 - 1] && _level(cur) % 2 == 1) { //父节点是否满足最大值要求
			_node[cur] = _node[(cur + 1) / 2 - 1];
			_node[(cur + 1) / 2 - 1] = x;
			x = _node[cur];
		}
	}
	_node[cur] = x;//交换最后一步
}
void CPriorityQueue::DeleteMax()
{
	int cur = _node[1] < _node[2] ? 2 : 1;// 用于标志待交换元素的位置
	int x = _node[--size];// 储存待交换元素
	int son, grandson, maxpos, max;
	
	while (_level(cur) < _level(size)) {
		if (LEFT_SON(cur) < size) {
			max = _node[LEFT_SON(cur)];// 找到儿子和孙子中最大值
			maxpos = LEFT_SON(cur);
		}
		else //没有儿子了，那么退出
			break;

		if (_node[RIGHT_SON(cur)] > max && RIGHT_SON(cur) < size) {
			max = _node[RIGHT_SON(cur)];
			maxpos = RIGHT_SON(cur);
		}
		for (int i = 0; i < 4; i++) {
			if (_node[LEFT_SON(LEFT_SON(cur)) + i] > max && LEFT_SON(LEFT_SON(cur)) + i < size) {
				max = _node[LEFT_SON(LEFT_SON(cur)) + i];
				maxpos = LEFT_SON(LEFT_SON(cur)) + i;
			}
		}
		if (x > max)break;

		_node[cur] = max;
		cur = maxpos; //交换最大的

		if (x < _node[(cur + 1) / 2 - 1] && _level(cur) == 0) { //父节点是否满足最小值要求
			_node[cur] = _node[(cur + 1) / 2 - 1];
			_node[(cur + 1) / 2 - 1] = x;
			x = _node[cur];
		}
	}
	_node[cur] = x;//交换最后一步
}
void CPriorityQueue::PrintHeap()
{
	
	for (int x = 0; x <= _level(size); x++) {
		for (int i = std::pow(2, x - 1) - 1; i <= std::pow(2, x) - 2 && i < size; i++) {
			std::cout.width(2);
			std::cout << _node[i];
		}
		std::cout << std::endl;
	}
}

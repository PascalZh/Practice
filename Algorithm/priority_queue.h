//@finished-time:2017/11/27
#pragma once
#include <memory>
#include <cmath>
#include <iostream>
#include <string>

class CPriorityQueue {
private:
	std::unique_ptr<int[]> _node; // ����ÿ���ڵ㶼Ϊһ��int���͵���
	int size; // �ڵ�ĸ���
	int array_size; // ����Ĵ�С
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
	if (size > array_size) //�������ռ䲻��
		_doubleSpace();
	if (size == 0) {
		_node[size] = n;
		size++;
		return;
	}

	int parent = (size+1) / 2 - 1; // ˫�׽ڵ�ı��
	int minormax = _level(size) % 2; // Ϊ0�������㣬Ϊ1������С��
	switch (minormax)
	{
	case 0://����
		if (_node[parent] > n) {
			_node[size] = _node[parent];
			_findInMinLayer(n, parent);
		}
		else {
			_findInMaxLayer(n, size);
		}
		break;
	case 1://��С��
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
	int cur = 0;// ���ڱ�־������Ԫ�ص�λ�ã���ʼ�����һ��Ԫ��ɾ��������ѵĶ���
	int x = _node[--size];// ���������Ԫ��
	int son, grandson, minpos, min;
	
#define LEFT_SON(i) ((i+1)*2-1)
#define RIGHT_SON(i) ((i+1)*2)
	while (_level(cur) < _level(size)) {
		if (LEFT_SON(cur) < size) {
			min = _node[LEFT_SON(cur)];// �ҵ����Ӻ���������Сֵ
			minpos = LEFT_SON(cur);
		}
		else //û�ж����ˣ���ô�˳�
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
		cur = minpos; //������С��

		if (x > _node[(cur + 1) / 2 - 1] && _level(cur) % 2 == 1) { //���ڵ��Ƿ��������ֵҪ��
			_node[cur] = _node[(cur + 1) / 2 - 1];
			_node[(cur + 1) / 2 - 1] = x;
			x = _node[cur];
		}
	}
	_node[cur] = x;//�������һ��
}
void CPriorityQueue::DeleteMax()
{
	int cur = _node[1] < _node[2] ? 2 : 1;// ���ڱ�־������Ԫ�ص�λ��
	int x = _node[--size];// ���������Ԫ��
	int son, grandson, maxpos, max;
	
	while (_level(cur) < _level(size)) {
		if (LEFT_SON(cur) < size) {
			max = _node[LEFT_SON(cur)];// �ҵ����Ӻ����������ֵ
			maxpos = LEFT_SON(cur);
		}
		else //û�ж����ˣ���ô�˳�
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
		cur = maxpos; //��������

		if (x < _node[(cur + 1) / 2 - 1] && _level(cur) == 0) { //���ڵ��Ƿ�������СֵҪ��
			_node[cur] = _node[(cur + 1) / 2 - 1];
			_node[(cur + 1) / 2 - 1] = x;
			x = _node[cur];
		}
	}
	_node[cur] = x;//�������һ��
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

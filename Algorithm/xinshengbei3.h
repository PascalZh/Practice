#pragma once
#include <iostream>
//finished time: 2017/11/19
//This code is writen for Xinshengbei(a small math modeling competition) when I am a sophomore.
//This is for the third subproblem.
int T[5][5] = {// T矩阵储存各生产线每个组件耗时情况，按原表格构造
		{10, 8, 20, 4, 8},
		{4, 10, 6, 12, 5},
		{22, 13, 4, 10, 8},
		{2, 16, 25, 8, 2},
		{6, 8, 13, 11, 16}
};
int relation[5][5][5] = {};


void MinMax() {
	bool RFirstIsSmaller(int Rx[][1], int R0[][1]);
	int GetFinishTime(int, bool [][5]);

	for (int i = 0; i < 5; i++)
		for (int j = 0; j < 5; j++)
			for (int k = 0; k < 5; k++)
				relation[i][j][k] = 0;
	relation[1][0][0] = 1; relation[3][0][2] = 1; relation[4][0][1] = 1; relation[4][0][3] = 1;
	relation[1][1][0] = 1; relation[2][1][1] = 1; relation[3][1][1] = 1; relation[4][1][2] = 1; relation[4][1][3] = 1;
	relation[1][2][0] = 1; relation[2][2][0] = 1; relation[3][2][1] = 1; relation[3][2][2] = 1; relation[4][2][3] = 1;
	relation[1][3][0] = 1; relation[3][3][1] = 1; relation[3][3][2] = 1; relation[4][3][3] = 1;
	relation[2][4][0] = 1; relation[3][4][1] = 1; relation[4][4][2] = 1; relation[4][4][3] = 1;
	bool x[5][5];
	/*	x矩阵如下所示
			生产线A	生产线B	生产线C	生产线D	生产线E
	组件1
	组件2
	组件3
	组件4
	组件5

	*/

	//用来储存最终每个组件在哪个生产线生产，小标表示第i个组件，值表示第x_result[i]条生产线
	int x_result[5];

	for(int g=0;g<5;g++)// set x = 0
		for (int h = 0; h < 5; h++)
			x[g][h] = 0;
		
	int Rx[5][1] = { {0},{0},{0},{0},{0} };//约束条件Rx<=R0
	int R0[5][1] = { {3},{4},{3},{4},{6} };

	
	int R[5][5][5]={// 第一个五（第一维）表示ABCDE，记得将原表格转置
{
{2,0,1,0,0},
{0,1,0,0,2},
{2,0,2,0,0},
{1,2,0,3,0},
{0,5,0,5,2}
},
{
{0,2,1,0,0},
{2,0,0,1,0},
{1,2,0,0,0},
{0,0,2,0,3},
{0,0,5,2,0}
},
{
{1,2,0,0,1},
{0,2,0,0,0},
{0,0,2,0,0},
{0,0,3,1,1},
{0,0,0,4,3}
},
{
{0,2,1,0,0},
{0,3,0,2,3},
{0,0,2,0,0},
{1,0,0,3,0},
{2,0,0,0,6}
},
{
{0,0,0,2,1},
{1,0,0,0,3},
{0,0,0,3,0},
{0,4,0,1,0},
{2,0,3,0,0}
}
	};
	int t_F[5];
	int Tall;//用于某个方案所需得总时长
	int minT=32767;


	for (int a = 0; a < 5; a++)
		for (int b = 0; b < 5; b++)
			for (int c = 0; c < 5; c++)
				for (int d = 0; d < 5; d++)
					for (int e = 0; e < 5; e++)
					{// initiating x;
						x[0][a] = 1;
						x[1][b] = 1;
						x[2][c] = 1;
						x[3][d] = 1;
						x[4][e] = 1;

						// 初始化为0 
						for (int j = 0; j < 5; j++)
							Rx[j][0] = 0;

						for (int k = 0; k < 5; k++)// 算出Rx
							for (int i = 0; i < 5; i++)
								for (int j = 0; j < 5; j++)
									Rx[i][0] += R[k][i][j] * x[j][k];

						if (RFirstIsSmaller(Rx,R0)) {// 是否满足线性约束条件Rx<=R0
							Tall = 0;
							for (int i = 0; i < 5; i++) {
								t_F[i] = GetFinishTime(i,x);
								if (Tall < t_F[i])
									Tall = t_F[i];//Tall即零件完成时间中最长的
							}
							if (minT > Tall) {//找出总时间（Tall）最小的，以及零件制造信息
								minT = Tall;
								x_result[0] = a;
								x_result[1] = b;
								x_result[2] = c;
								x_result[3] = d;
								x_result[4] = e;
							}
						}

						// 找出minT
						

						x[0][a] = 0;//还原x数组
						x[1][b] = 0;
						x[2][c] = 0;
						x[3][d] = 0;
						x[4][e] = 0;
					}

	// 穷举完毕，打印结果
		for (int i = 0; i < 5; i++)
			std::cout << "零件" << i+1 << ":" << "生产线" << x_result[i]+1 << std::endl;
		std::cout << "最短时间：" << minT << std::endl;
}

bool RFirstIsSmaller(int Rx[][1], int R0[][1]) {
	bool flag = 1;
	for (int i = 0; i < 5; i++)
		if (Rx[i][1] > R0[i][1])
			flag = 0;
	return flag;
}

int GetFinishTime(int i, bool x[][5]) {
	int j;
	for (j = 0; j < 5; j++)
		if (x[i][j] == 1)
			break;// 寻找i零件在哪个生产线生产，结果存在j中

	int tmp_Time=T[j][i];

	for (int k = 0; k < 5; k++) {
		if (relation[i][j][k] == 1)
			if (tmp_Time < T[j][i]+GetFinishTime(k, x))
				tmp_Time = T[j][i]+GetFinishTime(k, x);
	}
	
	return tmp_Time;
}
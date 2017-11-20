#pragma once
#include <iostream>
//finished time: 2017/11/19
//This code is writen for Xinshengbei(a small math modeling competition) when I am a sophomore.
//This is for the third subproblem.
int T[5][5] = {// T���󴢴��������ÿ�������ʱ�������ԭ�����
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
	/*	x����������ʾ
			������A	������B	������C	������D	������E
	���1
	���2
	���3
	���4
	���5

	*/

	//������������ÿ��������ĸ�������������С���ʾ��i�������ֵ��ʾ��x_result[i]��������
	int x_result[5];

	for(int g=0;g<5;g++)// set x = 0
		for (int h = 0; h < 5; h++)
			x[g][h] = 0;
		
	int Rx[5][1] = { {0},{0},{0},{0},{0} };//Լ������Rx<=R0
	int R0[5][1] = { {3},{4},{3},{4},{6} };

	
	int R[5][5][5]={// ��һ���壨��һά����ʾABCDE���ǵý�ԭ���ת��
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
	int Tall;//����ĳ�������������ʱ��
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

						// ��ʼ��Ϊ0 
						for (int j = 0; j < 5; j++)
							Rx[j][0] = 0;

						for (int k = 0; k < 5; k++)// ���Rx
							for (int i = 0; i < 5; i++)
								for (int j = 0; j < 5; j++)
									Rx[i][0] += R[k][i][j] * x[j][k];

						if (RFirstIsSmaller(Rx,R0)) {// �Ƿ���������Լ������Rx<=R0
							Tall = 0;
							for (int i = 0; i < 5; i++) {
								t_F[i] = GetFinishTime(i,x);
								if (Tall < t_F[i])
									Tall = t_F[i];//Tall��������ʱ�������
							}
							if (minT > Tall) {//�ҳ���ʱ�䣨Tall����С�ģ��Լ����������Ϣ
								minT = Tall;
								x_result[0] = a;
								x_result[1] = b;
								x_result[2] = c;
								x_result[3] = d;
								x_result[4] = e;
							}
						}

						// �ҳ�minT
						

						x[0][a] = 0;//��ԭx����
						x[1][b] = 0;
						x[2][c] = 0;
						x[3][d] = 0;
						x[4][e] = 0;
					}

	// �����ϣ���ӡ���
		for (int i = 0; i < 5; i++)
			std::cout << "���" << i+1 << ":" << "������" << x_result[i]+1 << std::endl;
		std::cout << "���ʱ�䣺" << minT << std::endl;
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
			break;// Ѱ��i������ĸ��������������������j��

	int tmp_Time=T[j][i];

	for (int k = 0; k < 5; k++) {
		if (relation[i][j][k] == 1)
			if (tmp_Time < T[j][i]+GetFinishTime(k, x))
				tmp_Time = T[j][i]+GetFinishTime(k, x);
	}
	
	return tmp_Time;
}
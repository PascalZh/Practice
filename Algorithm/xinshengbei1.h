#pragma once
#include <iostream>

//finished time: 2017/11/19
//This code is writen for Xinshengbei(a small math modeling competition) when I am a sophomore.
//This is for the first subproblem.
void MinMax() {
	bool RFirstIsSmaller(int Rx[][1], int R0[][1]);

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

	int T[5][5] = {// T���󴢴��������ÿ�������ʱ�������ԭ�����
		{10, 8, 20, 4, 8},
		{4, 10, 6, 12, 5},
		{22, 13, 4, 10, 8},
		{2, 16, 25, 8, 2},
		{6, 8, 13, 11, 16}
	};
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

	struct Time {
		int maxT;
		int time[5][1];
		Time() :maxT(0) {
			for (int i = 0; i < 5; i++)
				this->time[i][0] = 0;
		}
		Time(const Time &tmp) {
			this->maxT = tmp.maxT;
			for (int i = 0; i < 5; i++)
				this->time[i][0] = tmp.time[i][0];
		}
	};
	Time vectorT; // ��ѭ�������ݴ�ʱ��������ÿ��ֵ��ʾ��������������ʱ�䣩
	Time minT;
	minT.maxT = 32767;// ����Ϊһ���ܴ����
	//������Сʱ��Ľṹ��

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
							// �ҳ����������ߺ�ʱ�
							// for...
							for (int i = 0; i < 5; i++) {// ��ʼ��vectorTΪ��
								vectorT.time[i][0] = 0;
								vectorT.maxT = 0;
							}
							for (int i = 0; i < 5; i++)// �����vectorT
								for (int j = 0; j < 5; j++) {
									vectorT.time[i][0] += T[i][j] * x[j][i];

									//�ҳ�vectorT.maxT�����湤�����ܺ�ʱ
									if (vectorT.maxT < vectorT.time[i][0])
										vectorT.maxT = vectorT.time[i][0];
								}
				
							// �ҳ�minT
							if (minT.maxT > vectorT.maxT) {
								minT = vectorT;//�������Time�ṹ��ĸ��ƹ��캯��
								x_result[0] = a;
								x_result[1] = b;
								x_result[2] = c;
								x_result[3] = d;
								x_result[4] = e;
							}
						}
									
						x[0][a] = 0;//��ԭx����
						x[1][b] = 0;
						x[2][c] = 0;
						x[3][d] = 0;
						x[4][e] = 0;

									
						
					}

	// �����ϣ���ӡ���
	std::cout << "minT.maxT:" << minT.maxT << std::endl;
	for (int i = 0; i < 5; i++)
		std::cout << "������" << i+1 << "������ʱ��" << minT.time[i][0] << std::endl;

		for (int i = 0; i < 5; i++)
			std::cout << "���" << i+1 << ":" << "������" << x_result[i]+1 << std::endl;
}

bool RFirstIsSmaller(int Rx[][1], int R0[][1]) {
	bool flag = 1;
	for (int i = 0; i < 5; i++)
		if (Rx[i][1] > R0[i][1])
			flag = 0;
	return flag;
}

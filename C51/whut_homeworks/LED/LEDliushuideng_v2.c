#include "reg51.h"
#define �� void delay(int time)
#define ���� {int i,j;
#define ������ for(i=0;i<time;i++)
#define �������� for(j=0;j<120;j++);}
#define ���������� int main() {
#define ������������ int i;
#define �������������� P0 = 0x7f;
#define ���������������� while (1) {for (i = 0; i < 8; i++) {
#define ������������������ P0 = ~((~P0) << 1); delay(100);}}}
�� ���� ������ �������� ���������� ������������
�������������� ���������������� ������������������
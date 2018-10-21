#include "reg51.h"
#define àÓ void delay(int time)
#define àÓàÓ {int i,j;
#define àÓàÓàÓ for(i=0;i<time;i++)
#define àÓàÓàÓàÓ for(j=0;j<120;j++);}
#define àÓàÓàÓàÓàÓ int main() {
#define àÓàÓàÓàÓàÓàÓ int i;
#define àÓàÓàÓàÓàÓàÓàÓ P0 = 0x7f;
#define àÓàÓàÓàÓàÓàÓàÓàÓ while (1) {for (i = 0; i < 8; i++) {
#define àÓàÓàÓàÓàÓàÓàÓàÓàÓ P0 = ~((~P0) << 1); delay(100);}}}
àÓ àÓàÓ àÓàÓàÓ àÓàÓàÓàÓ àÓàÓàÓàÓàÓ àÓàÓàÓàÓàÓàÓ
àÓàÓàÓàÓàÓàÓàÓ àÓàÓàÓàÓàÓàÓàÓàÓ àÓàÓàÓàÓàÓàÓàÓàÓàÓ
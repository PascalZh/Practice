#include "reg51.h"
#define 嘤 void delay(int time)
#define 嘤嘤 {int i,j;
#define 嘤嘤嘤 for(i=0;i<time;i++)
#define 嘤嘤嘤嘤 for(j=0;j<120;j++);}
#define 嘤嘤嘤嘤嘤 int main() {
#define 嘤嘤嘤嘤嘤嘤 int i;
#define 嘤嘤嘤嘤嘤嘤嘤 P0 = 0x7f;
#define 嘤嘤嘤嘤嘤嘤嘤嘤 while (1) {for (i = 0; i < 8; i++) {
#define 嘤嘤嘤嘤嘤嘤嘤嘤嘤 P0 = ~((~P0) << 1); delay(100);}}}
嘤 嘤嘤 嘤嘤嘤 嘤嘤嘤嘤 嘤嘤嘤嘤嘤 嘤嘤嘤嘤嘤嘤
嘤嘤嘤嘤嘤嘤嘤 嘤嘤嘤嘤嘤嘤嘤嘤 嘤嘤嘤嘤嘤嘤嘤嘤嘤

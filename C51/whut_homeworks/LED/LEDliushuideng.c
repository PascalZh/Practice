#include <reg51.h>

void delay(int time)
{
	 int i,j;
	for(i=0;i<time;i++)
        for(j=0;j<120;j++);
}
int main() {
int i;
		P0 = 0x7f;
while (1) {for (i = 0; i < 8; i++) {
			P0 = ~((~P0) << 1);
delay(100);
}
}
}
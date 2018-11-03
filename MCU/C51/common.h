#ifndef __COMMON_H___
#define __COMMON_H___

typedef unsigned char u8;
typedef unsigned int u16;

sbit P00 = P0^0;
sbit P01 = P0^1;
sbit P02 = P0^2;
sbit P03 = P0^3;
sbit P04 = P0^4;
sbit P05 = P0^5;
sbit P06 = P0^6;
sbit P07 = P0^7;

sbit P30 = P3^0;
sbit P31 = P3^1;
sbit P32 = P3^2;
sbit P33 = P3^3;
sbit P34 = P3^4;
sbit P35 = P3^5;
sbit P36 = P3^6;
sbit P37 = P3^7;

void delay(u16 i)
{
    while(i--);
}

void delay_s(u8 i)
{
    u8 cnt;
    while(i--)
        for(cnt = 0; cnt < 100; cnt++)
            delay(1060);
}

#endif

#include <stdio.h>

void interchange(int x1, int y1);

void main(){
    int x = 50, y = 70;
    interchange(x,y);   /* Pasaje por valor */
    printf("x = %d, y = %d\n",x,y);
}

void interchange(int x1,int y1){
    int z1;
    z1 = x1;
    x1 = y1;
    y1 = z1;
    printf("interchange: x1 = %d, y1 = %d\n",x1,y1);
}

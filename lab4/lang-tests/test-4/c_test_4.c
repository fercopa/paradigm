#include <stdio.h>

char x[] = "estatico";

void g(void) {
    printf("C tiene alcance: %s\n", x);
}

void f(void) {
    char x[] = "dinamico";
    g();
}

int main(void) {
    f();
    return 0;
}

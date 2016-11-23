#include <stdio.h>
long factorial(int n) {
  if (n == 0)
    return 1;
  else
    return(n * factorial(n-1));
}

int main() {
    int result = factorial(5);
    printf("factorial result es: %d\n", result);
    return 0;
}


#include <stdio.h>

int fib(int n) {
    if (n <= 1) {
        printf("f%i\n", n);
        return n;
    }
    int a = fib(n - 1);
    printf("%i\n", n - 2);
    int b = fib(n - 2);
    printf("%i\n", b);
    return a + b;
}

int main() {
    printf("%i\n", fib(5));
}

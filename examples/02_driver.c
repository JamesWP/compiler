#include <stdio.h>

int foo(int a, int b);
int foomore(int a, int b, int c);


int main(int argc, char* argv[]) {
    printf("Hello world\n");
    printf("Calling foo with 123, 456\n");
    int return_value = foo(123, 456);
    printf("return value is %d\n", return_value);

    int i = 0;
    int j = 3;
    int k = 4;
    for(; i+j+k < 500; i+=1, j+=2, k += 100) {
        printf("Calling foomore with %d, %d, %d\n", i, j, k);
        int return_value = foomore(i,j,k);
        printf("return value is %d\n", return_value);
    }

    return return_value;
}
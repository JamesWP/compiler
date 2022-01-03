#include <stdio.h>

int foo(int a, int b);


int main(int argc, char* argv[]) {
    printf("Hello world\n");
    printf("Calling foo with 123, 456\n");
    int return_value = foo(123, 456);
    printf("return value is %d\n", return_value);

    return return_value == 580? 0: 1; 
}
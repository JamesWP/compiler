#include "examples/io.h"

#define AREA(l, b) (l * b)

int main(int argc, const char* argv) {
    int width = 4;
    int height = 7;
    printf("The area of a rectangle with side lengths %d and %d is %d\n", width, height, AREA(width, height));    

    return 0;
}
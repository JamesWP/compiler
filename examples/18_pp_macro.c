#include "examples/io.h"

#define AREA(l,w) (l*w)

int main(int argc, const char* argv) {
    int width = 4;
    int height = 7;
    printf("The area of a rectangle with side lengths %d and %d\n", width, height);
    printf("is %d\n", AREA(width, height));

    return AREA(width, height) == 28 ? 0: 1;
}

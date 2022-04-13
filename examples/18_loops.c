#include "examples/io.h"

int main(int argc, const char* argv[]) {
    int acc = 0;
    for (int i = 0; i < 10; i+=1) {
        acc += i * 10;
        printf("Val %d\n", acc);
    }

    return 0;
}
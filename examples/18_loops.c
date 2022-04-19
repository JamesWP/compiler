#include "examples/io.h"

int main(int argc, const char* argv[]) {
    int acc = 0;
    for (int i = 0; i < 10; i+=1) {
        acc += i * 10;
        printf("Val %d\n", acc);
    }

    for (acc+=100; acc < 1000; acc+=100) {
        printf("Val %d\n", acc);
    }

    for (;;) {
        printf("Val %d\n", acc);
        return 0;
    }

    return 1;
}
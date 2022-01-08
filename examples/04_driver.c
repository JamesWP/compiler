int foo(int a);

int printf(const char*, ...);

int main(int argc, char* argv[]) {
    printf("Calling Foo\n");

    int return_value = foo(1);
    if (return_value != 1234) {
        return 1;
    }

    return_value = bar(10);

    if (return_value != 10 + 1233 * 2) {
        return 1;
    }

    return 0;
}
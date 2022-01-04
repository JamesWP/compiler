int foo(int a);

int printf(const char*, ...);

int main(int argc, char* argv[]) {
    printf("Calling Foo\n");

    int return_value = foo(1);

    return return_value == 1234 ? 0: 1;
}
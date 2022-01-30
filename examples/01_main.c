int printf(const char *format, ...);

int foo(int a);


int main(int argc, char* argv[]) {
    printf("Hello world\n");
    printf("Calling foo with 123\n");
    int return_value = foo(123);
    printf("return value is %d\n", return_value);


    return return_value;
}
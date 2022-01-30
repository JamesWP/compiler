int printf(const char *format, ...);

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
    while(i+j+k < 500){
        i = i + 1;
        j = j + 2;
        k = k + 100;

        int total = i + j + k;
        printf("Calling foomore with %d, %d, %d\n", i, j, k);
        int return_value = foomore(i,j,k);

        printf("return value is %d\n", return_value);
        if (return_value != total) {
            return 1;
        }
    }

    return 0;
}
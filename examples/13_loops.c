// arg: testing
// arg: testing again

int printf(const char*, ...);
int main(int argc, const char* argv[]) {
    argv = argv+1;

    do {
        const char* arg = argv[0];

        printf("Arg: %s\n", arg);

        argv = argv+1;
    } while (argv[0] != 0);

    return 0;
}
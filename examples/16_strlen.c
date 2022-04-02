long long int strlen( const char *str );

int main(int argc, const char* argv[]) {
    long long int size = sizeof(sizeof(argc));

    if (size != 8) {
        return 1;
    }

    long long int length_of_hello_world = strlen("Hello world!");

    if (length_of_hello_world != 12) {
        return 2;
    }

    return 0;
}
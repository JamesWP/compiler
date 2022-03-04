// arg: testing
// arg: testing again

int printf(const char*, ...);

int main(int argc, const char* argv[]){
    if (argc != 3) {
        return 1;
    }

    printf("Hello %s\n", argv[1]);
    printf("Hello %s\n", argv[2]);

    if(argv[3] != 0) {
        return 2;
    } 

    const char* arg = argv[2];

    if (arg[2] != 115) {
        return 3;
    }
    
    return 0;
}
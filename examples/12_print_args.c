// arg: testing
// arg: testing again

int printf(const char*, ...);

int main(int argc, const char* argv[]){
    printf("Running size test\n");

    if (argc != 3) {
        return 1;
    }

    if (sizeof(argc) != 4) {
        printf("Sizeof argc %ld\n", sizeof(argc));
        return 10;
    }

    printf("Hello %s\n", argv[1]);
    printf("Hello %s\n", argv[2]);

    // +0 is to keep gcc happy
    if (sizeof(argv+0) != 8) {
        return 11;
    }

    if (sizeof(argv[0]) != 8) {
        return 12;
    }

    if(argv[3] != 0) {
        return 20;
    } 

    const char* arg = argv[2];

    if(sizeof(arg[2]) != 1) {
        return 21;
    }

    char v = arg[2];

    if(sizeof(v) != 1) {
        return 22;
    }

    if (v != 115) {
        return 30;
    }
    
    return 0;
}
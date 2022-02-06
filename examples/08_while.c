int printf(const char*, ...);

int main(int argc, const char* argv[]) {
    int i = 0;
    int total = 0;
    while(i < 10) {
        i += 1;
        total += 100;
    }

    if (total == 1000) {
        return 0;
    }
    return 1;
}
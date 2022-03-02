int main(int argc, const char* argv[]) {
    int i = -1;

    if (i + 1 != 0) {
        return 1;
    }

    if (-i != 1) {
        return 2;
    }

    if (-1 != -1) {
        return 3;
    }

    if (i != -1) {
        return 4;
    }

    return 0;
}
int thing(int a, int b) {
    if (a == b) {
        return 1;
    }

    if (a == 3) {
        return 0;
    }
}

int bval(int a) {
    return a == 123;
}

int main(int argc, const char* argv[]) {
    int a = 1+2;
    int b = bval(a);
    return thing(a, 1000);
}
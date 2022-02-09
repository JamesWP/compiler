
int test1() {
    int a = 1;
    a = a + 3;
    if (a != 4) { return 1; }
    a += 5;
    if (a != 9) { return 2; }
    a += a;
    if (a != 18) { return 3; }

    return 0;
}


int main(int argc, const char* argv[]) {
    int rc = test1();
    if (rc != 0) { return rc; }

    return 0;
}
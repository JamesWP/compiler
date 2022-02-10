int test() {
    int a = 10;
    {
        int b = a+a+a+a;
        a += 1;
        {
            int b = a+a+a+a;
            int a = 0;
            a += 1;

            b += a;

            a -= 1;
        }
        b += a;
        {
            int b = a+a+a+a;
            int a = 0;
            a += 1;

            b += a;

            a -= 1;
        }
        a -= 1;
    }
    if (a != 10) { return 8; }
    return 0;
}

int main(int argc, const char* argv[]) {
    int rc = test();
    if (rc != 0) { return rc; }
    return 0;
}
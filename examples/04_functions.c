int blah() {
    return 1233;
}

int foo(int a) {
    return blah() + a;
}

int bar(int b) {
    int a = blah();
    int c = blah();
    return a + b + c;
}
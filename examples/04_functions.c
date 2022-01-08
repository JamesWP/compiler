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

int a();

int b() {
    return a();   
}

int a() {
    return 10;
}

int call_with_args() {
    return bar(10);
}

int f() {
    return bar(1+2);
}

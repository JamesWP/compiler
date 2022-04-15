int mux(int a0, int a1, int a2, int a3) {
    return a2;
}
int main(int argc, const char* argv[]) {
    int a = mux(1,2,3,0*0);
    if (a != 3) return 1;
    int b = mux(1,2,3,mux(0,0,0,0));
    if (b != 3) return 2;

    return 0;
}

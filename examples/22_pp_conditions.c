#define A 1

// TODO: 
// #if defined(A)
// #if defined A
// #elif
// #ifdef A

int main(int argc, const char* argv[]) {
#if A
    return 0;
#else
    return 1;
#endif
}
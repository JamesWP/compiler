int puts(const char*);

int main(int argc, char* argv[]) {
    int blah = puts("Hello World!\n");
    return 0;
}

/*
int const*const main (int argc)

declaration_specifier: int const
declarator:
    pointer: * const
    direct_declarator: [main]
        parameter_list: 
            - parameter: int argc


const char* [main]

declaration_specifier: const char
declarator:
    pointer: *
    direct_declarator: [main]



type:
    int: 
        - is_const bool
    char:
        - is_const bool
    function: 
        - return_type type
        - argument_list vec[type]
    pointer:
        - is_const bool
        - inner_type type

*/
// simple test function
int foo(int a) {
    return 0; // test comment
}


/*

translation_unit:
function_declaration:
declaration_specifiers: declarator: compound_statement
type_specifier: declarator: compound_statement
INT: declarator: compound_statement
INT: direct_declarator '(' parameter_type_list ')' : compound_statement
INT: IDENTIFIER: '(' parameter_type_list ')' : compound_statement
INT: IDENTIFIER: '(' parameter_list ')' : compound_statement
INT: IDENTIFIER: '(' parameter_declaration ')' : compound_statement
INT: IDENTIFIER: '(' declaration_specifiers declarator ')' : compound_statement
INT: IDENTIFIER: '(' declaration_specifiers declarator ')' : compound_statement
INT: IDENTIFIER: '(' INT declarator ')' : compound_statement
INT: IDENTIFIER: '(' INT direct_declarator ')' : compound_statement
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : compound_statement
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : compound_statement
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' statement_list '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' statement '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' jump_statement '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN expression ';' '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN additive_expression ';' '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN unary_expression ';' '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN postfix_expression ';' '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN primary_expression ';' '}'
INT: IDENTIFIER: '(' INT IDENTIFIER ')' : '{' RETURN VALUE ';' '}'


*/
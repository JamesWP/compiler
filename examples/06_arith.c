int main(int argc, char* argv[]) {
    
    int a = 1;
    int b = 0 - 9;

    return (a - b - 10);
}

/*
JumpStatement(
ReturnWithValue(
    Expression {
        expr_type: INT(
            TypeQualifier {
                is_const: true,
            },
        ),
        node: Binary(
            Sum,
            Expression {
                expr_type: INT(
                    TypeQualifier {
                        is_const: true,
                    },
                ),
                node: Binary(
                    Quotient,
                    Expression {
                        expr_type: INT(
                            TypeQualifier {
                                is_const: true,
                            },
                        ),
                        node: Binary(
                            Difference,
                            Expression {
                                expr_type: INT(
                                    TypeQualifier {
                                        is_const: false,
                                    },
                                ),
                                node: Value(
                                    Identifier(
                                        "a",
                                    ),
                                ),
                            },
                            Expression {
                                expr_type: INT(
                                    TypeQualifier {
                                        is_const: true,
                                    },
                                ),
                                node: Binary(
                                    Product,
                                    Expression {
                                        expr_type: INT(
                                            TypeQualifier {
                                                is_const: false,
                                            },
                                        ),
                                        node: Value(
                                            Identifier(
                                                "b",
                                            ),
                                        ),
                                    },
                                    Expression {
                                        expr_type: INT(
                                            TypeQualifier {
                                                is_const: true,
                                            },
                                        ),
                                        node: Value(
                                            Literal(
                                                Int32(
                                                    10,
                                                ),
                                            ),
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                    Expression {
                        expr_type: INT(
                            TypeQualifier {
                                is_const: true,
                            },
                        ),
                        node: Value(
                            Literal(
                                Int32(
                                    10,
                                ),
                            ),
                        ),
                    },
                ),
            },
            Expression {
                expr_type: INT(
                    TypeQualifier {
                        is_const: true,
                    },
                ),
                node: Value(
                    Literal(
                        Int32(
                            1,
                        ),
                    ),
                ),
            },
        ),
    },
),
),
                    */
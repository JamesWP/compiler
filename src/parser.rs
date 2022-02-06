use crate::{
    ast::{self, Token, TypeQualifier},
    scope::Scope,
};

pub struct ParserInput {
    tokens: Vec<ast::Token>,
    print_lex: bool,
}

pub struct ParserState {
    input: ParserInput,
    scope: Scope,
}

// TODO: result error should point at error position
type ParseResult<T> = std::result::Result<T, String>;

impl ParserInput {
    fn peek(&self) -> Option<&ast::Token> {
        self.tokens.last()
    }
    fn pop(&mut self) -> Option<ast::Token> {
        let t = self.tokens.pop();
        if self.print_lex {
            if let Some(t) = t {
                println!("[{:?}]", t);
                Some(t)
            } else {
                t
            }
        } else {
            t
        }
    }
    fn expect(&mut self, token: &ast::Token) -> Result<(), String> {
        if self.peek() == Some(token) {
            self.pop();
            Ok(())
        } else {
            Err(format!(
                "expected token {:?} found {:?}",
                token,
                self.peek()
            ))
        }
    }

    pub fn enable_debug(mut self) -> ParserInput {
        self.print_lex = true;
        self
    }
}

impl From<Vec<ast::Token>> for ParserInput {
    fn from(mut val: Vec<ast::Token>) -> ParserInput {
        val.reverse();
        ParserInput { tokens: val, print_lex: false }
    }
}

fn is_type_decl(token: Option<&ast::Token>) -> bool {
    use ast::ResWord::*;
    match token {
        Some(x) => match x {
            Token::Reserved(r) => match r {
                Return => false,
                Int | Char | Const => true,
                If | Else | Do | For | While | Continue | Break => false,
            },
            _ => false,
        },
        None => false,
    }
}

impl ParserState {
    pub fn new(input: ParserInput) -> ParserState {
        ParserState {
            input,
            scope: Scope::default(),
        }
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        if self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Return)) {
            self.input.pop();
            if self.input.peek() == Some(&ast::Token::Semicolon) {
                Ok(ast::Statement::JumpStatement(ast::JumpStatement::Return))
            } else {
                let return_expr = self.parse_expression()?;
                self.input.expect(&ast::Token::Semicolon)?;

                Ok(ast::Statement::JumpStatement(
                    ast::JumpStatement::ReturnWithValue(return_expr),
                ))
            }
        } else if self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::While)) {
            self.input.pop();
            self.input.expect(&ast::Token::Paren('('))?;
            let condition_expression = self.parse_expression()?;
            self.input.expect(&ast::Token::Paren(')'))?;
            let loop_body = Box::new(self.parse_statement()?);

            Ok(ast::Statement::WhileStatement(ast::WhileStatement {
                condition_expression,
                loop_body,
            }))
        } else if self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::If)) {
            self.input.pop();
            self.input.expect(&ast::Token::Paren('('))?;
            let condition_expression = self.parse_expression()?;
            self.input.expect(&ast::Token::Paren(')'))?;
            let if_body = Box::new(self.parse_statement()?);
            let else_body = if self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Else))
            {
                self.input.pop();
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };

            Ok(ast::Statement::IfStatement(ast::IfStatement {
                condition_expression,
                if_body,
                else_body,
            }))
        } else if is_type_decl(self.input.peek()) {
            let base_type = self.parse_declaration_specifiers()?;
            let (name, decl_type) = self.parse_declarator(base_type)?;
            self.scope.define(&name, &decl_type);
            if self.input.peek() == Some(&ast::Token::Semicolon) {
                self.input.pop();
                Ok(ast::Statement::DeclarationStatement(
                    ast::DeclarationStatement::new(decl_type, name),
                ))
            } else {
                self.input.expect(&ast::Token::Equals)?;
                let expression = self.parse_expression()?;
                self.input.expect(&ast::Token::Semicolon)?;
                Ok(ast::Statement::DeclarationStatement(
                    ast::DeclarationStatement::new_with_expression(decl_type, name, expression),
                ))
            }
        } else if self.input.peek() == Some(&ast::Token::Paren('{')) {
            self.scope.begin_scope()?;
            let statement = self.parse_compound_statement()?;
            self.scope.end_scope()?;
            Ok(statement)
        } else {
            let expr = self.parse_expression()?;
            self.input.expect(&ast::Token::Semicolon)?;
            Ok(ast::Statement::Expression(expr))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut equality_expression = self.parse_equality_expression()?;
        loop {
            let op = match self.input.peek() {
                Some(&ast::Token::PlusEquals) => ast::BinOp::Assign(Some(ast::AssignOp::Sum)),
                Some(&ast::Token::MinusEquals) => ast::BinOp::Assign(Some(ast::AssignOp::Difference)),
                Some(&ast::Token::MultiplyEquals) => ast::BinOp::Assign(Some(ast::AssignOp::Product)),
                Some(&ast::Token::DivideEquals) => ast::BinOp::Assign(Some(ast::AssignOp::Quotient)),
                Some(&ast::Token::Equals) => ast::BinOp::Assign(None),
                _ => {
                    return Ok(equality_expression);
                }
            };
            
            self.input.pop();

            let next_equality = self.parse_equality_expression()?;

            equality_expression =
                ast::Expression::new_binop(op, equality_expression.into(), next_equality.into());
        }
    }

    fn parse_equality_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut relational_expression = self.parse_relational_expression()?;

        loop {
            let op = match self.input.peek() {
                Some(&ast::Token::Equality) => ast::BinOp::Equals,
                Some(&ast::Token::NotEquality) => ast::BinOp::NotEquals,
                _ => {
                    return Ok(relational_expression);
                }
            };

            self.input.pop();

            let next_relational = self.parse_relational_expression()?;

            relational_expression = ast::Expression::new_binop(
                op,
                relational_expression.into(),
                next_relational.into(),
            );
        }
    }

    fn parse_relational_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut additive_expression = self.parse_additive_expression()?;

        loop {
            let op = match self.input.peek() {
                Some(&ast::Token::LeftBitShift) => ast::BinOp::LeftBitShift,
                Some(&ast::Token::LessThan) => ast::BinOp::LessThan,
                Some(&ast::Token::RightBitShift) => ast::BinOp::RightBitShift,
                Some(&ast::Token::GreaterThan) => ast::BinOp::GreaterThan,
                _ => {
                    return Ok(additive_expression);
                }
            };

            self.input.pop();

            let next_additive = self.parse_additive_expression()?;

            additive_expression =
                ast::Expression::new_binop(op, additive_expression.into(), next_additive.into());
        }
    }

    fn parse_additive_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut multiplicative_expression = self.parse_multiplicative_expression()?;

        loop {
            let op = match self.input.peek() {
                Some(&ast::Token::Plus) => ast::BinOp::Sum,
                Some(&ast::Token::Minus) => ast::BinOp::Difference,
                _ => {
                    return Ok(multiplicative_expression);
                }
            };

            self.input.pop();

            let next_unary = self.parse_multiplicative_expression()?;

            multiplicative_expression =
                ast::Expression::new_binop(op, multiplicative_expression.into(), next_unary.into());
        }
    }

    fn parse_multiplicative_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut unary_expression = self.parse_unary_expression()?;

        loop {
            let op = match self.input.peek() {
                Some(&ast::Token::Star) => ast::BinOp::Product,
                Some(&ast::Token::Divide) => ast::BinOp::Quotient,
                _ => {
                    return Ok(unary_expression);
                }
            };

            self.input.pop();

            let next_unary = self.parse_unary_expression()?;

            unary_expression =
                ast::Expression::new_binop(op, unary_expression.into(), next_unary.into());
        }
    }

    fn parse_unary_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_postfix_expression()
    }

    fn parse_postfix_expression(&mut self) -> ParseResult<ast::Expression> {
        let value = self.parse_primary_expression()?;
        if self.input.peek() == Some(&ast::Token::Paren('[')) {
            unimplemented!();
        }
        if self.input.peek() != Some(&ast::Token::Paren('(')) {
            return Ok(value);
        }

        // this is a function call. e.g. blah(1,2,3+4)
        let function_expr = value;
        self.input.pop();
        let argument_expressions = self.parse_argument_expression_list()?;
        self.input.expect(&ast::Token::Paren(')'))?;

        Ok(ast::Expression::new_call(
            function_expr.into(),
            argument_expressions,
        ))
    }

    fn parse_primary_expression(&mut self) -> ParseResult<ast::Expression> {
        let (value, expr_type) = match self.input.peek() {
            Some(ast::Token::Value(v)) => {
                let value = *v;
                self.input.pop();
                (
                    ast::Value::Literal(ast::LiteralValue::Int32 { 0: value as i32 }),
                    ast::TypeDefinition::INT(TypeQualifier::from(true)),
                )
            }
            Some(ast::Token::StringLiteral(v)) => {
                let value = v.clone();
                self.input.pop();
                (
                    ast::Value::Literal(ast::LiteralValue::StringLiteral(value)),
                    ast::TypeDefinition::CHAR(TypeQualifier::from(true))
                        .as_pointer_to(TypeQualifier::from(false)),
                )
            }
            Some(ast::Token::Identifier(id)) => {
                let value = id.clone();
                let ident_type = self.scope.find(&value);
                self.input.pop();
                if let Some(type_decl) = ident_type {
                    (ast::Value::Identifier(value), type_decl.clone())
                } else {
                    unimplemented!("variable references undeclared identifier {}", value);
                }
            }
            Some(ast::Token::Paren('(')) => {
                self.input.pop();
                let expr = self.parse_expression()?;
                self.input.expect(&ast::Token::Paren(')'))?;
                return Ok(expr);
            }
            _ => unimplemented!(
                "Unable to parse primary expression. expected token found {:?}",
                self.input.peek()
            ),
        };

        let expr = ast::Expression::new_value(value, expr_type);
        Ok(expr)
    }

    pub fn parse_argument_expression_list(&mut self) -> ParseResult<Vec<ast::Expression>> {
        let mut args = vec![];
        loop {
            if self.input.peek() == Some(&ast::Token::Paren(')')) {
                return Ok(args);
            }

            let expr = self.parse_expression()?;
            args.push(expr);

            if self.input.peek() == Some(&ast::Token::Comma) {
                self.input.pop();
                continue;
            }
        }
    }

    pub fn parse_translation_unit(&mut self) -> ParseResult<ast::TranslationUnit> {
        let mut translation_unit = ast::TranslationUnit::default();
        while self.input.peek().is_some() {
            let func = self.parse_function_definition()?;
            let (name, def) = func;
            translation_unit.add_definition(&name, def);
        }
        Ok(translation_unit)
    }

    fn parse_function_definition(&mut self) -> ParseResult<(String, ast::FunctionDefinition)> {
        let base_type = self.parse_declaration_specifiers()?;
        let (name, decl_type) = self.parse_declarator(base_type)?;

        if let ast::TypeDefinition::FUNCTION(return_type, parameters, _is_local) = decl_type {
            let is_definition = self.input.peek() == Some(&ast::Token::Paren('{'));
            self.scope.define(
                &name,
                &return_type
                    .clone()
                    .as_function_taking(parameters.clone(), is_definition),
            );
            if is_definition {
                self.scope.begin_function_scope()?;
                for (arg_name, arg_type) in parameters.iter() {
                    self.scope.define(arg_name, arg_type);
                }
                let compound_statement = self.parse_compound_statement()?;
                self.scope.end_function_scope()?;
                Ok((
                    name,
                    ast::FunctionDefinition::new(
                        *return_type,
                        parameters.into(),
                        compound_statement,
                    ),
                ))
            } else {
                self.input.expect(&ast::Token::Semicolon)?;
                Ok((
                    name,
                    ast::FunctionDefinition::new_declaration(*return_type, parameters.into()),
                ))
            }
        } else {
            unimplemented!("Nope");
        }
    }

    fn parse_declaration_specifiers(&mut self) -> ParseResult<ast::TypeDefinition> {
        self.parse_type_specifier()
    }

    fn parse_type_specifier(&mut self) -> ParseResult<ast::TypeDefinition> {
        let mut type_word = None;
        let mut is_const = false;
        loop {
            match self.input.peek() {
                Some(ast::Token::Reserved(ast::ResWord::Const)) => {
                    self.input.pop();
                    is_const = true;
                }
                Some(ast::Token::Reserved(ast::ResWord::Char))
                | Some(ast::Token::Reserved(ast::ResWord::Int)) => {
                    if let Some(x) = type_word {
                        unimplemented!("Can't specify type twice, already specified as {:?}", x);
                    }
                    if let Some(ast::Token::Reserved(word)) = self.input.peek() {
                        type_word = Some(word.clone());
                        self.input.pop();
                    } else {
                        unimplemented!("bad code");
                    }
                }
                _ => {
                    break;
                }
            }
        }

        match type_word {
            Some(ast::ResWord::Int) => Ok(ast::TypeDefinition::INT(is_const.into())),
            Some(ast::ResWord::Char) => Ok(ast::TypeDefinition::CHAR(is_const.into())),
            None => Ok(ast::TypeDefinition::default()),
            Some(_) => unimplemented!(),
        }
    }

    fn parse_type_qualifier(&mut self) -> ParseResult<ast::TypeQualifier> {
        let mut is_const = false;
        while self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Const)) {
            self.input.pop();
            is_const = true;
        }

        Ok(ast::TypeQualifier::from(is_const))
    }

    fn parse_declarator(
        &mut self,
        mut base_type: ast::TypeDefinition,
    ) -> ParseResult<(String, ast::TypeDefinition)> {
        // parse pointers
        while self.input.peek() == Some(&ast::Token::Star) {
            self.input.pop();
            let type_qualifier = self.parse_type_qualifier()?;
            base_type = base_type.as_pointer_to(type_qualifier)
        }
        let name = match self.input.peek() {
            Some(ast::Token::Identifier(x)) => {
                let x = x.to_owned();
                self.input.pop();
                x
            }
            _ => return Ok(("".to_owned(), base_type)),
        };

        let result = match self.input.peek() {
            Some(&ast::Token::Paren('(')) => {
                self.input.pop();
                let parameter_list = self.parse_parameter_list()?;

                self.input.expect(&ast::Token::Paren(')'))?;

                base_type.as_function_taking(parameter_list, false)
            }
            Some(&ast::Token::Paren('[')) => {
                self.input.pop();
                self.input.expect(&ast::Token::Paren(']'))?;
                base_type.as_pointer_to(false.into())
            }
            _ => base_type,
        };

        Ok((name, result))
    }

    fn parse_parameter_list(&mut self) -> ParseResult<ast::ParameterList> {
        let mut param_list = Vec::new();

        loop {
            if self.input.peek() == Some(&ast::Token::Paren(')')) {
                break;
            }

            if self.input.peek() == Some(&ast::Token::Elipsis) {
                self.input.pop();
                let param_list: ast::ParameterList = param_list.into();
                return Ok(param_list.with_var_args());
            }
            let base_type = self.parse_declaration_specifiers()?;
            let (name, decl_type) = self.parse_declarator(base_type)?;

            param_list.push((name, decl_type));

            if self.input.peek() == Some(&ast::Token::Comma) {
                self.input.pop().unwrap();
                continue;
            }

            if self.input.peek() == Some(&ast::Token::Paren(')')) {
                break;
            }

            return Err(format!(
                "Expected comma or close paren, instead got {:?}",
                self.input.peek()
            ));
        }

        Ok(param_list.into())
    }

    fn parse_compound_statement(&mut self) -> ParseResult<ast::Statement> {
        self.input.expect(&ast::Token::Paren('{'))?;

        let mut statements = Vec::new();
        while self.input.peek() != Some(&ast::Token::Paren('}')) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        self.input.expect(&ast::Token::Paren('}'))?;

        Ok(statements.into())
    }
}

#[test]
fn test_parse_translation_unit() {
    let input = vec![
        ast::Token::Reserved(ast::ResWord::Int),
        ast::Token::Identifier("foo".to_owned()),
        ast::Token::Paren('('),
        ast::Token::Reserved(ast::ResWord::Int),
        ast::Token::Identifier("i".to_owned()),
        ast::Token::Paren(')'),
        ast::Token::Paren('{'),
        ast::Token::Reserved(ast::ResWord::Return),
        ast::Token::Value(3),
        ast::Token::Semicolon,
        ast::Token::Paren('}'),
    ];

    let parse_result = ParserState::new(input.into()).parse_translation_unit();

    println!("{:#?}", parse_result);
}

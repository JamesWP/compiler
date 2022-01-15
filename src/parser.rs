use crate::ast::{self};

pub struct ParserInput {
    tokens: Vec<ast::Token>,
}

pub struct ParserState {
    input: ParserInput,
}

// TODO: result error should point at error position
type ParseResult<T> = std::result::Result<T, String>;

impl ParserInput {
    fn peek(&self) -> Option<&ast::Token> {
        self.tokens.last()
    }
    fn pop(&mut self) -> Option<ast::Token> {
        let t = self.tokens.pop();
        println!("Token {:?}", t);
        t
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
}

impl From<Vec<ast::Token>> for ParserInput {
    fn from(mut val: Vec<ast::Token>) -> ParserInput {
        val.reverse();
        ParserInput { tokens: val }
    }
}

impl ParserState {
    pub fn new(input: ParserInput) -> ParserState {
        ParserState {
            input
        } 
    }

    #[allow(dead_code)]
    fn parse_statement_list(&mut self) -> ParseResult<ast::CompoundStatement> {
        let mut statements = Vec::new();
        statements.push(self.parse_statement()?);

        loop {
            if Some(&ast::Token::Reserved(ast::ResWord::Return)) != self.input.peek() {
                return Ok(statements.into());
            }

            statements.push(self.parse_statement()?);
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
        } else if self.input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Int)) {
            let base_type = self.parse_declaration_specifiers()?;
            let (name, decl_type) = self.parse_declarator(base_type)?;
            if self.input.peek() == Some(&ast::Token::Semicolon) {
                self.input.pop();
                Ok(ast::Statement::Declaration(ast::DeclarationStatement::new(decl_type, name)))
            } else {
                self.input.expect(&ast::Token::Equals)?;
                let expression = self.parse_expression()?;
                self.input.expect(&ast::Token::Semicolon)?;
                Ok(ast::Statement::Declaration(ast::DeclarationStatement::new_with_expression(decl_type, name, expression)))
            }
        } else {
            let expr = self.parse_expression()?;
            self.input.expect(&ast::Token::Semicolon)?;
            Ok(ast::Statement::Expression(expr))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut unary_expression = self.parse_unary_expression()?;

        loop {
            if Some(&ast::Token::Plus) != self.input.peek() {
                return Ok(unary_expression);
            }

            self.input.pop();

            let next_unary = self.parse_unary_expression()?;

            unary_expression = ast::Expression::Additive {
                0: Box::new(unary_expression),
                1: Box::new(next_unary),
            };
        }
    }

    fn parse_unary_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_postfix_expression()
    }

    fn parse_postfix_expression(&mut self) -> ParseResult<ast::Expression> {
        let value = self.parse_primary_expression()?;
        if self.input.peek() == Some(&ast::Token::Paren('[')) {
            unimplemented!();
        } else if self.input.peek() == Some(&ast::Token::Paren('(')) {
            self.input.pop();
            let argument_expressions = self.parse_argument_expression_list()?;
            self.input.expect(&ast::Token::Paren(')'))?;
            if let ast::Value::Identifier(value) = value {
                Ok(ast::Expression::Call(value, argument_expressions))
            } else {
                Err(format!("Can't call a non identifier."))
            }
        } else {
            Ok(ast::Expression::Unary(value))
        }
    }

    fn parse_primary_expression(&mut self) -> ParseResult<ast::Value> {
        match self.input.peek() {
            Some(ast::Token::Value(v)) => {
                let value = *v;
                self.input.pop();
                Ok(ast::Value::Literal(ast::LiteralValue::Int32 { 0: value as i32 }))
            },
            Some(ast::Token::StringLiteral(v)) => {
                let value = v.clone();
                self.input.pop();
                Ok(ast::Value::Literal(ast::LiteralValue::StringLiteral(value)))
            }
            Some(ast::Token::Identifier(id)) => {
                let value = id.clone();
                self.input.pop();
                Ok(ast::Value::Identifier(value))
            }
            _ => Err(format!("Unable to parse primary expression. expected token found {:?}", self.input.peek())),
        }
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

    fn parse_function_definition(
        &mut self,
    ) -> ParseResult<(String, ast::FunctionDefinition)> {
        let base_type = self.parse_declaration_specifiers()?;
        let (name, decl_type) = self.parse_declarator(base_type)?;

        if let ast::TypeDefinition::FUNCTION(return_type, arguments) = decl_type {
            if self.input.peek() == Some(&ast::Token::Paren('{')) {
                let compound_statement = self.parse_compound_statement()?;
                Ok((
                    name,
                    ast::FunctionDefinition::new(*return_type, arguments.into(), compound_statement),
                ))
            } else {
                self.input.expect(&ast::Token::Semicolon)?;
                Ok((
                    name,
                    ast::FunctionDefinition::new_declaration(*return_type, arguments.into()),
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
                Some(ast::Token::Reserved(ast::ResWord::Char)) |
                Some(ast::Token::Reserved(ast::ResWord::Int)) => {
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
                _ => { break; }
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

    fn parse_declarator(&mut self, mut base_type: ast::TypeDefinition) -> ParseResult<(String, ast::TypeDefinition)> {
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

                base_type.as_function_taking(parameter_list)
            }
            Some(&ast::Token::Paren('[')) => {
                self.input.pop();
                self.input.expect(&ast::Token::Paren(']'))?;
                base_type.as_pointer_to(false.into())
            }
            _ => base_type
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

    fn parse_compound_statement(&mut self) -> ParseResult<ast::CompoundStatement> {
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
    let mut input = vec![
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
#[test]
fn test_parse_statement() {
    let mut input = vec![
        ast::Token::Reserved(ast::ResWord::Return),
        ast::Token::Value(3),
        ast::Token::Semicolon,
    ];

    let parse_result = ParserState::new(input.into()).parse_statement_list();

    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(CompoundStatement { statements: [JumpStatement(ReturnWithValue(Unary(Literal(Int32(3)))))] })"
    );

    let mut input = vec![
        ast::Token::Reserved(ast::ResWord::Return),
        ast::Token::Semicolon,
    ];

    let parse_result = ParserState::new(input.into()).parse_statement_list();

    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(CompoundStatement { statements: [JumpStatement(Return)] })"
    );
}

#[test]
fn test_parse_expression() {
    let mut input = vec![ast::Token::Value(1), ast::Token::Plus, ast::Token::Value(3)];

    let parse_result = ParserState::new(input.into()).parse_expression();

    println!("Parse Result: {:?}", parse_result);
    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(Additive(Unary(Literal(Int32(1))), Unary(Literal(Int32(3)))))"
    );

    let mut input = vec![
        ast::Token::Value(1),
        ast::Token::Plus,
        ast::Token::Value(3),
        ast::Token::Plus,
        ast::Token::Value(10),
    ];

    let parse_result = ParserState::new(input.into()).parse_expression();

    println!("Parse Result: {:?}", parse_result);
    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(Additive(Additive(Unary(Literal(Int32(1))), Unary(Literal(Int32(3)))), Unary(Literal(Int32(10)))))"
    );
}

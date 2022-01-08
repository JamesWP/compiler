use crate::ast;

pub struct ParserInput {
    tokens: Vec<ast::Token>,
}

// TODO: result error should point at error position
type ParseResult<T> = std::result::Result<T, String>;

impl ParserInput {
    fn peek(&self) -> Option<&ast::Token> {
        self.tokens.last()
    }
    fn pop(&mut self) -> Option<ast::Token> {
        self.tokens.pop()
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

#[allow(dead_code)]
fn parse_statement_list(input: &mut ParserInput) -> ParseResult<ast::CompoundStatement> {
    let mut statements = Vec::new();
    statements.push(parse_statement(input)?);

    loop {
        if Some(&ast::Token::Reserved(ast::ResWord::Return)) != input.peek() {
            return Ok(statements.into());
        }

        statements.push(parse_statement(input)?);
    }
}

fn parse_statement(input: &mut ParserInput) -> ParseResult<ast::Statement> {
    
    if input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Return)) {
        input.pop();
        if input.peek() == Some(&ast::Token::Semicolon) {
            Ok(ast::Statement::JumpStatement(ast::JumpStatement::Return))
        } else {
            let return_expr = parse_expression(input)?;
            input.expect(&ast::Token::Semicolon)?;

            Ok(ast::Statement::JumpStatement(
                ast::JumpStatement::ReturnWithValue(return_expr),
            ))
        }
    } else if input.peek() == Some(&ast::Token::Reserved(ast::ResWord::Int)) {
        let declaration_type = parse_declaration_specifiers(input)?;
        let (name, arguments) = parse_declarator(input)?;
        if arguments.iter().next().is_some() {
            unimplemented!("Parameters found in declaration statement");
        }
        if input.peek() == Some(&ast::Token::Semicolon) {
            input.pop();
            Ok(ast::Statement::Declaration(ast::DeclarationStatement::new(declaration_type, name)))
        } else {
            input.expect(&ast::Token::Equals)?;
            let expression = parse_expression(input)?;
            input.expect(&ast::Token::Semicolon)?;
            Ok(ast::Statement::Declaration(ast::DeclarationStatement::new_with_expression(declaration_type, name, expression)))
        }
    } else {
        unimplemented!("Unexpexted parse in parse_statement, found : {:?}", input.peek());
    }
}

fn parse_expression(input: &mut ParserInput) -> ParseResult<ast::Expression> {
    parse_additive_expression(input)
}

fn parse_additive_expression(input: &mut ParserInput) -> ParseResult<ast::Expression> {
    let mut unary_expression = parse_unary_expression(input)?;

    loop {
        if Some(&ast::Token::Plus) != input.peek() {
            return Ok(unary_expression);
        }

        input.pop();

        let next_unary = parse_unary_expression(input)?;

        unary_expression = ast::Expression::Additive {
            0: Box::new(unary_expression),
            1: Box::new(next_unary),
        };
    }
}

fn parse_unary_expression(input: &mut ParserInput) -> ParseResult<ast::Expression> {
    parse_postfix_expression(input)
}

fn parse_postfix_expression(input: &mut ParserInput) -> ParseResult<ast::Expression> {
    let value = parse_primary_expression(input)?;
    if input.peek() == Some(&ast::Token::Paren('[')) {
        unimplemented!();
    } else if input.peek() == Some(&ast::Token::Paren('(')) {
        input.pop();
        let argument_expressions = parse_argument_expression_list(input)?;
        input.expect(&ast::Token::Paren(')'))?;
        if let ast::Value::Identifier(value) = value {
            Ok(ast::Expression::Call(value, argument_expressions))
        } else {
            Err(format!("Can't call a non identifier."))
        }
    } else {
        Ok(ast::Expression::Unary(value))
    }
}

fn parse_primary_expression(input: &mut ParserInput) -> ParseResult<ast::Value> {
    match input.peek() {
        Some(ast::Token::Value(v)) => {
            let value = *v;
            input.pop();
            Ok(ast::Value::Literal(ast::LiteralValue::Int32 { 0: value as i32 }))
        },
        Some(ast::Token::Identifier(id)) => {
            let value = id.clone();
            input.pop();
            Ok(ast::Value::Identifier(value))
        }
        _ => Err(format!("Unable to parse primary expression. expected token found {:?}", input.peek())),
    }
}

pub fn parse_argument_expression_list(input: &mut ParserInput) -> ParseResult<Vec<ast::Expression>> {
    let mut args = vec![];
    loop {
        if input.peek() == Some(&ast::Token::Paren(')')) {
            return Ok(args);
        } 

        let expr = parse_expression(input)?;
        args.push(expr);

        if input.peek() == Some(&ast::Token::Comma) {
            input.pop();
            continue;    
        }
    }
}

pub fn parse_translation_unit(input: &mut ParserInput) -> ParseResult<ast::TranslationUnit> {
    let mut translation_unit = ast::TranslationUnit::default();
    while input.peek().is_some() {
        let func = parse_function_definition(input)?;
        let (name, def) = func;
        translation_unit.add_definition(&name, def);
    }
    Ok(translation_unit)
}

fn parse_function_definition(
    input: &mut ParserInput,
) -> ParseResult<(String, ast::FunctionDefinition)> {
    let declaration_specifiers = parse_declaration_specifiers(input)?;
    let (name, type_list) = parse_declarator(input)?;

    if input.peek() == Some(&ast::Token::Paren('{')) {
        let compound_statement = parse_compound_statement(input)?;
        Ok((
            name,
            ast::FunctionDefinition::new(declaration_specifiers, type_list, compound_statement),
        ))
    } else {
        input.expect(&ast::Token::Semicolon)?;
        Ok((
            name,
            ast::FunctionDefinition::new_declaration(declaration_specifiers, type_list),
        ))
    }

}

fn parse_declaration_specifiers(input: &mut ParserInput) -> ParseResult<ast::TypeDefinition> {
    parse_type_specifier(input)
}

fn parse_type_specifier(input: &mut ParserInput) -> ParseResult<ast::TypeDefinition> {
    input.expect(&ast::Token::Reserved(ast::ResWord::Int))?;
    Ok(ast::BaseType::INT.into())
}

fn parse_declarator(input: &mut ParserInput) -> ParseResult<(String, ast::ParameterList)> {
    let name = match input.peek() {
        Some(ast::Token::Identifier(x)) => {
            let x = x.to_owned();
            input.pop();
            x
        }
        _ => return Err("expected declarator name".to_owned()),
    };

    if input.peek() != Some(&ast::Token::Paren('(')) {
        return Ok((name.to_owned(), Vec::new().into()));
    }

    input.expect(&ast::Token::Paren('('))?;

    let parameter_list = parse_parameter_list(input)?;

    input.expect(&ast::Token::Paren(')'))?;

    Ok((name.to_owned(), parameter_list))
}

fn parse_parameter_list(input: &mut ParserInput) -> ParseResult<ast::ParameterList> {
    let mut param_list = Vec::new();

    loop {
        if input.peek() == Some(&ast::Token::Paren(')')) {
            break;
        }

        let parameter_type = parse_declaration_specifiers(input)?;
        let (name, param_type_list) = parse_declarator(input)?;

        if !param_type_list.is_empty() {
            unimplemented!("Funky function type in param list");
        }

        param_list.push((parameter_type, name));

        if input.peek() == Some(&ast::Token::Comma) {
            input.pop().unwrap();
            continue;
        }

        if input.peek() == Some(&ast::Token::Paren(')')) {
            break;
        }

        return Err(format!(
            "Expected comma or close paren, instead got {:?}",
            input.peek()
        ));
    }

    Ok(param_list.into())
}

fn parse_compound_statement(input: &mut ParserInput) -> ParseResult<ast::CompoundStatement> {
    input.expect(&ast::Token::Paren('{'))?;

    let mut statements = Vec::new();
    while input.peek() != Some(&ast::Token::Paren('}')) {
        let statement = parse_statement(input)?;
        statements.push(statement);
    }

    input.expect(&ast::Token::Paren('}'))?;

    Ok(statements.into())
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

    let parse_result = parse_translation_unit(&mut input.into());

    println!("{:#?}", parse_result);
}
#[test]
fn test_parse_statement() {
    let mut input = vec![
        ast::Token::Reserved(ast::ResWord::Return),
        ast::Token::Value(3),
        ast::Token::Semicolon,
    ];

    let parse_result = parse_statement_list(&mut input.into());

    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(CompoundStatement { statements: [JumpStatement(ReturnWithValue(Unary(Literal(Int32(3)))))] })"
    );

    let mut input = vec![
        ast::Token::Reserved(ast::ResWord::Return),
        ast::Token::Semicolon,
    ];

    let parse_result = parse_statement_list(&mut input.into());

    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(CompoundStatement { statements: [JumpStatement(Return)] })"
    );
}

#[test]
fn test_parse_expression() {
    let mut input = vec![ast::Token::Value(1), ast::Token::Plus, ast::Token::Value(3)];

    let parse_result = parse_expression(&mut input.into());

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

    let parse_result = parse_expression(&mut input.into());

    println!("Parse Result: {:?}", parse_result);
    assert_eq!(
        format!("{:?}", parse_result),
        "Ok(Additive(Additive(Unary(Literal(Int32(1))), Unary(Literal(Int32(3)))), Unary(Literal(Int32(10)))))"
    );
}

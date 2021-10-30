use crate::ast;

struct ParserInput {
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
            Err(format!("expected token {:?} found {:?}", token, self.peek()))
        }
    }
}

impl From<Vec<ast::Token>> for ParserInput {
    fn from(mut val: Vec<ast::Token>) -> ParserInput {
        val.reverse();
        ParserInput { tokens: val }
    }
}

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

fn parse_statement(input:&mut ParserInput) -> ParseResult<ast::Statement> {
    input.expect(&ast::Token::Reserved(ast::ResWord::Return))?;
    if input.peek() == Some(&ast::Token::Semicolon) {
        Ok(ast::Statement::JumpStatement(ast::JumpStatement::Return))
    } else {
        let return_expr = parse_expression(input)?;
        input.expect(&ast::Token::Semicolon)?;

        Ok(ast::Statement::JumpStatement(ast::JumpStatement::ReturnWithValue(return_expr)))
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
    Ok(ast::Expression::Unary(value))
}

fn parse_primary_expression(input: &mut ParserInput) -> ParseResult<ast::LiteralValue> {
    match input.peek() {
        Some(ast::Token::Value(v)) => {
            let value = *v;
            input.pop();
            Ok(ast::LiteralValue::Int32 { 0: value as i32})
        }
        _ => Err("Unable to parse primary expression".to_owned()),
    }
}

#[test]
fn test_parse_statement() {
    let mut input = vec![ast::Token::Reserved(ast::ResWord::Return), ast::Token::Value(3), ast::Token::Semicolon];

    let parse_result = parse_statement_list(&mut input.into());

    println!("Parse Result: {:?}", parse_result);
    assert!(parse_result.is_ok());
    let compound = parse_result.unwrap();
    let statements: Vec<_> = compound.into_iter().collect();
    assert_eq!(statements.len(), 1);
    let statement = statements.get(0).unwrap();
    assert!(matches!(statement, ast::Statement::JumpStatement(_)));

    let mut input = vec![ast::Token::Reserved(ast::ResWord::Return), ast::Token::Semicolon];

    let parse_result = parse_statement_list(&mut input.into());

    println!("Parse Result: {:?}", parse_result);
    assert!(parse_result.is_ok());
    let compound = parse_result.unwrap();
    let statements: Vec<_> = compound.into_iter().collect();
    assert_eq!(statements.len(), 1);
    let statement = statements.get(0).unwrap();
    assert!(matches!(statement, ast::Statement::JumpStatement(_)));
}

#[test]
fn test_parse_expression() {
    let mut input = vec![ast::Token::Value(1), ast::Token::Plus, ast::Token::Value(3)];

    let parse_result = parse_expression(&mut input.into());

    println!("Parse Result: {:?}", parse_result);
    assert!(parse_result.is_ok());
    assert!(matches!(
        parse_result.unwrap(),
        ast::Expression::Additive(_, _)
    ));

    let mut input = vec![
        ast::Token::Value(1),
        ast::Token::Plus,
        ast::Token::Value(3),
        ast::Token::Plus,
        ast::Token::Value(10),
    ];

    let parse_result = parse_expression(&mut input.into());

    println!("Parse Result: {:?}", parse_result);
    assert!(parse_result.is_ok());
    assert!(matches!(
        parse_result.unwrap(),
        ast::Expression::Additive(_, _)
    ));
}

use std::iter::Peekable;

use crate::{
    ast,
    lexer::{self, Location},
    stringiter,
};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    location: lexer::Location,
    error: String,
}

pub fn parse_expression<T>(input: T) -> ParseResult<ast::Expression>
where
    T: Iterator<Item = ast::Token>,
{
    unimplemented!();
}

pub struct DummyTokenInput {
    tokens: Vec<ast::Token>,
    pos: usize,
}

impl Iterator for DummyTokenInput {
    type Item = ast::Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.get(self.pos).cloned();
        if None != token {
            self.pos += 1;
        }
        token
    }
}

#[test]
fn test_parse_expr() {
    let tokens = vec![ast::Token::Value(1), ast::Token::Plus, ast::Token::Value(2)];

    let ti = DummyTokenInput { tokens, pos: 0 };
    let result = parse_expression(ti);

    assert!(result.is_ok());
    assert!(if let ast::Expression::Additive(_, _) = result.unwrap() {
        true
    } else {
        false
    });
}

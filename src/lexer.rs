use crate::{
    ast::{Token, TokenType},
    stringiter::StringIter,
};

pub trait CharPeekIt: Iterator<Item = char> {
    fn peek(&mut self) -> Option<char>;
    fn peek_peek(&mut self) -> Option<char>;
}

pub struct Lexer {
    source: Box<dyn CharPeekIt>,
}

impl Lexer {
    #[allow(dead_code)]
    pub fn new_from_string(content: String) -> Lexer {
        Lexer {
            source: Box::new(StringIter::new(content)),
        }
    }

    pub fn new(source: Box<dyn CharPeekIt>, _filename: &str) -> Lexer {
        Lexer { source }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let next = self.next();
            if let Some(t) = next {
                if t.tt == TokenType::EOF {
                    break;
                }
                tokens.push(t);
            }
        }

        tokens
    }

    fn matches(&mut self, c: char) -> bool {
        if let Some(ch) = self.source.peek() {
            let matches = ch == c;
            if matches {
                self.source.next();
            }
            matches
        } else {
            false
        }
    }

    fn is_ident(c: char) -> bool {
        if c.is_alphanumeric() || c == '_' {
            return true;
        }
        false
    }

    fn skip_till_match(&mut self, c: char) -> String {
        let mut lexeme = String::new();
        while let Some(ch) = self.source.peek() {
            if ch == c {
                return lexeme;
            }
            self.source.next();
            lexeme.push(ch);
        }
        unimplemented!("unable to find '{}' before EOF", c)
    }

    fn skip_while_match_fn(&mut self, value: &mut String, f: fn(char) -> bool) {
        while let Some(ch) = self.source.peek() {
            if f(ch) {
                self.source.next();
                value.push(ch);
            } else {
                return;
            }
        }
        return;
    }

    fn next(&mut self) -> Option<Token> {
        let c: Option<char> = self.source.next();

        if None == c {
            return Some(Token { tt: TokenType::EOF });
        }

        let c = c.unwrap();

        let token_type = match c {
            ' ' | '\t' | '\n' | '\r' => {
                return None;
            }
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LSquare,
            ']' => TokenType::RSquare,
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            ':' => TokenType::Colon,
            '?' => TokenType::Question,
            '\'' => {
                let value = self.skip_till_match('\'');
                if self.matches('\'') {
                    TokenType::CharLiteral(value)
                } else {
                    unimplemented!("character literal not closed")
                }
            }
            '\"' => {
                let value = self.skip_till_match('\"');
                if self.matches('\"') {
                    TokenType::StringLiteral(value)
                } else {
                    unimplemented!("string literal not closed")
                }
            }
            '/' => {
                if self.matches('/') {
                    // comment '//'
                    self.skip_till_match('\n');
                    return None;
                }
                if self.matches('*') {
                    // comment '/*'
                    loop {
                        //TODO: handle EOF while searching for end
                        self.skip_till_match('*');
                        if self.matches('*') {
                            if self.matches('/') {
                                break;
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    return None;
                }

                if self.matches('=') {
                    TokenType::DivideEquals
                } else {
                    TokenType::Divide
                }
            }
            '+' => {
                if self.matches('=') {
                    TokenType::PlusEquals
                } else {
                    TokenType::Plus
                }
            }
            '-' => {
                if self.matches('=') {
                    TokenType::MinusEquals
                } else {
                    TokenType::Minus
                }
            }
            '*' => {
                if self.matches('=') {
                    TokenType::MultiplyEquals
                } else {
                    TokenType::Star
                }
            }
            '<' => {
                if self.matches('<') {
                    TokenType::LeftBitShift
                } else {
                    TokenType::LessThan
                }
            }
            '>' => {
                if self.matches('>') {
                    TokenType::RightBitShift
                } else {
                    TokenType::GreaterThan
                }
            }
            '=' => {
                if self.matches('=') {
                    TokenType::Equality
                } else {
                    TokenType::Equals
                }
            }
            '!' => {
                if self.matches('=') {
                    TokenType::NotEquality
                } else {
                    TokenType::Not
                }
            }
            '.' => {
                if self.matches('.') {
                    if self.matches('.') {
                        TokenType::Elipsis
                    } else {
                        unimplemented!("double dot");
                    }
                } else {
                    unimplemented!("single dot");
                }
            }
            c => {
                let mut value = String::new();
                value.push(c);

                if c.is_numeric() {
                    self.skip_while_match_fn(&mut value, char::is_numeric);
                    let value = value
                        .parse::<i64>()
                        .expect("Unable to parse numeric literal");
                    TokenType::Value(value)
                } else if Lexer::is_ident(c) {
                    self.skip_while_match_fn(&mut value, Lexer::is_ident);
                    value.into()
                } else {
                    unimplemented!("unable to lex '{}'", c);
                }
            }
        };

        return Some(Token { tt: token_type });
    }
}

#[test]
fn char() {
    assert!(Lexer::is_ident('i'));
    assert!(Lexer::is_ident('n'));
    assert!(Lexer::is_ident('t'));
    assert!(!Lexer::is_ident('('));
    assert!(!Lexer::is_ident(' '));
}

#[test]
fn test_lexer() {
    let mut lexer = Lexer::new_from_string("Hello World".to_owned());

    let tokens: Vec<_> = lexer.lex();

    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].tt, TokenType::Identifier("Hello".to_owned()));
    assert_eq!(tokens[1].tt, TokenType::Identifier("World".to_owned()));
}

#[test]
fn test_lexer_string_literal() {
    let mut lexer = Lexer::new_from_string("\"Hello\\n\\tWorld\"".to_owned());

    let tokens: Vec<_> = lexer.lex();

    assert_eq!(tokens.len(), 1);

    let ident = match &tokens[0].tt {
        TokenType::StringLiteral(i) => i,
        _ => unreachable!(),
    };
    assert_eq!(ident, "Hello\\n\\tWorld");
}

#[test]
fn test_simple() -> std::io::Result<()> {
    let mut file = std::fs::File::open("examples/01_simple.c")?;

    let mut buf = Vec::new();

    std::io::Read::read_to_end(&mut file, &mut buf)?;

    let content = String::from_utf8(buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    let mut lexer = Lexer::new_from_string(content);

    let tokens: Vec<_> = lexer.lex();

    for token in &tokens {
        println!("Token: {:?}", token);
        //println!("    -: {:?} - {:?}", token.0.start, token.0.end);
    }

    assert_eq!(tokens.len(), 51);

    assert_eq!(tokens[0].tt, TokenType::Int);

    Ok(())
}

use std::io::Read;

use crate::ast::{Token, TokenType};

struct LexerInput {
    source: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
    lines: Vec<String>,
}

impl LexerInput {
    pub fn peek(&self) -> Option<char> {
        self.source.get(self.pos).cloned()
    }

    pub fn next(&mut self) -> char {
        self.pos += 1;
        let c = *self.source.get(self.pos - 1).unwrap();

        match c {
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            _ => {
                self.column += 1;
            }
        }

        c
    }

    pub fn pos(&self) -> (usize, usize, usize) {
        (self.line, self.column, self.pos)
    }

    pub fn text(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }
}

impl From<String> for LexerInput {
    fn from(string: String) -> Self {
        LexerInput {
            source: string.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
            lines: string.lines().map(str::to_owned).collect(),
        }
    }
}

pub struct Lexer {
    source: LexerInput,
    is_bol: bool,
    is_wsep: bool,
}

pub fn lex_file(filename: &str) -> std::io::Result<Vec<Token>> {
    let mut buf = Vec::new();

    std::fs::File::open(filename.clone())?.read_to_end(&mut buf)?;

    let mut lexer = Lexer::new(std::str::from_utf8(&buf).unwrap().to_string(), filename);

    Ok(lexer.lex())
}

impl Lexer {
    pub fn new(source: String, _filename: &str) -> Lexer {
        Lexer {
            source: source.into(),
            is_bol: true,
            is_wsep: false,
        }
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
        let matches = self.source.peek() == Some(c);
        if matches {
            self.source.next();
        }
        matches
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
        if None == self.source.peek() {
            return Some(Token::default());
        }

        let token_start = self.source.pos();
        let is_bol = self.is_bol;
        let is_wsep = self.is_wsep;

        let c = self.source.next();

        let token_type = match c {
            ' ' | '\t' | '\r' => {
                self.is_wsep = true;
                return None;
            }
            '\n' => {
                self.is_bol = true;
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
            '#' => TokenType::Hash,
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

        self.is_bol = false;
        self.is_wsep = false;

        let token_end = self.source.pos();

        return Some(Token {
            tt: token_type,
            token_start,
            token_end,
            is_bol,
            is_wsep,
            token_text: self.source.text(token_start.2, token_end.2),
        });
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
    let mut lexer = Lexer::new("Hello World".to_owned(), "-");

    let tokens: Vec<_> = lexer.lex();

    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].tt, TokenType::Identifier("Hello".to_owned()));
    assert_eq!(tokens[1].tt, TokenType::Identifier("World".to_owned()));
}

#[test]
fn test_lexer_string_literal() {
    let mut lexer = Lexer::new("\"Hello\\n\\tWorld\"".to_owned(), "-");

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

    let mut lexer = Lexer::new(content, "-");

    let tokens: Vec<_> = lexer.lex();

    for token in &tokens {
        println!("Token: {:?}", token);
        //println!("    -: {:?} - {:?}", token.0.start, token.0.end);
    }

    assert_eq!(tokens.len(), 51);

    assert_eq!(tokens[0].tt, TokenType::Int);

    Ok(())
}

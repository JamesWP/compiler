use std::{fmt::Display, num::ParseIntError};

use crate::{
    ast::{Token, TokenType},
    source::{Source, SourceError, SourceFile},
};

struct LexerInput {
    source: SourceFile,
    pos: usize,
    line: usize,
    column: usize,
}

impl LexerInput {
    pub fn peek(&self) -> Option<char> {
        self.source.get(self.pos)
    }

    pub fn next(&mut self) -> char {
        self.pos += 1;
        let c = self.source.get(self.pos - 1).unwrap();

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
        self.source.text(start, end)
    }
}

impl From<SourceFile> for LexerInput {
    fn from(source: SourceFile) -> Self {
        LexerInput {
            source,
            pos: 0,
            line: 1,
            column: 1,
        }
    }
}

impl From<SourceError> for LexError {
    fn from(err: SourceError) -> Self {
        LexError::UnableToReadSource { source_error: err }
    }
}

pub struct Lexer {
    source: LexerInput,
    is_bol: bool,
    is_wsep: bool,
}

pub type LexResult<T> = std::result::Result<T, LexError>;

pub enum LexError {
    UnableToFind {
        the_char: char,
        starting_from: (usize, usize, usize),
        source: SourceFile,
    },
    UnableToReadSource {
        source_error: SourceError,
    },
    UnableToParseInt {
        starting_from: (usize, usize, usize),
        ending_at: (usize, usize, usize),
        message: String,
        source: SourceFile,
    },
    DoubleDot {
        starting_from: (usize, usize, usize),
        source: SourceFile,
    },
    SingleDot {
        starting_from: (usize, usize, usize),
        source: SourceFile,
    },
    UnknownLex {
        the_char: char,
        starting_from: (usize, usize, usize),
        source: SourceFile,
    },
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnableToFind {
                the_char,
                starting_from,
                source,
            } => f.write_fmt(format_args!(
                "Unexpected end of file while searching for '{}'. search started at {}",
                the_char,
                source.pos(*starting_from)
            )),
            Self::UnknownLex {
                the_char,
                starting_from,
                source,
            } => f.write_fmt(format_args!(
                "Unexpected character in source file '{}'. search started at {}",
                the_char,
                source.pos(*starting_from)
            )),
            Self::DoubleDot {
                starting_from,
                source,
            } => f.write_fmt(format_args!(
                "Unexpected double . character in source. search started at {}",
                source.pos(*starting_from)
            )),
            Self::SingleDot {
                starting_from,
                source,
            } => f.write_fmt(format_args!(
                "Unexpected single . character in source. search started at {}",
                source.pos(*starting_from)
            )),
            Self::UnableToReadSource {
                source_error: SourceError { filename, message },
            } => f.write_fmt(format_args!(
                "Unable to read the source file '{}': {}",
                filename, message
            )),
            Self::UnableToParseInt {
                starting_from,
                message,
                source,
                ..
            } => f.write_fmt(format_args!(
                "Unable to parse integer literal {}, {}",
                message,
                source.pos(*starting_from)
            )),
        }
    }
}

impl std::fmt::Debug for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub fn lex_file(filename: &str) -> LexResult<Vec<Token>> {
    let source = Source::new_from_path(filename)?;
    let mut lexer = Lexer::new(source);
    lexer.lex()
}

impl Lexer {
    pub fn new(source: SourceFile) -> Lexer {
        Lexer {
            source: LexerInput::from(source),
            is_bol: true,
            is_wsep: false,
        }
    }

    pub fn lex(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let next = self.next();
            if let Some(t) = next? {
                if t.tt == TokenType::EOF {
                    break;
                }
                tokens.push(t);
            }
        }

        Ok(tokens)
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

    fn skip_till_match(&mut self, c: char) -> LexResult<String> {
        let start = self.source.pos();
        let mut lexeme = String::new();
        while let Some(ch) = self.source.peek() {
            if ch == c {
                return Ok(lexeme);
            }
            self.source.next();
            lexeme.push(ch);
        }

        Err(LexError::UnableToFind {
            the_char: c,
            starting_from: start,
            source: SourceFile::clone(&self.source.source),
        })
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

    fn next(&mut self) -> LexResult<Option<Token>> {
        if None == self.source.peek() {
            return Ok(Some(Token::default()));
        }

        let token_start = self.source.pos();
        let is_bol = self.is_bol;
        let is_wsep = self.is_wsep;

        let c = self.source.next();

        let token_type = match c {
            ' ' | '\t' | '\r' => {
                self.is_wsep = true;
                return Ok(None);
            }
            '\n' => {
                self.is_bol = true;
                return Ok(None);
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
                let value = self.skip_till_match('\'')?;
                if self.matches('\'') {
                    TokenType::CharLiteral(value)
                } else {
                    unreachable!()
                }
            }
            '\"' => {
                let value = self.skip_till_match('\"')?;
                if self.matches('\"') {
                    TokenType::StringLiteral(value)
                } else {
                    unreachable!()
                }
            }
            '/' => {
                if self.matches('/') {
                    // comment '//'
                    self.skip_till_match('\n')?;
                    return Ok(None);
                }
                if self.matches('*') {
                    // comment '/*'
                    loop {
                        //TODO: handle EOF while searching for end
                        self.skip_till_match('*')?;
                        if self.matches('*') {
                            if self.matches('/') {
                                break;
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    return Ok(None);
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
                } else if self.matches('+') {
                    TokenType::PlusPlus
                } else {
                    TokenType::Plus
                }
            }
            '-' => {
                if self.matches('=') {
                    TokenType::MinusEquals
                } else if self.matches('-') {
                    TokenType::MinusMinus
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
                        return Err(LexError::DoubleDot {
                            starting_from: token_start,
                            source: SourceFile::clone(&self.source.source),
                        });
                    }
                } else {
                    return Err(LexError::SingleDot {
                        starting_from: token_start,
                        source: SourceFile::clone(&self.source.source),
                    });
                }
            }
            c => {
                let mut value = String::new();
                value.push(c);

                if c.is_numeric() {
                    self.skip_while_match_fn(&mut value, char::is_numeric);
                    let value = value
                        .parse::<i64>()
                        .map_err(|e| LexError::UnableToParseInt {
                            starting_from: token_start,
                            ending_at: self.source.pos(),
                            message: e.to_string(),
                            source: SourceFile::clone(&self.source.source),
                        })?;
                    TokenType::Value(value)
                } else if Lexer::is_ident(c) {
                    self.skip_while_match_fn(&mut value, Lexer::is_ident);
                    value.into()
                } else {
                    return Err(LexError::UnknownLex {
                        the_char: c,
                        starting_from: token_start,
                        source: SourceFile::clone(&self.source.source),
                    });
                }
            }
        };

        self.is_bol = false;
        self.is_wsep = false;

        let token_end = self.source.pos();

        return Ok(Some(Token {
            tt: token_type,
            token_start,
            token_end,
            is_bol,
            is_wsep,
            token_text: self.source.text(token_start.2, token_end.2),
            hideset: Default::default(),
            source: Some(SourceFile::clone(&self.source.source)),
        }));
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
fn test_lexer() -> LexResult<()> {
    let mut lexer = Lexer::new(Source::new_from_string("Hello World", "-"));

    let tokens: Vec<_> = lexer.lex()?;

    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].tt, TokenType::Identifier("Hello".to_owned()));
    assert_eq!(tokens[1].tt, TokenType::Identifier("World".to_owned()));

    Ok(())
}

#[test]
fn test_lexer_string_literal() -> LexResult<()> {
    let mut lexer = Lexer::new(Source::new_from_string("\"Hello\\n\\tWorld\"", "-"));

    let tokens: Vec<_> = lexer.lex()?;

    assert_eq!(tokens.len(), 1);

    let ident = match &tokens[0].tt {
        TokenType::StringLiteral(i) => i,
        _ => unreachable!(),
    };
    assert_eq!(ident, "Hello\\n\\tWorld");

    Ok(())
}

#[test]
fn test_simple() -> LexResult<()> {
    let source = Source::new_from_path("examples/01_simple.c")?;
    let mut lexer = Lexer::new(source);

    let tokens: Vec<_> = lexer.lex()?;

    for token in &tokens {
        println!("Token: {:?}", token);
    }

    assert_eq!(tokens.len(), 51);

    assert_eq!(tokens[0].tt, TokenType::Int);

    Ok(())
}

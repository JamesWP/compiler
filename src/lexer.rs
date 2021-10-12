use std::convert::TryInto;

use crate::stringiter::StringIter;

pub trait CharPeekIt: Iterator<Item = char> {
    fn peek(&mut self) -> Option<char>;
}

pub struct Lexer {
    source: Box<dyn CharPeekIt>,
    filename: String,
    pos: Pos,
}

#[derive(Clone, Debug)]
pub struct Pos {
    line: u32,
    col: u32,
}

#[derive(Debug)]
pub struct Location {
    filename: String,
    start: Pos,
    end: Pos,
}

/*
    "reserved:int"
    "identifier:foo"
    "paren:("
    "reserved:int"
    "identifier:a"
    "paren:)"
    "paren:{"
    "reserved:return"
    "value:0"
    "semicolon:;"
    "paren:}"
*/
#[derive(Debug, Clone, PartialEq)]
pub enum ResWord {
    Return,
    Int,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    StringLiteral(String),
    Reserved(ResWord),
    Identifier(String),
    Paren(char),
    Value(i64),
    Semicolon,
}

impl Lexer {
    pub fn new_from_string(content: String) -> Lexer {
        let chars = Box::new(StringIter::new(content));
        Lexer {
            source: chars,
            filename: "raw_input_from_string.txt".to_owned(),
            pos: Pos::default(),
        }
    }
}

impl Pos {
    pub fn new(line: u32, col: u32) -> Pos {
        Pos { line, col }
    }

    pub fn advance(&mut self) {
        self.col += 1;
    }
}

impl Default for Pos {
    fn default() -> Pos {
        Pos::new(0, 0)
    }
}

impl Location {
    pub fn new(filename: &str, start: &Pos, end: &Pos) -> Location {
        Location {
            filename: filename.to_owned(),
            start: start.clone(),
            end: end.clone(),
        }
    }
}

pub type Lex = (Location, Token);

impl Lexer {
    fn is_whitespace(c: char) -> bool {
        match c {
            ' ' | '\t' | '\r' | '\n' => true,
            _ => false,
        }
    }

    fn is_alpha(c: char) -> bool {
        match c {
            'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N'
            | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' => true,
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
            | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' => true,
            _ => false,
        }
    }

    fn is_num(c: char) -> bool {
        match c {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
            _ => false,
        }
    }

    fn is_ident(c: char) -> bool {
        if Self::is_whitespace(c) {
            return false;
        }
        if Self::is_alpha(c) || Self::is_num(c) {
            return true;
        }
        match c {
            '_' => true,
            _ => false,
        }
    }

    fn next_character(&mut self, ignore_whitespace: bool) -> Option<char> {
        while let Some(c) = self.source.next() {
            self.pos.advance();
            if Lexer::is_whitespace(c) && ignore_whitespace {
                continue;
            }
            return Some(c);
        }

        None
    }
    fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.source.peek() {
            return Some(c);
        }
        None
    }
}

impl Iterator for Lexer {
    type Item = Lex;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.pos.clone();
        let char = self.next_character(true)?;

        let token = match &char {
            '{' | '}' | '(' | ')' => Token::Paren(char),
            ';' => Token::Semicolon,
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut chars = String::new();
                chars.push(char);

                while let Some(char) = self.peek() {
                    if !Lexer::is_num(char) {
                        break;
                    }
                    chars.push(char);
                    self.next_character(false);
                }

                let value = chars.parse::<i64>();

                if let Ok(value) = value {
                    Token::Value(value)
                } else {
                    eprintln!("Unable to lex Value token from {}", chars);
                    return None;
                }
            }
            _ => {
                let mut chars = String::new();
                chars.push(char);

                while let Some(char) = self.peek() {
                    if Lexer::is_whitespace(char) {
                        break;
                    }
                    if !Lexer::is_ident(char) {
                        break;
                    }
                    chars.push(char);
                    self.next_character(false);
                }
                if chars == "int" {
                    Token::Reserved(ResWord::Int)
                } else if chars == "return" {
                    Token::Reserved(ResWord::Return)
                } else {
                    Token::Identifier(chars)
                }
            }
        };

        let location = Location::new(&self.filename, &start, &self.pos);

        return Some((location, token));
    }
}

#[test]
fn char() {
    assert!(Lexer::is_ident('i'));
    assert!(Lexer::is_ident('n'));
    assert!(Lexer::is_ident('t'));
    assert!(!Lexer::is_ident('('));
}

#[test]
fn test_lexer() {
    let mut lexer = Lexer::new_from_string("Hello World".to_owned());

    let tokens: Vec<_> = lexer.collect();

    assert_eq!(tokens.len(), 6);

    assert_eq!(tokens[0].1, Token::StringLiteral("He".to_owned()));
}

#[test]
fn test_simple() -> std::io::Result<()> {
    let mut file = std::fs::File::open("examples/01_simple.c")?;

    let mut buf = Vec::new();

    std::io::Read::read_to_end(&mut file, &mut buf)?;

    let content = String::from_utf8(buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    let mut lexer = Lexer::new_from_string(content);

    let tokens: Vec<_> = lexer.collect();

    for token in &tokens {
        println!("Token: {:?}", token.1);
        println!("    -: {:?}", token.0);
    }

    let tokens: Vec<_> = tokens.iter().map(|(_, t)| t).cloned().collect();
    assert_eq!(tokens.len(), 11);

    assert_eq!(tokens[0], Token::Reserved(ResWord::Int));

    assert_eq!(
        &[
            Token::Reserved(ResWord::Int),
            Token::Identifier("foo".to_owned()),
            Token::Paren('('),
            Token::Reserved(ResWord::Int),
            Token::Identifier("a".to_owned()),
            Token::Paren(')'),
            Token::Paren('{'),
            Token::Reserved(ResWord::Return),
            Token::Value(0),
            Token::Semicolon,
            Token::Paren('}'),
        ],
        &tokens[..]
    );
    Ok(())
}

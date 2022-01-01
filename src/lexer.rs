use crate::{
    ast::{ResWord, Token},
    stringiter::StringIter,
};

pub trait CharPeekIt: Iterator<Item = char> {
    fn peek(&mut self) -> Option<char>;
    fn peek_peek(&mut self) -> Option<char>;
    fn pos(&self) -> Pos;
}

#[derive(Clone, Debug)]
pub struct Pos {
    line: u32,
    col: u32,
}

impl Pos {
    pub fn new(line: u32, col: u32) -> Pos {
        Pos { line, col }
    }
}

impl Default for Pos {
    fn default() -> Pos {
        Pos::new(0, 0)
    }
}

pub struct Lexer {
    source: Box<dyn CharPeekIt>,
    filename: String,
}

#[derive(Debug)]
pub struct Location {
    filename: String,
    start: Pos,
    end: Pos,
}

impl Lexer {
    #[allow(dead_code)]
    pub fn new_from_string(content: String) -> Lexer {
        Lexer {
            source: Box::new(StringIter::new(content)),
            filename: "raw_input_from_string.txt".to_owned(),
        }
    }

    pub fn new(source: Box<dyn CharPeekIt>, filename: &str) -> Lexer {
        Lexer {
            source,
            filename: filename.to_owned(),
        }
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
    fn is_ident(c: char) -> bool {
        if c.is_alphanumeric() || c == '_' {
            return true;
        }
        false
    }

    fn read_token(&mut self, c: char, fun: fn(char) -> bool) -> String {
        let mut chars = String::new();
        chars.push(c);

        while let Some(c) = self.source.peek() {
            if !fun(c) {
                break;
            }
            chars.push(c);
            self.source.next();
        }

        chars
    }

    fn skip_comments(&mut self) -> Result<(), &'static str> {
        loop {
            self.read_token('0', |c| c.is_whitespace());

            let next = self.source.peek();
            let next_next = self.source.peek_peek();
            match (next, next_next) {
                (Some('/'), Some('*')) => {
                    self.source.next();
                    self.source.next();

                    loop {
                        let next = self.source.peek();
                        let next_next = self.source.peek_peek();

                        if None == next || None == next_next {
                            eprintln!("EOF while lexing comment");
                            return Err("EOF while lexing comment");
                        }

                        if Some('*') == next && Some('/') == next_next {
                            self.source.next();
                            self.source.next();
                            break;
                        }

                        self.source.next();
                    }
                }
                (Some('/'), Some('/')) => loop {
                    let next = self.source.peek();
                    if None == next {
                        eprintln!("EOF while lexing comment");
                        return Err("EOF while lexing comment");
                    }
                    if Some('\n') == next {
                        self.source.next();
                        break;
                    }

                    self.source.next();
                },
                _ => {
                    return Ok(());
                }
            }
        }
    }
}

impl Iterator for Lexer {
    type Item = Lex;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_comments().ok()?;

        let start = self.source.pos();
        let char = self.source.next()?;

        let token = match &char {
            '{' | '}' | '(' | ')' => Token::Paren(char),
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '0'..='9' => {
                let token = self.read_token(char, |c| c.is_numeric());
                let value = token.parse::<i64>();

                if let Ok(value) = value {
                    Token::Value(value)
                } else {
                    eprintln!("Unable to lex Value token from {}", token);
                    return None;
                }
            }
            '/' => Token::Divide,
            '+' => Token::Plus,
            '=' => Token::Equals,
            _ => {
                let token = self.read_token(char, |c| Lexer::is_ident(c));
                if token == "int" {
                    Token::Reserved(ResWord::Int)
                } else if token == "return" {
                    Token::Reserved(ResWord::Return)
                } else {
                    Token::Identifier(token)
                }
            }
        };

        let location = Location::new(&self.filename, &start, &self.source.pos());

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
    let lexer = Lexer::new_from_string("Hello World".to_owned());

    let tokens: Vec<_> = lexer.collect();

    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].1, Token::Identifier("Hello".to_owned()));
    assert_eq!(tokens[1].1, Token::Identifier("World".to_owned()));
}

#[test]
fn test_simple() -> std::io::Result<()> {
    let mut file = std::fs::File::open("examples/01_simple.c")?;

    let mut buf = Vec::new();

    std::io::Read::read_to_end(&mut file, &mut buf)?;

    let content = String::from_utf8(buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    let lexer = Lexer::new_from_string(content);

    let tokens: Vec<_> = lexer.collect();

    for token in &tokens {
        println!("Token: {:?}", token.1);
        println!("    -: {:?} - {:?}", token.0.start, token.0.end);
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

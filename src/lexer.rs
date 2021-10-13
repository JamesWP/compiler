use crate::stringiter::{CharPeekIt, Pos, StringIter};

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

#[derive(Debug, Clone, PartialEq)]
pub enum ResWord {
    Return,
    Int,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    //StringLiteral(String),
    Reserved(ResWord),
    Identifier(String),
    Paren(char),
    Value(i64),
    Semicolon,
}

impl Lexer {
    pub fn new_from_string(content: String) -> Lexer {
        Lexer {
            source: Box::new(StringIter::new(content)),
            filename: "raw_input_from_string.txt".to_owned(),
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
}

impl Iterator for Lexer {
    type Item = Lex;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token('0', |c| c.is_whitespace());

        let start = self.source.pos();
        let char = self.source.next()?;

        let token = match &char {
            '{' | '}' | '(' | ')' => Token::Paren(char),
            ';' => Token::Semicolon,
            '0' ..= '9' => {
                let token = self.read_token(char, |c| c.is_numeric());
                let value = token.parse::<i64>();

                if let Ok(value) = value {
                    Token::Value(value)
                } else {
                    eprintln!("Unable to lex Value token from {}", token);
                    return None;
                }
            }
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

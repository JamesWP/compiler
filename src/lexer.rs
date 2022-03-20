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
#[allow(dead_code)]
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
#[allow(dead_code)]
pub struct Location {
    filename: String,
    start: Pos,
    end: Pos,
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

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(t) = self.next() {
            tokens.push(t);
        }

        tokens
    }

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

    fn next(&mut self) -> Option<Token> {
        self.skip_comments().ok()?;

        let start = self.source.pos();
        let char: char = self.source.next()?;

        let token = match &char {
            '{' | '}' | '(' | ')' | '[' | ']' => Token::Paren(char),
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
            '\'' => {
                let token = self.read_token(char, |c| c != '\'');
                if self.source.next() != Some('\'') {
                    unimplemented!("Expected \' while lexing");
                }
                let token = &token.as_str()[1..];
                let char_value = match token.len() {
                    1 => token.chars().next().unwrap(),
                    _ => todo!("more complex char literal"),
                };

                Token::CharLiteral(char_value)
            }
            '\"' => {
                let token = self.read_token(char, |c| c != '\"');
                if self.source.next() != Some('\"') {
                    unimplemented!("Expected \" while lexing");
                }
                let token = &token.as_str()[1..];
                let token = token.to_owned();
                let token = token.replace("\\n", "\n");
                Token::StringLiteral(token.to_owned())
            }
            ':' => Token::Colon,
            '?' => Token::Question,
            '/' | '+' | '-' | '*' | '<' | '>' | '=' | '!' => {
                let t = char;
                let n = self.source.peek();

                match (t, n) {
                    ('/', Some('=')) => {
                        self.source.next();
                        Token::DivideEquals
                    }
                    ('*', Some('=')) => {
                        self.source.next();
                        Token::MultiplyEquals
                    }
                    ('-', Some('=')) => {
                        self.source.next();
                        Token::MinusEquals
                    }
                    ('+', Some('=')) => {
                        self.source.next();
                        Token::PlusEquals
                    }
                    ('<', Some('<')) => {
                        self.source.next();
                        Token::LeftBitShift
                    }
                    ('>', Some('>')) => {
                        self.source.next();
                        Token::RightBitShift
                    }
                    ('=', Some('=')) => {
                        self.source.next();
                        Token::Equality
                    }
                    ('!', Some('=')) => {
                        self.source.next();
                        Token::NotEquality
                    }
                    ('=', _) => Token::Equals,
                    ('*', _) => Token::Star,
                    ('/', _) => Token::Divide,
                    ('-', _) => Token::Minus,
                    ('+', _) => Token::Plus,
                    ('<', _) => Token::LessThan,
                    ('>', _) => Token::GreaterThan,
                    ('!', _) => Token::Not,
                    _ => unreachable!(),
                }
            }
            '.' => {
                let token = self.read_token(char, |c| c == '.');
                if token == "..." {
                    Token::Elipsis
                } else {
                    unimplemented!("single dot");
                }
            }
            _ => {
                let token = self.read_token(char, Lexer::is_ident);
                if token == "int" {
                    Token::Reserved(ResWord::Int)
                } else if token == "char" {
                    Token::Reserved(ResWord::Char)
                } else if token == "const" {
                    Token::Reserved(ResWord::Const)
                } else if token == "return" {
                    Token::Reserved(ResWord::Return)
                } else if token == "if" {
                    Token::Reserved(ResWord::If)
                } else if token == "else" {
                    Token::Reserved(ResWord::Else)
                } else if token == "while" {
                    Token::Reserved(ResWord::While)
                } else if token == "do" {
                    Token::Reserved(ResWord::Do)
                } else if token == "for" {
                    Token::Reserved(ResWord::For)
                } else if token == "continue" {
                    Token::Reserved(ResWord::Continue)
                } else if token == "break" {
                    Token::Reserved(ResWord::Break)
                } else if token == "sizeof" {
                    Token::Reserved(ResWord::Sizeof)
                } else {
                    Token::Identifier(token)
                }
            }
        };

        // TODO: add to token
        let _location = Location::new(&self.filename, &start, &self.source.pos());

        return Some(token);
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

    let tokens: Vec<_> = lexer.lex();

    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0], Token::Identifier("Hello".to_owned()));
    assert_eq!(tokens[1], Token::Identifier("World".to_owned()));
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

    assert_eq!(tokens[0], Token::Reserved(ResWord::Int));

    Ok(())
}

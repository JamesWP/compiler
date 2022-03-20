use crate::{
    ast::{ResWord, Token},
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
            if let Some(Token::EOF) = next {
                break;
            }
            if let Some(t) = next {
                tokens.push(t);
            }
        }

        tokens
    }

    fn matches(&mut self, c: char) -> bool {
        if Some(c) == self.source.peek() {
            self.next();
            return true;
        }
        return false;
    }

    fn is_ident(c: char) -> bool {
        if c.is_alphanumeric() || c == '_' {
            return true;
        }
        false
    }

    fn skip_till_match(&mut self, c: char) {
        while !self.matches(c) {
            self.source.next();
        }
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

    fn next(&mut self) -> Option<Token> {

        let c: Option<char> = self.source.next();

        if None == c {
            return Some(Token::EOF);
        }

        let c = c.unwrap();

        let token = match c {
            ' ' | '\t' | '\n' => {
                return None;
            }
            '{' => Token::Paren('{'),
            '}' => Token::Paren('}'),
            '(' => Token::Paren('('),
            ')' => Token::Paren(')'),
            '[' => Token::Paren('['),
            ']' => Token::Paren(']'),
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '\'' => {
                let token = self.read_token(' ', |c| c != '\'');
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
                let token = self.read_token(' ', |c| c != '\"');
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
                        if self.matches('/') {
                            break;
                        }
                    }
                    return None;
                }
                
                if self.matches('=') {
                    Token::DivideEquals
                } else {
                    Token::Divide
                }
            }
            '+' => {
                if self.matches('=') {
                    Token::PlusEquals
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.matches('=') {
                    Token::MinusEquals
                } else {
                    Token::Minus
                }
            }
            '*' => {
                if self.matches('=') {
                    Token::MultiplyEquals
                } else {
                    Token::Star
                }
            }
            '<' => {
                if self.matches('<') {
                    Token::LeftBitShift
                } else {
                    Token::LessThan
                }
            }
            '>' => {
                if self.matches('>') {
                    Token::RightBitShift
                } else {
                    Token::GreaterThan
                }
            }
            '=' => {
                if self.matches('=') {
                    Token::Equality
                } else {
                    Token::Equals
                }
            }
            '!' => {
                if self.matches('=') {
                    Token::NotEquality
                } else {
                    Token::Not
                }
            }
            '.' => {
                let token = self.read_token('.', |c| c == '.');
                if token == "..." {
                    Token::Elipsis
                } else {
                    unimplemented!("single dot");
                }
            }
            c => {
                if c.is_numeric() {
                    let token = self.read_token(c, |c| c.is_numeric());
                    let value = token.parse::<i64>();
                    if let Ok(value) = value {
                        Token::Value(value)
                    } else {
                        eprintln!("Unable to lex Value token from {}", token);
                        return None;
                    }
                } else {
                    let token = self.read_token(c, Lexer::is_ident);
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
            }
        };

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

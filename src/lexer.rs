use std::iter::FromIterator;

pub struct Lexer {
    source: Box<dyn std::io::Read>,
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

#[derive(Debug, PartialEq)]
pub enum Token {
    StringLiteral(String),
}

impl Lexer {
    pub fn new_from_string(content: String) -> Lexer {
        let bytes = std::io::Cursor::new(content.bytes().collect::<Vec<_>>());

        Lexer {
            source: Box::new(bytes),
            filename: "raw_input_from_string.txt".to_owned(),
            pos: Pos::default()
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
    fn next_character(&mut self) -> Option<char> {
        let mut buf = [0; 1];
        let num_bytes = self.source.read(&mut buf).ok()?;

        if num_bytes == 0 {
            return None
        }

        self.pos.advance();

        Some(buf[0] as char)
    }
}

impl Iterator for Lexer {
    type Item = Lex;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.pos.clone();

        let mut chars = Vec::new();

        chars.push(self.next_character()?);
        let char2 = self.next_character();
        if let Some(c) = char2 {
            chars.push(c);
        }

        let location = Location::new(&self.filename, &start, &self.pos);

        let literal = String::from_iter(&chars).to_string();

        Some((location, Token::StringLiteral(literal)))
    }
}

#[test]
fn test_lexer() {
    let mut lexer = Lexer::new_from_string("Hello World".to_owned());

    let tokens: Vec<_> = lexer.collect();

    assert_eq!(tokens.len(), 6);


    assert_eq!(tokens[0].1, Token::StringLiteral("He".to_owned()));
}
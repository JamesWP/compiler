pub struct Pos {
    line: u32,
    col: u32,
}

pub struct Location {
    filename: String,
    start: Pos,
    end: Pos,
}

pub enum Token {
    StringLiteral(String),
}

impl Pos {
    pub fn new(line: u32, col: u32) -> Pos {
        Pos { line, col }
    }
}

impl Location {
    pub fn new(filename: &str, start: Pos, end: Pos) -> Location {
        Location { filename: filename.to_owned(), start, end }
    }
}

pub type Lex(Location, Token);



pub fn tokenize(input: &str, filename: &str, line: u32) -> Vec<Lex> {
    let start_col = 0;
    let end_col = start_col + 10;
    let mut tokens = Vec::new();

    tokens.push(
        (
            Location::new(filename, Pos::new(line, start_col), Pos::new(line, end_col)),
            Token::StringLiteral("Tooken".to_owned())
        )
    );

    tokens
}

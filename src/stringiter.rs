pub trait CharPeekIt: Iterator<Item = char> {
    fn peek(&mut self) -> Option<char>;
    fn pos(&self) -> Pos;
}

pub struct StringIter {
    str: String,
    pos: usize,
    line: u32,
    col: u32,
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

impl StringIter {
    pub fn new(str: String) -> StringIter {
        StringIter {
            str,
            pos: 0,
            line: 1,
            col: 0,
        }
    }
}

impl CharPeekIt for StringIter {
    fn peek(&mut self) -> Option<char> {
        if self.str.len() <= self.pos {
            None
        } else {
            let char = self.str.as_bytes()[self.pos] as char;

            Some(char)
        }
    }

    fn pos(&self) -> Pos {
        Pos::new(self.line, self.col)
    }
}

impl Iterator for StringIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.str.len() <= self.pos {
            None
        } else {
            let char = self.str.as_bytes()[self.pos] as char;
            self.pos += 1;
            if !char.is_ascii_control() {
                self.col += 1;
            }
            if char == '\n' {
                self.col = 0;
                self.line += 1;
            }
            Some(char)
        }
    }
}

#[test]
fn stringter_works() {
    let mut iter = StringIter::new("this is a string".to_owned());

    assert_eq!(Some('t'), iter.next());
    assert_eq!(Some('h'), iter.next());
    assert_eq!(Some('i'), iter.next());
}

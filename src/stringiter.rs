use std::str::Chars;

use crate::lexer::CharPeekIt;

pub struct StringIter {
    str: String,
    pos: usize
}

impl StringIter {
    pub fn new(str: String) -> StringIter {
        StringIter {
            str, pos: 0
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
}

impl Iterator for StringIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.str.len() <= self.pos {
            None
        } else {
            let char = self.str.as_bytes()[self.pos] as char;
            self.pos += 1;
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
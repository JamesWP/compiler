use crate::lexer::CharPeekIt;
use std::io::Read;

pub struct FileIter {
    file: std::fs::File,
    line: u32,
    col: u32,
    buff: String,
}

impl FileIter {
    fn fill_buffer(&mut self) {
        // ensure there is at least 2 characters available in the buffer
        if self.buff.len() < 2 {
            assert!(self.buff.len() <= 10);
            let available = self.buff.capacity() - self.buff.len();
            let mut handle = (&mut self.file).take(available as u64);
            handle.read_to_string(&mut self.buff).unwrap();
        }
    }
}

impl From<std::fs::File> for FileIter {
    fn from(file: std::fs::File) -> FileIter {
        FileIter {
            file,
            line: 0,
            col: 0,
            buff: String::with_capacity(10),
        }
    }
}

impl Iterator for FileIter {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.fill_buffer();

        if self.buff.len() == 0 {
            return None;
        }

        let char = self.buff.chars().next().unwrap();

        // TODO: remove horrible hack
        self.buff.remove(0);

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

impl CharPeekIt for FileIter {
    fn peek(&mut self) -> Option<char> {
        self.fill_buffer();

        if self.buff.len() == 0 {
            return None;
        }

        let char = self.buff.chars().next().unwrap();

        Some(char)
    }

    fn peek_peek(&mut self) -> Option<char> {
        self.fill_buffer();

        if self.buff.len() < 2 {
            return None;
        }

        let char = self.buff.chars().nth(1).unwrap();

        Some(char)
    }
}

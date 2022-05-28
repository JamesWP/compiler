#[derive(Debug)]
pub struct SourceError {
    pub filename: String,
    pub message: String,
}
pub type SourceResult = std::result::Result<SourceFile, SourceError>;

pub struct SourceFile(std::rc::Rc<std::boxed::Box<Source>>);

pub struct Source {
    filename: String,
    file: Vec<char>,
    lines: Vec<String>,
}

impl Source {
    pub fn new_from_path(filename: &str) -> SourceResult {
        let mut buf = Vec::new();

        let mut open_file = std::fs::File::open(filename.clone()).map_err(|err| SourceError {
            filename: filename.to_string(),
            message: err.to_string(),
        })?;

        std::io::Read::read_to_end(&mut open_file, &mut buf).map_err(|err| SourceError {
            filename: filename.to_string(),
            message: err.to_string(),
        })?;

        let source = std::str::from_utf8(&buf).unwrap();

        Ok(Self::new_from_string(source, filename))
    }

    pub fn new_from_string(source: &str, filename: &str) -> SourceFile {
        let source = Source {
            filename: String::from(filename),
            file: source.chars().collect(),
            lines: source.lines().map(std::string::String::from).collect(),
        };

        SourceFile(std::rc::Rc::new(std::boxed::Box::new(source)))
    }
}

impl SourceFile {
    pub fn get(&self, pos: usize) -> Option<char> {
        self.0.as_ref().file.get(pos).cloned()
    }

    pub fn text(&self, start: usize, end: usize) -> String {
        self.0.as_ref().file[start..end].iter().collect()
    }

    pub fn pos(&self, p: (usize, usize, usize)) -> String {
        let context = 5;

        // Maths!
        let starting_line = p.0 - context;
        let ending_line = p.0 + context;

        let lines = &self.0.as_ref().lines;
        let context = lines[starting_line..ending_line]
            .iter()
            .enumerate()
            .map(|(number, line)| {
                let number = number + starting_line;
                format!("{number:3} | {line}")
            })
            .collect::<Vec<_>>().join("\n");

        format!("\n{}\n\n{}:{}:{}", context, self.0.as_ref().filename, p.0, p.1)
    }
}

impl Clone for SourceFile {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[cfg(test)]
mod test {
    use super::{Source, SourceError, SourceFile};

    #[test]
    fn test_source_basic() -> std::result::Result<(), SourceError> {
        let filename = "examples/01_simple.c";
        let s1 = Source::new_from_path(filename)?;
        let s2 = SourceFile::clone(&s1);

        assert_eq!(s1.get(10), s2.get(10));
        Ok(())
    }

    #[test]
    fn test_pos() -> std::result::Result<(), SourceError> {
        let filename = "examples/01_simple.c";
        let s1 = Source::new_from_path(filename)?;

        println!("{}",s1.pos((5,10,0)));
        Ok(())
    }
}

pub type SourceError = String;
pub type SourceResult = std::result::Result<SourceFile, SourceError>;

pub struct SourceFile(std::rc::Rc<std::boxed::Box<Source>>);

pub struct Source {
    filename: String,
    file: Vec<char>,
    lines: Vec<String>
}

fn map_io_error(err:std::io::Error) -> String {
    err.to_string()
}

impl Source {
    pub fn new_from_path(filename: &str) -> SourceResult {
        let mut buf = Vec::new();

        let mut open_file = std::fs::File::open(filename.clone()).map_err(map_io_error)?;

        std::io::Read::read_to_end(&mut open_file, &mut buf).map_err(map_io_error)?;

        let source = std::str::from_utf8(&buf).unwrap();

        Ok(Self::new_from_string(source, filename))
    }

    pub fn new_from_string(source: &str, filename: &str) -> SourceFile {
        let source = Source {
            filename: String::from(filename),
            file: source.chars().collect(),
            lines: source.lines().map(std::string::String::from).collect()
        };

        SourceFile(std::rc::Rc::new(std::boxed::Box::new(source)))
    }

}

impl SourceFile{
    pub fn get(&self, pos: usize) -> Option<char> {
        self.0.as_ref().file.get(pos).cloned()
    }

    pub fn text(&self, start:usize, end:usize) -> String {
        self.0.as_ref().file[start..end].iter().collect()
    }

    pub fn pos(&self, p: (usize, usize, usize)) -> String {
        format!("{}:{}:{}", self.0.as_ref().filename, p.0, p.1)
    }
}

impl Clone for SourceFile {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[cfg(test)]
mod test {
    use super::{Source, SourceFile};

    #[test]
    fn test_source_basic() -> std::result::Result<(), String> {
        let filename = "examples/01_simple.c";
        let s1 = Source::new_from_path(filename)?;
        let s2 = SourceFile::clone(&s1);

        assert_eq!(s1.get(10), s2.get(10));
        Ok(())
    }
}

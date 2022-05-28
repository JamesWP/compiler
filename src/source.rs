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

mod Colors {
    pub const RESET:&'static str = "\x1B[0m"; 
    pub const LINE_NUM:&'static str = "\x1B[0;49;34m"; 
    pub const CONTEXT:&'static str = "\x1B[0;49;90m"; 
    pub const SELECTED_LINE:&'static str = "\x1B[1;49;37m"; 
    pub const HIGHLIGHT:&'static str = "\x1B[7;49;37m"; 
    pub const SOURCE_LINK:&'static str = "\x1B[0;49;37m"; 
    pub const SOURCE_LINK_ARROW:&'static str = "\x1B[2;49;95m"; 
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
        use Colors::*;
        let context = 5;

        let obj = self.0.as_ref();

        let line_number = p.0 -1;
        let col_number = p.1 -1;

        // Maths!
        let starting_line = if context > line_number { 0 } else { p.0 - context };
        let ending_line = if line_number + context > obj.lines.len() {
            obj.lines.len()
        } else {
            line_number + context
        };

        let lines = &obj.lines;
        let context = lines[starting_line..ending_line]
            .iter()
            .enumerate()
            .map(|(number, line)| {
                let number = number + starting_line;
                let line_col = if number == line_number {SELECTED_LINE} else {CONTEXT};
                let prefix = format!("{LINE_NUM}{:3} ", number+1);
                let line_content = if number==line_number {
                    let highlight = HIGHLIGHT;
                    let (line_before, line_after) = line.split_at(col_number);
                    let (highlighted, line_after) = if line_after.len() > 1 {line_after.split_at(1)} else {(line_after, "")};
                    format!("{line_col}| {line_before}{highlight}{highlighted}{RESET}{line_col}{line_after}")
                } else {
                    format!("{line_col}| {line}")
                };

                format!("{prefix}{line_content}")
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("\n{}\n\n{SOURCE_LINK_ARROW}--> {SOURCE_LINK}{}:{}:{}{RESET}", context, obj.filename, p.0, p.1)
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

        println!("{}", s1.pos((2, 9, 0)));
        Ok(())
    }
}


mod ast;
mod compiler;
mod fileiter;
mod lexer;
mod parser;
mod platform;
mod stringiter;
mod examples;

fn main() -> std::io::Result<()> {
    let mut filename = "examples/01_simple.c".to_owned();
    let mut output_filename = "a.S".to_owned();

    let mut set_output = false;

    let mut args = std::env::args().skip(1);

    while let Some(arg) = args.next() {
        if arg == "-o" {
            set_output = true;
            continue;
        }
        if set_output {
            output_filename = arg;
            set_output = false;
        } else {
            filename = arg;
        }
    }

    println!("Reading {}", filename);

    let file = fileiter::FileIter::from(std::fs::File::open(filename.clone())?);
    let lexer = lexer::Lexer::new(Box::new(file), &filename);
    let parser_input = lexer.map(|(_l, t)| t).collect::<Vec<_>>();
    let translation_unit = parser::parse_translation_unit(&mut parser_input.into());

    if translation_unit.is_err() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            translation_unit.err().unwrap(),
        ));
    }

    let assembly = compiler::compile(&translation_unit.unwrap())?;

    std::fs::write(output_filename.clone(), &assembly)?;

    use std::io::Write;
    for (line_no, line) in assembly.lines().enumerate() {
        println!("{:<03}    {}", line_no+1, line);
    }   

    println!("Written to {}", output_filename);

    let mut child = std::process::Command::new("as")
        .arg("-")
        .args(["-o", &output_filename])
        .stdin(std::process::Stdio::piped())
        .spawn()?;
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(assembly.as_bytes())?;

    let exit = child.wait()?;

    if !exit.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("Assembler exited with error {}", exit.code().unwrap_or(-1)),
        ));
    }

    println!("Assembled to a.out");

    Ok(())
}

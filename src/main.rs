mod ast;
mod compiler;
mod examples;
mod fileiter;
mod intern;
mod labels;
mod lexer;
mod parser;
mod platform;
mod scope;
mod stringiter;

fn main() -> std::io::Result<()> {
    let mut filename = "examples/01_simple.c".to_owned();
    let mut output_filename = "a.S".to_owned();

    let mut set_output = false;
    let mut print_lex = false;
    let mut print_ast = false;

    let mut args = std::env::args().skip(1);

    while let Some(arg) = args.next() {
        if arg == "-o" {
            set_output = true;
            continue;
        }
        if arg == "-l" {
            print_lex = true;
            continue;
        }
        if arg == "-d" {
            print_ast = true;
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

    let parser_input:parser::ParserInput = lexer.map(|(_l, t)| t).collect::<Vec<_>>().into();
    let parser_input = if print_lex { parser_input.enable_debug() } else { parser_input };
    let translation_unit = parser::ParserState::new(parser_input).parse_translation_unit();

    if translation_unit.is_err() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            translation_unit.err().unwrap(),
        ));
    }

    let translation_unit = translation_unit.unwrap();

    if print_ast {
        println!("Ast Dump");

        println!("{:#?}", translation_unit);
    }

    let assembly = compiler::compile(&translation_unit)?;

    std::fs::write(output_filename.clone(), &assembly)?;

    use std::io::Write;
    for (line_no, line) in assembly.lines().enumerate() {
        println!("{:<03}    {}", line_no + 1, line);
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

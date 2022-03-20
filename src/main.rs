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
    let mut options = CompilerOptions {
        filename: "examples/01_simple.c".to_owned(),
        output_filename: "a.o".to_owned(),
        debug_ast: false,
        debug_lex: false,
    };

    let mut args = std::env::args().skip(1);

    while let Some(arg) = args.next() {
        if arg == "-o" {
            let output_filename = args.next().expect("filename should follow -o").clone();
            options.output_filename = output_filename;
            continue;
        }
        if arg == "-l" {
            options.debug_lex = true;
            continue;
        }
        if arg == "-d" {
            options.debug_ast = true;
            continue;
        } else {
            options.filename = arg;
        }
    }

    compile(&options)
}

pub struct CompilerOptions {
    filename: String,
    output_filename: String,
    debug_lex: bool,
    debug_ast: bool,
}

pub fn compile(compiler_options: &CompilerOptions) -> std::io::Result<()> {
    println!("Reading {}", compiler_options.filename);

    let file = fileiter::FileIter::from(std::fs::File::open(compiler_options.filename.clone())?);
    let mut lexer = lexer::Lexer::new(Box::new(file), &compiler_options.filename);

    let parser_input: parser::ParserInput = lexer.lex().into();
    let parser_input = if compiler_options.debug_lex {
        parser_input.enable_debug()
    } else {
        parser_input
    };
    let translation_unit = parser::ParserState::new(parser_input).parse_translation_unit();

    if translation_unit.is_err() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            translation_unit.err().unwrap(),
        ));
    }

    let translation_unit = translation_unit.unwrap();

    let assembly = compiler::compile(&translation_unit, compiler_options.debug_ast)?;

    std::fs::write(compiler_options.output_filename.clone(), &assembly)?;

    use std::io::Write;
    for (line_no, line) in assembly.lines().enumerate() {
        println!("{:<03}    {}", line_no + 1, line);
    }

    println!("Written to {}", compiler_options.output_filename);

    let mut child = std::process::Command::new("as")
        .arg("-")
        .args(["-o", &compiler_options.output_filename])
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

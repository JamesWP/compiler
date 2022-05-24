mod ast;
mod compiler;
mod constexpr;
mod examples;
mod intern;
mod labels;
mod lexer;
mod parser;
mod platform;
mod preprocessor;
mod scope;
mod source;

use std::io::Write;

use lexer::LexError;
use source::SourceError;

fn main() -> std::io::Result<()> {
    let mut options = CompilerOptions::default();

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
        if arg == "-p" {
            options.debug_pp = true;
            continue;
        }
        if arg == "-d" {
            options.debug_ast = true;
            continue;
        }
        if arg == "-a" {
            options.debug_assembly = true;
            continue;
        }

        options.filename = arg;
    }

    let result = compile(&options);

    match result {
        Ok(_) => Ok(()),
        Err(Error::Lex(l)) => {
            eprintln!("Lex error: {}", l);
            Err(std::io::Error::new(std::io::ErrorKind::Other, "Lex error"))
        },
        Err(Error::Source(s)) => {
            eprintln!("Source read error: {}", s);
            Err(std::io::Error::new(std::io::ErrorKind::Other, "Source read error"))
        },
    }
}

#[derive(Debug)]
pub struct CompilerOptions {
    filename: String,
    output_filename: String,
    debug_lex: bool,
    debug_ast: bool,
    debug_pp: bool,
    debug_assembly: bool,
}

impl Default for CompilerOptions {
    fn default() -> CompilerOptions {
        CompilerOptions {
            filename: "examples/01_simple.c".to_owned(),
            output_filename: "a.o".to_owned(),
            debug_ast: false,
            debug_lex: false,
            debug_pp: false,
            debug_assembly: false,
        }
    }
}

pub enum Error {
    Source(SourceError),
    Lex(LexError),
}

impl From<SourceError> for Error {
    fn from(s: SourceError) -> Self {
        Error::Source(s)
    }
}

impl From<LexError> for Error {
    fn from(l: LexError) -> Self {
        Error::Lex(l)
    }
}

impl From<std::io::Error> for Error {
    fn from(_: std::io::Error) -> Self {
        todo!()
    }
}

pub fn compile(compiler_options: &CompilerOptions) -> std::result::Result<(), Error> {
    println!("Reading {}", compiler_options.filename);

    let pp_input = lexer::lex_file(&compiler_options.filename)?;

    let pp_output = preprocessor::preprocess(pp_input, lexer::lex_file)?;

    if compiler_options.debug_pp {
        for token in &pp_output {
            print!("{}", token);
        }
        println!();
        return Ok(());
    }

    let parser_input = parser::ParserInput::from(pp_output);
    let parser_input = if compiler_options.debug_lex {
        parser_input.enable_debug()
    } else {
        parser_input
    };

    let translation_unit = match parser::ParserState::new(parser_input).parse_translation_unit() {
        Ok(translation_unit) => translation_unit,
        Err(error) => {
            eprintln!("Parser error {}", error);
            eprintln!("Found while parsing {:?}", &compiler_options);

            let error = std::io::Error::new(std::io::ErrorKind::InvalidInput, error);
            return Err(error.into());
        }
    };

    let assembly = match compiler::compile(&translation_unit, compiler_options.debug_ast) {
        Ok(assembly) => assembly,
        Err(error) => {
            eprintln!("Compilation error {}", error);
            eprintln!("Found while compiling {:?}", &compiler_options);
            return Err(error.into());
        }
    };

    std::fs::write(compiler_options.output_filename.clone(), &assembly)?;

    if compiler_options.debug_assembly {
        for (line_no, line) in assembly.lines().enumerate() {
            println!("{:<03}    {}", line_no + 1, line);
        }
        println!();
        return Ok(());
    }

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
        ).into());
    }

    println!("Assembled to {}", compiler_options.output_filename);

    Ok(())
}

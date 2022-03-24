mod ast;
mod compiler;
mod examples;
mod intern;
mod labels;
mod lexer;
mod parser;
mod platform;
mod preprocessor;
mod scope;

fn main() -> std::io::Result<()> {
    let mut options = CompilerOptions {
        filename: "examples/01_simple.c".to_owned(),
        output_filename: "a.o".to_owned(),
        debug_ast: false,
        debug_lex: false,
        debug_pp: false,
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
        if arg == "-p" {
            options.debug_pp = true;
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

#[derive(Debug)]
pub struct CompilerOptions {
    filename: String,
    output_filename: String,
    debug_lex: bool,
    debug_ast: bool,
    debug_pp: bool,
}

pub fn compile(compiler_options: &CompilerOptions) -> std::io::Result<()> {
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
            return Err(error);
        }
    };

    let assembly = match compiler::compile(&translation_unit, compiler_options.debug_ast) {
        Ok(assembly) => assembly,
        Err(error) => {
            eprintln!("Compilation error {}", error);
            eprintln!("Found while compiling {:?}", &compiler_options);
            return Err(error);
        }
    };

    std::fs::write(compiler_options.output_filename.clone(), &assembly)?;

    use std::io::Write;
    for (line_no, line) in assembly.lines().enumerate() {
        println!("{:<03}    {}", line_no + 1, line);
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
        ));
    }

    println!("Assembled to {}", compiler_options.output_filename);

    Ok(())
}

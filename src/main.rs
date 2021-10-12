use object::write::{Object, SectionId, StandardSection, Symbol, SymbolSection};
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use std::fs::File;
use std::io::{prelude::*, ErrorKind};
use std::process::Command;
use std::vec::Vec;

mod lexer;
mod parser;

fn compile() -> std::io::Result<object::write::Object> {
    let mut object = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

    let my_data_bytes = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0];
    let data_section_id = object.section_id(StandardSection::Data);

    let position =
        object.append_section_data(data_section_id, &my_data_bytes[..], 1 /*allign*/);

    let s_start = Symbol {
        name: Vec::from("my_data_bytes_start"),
        value: position,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(data_section_id),
        flags: SymbolFlags::<SectionId>::None,
    };

    object.add_symbol(s_start);

    let s_end = Symbol {
        name: Vec::from("my_data_bytes_end"),
        value: position + my_data_bytes.len() as u64,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(data_section_id),
        flags: SymbolFlags::<SectionId>::None,
    };

    object.add_symbol(s_end);

    Ok(object)
}

fn write(object: object::write::Object) -> std::io::Result<String> {
    let name = "code.o";
    let bytes = object.write().unwrap();

    let mut file = File::create(name)?;

    file.write_all(&bytes[..])?;

    println!("Written {}", name);

    Ok(name.to_owned())
}

fn link(objects: &[&str]) -> std::io::Result<String> {
    let output_file = "app.tsk".to_owned();

    let mut linker = Command::new("gcc")
        .args(objects)
        .args(["-lc", "-o", &output_file[..]])
        .spawn()?;

    let exit_code = linker.wait()?;

    if !exit_code.success() {
        return Err(std::io::Error::from(ErrorKind::InvalidData));
    }

    Ok(output_file)
}

fn dump(name: &str) -> std::io::Result<()> {
    let mut output = Command::new("objdump")
        .arg("-x")
        .arg(std::path::Path::new(name).canonicalize()?)
        .spawn()?;
    let exit_code = output.wait()?;

    if !exit_code.success() {
        return Err(std::io::Error::from(ErrorKind::InvalidData));
    }

    Ok(())
}

fn run(name: &str) -> std::io::Result<()> {
    let mut output = Command::new(std::path::Path::new(name).canonicalize()?).spawn()?;
    let exit_code = output.wait()?;

    if !exit_code.success() {
        return Err(std::io::Error::from(ErrorKind::InvalidData));
    }

    println!("Success from running {}", name);

    Ok(())
}

fn compile_c(file: &str) -> std::io::Result<String> {
    let mut output = Command::new("gcc")
        .arg("-c")
        .args(["-o", "test.o"])
        .arg(file)
        .spawn()?;

    let exit_code = output.wait()?;

    if !exit_code.success() {
        return Err(std::io::Error::from(ErrorKind::InvalidData));
    }

    println!("Compued driver {}", file);

    Ok("test.o".to_owned())
}

fn main() -> std::io::Result<()> {
    println!("Hello, world!");

    let object = compile()?;
    let object_name = write(object)?;
    dump(&object_name)?;
    let driver = compile_c("test.c")?;
    let output = link(&[&object_name, &driver])?;
    run(&output[..])?;

    Ok(())
}

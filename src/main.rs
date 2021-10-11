use object::write::{Object, SectionId, StandardSection, Symbol, SymbolSection};
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use std::fmt::Debug;
use std::fs::File;
use std::io::{prelude::*, ErrorKind};
use std::process::Command;
use std::vec::Vec;

fn compile() -> std::result::Result<object::write::Object, &'static str> {
    let mut o = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

    let my_data_bytes = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0];
    let data_section_id = o.section_id(StandardSection::Data);

    let position = o.append_section_data(data_section_id, &my_data_bytes[..], 1 /*allign*/);

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

    o.add_symbol(s_start);

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

    o.add_symbol(s_end);

    Ok(o)
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

    Ok(())
}

fn main() -> std::io::Result<()> {
    println!("Hello, world!");

    let o = compile();

    let o = o.map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

    let bytes = o.write().unwrap();

    let mut file = File::create("code.o")?;

    file.write_all(&bytes[..])?;

    println!("Written {} bytes.", bytes.len());

    dump("code.o")?;

    println!("Linking");

    let output = link(&["code.o", "test.o"])?;

    println!("Linked into {}", output);

    run(&output[..])?;

    println!("Success from running {}", output);

    Ok(())
}

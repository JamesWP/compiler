use std::fmt::Write;
use crate::ast;
use crate::platform::x86_64_reg;

pub fn compile(translation_unit: &ast::TranslationUnit) -> std::io::Result<String> {
    let mut content = String::new();

    writeln!(content, "# Begin output");
    compile_translation_unit(&mut content, translation_unit)?;
    writeln!(content, "# generated by: {}", std::env::args().fold(String::new(), |s,a|s + " " + &a));
    writeln!(content, "# End output");

    Ok(content)
}

fn compile_translation_unit<T: Write>(output: &mut T, translation_unit: &ast::TranslationUnit) -> std::io::Result<()> {

    writeln!(output, ".text");
    for func in &translation_unit.function_definitions {
        let (name, definition) = func;
        let statement = &definition.compound_statement;
        writeln!(output, ".global {}", name);
        writeln!(output, "{}:", name);
        for statement in statement.iter() {
            compile_statement(output, statement)?;
        }
    }
    

    Ok(())
}

fn compile_statement<T: Write>(output: &mut T, statement: &ast::Statement) -> std::io::Result<()> {
    match statement {
        ast::Statement::JumpStatement(ast::JumpStatement::Return) => {
            writeln!(output, "    ret");
        },
        ast::Statement::JumpStatement(ast::JumpStatement::ReturnWithValue(s)) => {
            compile_expression(output, s, x86_64_reg::RAX)?;
            writeln!(output, "    ret");
        }
    }

    Ok(())
}

fn compile_expression<T: Write>(output: &mut T, expression: &ast::Expression, destination: x86_64_reg) -> std::io::Result<()> {
    match expression {
        ast::Expression::Additive(lhs, rhs) => { unimplemented!(); },
        ast::Expression::Unary(ast::LiteralValue::Int32(value)) => { 
            writeln!(output, "    mov ${}, {}", value, destination);
        }
    }
    Ok(())
}
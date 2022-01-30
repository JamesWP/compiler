use crate::ast;
use crate::intern;
use crate::platform;
use crate::platform::DecimalLiteral as DL;
use crate::platform::StackRelativeLocation;
use crate::platform::X86_64Reg as reg;
use std::fmt::Display;
use std::fmt::Write;

struct CompilationState {
    output: String,
    function_stack_frame: Option<platform::StackLayout>,
    pub intern: intern::Intern,
}

impl Default for CompilationState {
    fn default() -> CompilationState {
        CompilationState {
            output: String::new(),
            function_stack_frame: None,
            intern: intern::Intern::new(),
        }
    }
}

impl CompilationState {
    fn output_comment<T: Display>(&mut self, message: T) -> std::io::Result<()> {
        writeln!(self.output, "# {}", message).unwrap();
        Ok(())
    }
    fn output_section<T: Display>(&mut self, section: T) -> std::io::Result<()> {
        writeln!(self.output, "        .{}", section).unwrap();
        Ok(())
    }
    fn output_type<T: Display, R: Display>(
        &mut self,
        name: T,
        symbol_type: R,
    ) -> std::io::Result<()> {
        writeln!(self.output, "        .type {}, @{}", name, symbol_type).unwrap();
        Ok(())
    }
    fn output_global_specifier<T: Display>(&mut self, name: T) -> std::io::Result<()> {
        writeln!(self.output, "        .global {}", name).unwrap();
        Ok(())
    }
    fn output_asciiz(&mut self, data: &str) -> std::io::Result<()> {
        writeln!(self.output, ".asciz {:?}", data).unwrap();
        Ok(())
    }
    fn output_label<T: Display>(&mut self, name: T) -> std::io::Result<()> {
        writeln!(self.output, "{}:", name).unwrap();
        Ok(())
    }
    fn output_function_label<T: Display>(&mut self, name: T) -> std::io::Result<()> {
        writeln!(self.output, "{}:", name).unwrap();
        Ok(())
    }
    fn output_newline(&mut self) -> std::io::Result<()> {
        writeln!(self.output).unwrap();
        Ok(())
    }
}

macro_rules! assemble {
    ($state:expr, $nmemon:expr) => {{
        writeln!($state.output, "        {:<8} ", $nmemon).unwrap();
    }};
    ($state:expr, $nmemon:expr, $arg1:expr) => {{
        write!($state.output, "        {:<8} ", $nmemon).unwrap();
        writeln!($state.output, "{}", $arg1).unwrap();
    }};
    ($state:expr, $nmemon:expr, $arg1:expr, $arg2:expr) => {{
        write!($state.output, "        {:<8} ", $nmemon).unwrap();
        writeln!($state.output, "{}, {}", $arg1, $arg2).unwrap();
    }};
}

pub fn compile(translation_unit: &ast::TranslationUnit) -> std::io::Result<String> {
    let mut state = CompilationState::default();

    state.output_comment("Begin output")?;
    state.compile_translation_unit(translation_unit)?;

    state.output_comment("Data time!")?;
    state.output_section("data")?;
    for (label, data) in state.intern.get_labels() {
        state.output_label(&label)?;
        state.output_asciiz(&data)?;
    }

    state.output_comment(format!(
        "generated by: {}",
        std::env::args().fold(String::new(), |s, a| s + " " + &a)
    ))?;
    state.output_comment("End output")?;

    Ok(state.output)
}

impl CompilationState {
    fn compile_translation_unit(
        &mut self,
        translation_unit: &ast::TranslationUnit,
    ) -> std::io::Result<()> {
        self.output_section("text")?;
        for func in &translation_unit.function_definitions {
            let (name, definition) = func;

            let parameter_list = &definition.parameter_list;

            if definition.compound_statement.is_none() {
                // skip declaration
                continue;
            }

            let statement = definition.compound_statement.as_ref().unwrap();

            self.output_newline()?;
            self.output_type(name, "function")?;
            self.output_global_specifier(name)?;

            // Function label
            self.output_function_label(name)?;

            // set up base pointer for frame
            // -> Does this need to change if we are not leaf?
            assemble!(self, "pushq", reg::RBP);
            assemble!(self, "movq", reg::RSP, reg::RBP);

            // Calculate layout of stack frame
            self.function_stack_frame = Some(compute_stack_layout_for_function(parameter_list));

            let mut platform_abi = platform::ParameterPlacement::default();

            // Move all parameters to stack
            for (stack_location, (_, decl_type)) in self
                .function_stack_frame
                .as_ref()
                .unwrap()
                .iter()
                .zip(parameter_list.iter())
            {
                // calculate which register this parameter comes in
                let param_location = platform_abi.place(decl_type);

                // TODO: handle parameters which don't come in registers
                let param_reg = param_location.reg.unwrap();

                let op = match stack_location.size() {
                    1 => "movb",
                    4 => "movl",
                    8 => "movq",
                    _ => unimplemented!(),
                };

                assemble!(self, op, param_reg, stack_location.stack_allocation);
            }

            // Allocate space for all local variables
            for declaration in definition.declarations() {
                let (name, type_def) = declaration;
                let size = type_def.size();
                self.function_stack_frame
                    .as_mut()
                    .unwrap()
                    .allocate(&name, &type_def, size);
            }

            // Fix stack pointer
            let stack_size = self.function_stack_frame.as_ref().unwrap().stack_size;
            if stack_size != 0 {
                assemble!(self, "subq", DL::new(stack_size as i32), reg::RSP);
            }

            for statement in statement.iter() {
                self.compile_statement(statement)?;
            }

            let stack_comments: Vec<_> = self
                .function_stack_frame
                .as_ref()
                .unwrap()
                .iter()
                .map(|x| format!("{:10} in {:?}", format!("{}", x.stack_allocation), x.name))
                .collect();

            for x in stack_comments {
                self.output_comment(x)?;
            }
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> std::io::Result<()> {
        let result_64 = reg::RAX;
        let result_32 = reg::EAX;

        let post_amble = |state: &mut CompilationState| {
            let stack_size = state.function_stack_frame.as_ref().unwrap().stack_size;
            if stack_size != 0 {
                assemble!(state, "movq", reg::RBP, reg::RSP);
            }
            // TODO: read information about all registers which need popping
            assemble!(state, "popq", reg::RBP);
        };

        match statement {
            ast::Statement::JumpStatement(ast::JumpStatement::Return) => {
                post_amble(self);
                assemble!(self, "ret");
            }
            ast::Statement::JumpStatement(ast::JumpStatement::ReturnWithValue(s)) => {
                // TODO: read information about all registers which need popping
                self.compile_expression(s)?;
                post_amble(self);
                assemble!(self, "ret");
            }
            ast::Statement::Declaration(declaration) => {
                let name = &declaration.name;
                let location = self
                    .function_stack_frame
                    .as_ref()
                    .unwrap()
                    .get_location(name)?;
                if let Some(e) = &declaration.expression {
                    self.compile_expression(&e)?;
                    match location.size {
                        8 => assemble!(self, "mov", result_64, location),
                        4 => assemble!(self, "movl", result_32, location),
                        s => unimplemented!("size error, {}", s),
                    }
                }
            }
            ast::Statement::Expression(e) => {
                self.compile_expression(e)?;
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> std::io::Result<()> {
        let result_64 = reg::RAX;
        let result_32 = reg::EAX;
        match &expression.node {
            ast::ExpressionNode::Binary(op, lhs, rhs) => {
                self.compile_expression(rhs.as_ref())?;

                assemble!(self, "pushq", result_64);

                self.compile_expression(&lhs)?;

                match op {
                    ast::BinOp::Sum => {
                        assemble!(self, "addl", StackRelativeLocation::top_32(), result_32);
                    }
                    ast::BinOp::Difference => {
                        assemble!(self, "subl", StackRelativeLocation::top_32(), result_32);
                    }
                    ast::BinOp::Product => {
                        assemble!(self, "mull", StackRelativeLocation::top_32());
                    }
                    ast::BinOp::Quotient => {
                        assemble!(self, "divl", StackRelativeLocation::top_32());
                    }
                }

                // Balance the stack
                assemble!(self, "add", DL::new(8), reg::RSP);
            }
            ast::ExpressionNode::Value(v) => match v {
                ast::Value::Literal(l) => match l {
                    ast::LiteralValue::Int32(value) => {
                        assemble!(self, "movl", DL::new(value), result_32);
                    }
                    ast::LiteralValue::StringLiteral(value) => {
                        let label = self.intern.add(&value);
                        assemble!(self, "lea", format!("{}(%rip)", label), result_64);
                    }
                },
                ast::Value::Identifier(name) => {
                    if let ast::TypeDefinition::FUNCTION(_, _, is_local) = expression.expr_type {
                        if is_local {
                            assemble!(self, "lea", format!("{}(%rip)", name), result_64);
                        } else {
                            assemble!(self, "mov", format!("{}@GOTPCREL(%rip)", name), result_64);
                        }
                    } else {
                        let location = self
                            .function_stack_frame
                            .as_ref()
                            .unwrap()
                            .get_location(&name)?;
                        assemble!(self, "movl", location, result_32);
                    }
                }
            },
            ast::ExpressionNode::Call(lhs, args) => {
                // TODO: check function types
                let call_is_vararg =
                    if let ast::TypeDefinition::FUNCTION(_, params, _) = &lhs.expr_type {
                        params.var_args
                    } else {
                        false
                    };

                let mut param_place = platform::ParameterPlacement::default();

                for arg in args {
                    let expr_type = &arg.expr_type;
                    let param = param_place.place(&expr_type);
                    if let Some(ref reg) = param.reg {
                        self.compile_expression(&arg)?;
                        match expr_type.size() {
                            4 => assemble!(self, "movl", reg::EAX, reg),
                            8 => assemble!(self, "movq", reg::RAX, reg),
                            _ => todo!("Can't move parameter of this size"),
                        }
                    } else {
                        unimplemented!();
                    }
                }

                // assemble call instruction
                self.compile_expression(lhs.as_ref())?;

                //TODO: if is var arg, and fpu is used, calculate number of fpu regs used
                if call_is_vararg {
                    assemble!(self, "mov", reg::RAX, reg::RBX);
                    assemble!(self, "xor", reg::EAX, reg::EAX);
                    assemble!(self, "call", format!("*{}", reg::RBX));
                } else {
                    assemble!(self, "call", format!("*{}", reg::RAX));
                }
            }
        }
        Ok(())
    }
}

fn compute_stack_layout_for_function(parameter_list: &ast::ParameterList) -> platform::StackLayout {
    let mut layout = platform::StackLayout::default();

    for (name, decl_type) in parameter_list.iter() {
        let size_in_bytes = decl_type.size();
        layout.allocate(name, decl_type, size_in_bytes);
    }

    layout
}

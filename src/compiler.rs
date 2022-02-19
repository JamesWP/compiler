use crate::ast;
use crate::intern;
use crate::labels;
use crate::platform;
use crate::platform::DecimalLiteral as DL;
use crate::platform::RegisterIndirectLocation;
use crate::platform::StackRelativeLocation;
use crate::platform::X86_64Reg as reg;
use std::fmt::Display;
use std::fmt::Write;
use std::rc::Rc;

struct CompilationState {
    output: String,
    function_stack_frame: Option<platform::StackLayout>,
    pub intern: intern::Intern,
    pub labels: labels::LabelAllocator,
}

impl Default for CompilationState {
    fn default() -> CompilationState {
        CompilationState {
            output: String::new(),
            function_stack_frame: None,
            intern: intern::Intern::new(),
            labels: labels::LabelAllocator::default(),
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

            if definition.function_body.is_none() {
                // skip declaration
                continue;
            }

            let statement = definition.function_body.as_ref().unwrap();

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
            for ast::DeclarationStatement{ name, decl_type, location, ..} in definition.declarations() {
                let size = decl_type.size();
                let allocated_location = self.function_stack_frame
                    .as_mut()
                    .unwrap()
                    .allocate(&name, &decl_type, size);
                *location.borrow_mut() = Some(allocated_location);
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

    fn push(&mut self) {
        assemble!(self, "pushq", reg::RAX);
    }

    fn pop(&mut self) {
        assemble!(self, "add", DL::new(8), reg::RSP);
    }

    /// store the value in RAX into the address pointed to by the value in the top of the stack
    /// also consumes the top of stack
    /// leaves the value stored in RAX
    fn compile_store(&mut self, type_def: &ast::TypeDefinition) -> std::io::Result<()> {
        assemble!(self, "pop", reg::RDI);
        match type_def.size() {
            8 => assemble!(
                self,
                "mov",
                reg::RAX,
                RegisterIndirectLocation::new(reg::RDI)
            ),
            4 => assemble!(
                self,
                "movl",
                reg::EAX,
                RegisterIndirectLocation::new(reg::RDI)
            ),
            s => unimplemented!("size error, {}", s),
        }
        Ok(())
    }

    fn compile_load(&mut self, type_def: &ast::TypeDefinition) -> std::io::Result<()> {
        let load_size = type_def.size();
        match type_def {
            ast::TypeDefinition::FUNCTION(_, _, _) => {
                // functions are code, we don't load code
            }
            _ => match load_size {
                8 => assemble!(
                    self,
                    "mov",
                    RegisterIndirectLocation::new(reg::RAX),
                    reg::RAX
                ),
                4 => assemble!(
                    self,
                    "movl",
                    RegisterIndirectLocation::new(reg::RAX),
                    reg::EAX
                ),
                s => unimplemented!("size error, {}", s),
            },
        }
        Ok(())
    }

    fn compile_address(&mut self, expression: &ast::Expression) -> std::io::Result<()> {
        match &expression.node {
            ast::ExpressionNode::Binary(_, _, _) => todo!(),
            ast::ExpressionNode::Value(v) => match v {
                ast::Value::Literal(l) => match l {
                    ast::LiteralValue::Int32(_) => todo!(),
                    ast::LiteralValue::StringLiteral(value) => {
                        let label = self.intern.add(&value);
                        assemble!(self, "lea", format!("{}(%rip)", label), reg::RAX);
                        Ok(())
                    }
                },
                ast::Value::Identifier(ident, location) => match expression.expr_type {
                    ast::TypeDefinition::FUNCTION(_, _, true) => {
                        assemble!(self, "lea", format!("{}(%rip)", ident), reg::RAX);
                        Ok(())
                    }
                    ast::TypeDefinition::FUNCTION(_, _, false) => {
                        assemble!(self, "mov", format!("{}@GOTPCREL(%rip)", ident), reg::RAX);
                        Ok(())
                    }
                    _ => {
                        let location = location.borrow().as_ref().unwrap().clone();
                        assemble!(self, "lea", location, reg::RAX);
                        Ok(())
                    }
                },
            },
            ast::ExpressionNode::Call(_, _) => todo!(),
        }
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> std::io::Result<()> {
        //self.output_comment(format!("-stmt {:?}", statement));
        self.output_comment("");
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
            ast::Statement::WhileStatement(ast::WhileStatement {
                condition_expression,
                loop_body,
            }) => {
                let start_label = self.labels.allocate_label();
                let end_label = self.labels.allocate_label();
                // start_label:
                self.output_label(&start_label)?;
                //   evaluate condition
                self.compile_expression(condition_expression)?;
                assemble!(self, "cmp", DL::new(0), reg::RAX);
                assemble!(self, "jz", &end_label);
                //   if condition is not met jump to end_label
                //   while body
                self.compile_statement(loop_body)?;
                assemble!(self, "jmp", &start_label);
                //   jump to start
                // end_label:
                self.output_label(end_label)?;
            }
            ast::Statement::IfStatement(ast::IfStatement {
                condition_expression,
                if_body,
                else_body: None,
            }) => {
                self.compile_expression(condition_expression)?;
                let end_label = self.labels.allocate_label();

                // jump to end if $eax == zero
                assemble!(self, "cmp", DL::new(0), reg::RAX);
                assemble!(self, "jz", end_label);

                // --[if body]
                self.compile_statement(if_body)?;

                // end:
                self.output_label(end_label)?;
            }
            ast::Statement::IfStatement(ast::IfStatement {
                condition_expression,
                if_body,
                else_body: Some(else_body),
            }) => {
                self.compile_expression(condition_expression)?;
                let else_label = self.labels.allocate_label();
                let end_label = self.labels.allocate_label();

                // jump to else if $eax == zero
                assemble!(self, "cmp", DL::new(0), reg::RAX);
                assemble!(self, "jz", else_label);

                // --[if body]
                self.compile_statement(if_body)?;

                // -- jump to end
                assemble!(self, "jmp", end_label);

                // else:
                self.output_label(else_label)?;

                // --[else body]
                self.compile_statement(else_body)?;

                // end:
                self.output_label(end_label)?;
            }
            ast::Statement::DeclarationStatement(declaration) => {
                let name = &declaration.name;

                if let Some(e) = &declaration.expression {
                    // Store address is pushed onto the stack
                    self.compile_address(&ast::Expression::new_value(
                        ast::Value::Identifier(name.to_string(), Rc::clone(&declaration.location)),
                        declaration.decl_type.to_owned(),
                    ))?;
                    self.push(); // This is consumed by compile_store

                    // Value to store is placed in RAX
                    self.compile_expression(&e)?;
                    self.compile_store(&declaration.decl_type)?; // Stack popped here
                }
            }
            ast::Statement::Expression(e) => {
                self.compile_expression(e)?;
            }
            ast::Statement::CompoundStatement(statements) => {
                for statement in statements {
                    self.compile_statement(statement)?;
                }
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> std::io::Result<()> {
        let _result_64 = reg::RAX;
        let result_32 = reg::EAX;
        let result_8 = reg::AL;
        match &expression.node {
            ast::ExpressionNode::Binary(ast::BinOp::Assign(op), lhs, rhs) => {
                // Put the lhs address on the stack
                self.compile_address(lhs)?;
                self.push();

                // Put the rhs value in RDI
                self.compile_expression(rhs)?;

                if let Some(op) = op {
                    assemble!(self, "mov", reg::RAX, reg::RDI);

                    // Put the lhs value in RAX
                    assemble!(
                        self,
                        "mov",
                        RegisterIndirectLocation::new(reg::RSP),
                        reg::RAX
                    ); // The stack pointer points to the stack,
                    assemble!(
                        self,
                        "movl",
                        RegisterIndirectLocation::new(reg::RAX),
                        reg::EAX
                    ); // And the value in the stack is a pointer to the value we want

                    match op {
                        ast::AssignOp::Sum => {
                            assemble!(self, "addl", reg::EDI, reg::EAX);
                        }
                        ast::AssignOp::Difference => {
                            assemble!(self, "subl", reg::EDI, reg::EAX);
                        }
                        ast::AssignOp::Product => {
                            assemble!(self, "mull", reg::EDI);
                        }
                        ast::AssignOp::Quotient => {
                            assemble!(self, "divl", reg::EDI);
                        }
                    }
                }

                self.compile_store(&lhs.expr_type)?; // pops the stack
            }
            ast::ExpressionNode::Binary(op, lhs, rhs) => {
                self.compile_expression(rhs.as_ref())?;

                self.push();
                self.compile_expression(&lhs)?;

                match op {
                    ast::BinOp::Sum => {
                        assemble!(self, "addl", StackRelativeLocation::top(), result_32);
                    }
                    ast::BinOp::Difference => {
                        assemble!(self, "subl", StackRelativeLocation::top(), result_32);
                    }
                    ast::BinOp::Product => {
                        assemble!(self, "mull", StackRelativeLocation::top());
                    }
                    ast::BinOp::Quotient => {
                        assemble!(self, "divl", StackRelativeLocation::top());
                    }
                    ast::BinOp::Equals => {
                        assemble!(self, "cmp", StackRelativeLocation::top(), result_32);
                        assemble!(self, "sete", result_8);
                    }
                    ast::BinOp::NotEquals => {
                        assemble!(self, "cmp", StackRelativeLocation::top(), result_32);
                        assemble!(self, "setne", result_8);
                    }
                    ast::BinOp::LessThan => {
                        // signed
                        assemble!(self, "cmp", StackRelativeLocation::top(), result_32);
                        assemble!(self, "setb", result_8);
                    }
                    ast::BinOp::GreaterThan => {
                        // signed
                        assemble!(self, "cmp", StackRelativeLocation::top(), result_32);
                        assemble!(self, "setg", result_8);
                    }
                    _ => todo!("implement binop {:?}", op),
                }

                self.pop();
            }
            ast::ExpressionNode::Value(v) => match v {
                ast::Value::Literal(l) => match l {
                    ast::LiteralValue::Int32(value) => {
                        assemble!(self, "movl", DL::new(value), result_32);
                    }
                    ast::LiteralValue::StringLiteral(_) => {
                        self.compile_address(expression)?;
                    }
                },
                ast::Value::Identifier(_, _) => {
                    self.compile_address(expression)?;
                    self.compile_load(&expression.expr_type)?;
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

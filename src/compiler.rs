use crate::ast;
use crate::intern;
use crate::labels;
use crate::platform;
use crate::platform::CharLiteral as CL;
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
    stack_depth: i32,
    pub intern: intern::Intern,
    pub labels: labels::LabelAllocator,
    pub debug: bool,
}

impl Default for CompilationState {
    fn default() -> CompilationState {
        CompilationState {
            output: String::new(),
            function_stack_frame: None,
            stack_depth: 0,
            intern: intern::Intern::new(),
            labels: labels::LabelAllocator::default(),
            debug: false,
        }
    }
}

fn fmt_to_io(fmt_err: std::fmt::Error) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidInput, fmt_err)
}

impl CompilationState {
    fn output_comment<T: Display>(&mut self, message: T) -> std::io::Result<()> {
        for line in format!("{}", message).lines() {
            writeln!(self.output, "# {}", line).map_err(fmt_to_io)?;
        }
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
        writeln!(self.output, ".asciz \"{:}\"", data).unwrap();
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

pub fn compile(translation_unit: &ast::TranslationUnit, debug: bool) -> std::io::Result<String> {
    let mut state = CompilationState::default();

    state.debug = debug;

    if debug {
        state.output_comment("Ast Dump")?;
        state.output_comment(format!("{:#?}", translation_unit))?;
    }

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

            assert_eq!(self.stack_depth, 0);

            let statement = definition.function_body.as_ref().unwrap();

            self.output_newline()?;
            self.output_type(name, "function")?;
            self.output_global_specifier(name)?;

            // Function label
            self.output_function_label(name)?;

            // set up base pointer for frame
            self.push_frame();

            // Calculate layout of stack frame
            let mut layout = platform::StackLayout::default();
            let mut platform_abi = platform::ParameterPlacement::default();

            // Move all parameters to stack
            for param in parameter_list.iter() {
                let size_in_bytes = param.decl_type.size();
                let stack_location = layout.allocate(size_in_bytes);

                // calculate which register this parameter comes in
                let param_location = platform_abi.place(&param.decl_type);

                self.output_comment(format!("Assigning {} to {:?}", param.name, stack_location))?;

                // TODO: handle parameters which don't come in registers
                let param_reg = param_location.reg.unwrap();
                match param.decl_type.size() {
                    4 => assemble!(self, "movl", param_reg, stack_location),
                    8 => assemble!(self, "mov", param_reg, stack_location),
                    _ => unimplemented!("size not implemented"),
                }

                *param.location.borrow_mut() = Some(stack_location.clone());
            }

            // Allocate space for all local variables
            for ast::DeclarationStatement {
                name,
                decl_type,
                location,
                ..
            } in definition.declarations()
            {
                let size = decl_type.size();
                let allocated_location = layout.allocate(size);
                *location.borrow_mut() = Some(allocated_location.clone());
                self.output_comment(format!("Assigned {} to {:?}", name, allocated_location))?;
            }

            layout.finalize();

            self.output_comment(format!("{:?}", &layout))?;

            self.setup_stack(layout);

            for statement in statement.iter() {
                self.compile_statement(statement)?;
            }

            self.reset_stack();

            assert_eq!(self.stack_depth, 0);
        }

        Ok(())
    }

    fn setup_stack(&mut self, layout: platform::StackLayout) {
        // Fix stack pointer
        let stack_size = layout.stack_size;
        if stack_size != 0 {
            assemble!(self, "subq", DL::new(stack_size as i32), reg::RSP);
        }

        self.stack_depth += stack_size as i32 / 8;
        self.function_stack_frame = Some(layout);
    }

    fn reset_stack(&mut self) {
        assert!(self.function_stack_frame.is_some());

        let stack_size = self.function_stack_frame.as_ref().unwrap().stack_size;
        self.stack_depth -= stack_size as i32 / 8;
        self.function_stack_frame = None;
    }

    fn setup_stack_for_call(&mut self) -> i32 {
        if self.stack_depth %2 == 1 {
            // stack is not aligned on 16byte boundary
            self.push();
            1 // one push made
        } else {
            0 // no push made
        }
    }

    fn reset_stack_for_call(&mut self, alignment: i32) {
       if alignment == 1 {
           self.pop_discard();
       } 
    }

    fn push_frame(&mut self) {
        // don't count the rbp on the stack which will be poped in each
        // return statement
        self.stack_depth -= 1; 

        self.push_reg(reg::RBP);
        assemble!(self, "movq", reg::RSP, reg::RBP);
    }
    fn pop_frame(&mut self) {
        // don't count the rbp on the stack which will be poped in each
        // return statement
        self.stack_depth += 1; 

        self.pop_reg(reg::RBP);
    }

    fn push_reg(&mut self, register: reg) {
        self.stack_depth += 1;
        match register.size() {
            8 => {
                assemble!(self, "push", register);
            }
            4 => {
                assemble!(self, "push", register.to_full_reg());
            }
            _ => {
                unimplemented!("pushing odd sized register");
            }
        }
    }

    fn pop_reg(&mut self, register: reg) {
        self.stack_depth -= 1;
        match register.size() {
            8 => {
                assemble!(self, "pop", register);
            }
            4 => {
                assemble!(self, "pop", register.to_full_reg());
            }
            _ => {
                unimplemented!("popping odd sized register");
            }
        }
    }

    // implicit 8 bytes, using RAX
    fn push(&mut self) {
        self.push_reg(reg::RAX);
    }

    // implicit 8 bytes
    fn pop_discard(&mut self) {
        self.stack_depth -= 1;
        assemble!(self, "add", DL::new(8), reg::RSP);
    }

    fn peek_stack(&mut self, output_register: reg, position_in_stack: i32) {
        assemble!(
            self,
            "mov",
            StackRelativeLocation::new_with_reg(reg::RSP, position_in_stack * 8),
            output_register
        );
    }

    /// store the value in RAX into the address pointed to by the value in the top of the stack
    /// also consumes the top of stack
    /// leaves the value stored in RAX
    fn compile_store(
        &mut self,
        type_def: &ast::TypeDefinition,
        position_in_stack: i32,
    ) -> std::io::Result<()> {
        // get store address from stack.
        self.peek_stack(reg::RDI, position_in_stack);

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
            1 => assemble!(
                self,
                "movb",
                reg::AL,
                RegisterIndirectLocation::new(reg::RDI)
            ),
            s => unimplemented!("size error, {}", s),
        }
        Ok(())
    }

    /**
    load a value into RAX from the address pointed to by RAX

    input:
      RAX: Pointer to value

    output:
      RAX: Value
    */
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
                1 => {
                    assemble!(
                        self,
                        "movb",
                        RegisterIndirectLocation::new(reg::RAX),
                        reg::AL
                    );
                    assemble!(self, "movzx", reg::AL, reg::EAX);
                }
                s => unimplemented!("size error, {}", s),
            },
        }
        Ok(())
    }

    fn debug_expression(
        &mut self,
        message: &str,
        expression: &ast::Expression,
    ) -> std::io::Result<()> {
        if self.debug {
            self.output_comment(format!("{}: {:#?}", message, expression))?;
        } else {
            self.output_comment(&format!("{}: {:?}", message, expression)[0..40])?;
        }
        Ok(())
    }
    fn debug_statement(&mut self, statement: &ast::Statement) -> std::io::Result<()> {
        if self.debug {
            self.output_comment(format!("{:#?}", statement))?;
        } else {
            self.output_comment(&format!("{:?}", statement)[0..40])?;
        }
        Ok(())
    }

    fn compile_address(&mut self, expression: &ast::Expression) -> std::io::Result<()> {
        match &expression.node {
            ast::ExpressionNode::Binary(_, _, _) => todo!(),
            ast::ExpressionNode::Unary(_, _) => todo!(),
            ast::ExpressionNode::Value(v) => match v {
                ast::Value::Literal(l) => match l {
                    ast::LiteralValue::Int32(_) => todo!(),
                    ast::LiteralValue::CharLiteral(_) => todo!(),
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
                        if let Some(location) = location.borrow().clone() {
                            assemble!(self, "lea", location, reg::RAX);
                            Ok(())
                        } else {
                            unimplemented!("Location of {} is undefined", ident)
                        }
                    }
                },
            },
            ast::ExpressionNode::Call(_, _) => todo!(),
            ast::ExpressionNode::Conditional(_, _, _) => {
                todo!();
            }
        }
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> std::io::Result<()> {
        match statement {
            ast::Statement::NoopStatement => {}
            ast::Statement::JumpStatement(js) => {
                self.debug_statement(statement)?;
                // TODO: read information about all registers which need popping
                match js {
                    ast::JumpStatement::Return => {}
                    ast::JumpStatement::ReturnWithValue(s) => {
                        self.compile_expression(s)?;
                    }
                };

                let stack_size = self.function_stack_frame.as_ref().unwrap().stack_size;
                if stack_size != 0 {
                    assemble!(self, "movq", reg::RBP, reg::RSP);
                }

                // TODO: read information about all registers which need popping
                self.pop_frame();

                assemble!(self, "ret");
            }
            ast::Statement::DoStatement(loop_body, None) => {
                let start_label = self.labels.allocate_label();
                self.output_label(&start_label)?;
                self.compile_statement(loop_body)?;
                assemble!(self, "jmp", &start_label);
            }
            ast::Statement::DoStatement(loop_body, Some(condition_expression)) => {
                let start_label = self.labels.allocate_label();
                self.output_label(&start_label)?;
                self.compile_statement(loop_body)?;

                self.compile_expression(condition_expression)?;
                assemble!(self, "cmp", DL::new(0), reg::RAX);
                assemble!(self, "jnz", &start_label);
            }
            ast::Statement::ForStatement(ast::ForStatement {
                initialization,
                control_expression,
                post_iteration_expression,
                loop_body,
            }) => {
                match &initialization {
                    ast::ForHead::WithDeclaration(decl) => {
                        self.compile_declaration(decl)?;
                    }
                    ast::ForHead::WithExpression(expr) => {
                        self.compile_expression(expr)?;
                    }
                    ast::ForHead::WithNoExpression => {}
                };

                self.debug_expression("for", control_expression)?;
                let start_label = self.labels.allocate_label();
                let end_label = self.labels.allocate_label();
                // start_label:
                self.output_label(&start_label)?;
                //   evaluate condition
                self.compile_expression(control_expression)?;
                assemble!(self, "cmp", DL::new(0), reg::RAX);
                assemble!(self, "jz", &end_label);
                //   if condition is not met jump to end_label
                //   for body
                self.compile_statement(loop_body)?;

                if let Some(expression) = post_iteration_expression {
                    self.compile_expression(expression)?;
                }

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
                self.debug_expression("if", condition_expression)?;
                self.compile_expression(condition_expression)?;
                let end_label = self.labels.allocate_label();

                // jump to end if $eax == zero
                // TODO: support 8 byte values
                assemble!(self, "cmp", DL::new(0), reg::EAX);
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
                self.debug_expression("if", condition_expression)?;
                self.compile_expression(condition_expression)?;
                let else_label = self.labels.allocate_label();
                let end_label = self.labels.allocate_label();

                // jump to else if $eax == zero
                // TODO: support 8 byte values
                assemble!(self, "cmp", DL::new(0), reg::EAX);
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
                self.debug_statement(statement)?;
                let name = &declaration.name;

                if let Some(e) = &declaration.expression {
                    // Store address is pushed onto the stack
                    self.compile_address(&ast::Expression::new_value(
                        ast::Value::Identifier(name.to_string(), Rc::clone(&declaration.location)),
                        declaration.decl_type.to_owned(),
                    ))?;
                    self.push();

                    // Value to store is placed in RAX
                    self.compile_expression(&e)?;
                    self.compile_store(&declaration.decl_type, 0)?;
                    self.pop_discard();
                }
            }
            ast::Statement::Expression(e) => {
                self.debug_statement(statement)?;
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

    fn compile_declaration(
        &mut self,
        declaration: &ast::DeclarationStatement,
    ) -> std::io::Result<()> {
        let name = &declaration.name;

        if let Some(e) = &declaration.expression {
            self.debug_expression(name, e)?;
            // Store address is pushed onto the stack
            self.compile_address(&ast::Expression::new_value(
                ast::Value::Identifier(name.to_string(), Rc::clone(&declaration.location)),
                declaration.decl_type.to_owned(),
            ))?;
            self.push(); // This is consumed by compile_store

            // Value to store is placed in RAX
            self.compile_expression(&e)?;
            self.compile_store(&declaration.decl_type, 0)?;
            self.pop_discard();
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> std::io::Result<()> {
        let result_64 = reg::RAX;
        let result_32 = reg::EAX;
        let result_8 = reg::AL;
        let result = match expression.expr_type.size() {
            1 => result_8,
            4 => result_32,
            8 => result_64,
            s => unimplemented!("size not implemented, {}", s),
        };
        let extra_64 = reg::RDI;
        let extra_32 = reg::EDI;
        let extra_8 = reg::DL;
        let extra = match expression.expr_type.size() {
            1 => extra_8,
            4 => extra_32,
            8 => extra_64,
            s => unimplemented!("size not implemented, {}", s),
        };
        let op_suffix = match expression.expr_type.size() {
            1 => "b",
            4 => "l",
            8 => "q",
            _ => todo!(),
        };

        let op_suffix = |op: &str| format!("{}{}", op, op_suffix);

        match &expression.node {
            ast::ExpressionNode::Binary(ast::BinOp::Assign(op), lhs, rhs) => {
                // Put the lhs address on the stack
                self.compile_address(lhs)?;
                self.push();

                // Put the rhs value in RDI
                self.compile_expression(rhs)?;

                if let Some(op) = op {
                    // Store the value into RDI
                    // TODO: move larger than 8 bytes here
                    assemble!(self, "mov", result, extra);

                    // The stack pointer points to the stack, where we pushed the address
                    assemble!(
                        self,
                        "mov",
                        RegisterIndirectLocation::new(reg::RSP),
                        reg::RAX
                    );

                    // Put the lhs value in RAX
                    self.compile_load(&lhs.expr_type)?;

                    match op {
                        ast::AssignOp::Sum => {
                            assemble!(self, op_suffix("add"), extra, result);
                        }
                        ast::AssignOp::Difference => {
                            assemble!(self, op_suffix("sub"), extra, result);
                        }
                        ast::AssignOp::Product => {
                            assemble!(self, op_suffix("mul"), extra);
                        }
                        ast::AssignOp::Quotient => {
                            assemble!(self, op_suffix("div"), extra);
                        }
                    }
                }

                self.compile_store(&lhs.expr_type, 0)?;
                self.pop_discard();
            }
            ast::ExpressionNode::Binary(op, lhs, rhs) => {
                self.compile_expression(rhs.as_ref())?;

                self.push();

                let top_of_stack = StackRelativeLocation::top(rhs.expr_type.size());
                self.compile_expression(&lhs)?;

                // allow 1byte, 4byte, 8byte operations
                match op {
                    ast::BinOp::Sum => {
                        assemble!(self, op_suffix("add"), top_of_stack, result);
                    }
                    ast::BinOp::Difference => {
                        assemble!(self, op_suffix("sub"), top_of_stack, result);
                    }
                    ast::BinOp::Product => {
                        assemble!(self, op_suffix("mul"), top_of_stack);
                    }
                    ast::BinOp::Quotient => {
                        assemble!(self, op_suffix("div"), top_of_stack);
                    }
                    ast::BinOp::Equals => {
                        assemble!(self, op_suffix("cmp"), top_of_stack, result);
                        assemble!(self, "sete", result_8);
                        assemble!(self, op_suffix("movzb"), result_8, result);
                    }
                    ast::BinOp::NotEquals => {
                        assemble!(self, op_suffix("cmp"), top_of_stack, result);
                        assemble!(self, "setne", result_8);
                        assemble!(self, op_suffix("movzb"), result_8, result);
                    }
                    ast::BinOp::LessThan => {
                        // signed
                        assemble!(self, op_suffix("cmp"), top_of_stack, result);
                        assemble!(self, "setb", result_8);
                        assemble!(self, op_suffix("movzb"), result_8, result);
                    }
                    ast::BinOp::GreaterThan => {
                        // signed
                        assemble!(self, op_suffix("cmp"), top_of_stack, result);
                        assemble!(self, "setg", result_8);
                        assemble!(self, op_suffix("movzb"), result_8, result);
                    }
                    _ => todo!("implement binop {:?}", op),
                }

                self.pop_discard();
            }
            ast::ExpressionNode::Unary(op, lhs) => match op {
                ast::UnaryOp::Deref => {
                    self.compile_expression(lhs.as_ref())?;
                    assemble!(
                        self,
                        "mov",
                        RegisterIndirectLocation::new(result_64),
                        result_64
                    )
                }
                ast::UnaryOp::Negate => {
                    self.compile_expression(lhs.as_ref())?;
                    assemble!(self, op_suffix("neg"), result);
                }
                ast::UnaryOp::PreDecrement | ast::UnaryOp::PreIncrement => {
                    self.compile_address(lhs)?;
                    self.push();
                    self.compile_load(&lhs.expr_type)?;
                    let op = match op {
                        ast::UnaryOp::PreDecrement => op_suffix("dec"),
                        ast::UnaryOp::PreIncrement => op_suffix("inc"),
                        _ => unreachable!(),
                    };
                    assemble!(self, op, result);
                    self.compile_store(&lhs.expr_type, 0)?;
                    self.pop_discard();
                }
                ast::UnaryOp::PostDecrement | ast::UnaryOp::PostIncrement => {
                    self.compile_address(lhs)?;
                    self.push();
                    self.compile_load(&lhs.expr_type)?;
                    self.push();
                    let op = match op {
                        ast::UnaryOp::PostDecrement => op_suffix("dec"),
                        ast::UnaryOp::PostIncrement => op_suffix("inc"),
                        _ => unreachable!(),
                    };
                    assemble!(self, op, result);
                    self.compile_store(&lhs.expr_type, 1)?;
                    self.pop_reg(result);
                    self.pop_discard();
                }
            },
            ast::ExpressionNode::Conditional(
                condition_expression,
                expression_if_true,
                expression_if_false,
            ) => {
                self.debug_expression("conditional", condition_expression)?;
                self.compile_expression(condition_expression)?;
                let else_label = self.labels.allocate_label();
                let end_label = self.labels.allocate_label();

                // jump to else if $eax == zero
                // TODO: support 8 byte values
                assemble!(self, "cmp", DL::new(0), reg::EAX);
                assemble!(self, "jz", else_label);

                // --[if body]
                self.compile_expression(expression_if_true)?;

                // -- jump to end
                assemble!(self, "jmp", end_label);

                // else:
                self.output_label(else_label)?;

                // --[else body]
                self.compile_expression(expression_if_false)?;

                // end:
                self.output_label(end_label)?;
            }
            ast::ExpressionNode::Value(v) => match v {
                ast::Value::Literal(l) => match l {
                    ast::LiteralValue::Int32(value) => {
                        assemble!(self, "movl", DL::new(value), result_32);
                    }
                    ast::LiteralValue::StringLiteral(_) => {
                        self.compile_address(expression)?;
                    }
                    ast::LiteralValue::CharLiteral(c) => {
                        assemble!(self, "movl", CL::new(c), result_32);
                    }
                },
                ast::Value::Identifier(_, _) => {
                    self.compile_address(expression)?;
                    self.compile_load(&expression.expr_type)?;
                }
            },
            ast::ExpressionNode::Call(lhs, args) => {
                // TODO: check function types

                if args.len() > platform::ParameterPlacement::max_params() {
                    unimplemented!("too many params for registers");
                }

                let call_is_vararg = lhs.expr_type.is_vararg_call();

                let mut param_place = platform::ParameterPlacement::default();

                let mut arg_reg = Vec::new();

                for arg in args {
                    let expr_type = &arg.expr_type;
                    let param = param_place.place(&expr_type);
                    arg_reg.push(param.reg);

                    if param.reg.is_some() {
                        self.compile_expression(arg)?;
                        self.push();
                        match expr_type.size() {
                            4 | 8 => {}
                            _ => todo!("Can't call with parameter of this size"),
                        }
                    } else {
                        unimplemented!("argument not passed by register");
                    }
                }

                // Stack now contains arguments, popping will get them in
                // reversed order. put each argument in its register
                for reg in arg_reg.iter().rev() {
                    if let Some(reg) = reg {
                        self.pop_reg(*reg);
                    } else {
                        unimplemented!("argument not passed by register");
                    }
                }

                let call_align = self.setup_stack_for_call();

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

                self.reset_stack_for_call(call_align);
            }
        }
        Ok(())
    }
}

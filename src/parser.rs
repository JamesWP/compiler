use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{ast, scope::Scope};

pub struct ParserInput {
    tokens: Vec<ast::Token>,
    print_lex: bool,
}

pub struct ParserState {
    input: ParserInput,
    scope: Scope,
}

// TODO: result error should point at error position
type ParseResult<T> = std::result::Result<T, String>;

impl ParserInput {
    fn peek(&self) -> Option<&ast::Token> {
        self.tokens.last()
    }
    fn pop(&mut self) -> Option<ast::Token> {
        let t = self.tokens.pop();
        if self.print_lex {
            if let Some(t) = t {
                println!("[{:?}]", t);
                Some(t)
            } else {
                t
            }
        } else {
            t
        }
    }

    #[must_use]
    fn expect<TT: AsRef<ast::TokenType>>(&mut self, token: TT) -> Result<(), String> {
        if self.peek().map(|t| t.tt.as_ref()) == Some(token.as_ref()) {
            self.pop();
            Ok(())
        } else {
            Err(format!(
                "expected token {:?} found {:?}",
                token.as_ref(),
                self.peek()
            ))
        }
    }

    pub fn enable_debug(mut self) -> ParserInput {
        self.print_lex = true;
        self
    }
}

impl From<Vec<ast::Token>> for ParserInput {
    fn from(mut val: Vec<ast::Token>) -> ParserInput {
        val.reverse();
        ParserInput {
            tokens: val,
            print_lex: false,
        }
    }
}

fn is_type_decl(token: Option<ast::TokenType>) -> bool {
    use crate::ast::TokenType::*;
    match token {
        Some(Int) | Some(Char) | Some(Const) | Some(Long) => true,
        _ => false,
    }
}

impl ParserState {
    pub fn new(input: ParserInput) -> ParserState {
        ParserState {
            input,
            scope: Scope::default(),
        }
    }

    fn matches<TT: AsRef<ast::TokenType>>(&mut self, tt: TT) -> bool {
        let token = self.input.peek();
        if let Some(token) = token {
            let matches = &token.tt == tt.as_ref();
            if matches {
                self.input.pop();
            }
            matches
        } else {
            false
        }
    }

    fn peek_type(&mut self) -> Option<ast::TokenType> {
        self.input.peek().map(|t| t.tt.clone())
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        if self.matches(ast::TokenType::Return) {
            if self.matches(ast::TokenType::Semicolon) {
                Ok(ast::Statement::JumpStatement(ast::JumpStatement::Return))
            } else {
                let return_expr = self.parse_expression()?;
                self.input.expect(ast::TokenType::Semicolon)?;

                Ok(ast::Statement::JumpStatement(
                    ast::JumpStatement::ReturnWithValue(return_expr),
                ))
            }
        } else if self.matches(ast::TokenType::Do) {
            let loop_body = Box::new(self.parse_statement()?);
            let condition_expression = if self.matches(ast::TokenType::While) {
                self.input.expect(ast::TokenType::LParen)?;
                let condition_expression = self.parse_expression()?;
                self.input.expect(ast::TokenType::RParen)?;
                Some(condition_expression)
            } else {
                None
            };

            self.input.expect(ast::TokenType::Semicolon)?;

            Ok(ast::Statement::DoStatement(loop_body, condition_expression))
        } else if self.matches(ast::TokenType::While) {
            self.input.expect(ast::TokenType::LParen)?;
            let condition_expression = self.parse_expression()?;
            self.input.expect(ast::TokenType::RParen)?;
            let loop_body = Box::new(self.parse_statement()?);

            Ok(ast::Statement::ForStatement(ast::ForStatement {
                initialization: ast::ForHead::WithNoExpression,
                post_iteration_expression: None,
                control_expression: condition_expression,
                loop_body,
            }))
        } else if self.matches(ast::TokenType::For) {
            // 6.8.5.3 The for statement
            self.input.expect(ast::TokenType::LParen)?;

            let head = if is_type_decl(self.peek_type()) {
                self.scope.begin_scope()?;
                // This declares a new variable scoped to the other expressions and the body
                ast::ForHead::WithDeclaration(self.parse_declaration()?)
            } else {
                if self.matches(ast::TokenType::Semicolon) {
                    ast::ForHead::WithNoExpression
                } else {
                    let expr = ast::ForHead::WithExpression(self.parse_expression()?);
                    assert!(self.matches(ast::TokenType::Semicolon));
                    expr
                }
            };

            // This expression will decide if the body will be executed
            // if this expression evaluates to zero the loop will continue
            let controlling_expression = if self.matches(ast::TokenType::Semicolon) {
                // The expresson was not provided, evaluate to some none zero value
                let literal_value = ast::Value::Literal(ast::LiteralValue::Int32(1));
                let literal_type = ast::TypeDefinition::INT {
                    size: ast::IntSize::Four,
                    qualifier: ast::TypeQualifier::from(true),
                };
                ast::Expression::new_value(literal_value, literal_type)
            } else {
                let expression = self.parse_expression()?;
                self.input.expect(ast::TokenType::Semicolon)?;
                expression
            };

            // This expression is evaluated after each execution of the loop body
            let post_iteration_expression = if self.matches(ast::TokenType::RParen) {
                let literal_value = ast::Value::Literal(ast::LiteralValue::Int32(1));
                let literal_type = ast::TypeDefinition::INT {
                    size: ast::IntSize::Four,
                    qualifier: ast::TypeQualifier::from(true),
                };
                ast::Expression::new_value(literal_value, literal_type)
            } else {
                let expression = self.parse_expression()?;
                self.input.expect(ast::TokenType::RParen)?;
                expression
            };

            let loop_body = Box::new(self.parse_statement()?);

            match &head {
                ast::ForHead::WithDeclaration(_) => {
                    self.scope.end_scope()?;
                }
                _ => {}
            }

            Ok(ast::Statement::ForStatement(ast::ForStatement {
                initialization: head,
                control_expression: controlling_expression,
                post_iteration_expression: Some(post_iteration_expression),
                loop_body,
            }))
        } else if self.matches(ast::TokenType::If) {
            self.input.expect(ast::TokenType::LParen)?;
            let condition_expression = self.parse_expression()?;
            self.input.expect(ast::TokenType::RParen)?;
            let if_body = Box::new(self.parse_statement()?);
            let else_body = if self.matches(ast::TokenType::Else) {
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };

            Ok(ast::Statement::IfStatement(ast::IfStatement {
                condition_expression,
                if_body,
                else_body,
            }))
        } else if is_type_decl(self.peek_type()) {
            let declaration_statement = self.parse_declaration()?;

            Ok(ast::Statement::DeclarationStatement(declaration_statement))
        } else if self.peek_type() == Some(ast::TokenType::LBrace) {
            self.scope.begin_scope()?;
            let statement = self.parse_compound_statement()?;
            self.scope.end_scope()?;
            Ok(statement)
        } else if self.matches(ast::TokenType::Semicolon) {
            Ok(ast::Statement::NoopStatement)
        } else {
            let expr = self.parse_expression()?;
            self.input.expect(ast::TokenType::Semicolon)?;
            Ok(ast::Statement::Expression(expr))
        }
    }

    fn parse_declaration(&mut self) -> ParseResult<ast::DeclarationStatement> {
        assert!(is_type_decl(self.peek_type()));
        let base_type = self.parse_declaration_specifiers()?;
        let (name, decl_type) = self.parse_declarator(base_type)?;
        let location = self.scope.define(&name, &decl_type, false);
        if self.matches(ast::TokenType::Semicolon) {
            Ok(ast::DeclarationStatement::new(decl_type, name, location))
        } else {
            self.input.expect(ast::TokenType::Equals)?;
            let expression = self.parse_expression()?;
            self.input.expect(ast::TokenType::Semicolon)?;
            Ok(ast::DeclarationStatement::new_with_expression(
                decl_type, name, expression, location,
            ))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut conditional_expression = self.parse_conditional_expression()?;
        loop {
            let op = match self.peek_type() {
                Some(ast::TokenType::PlusEquals) => ast::BinOp::Assign(Some(ast::AssignOp::Sum)),
                Some(ast::TokenType::MinusEquals) => {
                    ast::BinOp::Assign(Some(ast::AssignOp::Difference))
                }
                Some(ast::TokenType::MultiplyEquals) => {
                    ast::BinOp::Assign(Some(ast::AssignOp::Product))
                }
                Some(ast::TokenType::DivideEquals) => {
                    ast::BinOp::Assign(Some(ast::AssignOp::Quotient))
                }
                Some(ast::TokenType::Equals) => ast::BinOp::Assign(None),
                _ => {
                    return Ok(conditional_expression);
                }
            };

            self.input.pop();

            let next_equality = self.parse_conditional_expression()?;

            conditional_expression =
                ast::Expression::new_binop(op, conditional_expression.into(), next_equality.into());
        }
    }

    pub fn parse_conditional_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut equality_expression = self.parse_equality_expression()?;

        if self.matches(ast::TokenType::Question) {
            let expression_if_true = self.parse_expression()?;
            self.input.expect(ast::TokenType::Colon)?;
            let expression_if_false = self.parse_expression()?;

            equality_expression = ast::Expression::new_conditional(
                Box::new(equality_expression),
                Box::new(expression_if_true),
                Box::new(expression_if_false),
            );
        }

        Ok(equality_expression)
    }

    fn parse_equality_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut relational_expression = self.parse_relational_expression()?;

        loop {
            let op = match self.peek_type() {
                Some(ast::TokenType::Equality) => ast::BinOp::Equals,
                Some(ast::TokenType::NotEquality) => ast::BinOp::NotEquals,
                _ => {
                    return Ok(relational_expression);
                }
            };

            self.input.pop();

            let next_relational = self.parse_relational_expression()?;

            relational_expression = ast::Expression::new_binop(
                op,
                relational_expression.into(),
                next_relational.into(),
            );
        }
    }

    fn parse_relational_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut additive_expression = self.parse_additive_expression()?;

        loop {
            let op = match self.peek_type() {
                Some(ast::TokenType::LeftBitShift) => ast::BinOp::LeftBitShift,
                Some(ast::TokenType::LessThan) => ast::BinOp::LessThan,
                Some(ast::TokenType::RightBitShift) => ast::BinOp::RightBitShift,
                Some(ast::TokenType::GreaterThan) => ast::BinOp::GreaterThan,
                _ => {
                    return Ok(additive_expression);
                }
            };

            self.input.pop();

            let next_additive = self.parse_additive_expression()?;

            additive_expression =
                ast::Expression::new_binop(op, additive_expression.into(), next_additive.into());
        }
    }

    fn parse_additive_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut multiplicative_expression = self.parse_multiplicative_expression()?;

        loop {
            let op = match self.peek_type() {
                Some(ast::TokenType::Plus) => ast::BinOp::Sum,
                Some(ast::TokenType::Minus) => ast::BinOp::Difference,
                _ => {
                    return Ok(multiplicative_expression);
                }
            };

            self.input.pop();

            let next_unary = self.parse_multiplicative_expression()?;

            multiplicative_expression =
                ast::Expression::new_binop(op, multiplicative_expression.into(), next_unary.into());
        }
    }

    fn parse_multiplicative_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut unary_expression = self.parse_unary_expression()?;

        loop {
            let op = match self.peek_type() {
                Some(ast::TokenType::Star) => ast::BinOp::Product,
                Some(ast::TokenType::Divide) => ast::BinOp::Quotient,
                _ => {
                    return Ok(unary_expression);
                }
            };

            self.input.pop();

            let next_unary = self.parse_unary_expression()?;

            unary_expression =
                ast::Expression::new_binop(op, unary_expression.into(), next_unary.into());
        }
    }

    fn parse_unary_expression(&mut self) -> ParseResult<ast::Expression> {
        // self.parse_postfix_expression()
        let op = match self.peek_type() {
            Some(ast::TokenType::Minus) => ast::UnaryOp::Negate,
            Some(ast::TokenType::Plus) => todo!(),
            Some(ast::TokenType::PlusPlus) => ast::UnaryOp::PreIncrement,
            Some(ast::TokenType::MinusMinus) => ast::UnaryOp::PreDecrement,
            Some(ast::TokenType::Star) => todo!(),
            Some(ast::TokenType::Not) => todo!(),
            _ => {
                return self.parse_postfix_expression();
            }
        };

        self.input.pop();
        let cast_expr = self.parse_cast_expression()?;
        Ok(ast::Expression::new_unaryop(op, Box::new(cast_expr)))
    }

    fn parse_cast_expression(&mut self) -> ParseResult<ast::Expression> {
        //TODO: add cast expression
        self.parse_unary_expression()
    }

    fn parse_postfix_expression(&mut self) -> ParseResult<ast::Expression> {
        let mut value = self.parse_primary_expression()?;

        loop {
            if self.matches(ast::TokenType::LSquare) {
                // this is a postfix "array access"
                // a[b] => *(a+b)
                let postfix_expr = self.parse_expression()?;

                self.input.expect(ast::TokenType::RSquare)?;

                value = ast::Expression::new_unaryop(
                    ast::UnaryOp::Deref,
                    Box::new(ast::Expression::new_binop(
                        ast::BinOp::Sum,
                        Box::new(value),
                        Box::new(postfix_expr),
                    )),
                )
            } else if self.matches(ast::TokenType::LParen) {
                // this is a function call. e.g. blah(1,2,3+4)
                let argument_expressions = self.parse_argument_expression_list()?;
                self.input.expect(ast::TokenType::RParen)?;

                value = ast::Expression::new_call(value.into(), argument_expressions)
            } else if self.matches(ast::TokenType::PlusPlus) {
                value = ast::Expression::new_unaryop(ast::UnaryOp::PostIncrement, Box::new(value))
            } else if self.matches(ast::TokenType::MinusMinus) {
                value = ast::Expression::new_unaryop(ast::UnaryOp::PostDecrement, Box::new(value))
            } else {
                return Ok(value);
            }
        }
    }

    fn parse_primary_expression(&mut self) -> ParseResult<ast::Expression> {
        let (value, expr_type) = match self.peek_type() {
            Some(ast::TokenType::Value(v)) => {
                self.input.pop();
                (
                    ast::Value::Literal(ast::LiteralValue::Int32 { 0: v as i32 }),
                    ast::TypeDefinition::INT {
                        size: ast::IntSize::Four,
                        qualifier: ast::TypeQualifier::from(true),
                    },
                )
            }
            Some(ast::TokenType::StringLiteral(v)) => {
                self.input.pop();
                (
                    ast::Value::Literal(ast::LiteralValue::StringLiteral(v)),
                    ast::TypeDefinition::INT {
                        size: ast::IntSize::One,
                        qualifier: ast::TypeQualifier::from(true),
                    }
                    .as_pointer_to(ast::TypeQualifier::from(false)),
                )
            }
            Some(ast::TokenType::CharLiteral(v)) => {
                self.input.pop();
                (
                    ast::Value::Literal(ast::LiteralValue::CharLiteral(
                        v.chars()
                            .next()
                            .expect("char literal needs at least one char"),
                    )),
                    ast::TypeDefinition::INT {
                        size: ast::IntSize::Four,
                        qualifier: ast::TypeQualifier::from(true),
                    }, // This is strange, apparently in C sizeof('a') == 4!
                )
            }
            Some(ast::TokenType::Identifier(v)) => {
                let ident_type = self.scope.find(&v);
                self.input.pop();
                if let Some((type_decl, location)) = ident_type {
                    (
                        ast::Value::Identifier(v, Rc::clone(location)),
                        type_decl.clone(),
                    )
                } else {
                    unimplemented!("variable references undeclared identifier {}", v);
                }
            }
            Some(ast::TokenType::LParen) => {
                self.input.pop();
                let expr = self.parse_expression()?;
                self.input.expect(ast::TokenType::RParen)?;
                return Ok(expr);
            }
            Some(ast::TokenType::Sizeof) => {
                self.input.pop();

                let expr = self.parse_unary_expression()?;

                // TODO: this should be an unsigned type
                (
                    ast::Value::Literal(ast::LiteralValue::Int32(
                        expr.expr_type.size().try_into().unwrap(),
                    )),
                    ast::TypeDefinition::INT {
                        size: ast::IntSize::Eight,
                        qualifier: ast::TypeQualifier::from(true),
                    },
                )
            }
            _ => unimplemented!(
                "Unable to parse primary expression. expected token found {:?}",
                self.input.peek()
            ),
        };

        let expr = ast::Expression::new_value(value, expr_type);
        Ok(expr)
    }

    fn parse_argument_expression_list(&mut self) -> ParseResult<Vec<ast::Expression>> {
        let mut args = vec![];
        loop {
            if self.peek_type() == Some(ast::TokenType::RParen) {
                return Ok(args);
            }

            let expr = self.parse_expression()?;
            args.push(expr);

            if self.matches(ast::TokenType::Comma) {
                continue;
            }
        }
    }

    pub fn parse_translation_unit(&mut self) -> ParseResult<ast::TranslationUnit> {
        let mut translation_unit = ast::TranslationUnit::default();
        while self.input.peek().is_some() {
            let func = self.parse_function_definition()?;
            let (name, def) = func;
            translation_unit.add_definition(&name, def);
        }
        Ok(translation_unit)
    }

    fn parse_function_definition(&mut self) -> ParseResult<(String, ast::FunctionDefinition)> {
        let base_type = self.parse_declaration_specifiers()?;
        let (name, decl_type) = self.parse_declarator(base_type)?;

        if let ast::TypeDefinition::FUNCTION(return_type, parameters, _is_local) = decl_type {
            let is_definition = self.peek_type() == Some(ast::TokenType::LBrace);
            self.scope.define(
                &name,
                &return_type
                    .clone()
                    .as_function_taking(parameters.clone(), is_definition),
                true,
            );
            if is_definition {
                self.scope.begin_function_scope()?;
                // Construct a new parameter list from the old one, using the locations defined in the scope
                let parameters: Vec<_> = parameters
                    .iter()
                    .map(|param| {
                        let location = self.scope.define(&param.name, &param.decl_type, false);
                        ast::Parameter {
                            name: param.name.clone(),
                            decl_type: param.decl_type.clone(),
                            location,
                        }
                    })
                    .collect();
                let compound_statement = self.parse_compound_statement()?;
                self.scope.end_function_scope()?;
                Ok((
                    name,
                    ast::FunctionDefinition::new(
                        *return_type,
                        parameters.into(),
                        compound_statement,
                    ),
                ))
            } else {
                self.input.expect(ast::TokenType::Semicolon)?;
                Ok((
                    name,
                    ast::FunctionDefinition::new_declaration(*return_type, parameters.into()),
                ))
            }
        } else {
            unimplemented!("Nope");
        }
    }

    fn parse_declaration_specifiers(&mut self) -> ParseResult<ast::TypeDefinition> {
        self.parse_type_specifier()
    }

    fn parse_type_specifier(&mut self) -> ParseResult<ast::TypeDefinition> {
        let mut type_word = None;
        let mut is_const = false;
        let mut longs = 0;
        loop {
            if self.matches(ast::TokenType::Const) {
                is_const = true;
                continue;
            }
            if self.matches(ast::TokenType::Long) {
                longs += 1;
                continue;
            }
            let new_type = if self.matches(ast::TokenType::Char) {
                ast::TokenType::Char
            } else if self.matches(ast::TokenType::Int) {
                ast::TokenType::Int
            } else {
                break;
            };

            if matches!(type_word, Some(_)) {
                unimplemented!("can't specify two types in definition");
            }

            type_word = Some(new_type);
        }

        match type_word {
            Some(ast::TokenType::Int) => Ok(ast::TypeDefinition::INT {
                size: match longs {
                    1 | 0 => ast::IntSize::Four,
                    2 => ast::IntSize::Eight,
                    _ => unimplemented!(),
                },
                qualifier: ast::TypeQualifier::from(is_const),
            }),
            Some(ast::TokenType::Char) => Ok(ast::TypeDefinition::INT {
                size: ast::IntSize::One,
                qualifier: ast::TypeQualifier::from(is_const),
            }),
            None => Ok(ast::TypeDefinition::default()),
            Some(_) => unimplemented!(),
        }
    }

    fn parse_type_qualifier(&mut self) -> ParseResult<ast::TypeQualifier> {
        let mut is_const = false;
        while self.matches(ast::TokenType::Const) {
            is_const = true;
        }

        Ok(ast::TypeQualifier::from(is_const))
    }

    fn parse_declarator(
        &mut self,
        mut base_type: ast::TypeDefinition,
    ) -> ParseResult<(String, ast::TypeDefinition)> {
        // parse pointers
        while self.matches(ast::TokenType::Star) {
            let type_qualifier = self.parse_type_qualifier()?;
            base_type = base_type.as_pointer_to(type_qualifier)
        }

        if let Some(ast::TokenType::Identifier(name)) = self.peek_type() {
            self.input.pop();
            match self.peek_type() {
                Some(ast::TokenType::LParen) => {
                    self.input.pop();
                    let parameter_list = self.parse_parameter_list()?;

                    self.input.expect(ast::TokenType::RParen)?;

                    return Ok((name, base_type.as_function_taking(parameter_list, false)));
                }
                Some(ast::TokenType::LSquare) => {
                    self.input.pop();
                    self.input.expect(ast::TokenType::RSquare)?;

                    return Ok((name, base_type.as_pointer_to(false.into())));
                }
                _ => {
                    return Ok((name, base_type));
                }
            };
        } else {
            return Ok((String::new(), base_type));
        }
    }

    fn parse_parameter_list(&mut self) -> ParseResult<ast::ParameterList> {
        let mut param_list = Vec::new();

        loop {
            if self.peek_type() == Some(ast::TokenType::RParen) {
                break;
            }

            if self.matches(ast::TokenType::Elipsis) {
                let param_list: ast::ParameterList = param_list.into();
                return Ok(param_list.with_var_args());
            }

            let base_type = self.parse_declaration_specifiers()?;
            let (name, decl_type) = self.parse_declarator(base_type)?;
            let location = Rc::new(RefCell::new(None));
            param_list.push(ast::Parameter {
                name,
                decl_type,
                location,
            });

            if self.matches(ast::TokenType::Comma) {
                continue;
            }

            if self.peek_type() == Some(ast::TokenType::RParen) {
                break;
            }

            unimplemented!(
                "Expected comma or close paren, instead got {:?}",
                self.input.peek()
            );
        }

        Ok(param_list.into())
    }

    fn parse_compound_statement(&mut self) -> ParseResult<ast::Statement> {
        self.input.expect(ast::TokenType::LBrace)?;

        let mut statements = Vec::new();
        while self.peek_type() != Some(ast::TokenType::RBrace) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        self.input.expect(ast::TokenType::RBrace)?;

        Ok(statements.into())
    }
}

#[test]
fn test_parse_translation_unit() {
    let input = vec![
        ast::TokenType::Int,
        ast::TokenType::Identifier("foo".to_owned()),
        ast::TokenType::LParen,
        ast::TokenType::Int,
        ast::TokenType::Identifier("i".to_owned()),
        ast::TokenType::RParen,
        ast::TokenType::LBrace,
        ast::TokenType::Return,
        ast::TokenType::Value(3),
        ast::TokenType::Semicolon,
        ast::TokenType::RBrace,
    ];

    let input: Vec<_> = input.into_iter().map(ast::Token::from).collect();

    let parse_result = ParserState::new(input.into()).parse_translation_unit();

    println!("{:#?}", parse_result);
}

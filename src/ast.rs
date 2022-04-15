use std::{collections::HashSet, fmt::Display};

use crate::scope::SharedOptionStackLocation;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub tt: TokenType,
    pub token_start: (usize, usize, usize),
    pub token_end: (usize, usize, usize),
    pub is_bol: bool,
    pub is_wsep: bool,
    pub token_text: String,
    pub hideset: Option<HashSet<String>>,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            tt: TokenType::EOF,
            token_start: (0, 0, 0),
            token_end: (0, 0, 0),
            is_bol: true,
            is_wsep: true,
            token_text: String::new(),
            hideset: Default::default(),
        }
    }
}

#[cfg(test)]
impl From<TokenType> for Token {
    fn from(tt: TokenType) -> Self {
        Token {
            tt,
            token_start: (0, 0, 0),
            token_end: (0, 0, 0),
            is_bol: true,
            is_wsep: true,
            token_text: String::new(),
            hideset: Default::default(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_bol {
            f.write_str("\n")?;
            for _ in 0..self.token_start.1 {
                f.write_str(" ")?
            }
        } else if self.is_wsep {
            f.write_str(" ")?;
        }

        f.write_str(&self.token_text)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    Identifier(String),
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    LParen,
    RParen,
    Value(i64),
    StringLiteral(String),
    CharLiteral(String),
    Question,
    Colon,
    Semicolon,
    Divide,
    Plus,
    PlusPlus,
    Minus,
    Comma,
    Equals,
    Elipsis,
    Star,
    LessThan,
    GreaterThan,
    LeftBitShift,
    RightBitShift,
    Equality,
    NotEquality,
    PlusEquals,
    MinusEquals,
    DivideEquals,
    MultiplyEquals,
    Not,
    Return,
    Int,
    Char,
    Const,
    Long,
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Sizeof,
    Hash,
}

impl AsRef<TokenType> for TokenType {
    fn as_ref(&self) -> &TokenType {
        &self
    }
}

impl From<String> for TokenType {
    fn from(token: String) -> Self {
        if token == "int" {
            TokenType::Int
        } else if token == "char" {
            TokenType::Char
        } else if token == "const" {
            TokenType::Const
        } else if token == "long" {
            TokenType::Long
        } else if token == "return" {
            TokenType::Return
        } else if token == "if" {
            TokenType::If
        } else if token == "else" {
            TokenType::Else
        } else if token == "while" {
            TokenType::While
        } else if token == "do" {
            TokenType::Do
        } else if token == "for" {
            TokenType::For
        } else if token == "continue" {
            TokenType::Continue
        } else if token == "break" {
            TokenType::Break
        } else if token == "sizeof" {
            TokenType::Sizeof
        } else {
            TokenType::Identifier(token)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub function_definitions: Vec<(String, FunctionDefinition)>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub return_type: TypeDefinition,
    pub parameter_list: ParameterList,
    pub function_body: Option<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub decl_type: TypeDefinition,
    pub location: SharedOptionStackLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterList {
    parameters: Vec<Parameter>,
    pub var_args: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeQualifier {
    pub is_const: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntSize {
    One = 1,
    Four = 4,
    Eight = 8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    INT {
        size: IntSize,
        qualifier: TypeQualifier,
    },
    FUNCTION(Box<TypeDefinition>, ParameterList, bool), // bool is definition / is local
    POINTER(TypeQualifier, Box<TypeDefinition>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    JumpStatement(JumpStatement),
    ForStatement(ForStatement),
    DoStatement(Box<Statement>, Option<Expression>),
    IfStatement(IfStatement),
    DeclarationStatement(DeclarationStatement),
    CompoundStatement(Vec<Statement>),
    Expression(Expression),
    NoopStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationStatement {
    pub name: String,
    pub decl_type: TypeDefinition,
    pub expression: Option<Expression>,
    pub location: SharedOptionStackLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpStatement {
    Return,
    ReturnWithValue(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForHead {
    WithDeclaration(DeclarationStatement),
    WithExpression(Expression),
    WithNoExpression
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub initialization: ForHead,
    pub control_expression: Expression,
    pub post_iteration_expression: Option<Expression>,
    pub loop_body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition_expression: Expression,
    pub if_body: Box<Statement>,
    pub else_body: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Sum,
    Difference,
    Product,
    Quotient,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Sum,
    Difference,
    Product,
    Quotient,
    Assign(Option<AssignOp>),
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    LeftBitShift,
    RightBitShift,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Deref,
    PostfixIncrement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Value(Value),
    Call(Box<Expression>, Vec<Expression>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expr_type: TypeDefinition,
    pub node: ExpressionNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(LiteralValue),
    Identifier(String, SharedOptionStackLocation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int32(i32),
    StringLiteral(String),
    CharLiteral(char),
}

impl From<Vec<Parameter>> for ParameterList {
    fn from(parameters: Vec<Parameter>) -> ParameterList {
        ParameterList {
            parameters,
            var_args: false,
        }
    }
}

impl Into<Vec<Parameter>> for ParameterList {
    fn into(self) -> Vec<Parameter> {
        self.parameters
    }
}

impl ParameterList {
    pub fn with_var_args(mut self) -> Self {
        self.var_args = true;
        self
    }
}

impl From<Vec<Statement>> for Statement {
    fn from(statements: Vec<Statement>) -> Statement {
        Statement::CompoundStatement(statements)
    }
}

impl Default for TypeDefinition {
    fn default() -> Self {
        TypeDefinition::INT {
            size: IntSize::Four,
            qualifier: TypeQualifier::default(),
        }
    }
}

impl IntoIterator for Statement {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Statement::CompoundStatement(statements) => statements.into_iter(),
            _ => unimplemented!(),
        }
    }
}

impl Statement {
    pub fn iter(&self) -> std::slice::Iter<Statement> {
        match self {
            Statement::CompoundStatement(statements) => statements.iter(),
            _ => unimplemented!(),
        }
    }

    pub fn visit_statements<F>(&self, f: &mut F)
    where
        F: FnMut(&Statement) -> (),
    {
        f(self);

        // TODO: when statements can be nested, this needs to visit all of them!
        match self {
            // These statements can't contain any other statements
            Statement::NoopStatement => {}
            Statement::JumpStatement(_) => {}
            Statement::DeclarationStatement(_) => {}
            Statement::Expression(_) => {}
            // These statements can contain other statements, visit them
            Statement::DoStatement(w, _) => {
                w.visit_statements(f);
            }
            Statement::ForStatement(fr) => {
                fr.loop_body.visit_statements(f);
            }
            Statement::IfStatement(i) => {
                i.if_body.visit_statements(f);
                if let Some(else_body) = &i.else_body {
                    else_body.visit_statements(f);
                }
            }
            Statement::CompoundStatement(statements) => {
                for statement in statements {
                    statement.visit_statements(f);
                }
            }
        }
    }
}

impl ParameterList {
    pub fn iter(&self) -> std::slice::Iter<Parameter> {
        self.parameters.iter()
    }
}

impl Default for TranslationUnit {
    fn default() -> TranslationUnit {
        TranslationUnit {
            function_definitions: Vec::new(),
        }
    }
}

impl Default for TypeQualifier {
    fn default() -> Self {
        TypeQualifier { is_const: false }
    }
}

impl From<bool> for TypeQualifier {
    fn from(is_const: bool) -> Self {
        TypeQualifier { is_const }
    }
}

impl TranslationUnit {
    pub fn add_definition(&mut self, name: &str, definition: FunctionDefinition) {
        self.function_definitions
            .push((name.to_owned(), definition));
    }
}

impl FunctionDefinition {
    pub fn new(
        return_type: TypeDefinition,
        parameters: ParameterList,
        body: Statement,
    ) -> FunctionDefinition {
        FunctionDefinition {
            return_type,
            parameter_list: parameters,
            function_body: Some(body),
        }
    }

    pub fn new_declaration(
        return_type: TypeDefinition,
        parameters: ParameterList,
    ) -> FunctionDefinition {
        FunctionDefinition {
            return_type,
            parameter_list: parameters,
            function_body: None,
        }
    }

    /// return declarations in this function.
    pub fn declarations(&self) -> Vec<DeclarationStatement> {
        let mut declarations = vec![];

        let mut add_definition = |statement: &Statement| match statement {
            Statement::DeclarationStatement(d) => {
                declarations.push(d.clone());
            }
            Statement::ForStatement(ForStatement{initialization:ForHead::WithDeclaration(d), ..}) => {
                declarations.push(d.clone());
            }
            _ => {}
        };

        if let Some(statement) = &self.function_body {
            statement.visit_statements(&mut add_definition);
        }

        declarations
    }
}

impl DeclarationStatement {
    pub fn new(
        decl_type: TypeDefinition,
        name: String,
        location: SharedOptionStackLocation,
    ) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: None,
            location,
        }
    }

    pub fn new_with_expression(
        decl_type: TypeDefinition,
        name: String,
        expression: Expression,
        location: SharedOptionStackLocation,
    ) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: Some(expression),
            location,
        }
    }
}

impl TypeDefinition {
    pub fn size(&self) -> usize {
        match self {
            TypeDefinition::INT {
                size: IntSize::One,
                qualifier: _,
            } => 1,
            TypeDefinition::INT {
                size: IntSize::Four,
                qualifier: _,
            } => 4,
            TypeDefinition::INT {
                size: IntSize::Eight,
                qualifier: _,
            } => 8,
            TypeDefinition::FUNCTION(_, _, _) => 8,
            TypeDefinition::POINTER(_, _) => 8,
        }
    }

    pub fn as_function_taking(self, args: ParameterList, is_definition: bool) -> Self {
        TypeDefinition::FUNCTION(Box::new(self), args, is_definition)
    }

    pub fn as_pointer_to(self, specifiers: TypeQualifier) -> Self {
        TypeDefinition::POINTER(specifiers, Box::new(self))
    }

    pub fn is_vararg_call(&self) -> bool {
        match self {
          TypeDefinition::FUNCTION(_, params, _) => params.var_args,
          _ => false
        }
    }
}

impl Expression {
    pub fn new_binop(op: BinOp, lhs: Box<Expression>, rhs: Box<Expression>) -> Expression {
        let int_result = TypeDefinition::INT {
            size: IntSize::Four,
            qualifier: TypeQualifier::from(true),
        };
        let expr_type = match op {
            BinOp::Difference | BinOp::Product | BinOp::Quotient | BinOp::Sum => {
                use crate::ast::TypeDefinition::*;
                match (&lhs.expr_type, &rhs.expr_type) {
                    (INT { size: s1, .. }, INT { size: s2, .. }) => TypeDefinition::INT {
                        size: IntSize::max_size(s1, s2),
                        qualifier: TypeQualifier { is_const: true },
                    },
                    (INT { .. }, FUNCTION(_, _, _)) => todo!(),
                    (INT { .. }, POINTER(_, _)) => todo!(),
                    (FUNCTION(_, _, _), INT { .. }) => todo!(),
                    (FUNCTION(_, _, _), FUNCTION(_, _, _)) => todo!(),
                    (FUNCTION(_, _, _), POINTER(_, _)) => todo!(),
                    (POINTER(_, _), INT { .. }) => lhs.expr_type.clone(),
                    (POINTER(_, _), FUNCTION(_, _, _)) => todo!(),
                    (POINTER(_, _), POINTER(_, _)) => todo!(),
                }
            }
            BinOp::Assign(_) => lhs.expr_type.clone(),
            BinOp::Equals => int_result,
            BinOp::NotEquals => int_result,
            BinOp::GreaterThan => int_result,
            BinOp::LessThan => int_result,
            BinOp::LeftBitShift => todo!(),
            BinOp::RightBitShift => todo!(),
        };

        let node = match (&lhs.expr_type, &rhs.expr_type) {
            (TypeDefinition::INT { .. }, TypeDefinition::POINTER(_, _)) => {
                todo!("support int(+-)ptr")
            }
            (TypeDefinition::POINTER(_, p_type), TypeDefinition::INT { .. }) => {
                // todo: scale the int
                // Assume the lhs is a pointer to some type
                let scale_literal = LiteralValue::Int32(p_type.size() as i32);

                // If the size of the pointed is larger than one, we need to multiply
                // e.g. skipping to the next 4 byte int in memory required 1*4 to be added
                let rhs = match scale_literal {
                    LiteralValue::Int32(1) => rhs,
                    lv => Box::new(Expression::new_binop(
                        BinOp::Product,
                        rhs,
                        Box::new(Expression::new_value(
                            Value::Literal(lv),
                            TypeDefinition::INT {
                                size: IntSize::Eight,
                                qualifier: TypeQualifier::from(true),
                            },
                        )),
                    )),
                };

                ExpressionNode::Binary(op, lhs, rhs)
            }
            _ => ExpressionNode::Binary(op, lhs, rhs),
        };

        Expression { expr_type, node }
    }
    pub fn new_unaryop(op: UnaryOp, lhs: Box<Expression>) -> Expression {
        // TODO: correctly detemine the type of the Deref op
        let result_type = match op {
            UnaryOp::Deref => match &lhs.expr_type {
                TypeDefinition::INT { .. } => todo!(),
                TypeDefinition::FUNCTION(_, _, _) => todo!(),
                TypeDefinition::POINTER(_, p_type) => p_type.as_ref().clone(),
            },
            _ => lhs.expr_type.clone(),
        };

        Expression {
            expr_type: result_type,
            node: ExpressionNode::Unary(op, lhs),
        }
    }
    pub fn new_call(lhs: Box<Expression>, arguments: Vec<Expression>) -> Expression {
        // TODO: get return type from lhs expression
        if let TypeDefinition::FUNCTION(return_type, _, _) = lhs.as_ref().clone().expr_type {
            Expression {
                expr_type: *return_type,
                node: ExpressionNode::Call(lhs, arguments),
            }
        } else {
            unimplemented!("attempt to call non function {:?}", lhs.as_ref());
        }
    }
    pub fn new_value(value: Value, expr_type: TypeDefinition) -> Expression {
        Expression {
            expr_type,
            node: ExpressionNode::Value(value),
        }
    }
    pub fn new_conditional(
        condition: Box<Expression>,
        expression_if_true: Box<Expression>,
        expression_if_false: Box<Expression>,
    ) -> Expression {
        if expression_if_true.expr_type != expression_if_false.expr_type {
            todo!("conditional has different types");
        }

        let expr_type = expression_if_true.expr_type.clone();

        Expression {
            expr_type,
            node: ExpressionNode::Conditional(condition, expression_if_true, expression_if_false),
        }
    }
}

impl Token {
    pub fn pp_hide(&mut self, name: String) {
        self.hideset
            .get_or_insert_with(HashSet::default)
            .insert(name);
    }
}

impl IntSize {
    fn max_size(s1: &IntSize, s2: &IntSize) -> IntSize {
        if *s1 as u32 > *s2 as u32 {
            *s1
        } else {
            *s2
        }
    }
}

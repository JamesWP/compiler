use crate::scope::SharedOptionStackLocation;

#[derive(Debug, Clone, PartialEq)]
pub enum ResWord {
    Return,
    Int,
    Char,
    Const,
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Sizeof,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Reserved(ResWord),
    Identifier(String),
    Paren(char),
    Value(i64),
    StringLiteral(String),
    CharLiteral(char),
    Semicolon,
    Divide,
    Plus,
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    INT(TypeQualifier),
    CHAR(TypeQualifier),
    FUNCTION(Box<TypeDefinition>, ParameterList, bool), // bool is definition / is local
    POINTER(TypeQualifier, Box<TypeDefinition>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    JumpStatement(JumpStatement),
    WhileStatement(WhileStatement),
    IfStatement(IfStatement),
    DeclarationStatement(DeclarationStatement),
    CompoundStatement(Vec<Statement>),
    Expression(Expression),
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
pub struct WhileStatement {
    pub condition_expression: Expression,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
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
        TypeDefinition::INT(TypeQualifier::default())
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
            Statement::JumpStatement(_) => {}
            Statement::DeclarationStatement(_) => {}
            Statement::Expression(_) => {}
            // These statements can contain other statements, visit them
            Statement::WhileStatement(w) => {
                w.loop_body.visit_statements(f);
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
            TypeDefinition::INT(_) => 4,
            TypeDefinition::CHAR(_) => 1,
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
}

impl Expression {
    pub fn new_binop(op: BinOp, lhs: Box<Expression>, rhs: Box<Expression>) -> Expression {
        let int_result = TypeDefinition::INT(TypeQualifier::from(true));
        let char_result = TypeDefinition::INT(TypeQualifier::from(true));
        let result_type = match op {
            BinOp::Difference | BinOp::Product | BinOp::Quotient | BinOp::Sum => {
                use crate::ast::TypeDefinition::*;
                match (&lhs.expr_type, &rhs.expr_type) {
                    (INT(_), INT(_)) => int_result,
                    (INT(_), CHAR(_)) => int_result,
                    (INT(_), FUNCTION(_, _, _)) => todo!(),
                    (INT(_), POINTER(_, _)) => todo!(),
                    (CHAR(_), INT(_)) => int_result,
                    (CHAR(_), CHAR(_)) => char_result,
                    (CHAR(_), FUNCTION(_, _, _)) => todo!(),
                    (CHAR(_), POINTER(_, _)) => todo!(),
                    (FUNCTION(_, _, _), INT(_)) => todo!(),
                    (FUNCTION(_, _, _), CHAR(_)) => todo!(),
                    (FUNCTION(_, _, _), FUNCTION(_, _, _)) => todo!(),
                    (FUNCTION(_, _, _), POINTER(_, _)) => todo!(),
                    (POINTER(_, _), INT(_)) => lhs.expr_type.clone(),
                    (POINTER(_, _), CHAR(_)) => todo!(),
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

        Expression {
            expr_type: result_type,
            node: ExpressionNode::Binary(op, lhs, rhs),
        }
    }
    pub fn new_unaryop(op: UnaryOp, lhs: Box<Expression>) -> Expression {
        // TODO: correctly detemine the type of the Deref op
        let result_type = match op {
            UnaryOp::Deref => match &lhs.expr_type {
                TypeDefinition::INT(_) => todo!(),
                TypeDefinition::CHAR(_) => todo!(),
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
}

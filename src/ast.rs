use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum ResWord {
    Return,
    Int,
    Char,
    Const
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Reserved(ResWord),
    Identifier(String),
    Paren(char),
    Value(i64),
    StringLiteral(String),
    Semicolon,
    Divide,
    Plus,
    Comma,
    Equals,
    Elipsis,
    Star,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub function_definitions: Vec<(String, FunctionDefinition)>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub return_type: TypeDefinition,
    pub parameter_list: ParameterList,
    pub compound_statement: Option<CompoundStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterList {
    parameters: Vec<(String, TypeDefinition)>,
    pub var_args: bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeQualifier {
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    INT(TypeQualifier),
    CHAR(TypeQualifier),
    FUNCTION(Box<TypeDefinition>, ParameterList),
    POINTER(TypeQualifier, Box<TypeDefinition>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    JumpStatement(JumpStatement),
    Declaration(DeclarationStatement),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationStatement {
    pub name: String,
    pub decl_type: TypeDefinition,
    pub expression: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpStatement {
    Return,
    ReturnWithValue(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Additive(Box<Expression>, Box<Expression>),
    Unary(Value),
    Call(String, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(LiteralValue),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int32(i32),
}

impl From<Vec<(String, TypeDefinition)>> for ParameterList {
    fn from(parameters: Vec<(String, TypeDefinition)>) -> ParameterList {
        ParameterList { parameters, var_args: false }
    }
}

impl Into<Vec<(String, TypeDefinition)>> for ParameterList {
    fn into(self) -> Vec<(String, TypeDefinition)> {
        self.parameters
    }
}

impl ParameterList {
    pub fn len(&self) -> u32{
        self.parameters.len() as u32
    }

    pub fn with_var_args(mut self) -> Self {
        self.var_args = true;
        self
    }
}

impl From<Vec<Statement>> for CompoundStatement {
    fn from(statements: Vec<Statement>) -> CompoundStatement {
        CompoundStatement { statements }
    }
}

impl Default for TypeDefinition {
    fn default() -> Self {
        TypeDefinition::INT(TypeQualifier::default())
    }
}

impl IntoIterator for CompoundStatement {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

impl CompoundStatement {
    pub fn iter(&self) -> std::slice::Iter<Statement> {
        self.statements.iter()
    }

    pub fn visit_statements<F>(&self, f: &mut F)
    where
        F: FnMut(&Statement) -> (),
    {
        for statement in &self.statements {
            f(statement);

            // TODO: when statements can be nested, this needs to visit all of them!
            match statement {
                Statement::JumpStatement(_) => {}
                Statement::Declaration(_) => {}
                Statement::Expression(_) => {}
            }
        }
    }
}

impl ParameterList {
    pub fn iter(&self) -> std::slice::Iter<(String, TypeDefinition)> {
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
        TypeQualifier {
            is_const: false
        }
    }
}

impl From<bool> for TypeQualifier {
    fn from(is_const: bool) -> Self {
        TypeQualifier {
            is_const
        }
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
        body: CompoundStatement,
    ) -> FunctionDefinition {
        FunctionDefinition {
            return_type,
            parameter_list: parameters,
            compound_statement: Some(body),
        }
    }

    pub fn new_declaration(
        return_type: TypeDefinition,
        parameters: ParameterList,
    ) -> FunctionDefinition {
        FunctionDefinition {
            return_type,
            parameter_list: parameters,
            compound_statement: None,
        }
    }

    /// return just the names of the declarations in this function.
    pub fn declarations(&self) -> Vec<(String, TypeDefinition)> {
        let mut names = vec![];

        let mut add_definition = |statement: &Statement| match statement {
            Statement::Declaration(DeclarationStatement {
                name,
                decl_type,
                expression,
            }) => {
                names.push((name.clone(), decl_type.clone()));
            }
            _ => {}
        };

        if let Some(statement) = &self.compound_statement {
            statement.visit_statements(&mut add_definition);
        }

        names
    }
}

impl DeclarationStatement {
    pub fn new(decl_type: TypeDefinition, name: String) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: None,
        }
    }

    pub fn new_with_expression(
        decl_type: TypeDefinition,
        name: String,
        expression: Expression,
    ) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: Some(expression),
        }
    }
}

impl ParameterList {
    pub fn is_empty(&self) -> bool {
        self.parameters.is_empty()
    }
}

impl TypeDefinition {
    pub fn size(&self) -> usize {
        match self {
            TypeDefinition::INT(_) => 4,
            TypeDefinition::CHAR(_) => 1,
            TypeDefinition::FUNCTION(_, _) => 8,
            TypeDefinition::POINTER(_, _) => 8,
        }
    }

    pub fn as_function_taking(self, args: ParameterList) -> Self {
        TypeDefinition::FUNCTION(Box::new(self), args)
    }

    pub fn as_pointer_to(self, specifiers: TypeQualifier) -> Self {
        TypeDefinition::POINTER(specifiers, Box::new(self))
    }
}

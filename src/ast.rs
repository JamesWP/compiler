use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum ResWord {
    Return,
    Int,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    //StringLiteral(String),
    Reserved(ResWord),
    Identifier(String),
    Paren(char),
    Value(i64),
    Semicolon,
    Divide,
    Plus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    function_definitions: HashMap<String, FunctionDefinition>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    return_type: TypeDefinition,
    parameter_list: ParameterList,
    compound_statement: CompoundStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterList {
    parameters: Vec<(TypeDefinition, String)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseType {
    INT,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    base_type: BaseType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    JumpStatement(JumpStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpStatement {
    Return,
    ReturnWithValue(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Additive(Box<Expression>, Box<Expression>),
    Unary(LiteralValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int32(i32),
}

impl From<Vec<Statement>> for CompoundStatement {
    fn from(statements: Vec<Statement>) -> CompoundStatement {
        CompoundStatement {
            statements
        }
    }
}

impl IntoIterator for CompoundStatement {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}
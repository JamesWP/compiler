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
    Comma,
    Equals
}

#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit {
    pub function_definitions: HashMap<String, FunctionDefinition>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    return_type: TypeDefinition,
    pub parameter_list: ParameterList,
    pub compound_statement: CompoundStatement,
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
    pub base_type: BaseType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    JumpStatement(JumpStatement),
    Declaration(DeclarationStatement)
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationStatement {
    pub name: String,
    pub decl_type: TypeDefinition,
    pub expression: Option<Expression>
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
    Call(String, Vec<Expression>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(LiteralValue),
    Identifier(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int32(i32),
}

impl From<Vec<(TypeDefinition, String)>> for ParameterList {
    fn from(parameters: Vec<(TypeDefinition, String)>) -> ParameterList {
        ParameterList { parameters }
    }
}
impl From<Vec<Statement>> for CompoundStatement {
    fn from(statements: Vec<Statement>) -> CompoundStatement {
        CompoundStatement { statements }
    }
}

impl From<BaseType> for TypeDefinition {
    fn from(base: BaseType) -> TypeDefinition {
        TypeDefinition { base_type: base }
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

    pub fn visit_statements<F>(&self, f: &mut F) where 
        F: FnMut(&Statement) -> () {
        for statement in &self.statements {
            f(statement);

            // TODO: when statements can be nested, this needs to visit all of them!
            match statement {
                Statement::JumpStatement(_) => {},
                Statement::Declaration(_) => {},
            }
        }
    }
}

impl ParameterList {
    pub fn iter(&self) -> std::slice::Iter<(TypeDefinition, String)> {
        self.parameters.iter()
    }
}

impl Default for TranslationUnit {
    fn default() -> TranslationUnit {
        TranslationUnit {
            function_definitions: HashMap::new(),
        }
    }
}

impl TranslationUnit {
    pub fn add_definition(
        &mut self,
        name: &str,
        definition: FunctionDefinition,
    ) -> std::result::Result<(), &'static str> {
        use std::collections::hash_map::Entry;
        match self.function_definitions.entry(name.to_owned()) {
            Entry::Vacant(entry) => {
                entry.insert(definition);
                Ok(())
            }
            Entry::Occupied(_) => Err("function redeclaration"),
        }
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
            compound_statement: body,
        }
    }

    /// return just the names of the declarations in this function.
    pub fn declarations(&self) -> Vec<(String, TypeDefinition)> {

        let mut names = vec![];

        let mut add_definition = |statement: &Statement| {
            match statement {
                Statement::Declaration(DeclarationStatement { name, decl_type, expression }) => {
                    names.push((name.clone(), decl_type.clone()));
                },
                _ => {}
            }
        };

        self.compound_statement.visit_statements(&mut add_definition);

        names
    } 
}

impl DeclarationStatement {
    pub fn new(decl_type: TypeDefinition, name: String) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: None
        }
    }

    pub fn new_with_expression(decl_type: TypeDefinition, name: String, expression: Expression) -> DeclarationStatement {
        DeclarationStatement {
            decl_type,
            name,
            expression: Some(expression)
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
        if self.base_type != BaseType::INT {
            unimplemented!();
        }

        4
    }
}

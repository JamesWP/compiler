use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::TypeDefinition, platform::StackRelativeLocation};

pub type SharedOptionStackLocation = Rc<RefCell<Option<StackRelativeLocation>>>;
pub type SymbolTable = HashMap<String, (TypeDefinition, SharedOptionStackLocation)>;
#[derive(Default)]
pub struct Scope {
    file: SymbolTable,
    block: Vec<SymbolTable>,
}

pub type ScopeResult<T> = std::result::Result<T, String>;

fn define(
    map: &mut SymbolTable,
    name: &str,
    definition: &TypeDefinition,
    allow_redefinition: bool,
) -> SharedOptionStackLocation {
    let name = name.to_owned();
    let location = Rc::new(RefCell::new(None));
    let old_value = map.insert(name.clone(), (definition.clone(), Rc::clone(&location)));

    if let Some((old_definition, _)) = &old_value {
        match old_definition {
            TypeDefinition::FUNCTION(_, _, _) => {
                if !allow_redefinition {
                    unimplemented!("redefinition of {} with type to {:?}", name, definition);
                }

                // TODO: check redefinitions are equivalent
            }
            _ => todo!("maybe allow redefinition of other types"),
        }
    }

    location
}

fn push(stack: &mut Vec<SymbolTable>) {
    stack.push(HashMap::default());
}

fn pop(stack: &mut Vec<SymbolTable>) {
    stack.pop();
}

impl Scope {
    pub fn begin_function_scope(&mut self) -> ScopeResult<()> {
        if !self.block.is_empty() {
            unimplemented!("begin function scope called twice");
        }

        push(&mut self.block);

        Ok(())
    }

    pub fn end_function_scope(&mut self) -> ScopeResult<()> {
        if self.block.is_empty() {
            unimplemented!("end function scope called twice");
        }

        if self.block.len() != 1 {
            unimplemented!("unbalanced scope stack");
        }

        pop(&mut self.block);

        Ok(())
    }

    pub fn begin_scope(&mut self) -> ScopeResult<()> {
        if self.block.is_empty() {
            unimplemented!("begin called without function scope");
        }

        push(&mut self.block);

        Ok(())
    }

    pub fn end_scope(&mut self) -> ScopeResult<()> {
        if self.block.is_empty() {
            unimplemented!("end called without function scope");
        }

        pop(&mut self.block);

        Ok(())
    }

    pub fn define(
        &mut self,
        name: &str,
        definition: &TypeDefinition,
        allow_redefinition: bool,
    ) -> SharedOptionStackLocation {
        if let Some(last) = self.block.last_mut() {
            define(last, name, definition, allow_redefinition)
        } else {
            define(&mut self.file, name, definition, allow_redefinition)
        }
    }

    pub fn find(&self, name: &str) -> Option<&(TypeDefinition, SharedOptionStackLocation)> {
        let name = name.to_owned();
        for scope in self.block.iter().rev() {
            if let Some(definition) = scope.get(&name) {
                return Some(definition);
            }
        }

        return self.file.get(&name);
    }
}

#[cfg(test)]
use crate::ast::TypeQualifier;

#[cfg(test)]
use crate::ast::IntSize;

#[test]
fn test_scope() {
    let mut scope = Scope::default();

    let int = TypeDefinition::INT {
        size: IntSize::Four,
        qualifier: TypeQualifier::default(),
    };
    let char = TypeDefinition::INT {
        size: IntSize::One,
        qualifier: TypeQualifier::default(),
    };
    let int_p = TypeDefinition::POINTER(TypeQualifier::default(), Box::new(int.clone()));

    let find = |scope: &Scope, name| {
        scope
            .find(name)
            .map(|(decl_type, _location)| decl_type)
            .unwrap()
            .clone()
    };
    scope.define("global", &char, false);
    assert_eq!(find(&scope, "global"), char);

    scope.define("a", &int, false);
    assert_eq!(find(&scope, "a"), int);
    assert_eq!(find(&scope, "global"), char);

    {
        scope.begin_function_scope().unwrap();
        scope.define("a", &char, false);
        assert_eq!(find(&scope, "a"), char);
        assert_eq!(find(&scope, "global"), char);

        {
            scope.begin_scope().unwrap();
            scope.define("a", &int_p, false);
            assert_eq!(find(&scope, "a"), int_p);
            assert_eq!(find(&scope, "global"), char);
            scope.end_scope().unwrap();
        }

        assert_eq!(find(&scope, "a"), char);
        assert_eq!(find(&scope, "global"), char);

        scope.end_function_scope().unwrap();
    }

    assert_eq!(find(&scope, "a"), int);
    assert_eq!(find(&scope, "global"), char);
}

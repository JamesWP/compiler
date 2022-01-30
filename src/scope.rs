use std::collections::HashMap;

use crate::ast::{TypeDefinition, TypeQualifier};
#[derive(Default)]
pub struct Scope {
    file: HashMap<String, TypeDefinition>,
    block: Vec<HashMap<String, TypeDefinition>>,
}

pub type ScopeResult<T> = std::result::Result<T, String>;

fn define(map: &mut HashMap<String, TypeDefinition>, name: &str, definition: &TypeDefinition) {
    let name = name.to_owned();
    let old_value = map.insert(name.clone(), definition.clone());

    if let Some(TypeDefinition::FUNCTION(return_type, arguments, is_local)) = &old_value {
        if let TypeDefinition::FUNCTION(return_type_new, arguments_new, is_local_new) = definition {
            if return_type != return_type_new || arguments != arguments_new {
                unimplemented!(
                    "redefinition of {} from {:?} to {:?}",
                    name,
                    definition,
                    old_value
                );
            }

            // Allow redeclaration
            let is_local = is_local | is_local_new;
            match map.get_mut(&name).unwrap() {
                TypeDefinition::FUNCTION(_, _, is_local_mut) => {
                    *is_local_mut = is_local;
                }
                _ => {}
            }

            return;
        }
    }

    if let Some(old_definition) = &old_value {
        if definition != old_definition {
            unimplemented!(
                "redefinition of {} from {:?} to {:?}",
                name,
                definition,
                old_value
            );
        }
    }
}

fn push(stack: &mut Vec<HashMap<String, TypeDefinition>>) {
    stack.push(HashMap::default());
}

fn pop(stack: &mut Vec<HashMap<String, TypeDefinition>>) {
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

    pub fn define(&mut self, name: &str, definition: &TypeDefinition) {
        if let Some(last) = self.block.last_mut() {
            define(last, name, definition);
        } else {
            define(&mut self.file, name, definition);
        }
    }

    pub fn find(&self, name: &str) -> Option<&TypeDefinition> {
        let name = name.to_owned();
        for scope in self.block.iter().rev() {
            if let Some(definition) = scope.get(&name) {
                return Some(definition);
            }
        }

        return self.file.get(&name);
    }
}

#[test]
fn test_scope() {
    let mut scope = Scope::default();

    let int = TypeDefinition::INT(TypeQualifier::default());
    let char = TypeDefinition::CHAR(TypeQualifier::default());
    let int_p = TypeDefinition::POINTER(TypeQualifier::default(), Box::new(int.clone()));

    scope.define("global", &char);
    assert_eq!(scope.find("global"), Some(&char));

    scope.define("a", &int);
    assert_eq!(scope.find("a"), Some(&int));
    assert_eq!(scope.find("global"), Some(&char));

    {
        scope.begin_function_scope().unwrap();
        scope.define("a", &char);
        assert_eq!(scope.find("a"), Some(&char));
        assert_eq!(scope.find("global"), Some(&char));

        {
            scope.begin_scope().unwrap();
            scope.define("a", &int_p);
            assert_eq!(scope.find("a"), Some(&int_p));
            assert_eq!(scope.find("global"), Some(&char));
            scope.end_scope().unwrap();
        }

        assert_eq!(scope.find("a"), Some(&char));
        assert_eq!(scope.find("global"), Some(&char));

        scope.end_function_scope().unwrap();
    }

    assert_eq!(scope.find("a"), Some(&int));
    assert_eq!(scope.find("global"), Some(&char));
}

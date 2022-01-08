use crate::ast;
use std::fmt::Display;
use std::collections::HashMap;

#[derive(Clone, PartialEq)]
#[allow(dead_code)]
pub enum X86_64Reg {
    RAX,  // register a extended
    RBX,  // register b extended
    RCX,  // register c extended
    RDX,  // register d extended
    RBP,  // register base pointer (start of stack)
    RSP,  // register stack pointer (current location in stack, growing downwards)
    RSI,  // register source index (source for data copies)
    RDI,  // register destination index (destination for data copies)
    R8,   // register 8
    R9,   // register 9
    R10,  // register 10
    R11,  // register 11
    R12,  // register 12
    R13,  // register 13
    R14,  // register 14
    R15,  // register 15
    EAX,  // lower half of RAX
    EBX,  // lower half of RBX
    ECX,  // lower half of RCX
    EDX,  // lower half of RDX
    EBP,  // lower half of RBP
    ESP,  // lower half of RSP
    ESI,  // lower half of RSI
    EDI,  // lower half of RDI
    R8D,  // lower half of R8
    R9D,  // lower hald of R9
    R10D, // lower hald of R10
    R11D, // lower hald of R11
    R12D, // lower hald of R12
    R13D, // lower hald of R13
    R14D, // lower hald of R14
    R15D, // lower hald of R15
}

#[derive(Clone)]
pub struct StackRelativeLocation {
    // offset from this register (RBP)
    reg: X86_64Reg,
    // location relative to register
    offset: i32,
    // size of location,
    pub size: usize,
}

pub struct StackLayout {
    pub stack_size: usize,
    next_free_location: usize,
    allocated: Vec<ParameterInfo>,
    lookup_map: HashMap<String, usize>,
}

pub struct ParameterInfo {
    pub name: String,
    pub param_type: ast::TypeDefinition,
    pub stack_allocation: StackRelativeLocation,
}

pub struct DecimalLiteral {
    value: i32
}

#[allow(dead_code)]
const INTEGER_64_REGISTER_ORDER: [X86_64Reg; 6] = [
    X86_64Reg::RDI,
    X86_64Reg::RSI,
    X86_64Reg::RDX,
    X86_64Reg::RCX,
    X86_64Reg::R8,
    X86_64Reg::R9,
];
const INTEGER_32_REGISTER_ORDER: [X86_64Reg; 6] = [
    X86_64Reg::EDI,
    X86_64Reg::ESI,
    X86_64Reg::EDX,
    X86_64Reg::ECX,
    X86_64Reg::R8D,
    X86_64Reg::R9D,
];

pub struct ParameterPlacement {
    num_integer_args: usize,
}

pub struct Parameter {
    pub reg: Option<X86_64Reg>,
}

pub trait Operand: Display {
    fn is_memory(&self) -> bool;
    fn reg(&self) -> Option<&X86_64Reg>;
}

impl Operand for X86_64Reg {
    fn is_memory(&self) -> bool {
        false
    }
    fn reg(&self) -> Option<&X86_64Reg> {
        Some(self)
    }
}

impl Operand for StackRelativeLocation {
    fn is_memory(&self) -> bool {
        true
    }
    fn reg(&self) -> Option<&X86_64Reg> {
        None
    }
}

impl Display for X86_64Reg {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            formatter,
            "%{}",
            match self {
                X86_64Reg::RAX => "rax",
                X86_64Reg::RBX => "rbx",
                X86_64Reg::RCX => "rcx",
                X86_64Reg::RDX => "rdx",
                X86_64Reg::RBP => "rbp",
                X86_64Reg::RSP => "rsp",
                X86_64Reg::RSI => "rsi",
                X86_64Reg::RDI => "rdi",
                X86_64Reg::R8 => "r8",
                X86_64Reg::R9 => "r9",
                X86_64Reg::EAX => "eax",
                X86_64Reg::EBX => "ebx",
                X86_64Reg::ECX => "ecx",
                X86_64Reg::EDX => "edx",
                X86_64Reg::EBP => "ebp",
                X86_64Reg::ESP => "esp",
                X86_64Reg::ESI => "esi",
                X86_64Reg::EDI => "edi",
                X86_64Reg::R8D => "r8d",
                X86_64Reg::R9D => "r8d",
                _ => unimplemented!(),
            }
        )?;
        Ok(())
    }
}

impl Display for DecimalLiteral {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "${}", self.value)
    }
}

impl DecimalLiteral {
    pub fn new<T: std::borrow::Borrow<i32>>(value: T) -> DecimalLiteral {
        DecimalLiteral {
            value: *value.borrow()
        }
    }
}

impl Parameter {
    pub fn new(reg: X86_64Reg) -> Parameter {
        Parameter { reg: Some(reg) }
    }
}

impl Default for ParameterPlacement {
    fn default() -> ParameterPlacement {
        ParameterPlacement {
            num_integer_args: 0,
        }
    }
}

impl ParameterPlacement {
    pub fn place(&mut self, param_type: &ast::TypeDefinition) -> Parameter {
        match param_type {
            ast::TypeDefinition {
                base_type: ast::BaseType::INT,
            } => {
                let reg = INTEGER_32_REGISTER_ORDER[self.num_integer_args].clone();
                self.num_integer_args += 1;
                Parameter::new(reg)
            }
        }
    }
}
impl ParameterInfo {
    pub fn new(
        name: &str,
        param_type: &ast::TypeDefinition,
        stack_allocation: StackRelativeLocation,
    ) -> ParameterInfo {
        ParameterInfo {
            name: name.to_owned(),
            param_type: param_type.clone(),
            stack_allocation,
        }
    }

    pub fn is_32_bit(&self) -> bool {
        true
    }
}

impl StackRelativeLocation {
    pub fn new(offset: i32, size: usize) -> StackRelativeLocation {
        StackRelativeLocation {
            reg: X86_64Reg::RBP,
            offset,
            size,
        }
    }
    pub fn stack_top() -> StackRelativeLocation {
        StackRelativeLocation {
            reg: X86_64Reg::RSP,
            offset: 0,
            size: 8
        }
    }
}

impl Display for StackRelativeLocation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "{}({})", self.offset, self.reg)?;
        Ok(())
    }
}

impl StackLayout {
    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, ParameterInfo> {
        self.allocated.iter()
    }
}

impl Default for StackLayout {
    fn default() -> StackLayout {
        StackLayout {
            stack_size: 0,
            next_free_location: 0,
            allocated: Vec::default(),
            lookup_map: HashMap::default(),
        }
    }
}

impl StackLayout {
    pub fn allocate(&mut self, name: &str, type_def: &ast::TypeDefinition, size_in_bytes: usize) -> StackRelativeLocation {
        // Make space in the stack
        self.stack_size += size_in_bytes;
        self.next_free_location += size_in_bytes;

        let location_in_stack = 0 - self.next_free_location as i32;

        // TODO: worry about allignment
        if size_in_bytes != 4 {
            unimplemented!();
        }
        if (self.next_free_location & 0x3) != 0 {
            unimplemented!();
        }

        let allocation = StackRelativeLocation::new(location_in_stack, size_in_bytes);
        let param_info = ParameterInfo::new(name, type_def, allocation.clone());
        self.allocated.push(param_info);

        self.lookup_map.insert(name.to_owned(), self.allocated.len()-1);

        allocation
    }

    pub fn get_location(&self, name: &String) -> std::io::Result<StackRelativeLocation> {
        match self.lookup_map.get(name) {
            Some(allocation) => Ok(self.allocated[*allocation].stack_allocation.clone()),
            None => Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("variable {} not defined", name)))
        }
    }
}
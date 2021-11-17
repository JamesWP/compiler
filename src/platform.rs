use std::fmt::Display;
use crate::ast;

#[derive(Clone)]
pub enum x86_64_reg {
    RAX, // register a extended
    RBX, // register b extended
    RCX, // register c extended
    RDX, // register d extended
    RBP, // register base pointer (start of stack)
    RSP, // register stack pointer (current location in stack, growing downwards)
    RSI, // register source index (source for data copies)
    RDI, // register destination index (destination for data copies)
    R8,  // register 8
    R9,  // register 9
    R10, // register 10
    R11, // register 11
    R12, // register 12
    R13, // register 13
    R14, // register 14
    R15, // register 15
    EAX, // lower half of RAX
    EBX, // lower half of RBX
    ECX, // lower half of RCX
    EDX, // lower half of RDX
    EBP, // lower half of RBP
    ESP, // lower half of RSP
    ESI, // lower half of RSI
    EDI, // lower half of RDI
    R8D,  // lower half of R8
    R9D,  // lower hald of R9
    R10D, // lower hald of R10
    R11D, // lower hald of R11
    R12D, // lower hald of R12
    R13D, // lower hald of R13
    R14D, // lower hald of R14
    R15D, // lower hald of R15
}

impl Display for x86_64_reg {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            formatter,
            "%{}",
            match self {
                x86_64_reg::RAX => "rax",
                x86_64_reg::RBX => "rbx",
                x86_64_reg::RCX => "rcx",
                x86_64_reg::RDX => "rdx",
                x86_64_reg::RBP => "rbp",
                x86_64_reg::RSP => "rsp",
                x86_64_reg::RSI => "rsi",
                x86_64_reg::RDI => "rdi",
                x86_64_reg::R8 => "r8",
                x86_64_reg::R9 => "r9",
                x86_64_reg::EAX => "eax",
                x86_64_reg::EBX => "ebx",
                x86_64_reg::ECX => "ecx",
                x86_64_reg::EDX => "edx",
                x86_64_reg::EBP => "ebp",
                x86_64_reg::ESP => "esp",
                x86_64_reg::ESI => "esi",
                x86_64_reg::EDI => "edi",
                x86_64_reg::R8D => "r8d",
                x86_64_reg::R9D => "r8d",
                _ => unimplemented!(),
            }
        )?;
        Ok(())
    }
}

pub struct StackRelativeLocation {
    // offset from this register (RBP)
    reg: x86_64_reg,
    // location relative to register
    offset: i32,
    // size of location,
    size: usize,
}

pub type StackLayout = Vec<ParameterInfo>;

pub struct ParameterInfo {
    pub name: String,  
    pub param_type: ast::TypeDefinition,
    pub reg: Option<x86_64_reg>,
    pub stack_allocation: StackRelativeLocation,
}

const INTEGER_64_REGISTER_ORDER : [x86_64_reg; 6]= [
    x86_64_reg::RDI, 
    x86_64_reg::RSI, 
    x86_64_reg::RDX, 
    x86_64_reg::RCX, 
    x86_64_reg::R8, 
    x86_64_reg::R9, 
];
const INTEGER_32_REGISTER_ORDER : [x86_64_reg; 6]= [
    x86_64_reg::EDI, 
    x86_64_reg::ESI, 
    x86_64_reg::EDX, 
    x86_64_reg::ECX, 
    x86_64_reg::R8D, 
    x86_64_reg::R9D, 
];

pub struct ParameterPlacement {
    num_integer_args: usize
}

pub struct Parameter {
    pub reg: Option<x86_64_reg>
}

impl Parameter {
    pub fn new(reg: x86_64_reg) -> Parameter{
        Parameter {
            reg: Some(reg)
        }
    }
}

impl Default for ParameterPlacement {
    fn default() -> ParameterPlacement {
        ParameterPlacement {
            num_integer_args: 0
        }
    }
}

impl ParameterPlacement {
    pub fn place(&mut self, param_type: &ast::TypeDefinition) -> Parameter {
        match param_type {
            ast::TypeDefinition{base_type: ast::BaseType::INT} => {
                let reg = INTEGER_32_REGISTER_ORDER[self.num_integer_args].clone();
                self.num_integer_args += 1;
                Parameter::new(reg)
            },
            _ => { unimplemented!(); }
        }
    }
}
impl ParameterInfo {
    pub fn is_32_bit(&self) -> bool {
        true
    }
}

impl StackRelativeLocation {
    pub fn new(offset: i32, size: usize) -> StackRelativeLocation {
        StackRelativeLocation {
            reg: x86_64_reg::RBP,
            offset,
            size,
        }
    }
}

impl Display for StackRelativeLocation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "{}({})", self.offset, self.reg)?;
        Ok(())
    }
}

impl ParameterInfo {
    pub fn new(name: &str, reg: x86_64_reg, param_type: &ast::TypeDefinition, stack_allocation: StackRelativeLocation) -> ParameterInfo {
        ParameterInfo {
            name: name.to_owned(),
            reg: Some(reg),
            param_type: param_type.clone(),
            stack_allocation
        }
    }
}
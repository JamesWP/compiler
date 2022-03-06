use crate::ast;
use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Debug)]
#[allow(dead_code)]
pub enum X86_64Reg {
    AL,   // register al lower 8 bits of a register
    DL,   // register dl lower 8 bits of d register
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

#[derive(Clone, Debug, PartialEq)]
pub struct StackRelativeLocation {
    // offset from this register (RBP)
    reg: X86_64Reg,
    // location relative to register
    offset: i32,
    // size of location,
    pub size: usize,
}

#[derive(Clone)]
pub struct RegisterIndirectLocation {
    reg: X86_64Reg,
}

pub struct StackLayout {
    pub stack_size: usize,
    next_free_location: usize,
}

pub struct DecimalLiteral {
    value: i32,
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
                X86_64Reg::AL => "al",
                X86_64Reg::DL => "dl",
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
            value: *value.borrow(),
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
            ast::TypeDefinition::INT(_) | ast::TypeDefinition::CHAR(_) => {
                let reg = INTEGER_32_REGISTER_ORDER[self.num_integer_args].clone();
                self.num_integer_args += 1;
                Parameter::new(reg)
            }
            ast::TypeDefinition::FUNCTION(_, _, _) | ast::TypeDefinition::POINTER(_, _) => {
                let reg = INTEGER_64_REGISTER_ORDER[self.num_integer_args].clone();
                self.num_integer_args += 1;
                Parameter::new(reg)
            }
        }
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
    pub fn top(size: usize) -> StackRelativeLocation {
        match size {
            1 | 4 | 8 => {}
            _ => todo!(),
        };

        StackRelativeLocation {
            reg: X86_64Reg::RSP,
            offset: 0,
            size,
        }
    }
}

impl RegisterIndirectLocation {
    pub fn new(reg: X86_64Reg) -> RegisterIndirectLocation {
        RegisterIndirectLocation { reg }
    }
}

impl Display for StackRelativeLocation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "{}({})", self.offset, self.reg)?;
        Ok(())
    }
}

impl Display for RegisterIndirectLocation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "({})", self.reg)?;
        Ok(())
    }
}

impl Default for StackLayout {
    fn default() -> StackLayout {
        StackLayout {
            stack_size: 0,
            next_free_location: 0,
        }
    }
}

//  rbp=0x100
//  |
//  .---.---.---.---.---.---> more things on stack, lower addresses
//  0   4   8   2   6   0
//              1   1   2


// the int at rbp-4:
//   ---.
//   1234
//      |< pointer to here
//   .  .
// msb  lsb

impl StackLayout {
    pub fn allocate(
        &mut self,
        size_in_bytes: usize,
    ) -> StackRelativeLocation {
        // Make space in the stack
        self.next_free_location += size_in_bytes;

        if (size_in_bytes & (size_in_bytes - 1)) != 0 {
            unimplemented!("cant allocate non multiple of 2 sized space in stack");
        }

        self.next_free_location =
            (self.next_free_location + size_in_bytes - 1) / size_in_bytes * size_in_bytes;

        self.stack_size = self.next_free_location;

        let location_in_stack = 0 - self.next_free_location as i32;

        let allocation = StackRelativeLocation::new(location_in_stack, size_in_bytes);

        allocation
    }
}

#[test]
pub fn test_stack_allocate() {
    let mut layout = StackLayout::default();

    let a = layout.allocate(1).offset;
    let b = layout.allocate(1).offset;
    let c = layout.allocate(1).offset;
    let int = layout.allocate(4).offset;

    assert_eq!(a, -1);
    assert_eq!(b, -2);
    assert_eq!(c, -3);
    assert_eq!(int, -8);
}

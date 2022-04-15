use crate::ast;
use std::fmt::{Debug, Display};

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

impl X86_64Reg {
    /** size of the reister in bytes */
    pub fn size(&self) -> usize {
        use X86_64Reg::*;
        match self {
            RAX | RBX | RCX | RDX | RSI | RDI | RBP => 8,
            EAX | EBX | ECX | EDX | ESI | EDI => 4,
            _ => unimplemented!("Size of {} not implemented", self),
        }
    }
    pub fn to_full_reg(&self) -> X86_64Reg {
        assert_ne!(8, self.size());
        use X86_64Reg::*;
        match self {
            EAX  => RAX,
            EBX  => RBX,
            ECX  => RCX,
            EDX  => RDX,
            ESI  => RSI,
            EDI  => RDI,
            _ => unimplemented!("full reg of {} not implemented", self),
        }
    }
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
    last_allocated_byte: usize,
    debug_information: Vec<i32>,
    debug_total_allocations: i32,
}

pub struct DecimalLiteral {
    value: i32,
}
pub struct CharLiteral {
    value: char,
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

impl Display for CharLiteral {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "$'{}'", self.value)
    }
}

impl DecimalLiteral {
    pub fn new<T: std::borrow::Borrow<i32>>(value: T) -> DecimalLiteral {
        DecimalLiteral {
            value: *value.borrow(),
        }
    }
}

impl CharLiteral {
    pub fn new<T: std::borrow::Borrow<char>>(value: T) -> CharLiteral {
        CharLiteral {
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
            ast::TypeDefinition::INT {
                size: ast::IntSize::One,
                ..
            } => {
                unimplemented!();
            }
            ast::TypeDefinition::INT {
                size: ast::IntSize::Four,
                ..
            } => {
                let reg = INTEGER_32_REGISTER_ORDER[self.num_integer_args].clone();
                self.num_integer_args += 1;
                Parameter::new(reg)
            }
            ast::TypeDefinition::INT {
                size: ast::IntSize::Eight,
                ..
            } => {
                let reg = INTEGER_64_REGISTER_ORDER[self.num_integer_args].clone();
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
  
    /** the number of parameters which can be passed by register */ 
    pub fn max_params() -> usize {
        // this is a temporary limit, other args are passed by stack
        assert_eq!(INTEGER_64_REGISTER_ORDER.len(), INTEGER_32_REGISTER_ORDER.len());
        INTEGER_64_REGISTER_ORDER.len()
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
            last_allocated_byte: 0,
            debug_information: Vec::new(),
            debug_total_allocations: 0,
        }
    }
}

impl Debug for StackLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack_diagram: String = self
            .debug_information
            .iter()
            .map(|num| (num % 10).to_string())
            .collect();

        f.debug_struct("StackLayout")
            .field("stack_size", &self.stack_size)
            .field("stack_diagram", &stack_diagram)
            .finish()
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
    fn allign_to(value: usize, num_bytes: usize) -> usize {
        if (num_bytes & (num_bytes - 1)) != 0 {
            unimplemented!("cant allocate non multiple of 2 sized space in stack");
        }

        (value + num_bytes - 1) / num_bytes * num_bytes
    }

    pub fn allocate(&mut self, size_in_bytes: usize) -> StackRelativeLocation {
        let last_allocation = self.last_allocated_byte;

        // Make space in the stack
        self.last_allocated_byte =
            Self::allign_to(self.last_allocated_byte + size_in_bytes, size_in_bytes);

        self.stack_size = self.last_allocated_byte;

        self.debug_total_allocations += 1;

        let location_in_stack = 0 - self.last_allocated_byte as i32;

        // debug bookkeeping, track what each byte in the stack is used for
        for loc in last_allocation..self.last_allocated_byte {
            let debug_info = if loc < self.last_allocated_byte - size_in_bytes {
                0
            } else {
                self.debug_total_allocations
            };

            self.debug_information.push(debug_info);
        }

        let allocation = StackRelativeLocation::new(location_in_stack, size_in_bytes);

        allocation
    }

    pub fn finalize(&mut self) {
        // the stack pointer must be alligned to an 8 byte boundary before it can be used to push and pop
        // TODO: (stack pointer + 8) must be alligned to a 16 byte boundary before a function call

        let last_allocation = self.last_allocated_byte;
        let size_in_bytes = 8;

        // allign stack to 8 byte boundary
        self.last_allocated_byte = Self::allign_to(self.last_allocated_byte, size_in_bytes);

        self.stack_size = self.last_allocated_byte;

        // debug bookkeeping, track what each byte in the stack is used for
        for _ in last_allocation..self.last_allocated_byte {
            self.debug_information.push(0);
        }
    }
}

#[test]
pub fn test_stack_allocate() {
    let mut layout = StackLayout::default();

    let a = layout.allocate(1).offset;
    let b = layout.allocate(1).offset;
    let c = layout.allocate(1).offset;
    let int = layout.allocate(4).offset;
    let int2 = layout.allocate(4).offset;

    layout.finalize();
    println!("{:#?}", layout);

    assert_eq!(a, -1);
    assert_eq!(b, -2);
    assert_eq!(c, -3);
    assert_eq!(int, -8);
    assert_eq!(int2, -12);

    assert_eq!(layout.stack_size, 16);
}

use std::fmt::Display;

pub enum x86_64_reg {
    RAX,   // register a extended
    RBX,   // register b extended
    RCX,   // register c extended
    RDX,   // register d extended
    RBP,   // register base pointer (start of stack)
    RSP,   // register stack pointer (current location in stack, growing downwards)
    RSI,   // register source index (source for data copies)
    RDI,   // register destination index (destination for data copies)
    R8,    // register 8
    R9,    // register 9
    R10,   // register 10
    R11,   // register 11
    R12,   // register 12
    R13,   // register 13
    R14,   // register 14
    R15    // register 15
}

impl Display for x86_64_reg {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>)-> Result<(), std::fmt::Error> {
        write!(formatter, "%{}", match self {
            RAX => "rax",
            _ => unimplemented!()
        });
       Ok(()) 
    }
}

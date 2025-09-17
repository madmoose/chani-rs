mod decoded_instruction;
mod disassemble;
mod memory_reference;
mod opcode_table;

use std::fmt::Display;

pub use disassemble::decode;
pub use memory_reference::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SReg {
    ES,
    CS,
    SS,
    DS,
}

impl Display for SReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SReg::ES => write!(f, "es"),
            SReg::CS => write!(f, "cs"),
            SReg::SS => write!(f, "ss"),
            SReg::DS => write!(f, "ds"),
        }
    }
}

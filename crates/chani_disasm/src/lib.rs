pub mod attributes;
pub mod branch_map;
mod decoded_instruction;
mod disassemble;
pub mod exe_mz;
pub mod listing_line;
mod memory_reference;
mod opcode_table;
pub mod project;
pub mod work_queue;

use std::fmt::Display;

pub use decoded_instruction::{DecodedInstruction, DisplayContext, RegisterFile};
pub use disassemble::decode;
pub use memory_reference::*;
pub use opcode_table::Opcode;

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

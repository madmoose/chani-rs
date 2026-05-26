pub mod abstract_register_value_flow;
pub mod address_attributes;
pub mod basic_block;
pub mod branch_map;
pub mod data_type;
mod decoded_instruction;
mod disassemble;
pub mod exe_mz;
pub mod function_map;
pub mod function_summary;
pub mod int_descriptions;
pub mod layout;
mod opcode_table;
pub mod project;
pub mod seg_dataflow;
pub mod simple_const_propagation;
pub mod work_queue;

use std::fmt::Display;

pub use decoded_instruction::{
    BaseReg, DataWidth, DecodedInstruction, DisplayContext, GpReg8, GpReg16, IndexReg, MemRef,
    Operand, RegisterFile, SRegMap, SymbolLookup,
};
pub use disassemble::{DisasmCtx, decode, decode_with_ctx};
pub use opcode_table::Opcode;

use crate::project::SegmentIdx;

type SmallString = String;

pub type Address = (SegmentIdx, u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SReg {
    ES,
    CS,
    SS,
    DS,
}

impl SReg {
    /// Decode an `SReg` from the 2-bit segment-register field of a modrm byte.
    pub fn from_bits(bits: u8) -> Self {
        match bits & 3 {
            0 => SReg::ES,
            1 => SReg::CS,
            2 => SReg::SS,
            _ => SReg::DS,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            SReg::ES => "es",
            SReg::CS => "cs",
            SReg::SS => "ss",
            SReg::DS => "ds",
        }
    }
}

impl Display for SReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

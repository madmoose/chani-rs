use std::fmt::Display;

use crate::SReg;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BaseReg {
    BP,
    BX,
}

impl Display for BaseReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseReg::BP => write!(f, "bp"),
            BaseReg::BX => write!(f, "bx"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IndexReg {
    SI,
    DI,
}

impl Display for IndexReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexReg::SI => write!(f, "si"),
            IndexReg::DI => write!(f, "di"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DataWidth {
    Byte,  // 8-bit data
    Word,  // 16-bit data
    Dword, // 32-bit data
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Offset {
    Offset8(i8),   // 8-bit signed displacement
    Offset16(i16), // 16-bit signed displacement
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemRef {
    Direct {
        seg: u16,         // Direct segment
        ofs: u16,         // Direct offset
        width: DataWidth, // Data width
    },
    Indirect {
        seg: SReg,               // Segment register (CS, DS, ES, SS)
        base: Option<BaseReg>,   // Base register (BX, BP)
        index: Option<IndexReg>, // Index register (SI, DI)
        disp: u16,               // Displacement
        width: DataWidth,        // Data width
    },
}

impl MemRef {
    pub fn width(&self) -> DataWidth {
        match self {
            MemRef::Direct { width, .. } => *width,
            MemRef::Indirect { width, .. } => *width,
        }
    }
}

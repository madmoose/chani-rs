use std::fmt::Display;

use crate::SReg;

/// 16-bit general-purpose register, indexed in standard 8086 modrm.reg / modrm.rm order.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GpReg16 {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

impl GpReg16 {
    /// Decode a `GpReg16` from the low 3 bits of a modrm reg or rm field.
    pub fn from_bits(bits: u8) -> Self {
        match bits & 7 {
            0 => Self::AX,
            1 => Self::CX,
            2 => Self::DX,
            3 => Self::BX,
            4 => Self::SP,
            5 => Self::BP,
            6 => Self::SI,
            _ => Self::DI,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::AX => "ax",
            Self::CX => "cx",
            Self::DX => "dx",
            Self::BX => "bx",
            Self::SP => "sp",
            Self::BP => "bp",
            Self::SI => "si",
            Self::DI => "di",
        }
    }
}

impl Display for GpReg16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// 8-bit general-purpose register, indexed in standard 8086 modrm.reg / modrm.rm order.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GpReg8 {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
}

impl GpReg8 {
    /// Decode a `GpReg8` from the low 3 bits of a modrm reg or rm field.
    pub fn from_bits(bits: u8) -> Self {
        match bits & 7 {
            0 => Self::AL,
            1 => Self::CL,
            2 => Self::DL,
            3 => Self::BL,
            4 => Self::AH,
            5 => Self::CH,
            6 => Self::DH,
            _ => Self::BH,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::AL => "al",
            Self::CL => "cl",
            Self::DL => "dl",
            Self::BL => "bl",
            Self::AH => "ah",
            Self::CH => "ch",
            Self::DH => "dh",
            Self::BH => "bh",
        }
    }
}

impl Display for GpReg8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

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

impl DataWidth {
    pub fn sign_extend(self, v: u32) -> i32 {
        match self {
            DataWidth::Byte => v as u8 as i8 as i32,
            DataWidth::Word => v as u16 as i16 as i32,
            DataWidth::Dword => v as i32,
        }
    }
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

/// A fully-decoded instruction operand, resolving modrm bits, immediate bytes,
/// and relative-branch arithmetic into concrete register / memory / immediate
/// values. See [`crate::DecodedInstruction::operand`].
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    /// Operand index past the end of `arg_count()`, or an unrecognized operand.
    None,
    /// Literal constant baked into the opcode (e.g. shift-by-1 / int 3).
    Const(u32),
    Gp8(GpReg8),
    Gp16(GpReg16),
    Sreg(SReg),
    Imm {
        value: u32,
        width: DataWidth,
    },
    /// A near branch target. `target` is the absolute `(seg, ofs)` after
    /// applying the relative-branch arithmetic.
    Rel {
        target: (u16, u16),
    },
    Mem(MemRef),
}

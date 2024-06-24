mod bitops;
mod cpu;
mod flags;
mod register_file;

pub use cpu::Cpu;

#[derive(Copy, Clone, Debug, PartialEq)]
enum Register {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
    ES,
    CS,
    SS,
    DS,
    IP,
}

#[derive(Debug, Clone, Copy)]
enum SReg {
    ES,
    CS,
    SS,
    DS,
}

pub const FLAG_CF: u16 = 1 << 0;
pub const FLAG_PF: u16 = 1 << 2;
pub const FLAG_AF: u16 = 1 << 4;
pub const FLAG_ZF: u16 = 1 << 6;
pub const FLAG_SF: u16 = 1 << 7;
pub const FLAG_TF: u16 = 1 << 8;
pub const FLAG_IF: u16 = 1 << 9;
pub const FLAG_DF: u16 = 1 << 10;
pub const FLAG_OF: u16 = 1 << 11;

#[derive(Debug, Clone, Copy)]
enum RegMem {
    Reg(u8),
    Mem { sreg: SReg, ofs: u16 },
}

#[derive(Debug, Clone, Copy)]
struct ModRM {
    w: Width,
    rm: RegMem,
}

impl ModRM {
    fn is_mem(&self) -> bool {
        matches!(self.rm, RegMem::Mem { sreg: _, ofs: _ })
    }

    fn ofs(&self) -> u16 {
        match self.rm {
            RegMem::Reg(_) => unreachable!(),
            RegMem::Mem { sreg: _, ofs } => ofs,
        }
    }

    fn wrapping_add(&self, rhs: u16) -> Self {
        match self.rm {
            RegMem::Reg(_) => unreachable!(),
            RegMem::Mem { sreg, ofs } => ModRM {
                w: self.w,
                rm: RegMem::Mem {
                    sreg,
                    ofs: ofs.wrapping_add(rhs),
                },
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum AluOp {
    Add,
    Or,
    Adc,
    Sbb,
    And,
    Sub,
    Xor,
    Cmp,
}

impl From<u8> for AluOp {
    fn from(v: u8) -> Self {
        match v {
            0b000 => AluOp::Add,
            0b001 => AluOp::Or,
            0b010 => AluOp::Adc,
            0b011 => AluOp::Sbb,
            0b100 => AluOp::And,
            0b101 => AluOp::Sub,
            0b110 => AluOp::Xor,
            0b111 => AluOp::Cmp,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Width {
    W8,
    W16,
}

use Width::*;

impl Width {
    #[inline]
    fn from_op(op: u8) -> Width {
        if op & 1 == 0 {
            W8
        } else {
            W16
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Dir {
    RegToRM,
    RMToReg,
}

impl Dir {
    #[inline]
    fn from_op(op: u8) -> Self {
        if op & 2 == 0 {
            Self::RegToRM
        } else {
            Self::RMToReg
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StrOp {
    Movs,
    Cmps,
    Scas,
    Lods,
    Stos,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum RepMode {
    #[default]
    None,
    Repne,
    Rep,
}

#[inline]
pub fn ea(seg: u16, ofs: u16) -> u32 {
    (((seg as u32) << 4) + ofs as u32) & 0xfffff
}

#[inline]
fn read_lo(v: u16) -> u8 {
    v as u8
}

#[inline]
fn read_hi(v: u16) -> u8 {
    (v >> 8) as u8
}

fn sext(v: u8) -> u16 {
    v as i8 as u16
}

fn sext16(v: u16) -> u32 {
    v as i16 as u32
}

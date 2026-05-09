//! Local (intra–basic-block) constant propagation for general-purpose
//! registers.
//!
//! Each block is analysed independently from a fully-Unknown entry state.
//! The single product of this pass is a map from `int N` instruction
//! addresses to the abstract register state immediately before the
//! interrupt — enough to recognise patterns like `int 21h, ah=4ch`.
//!
//! Tracked transfers:
//! - `mov reg, imm`
//! - `mov reg, reg` (same width)
//! - `xor reg, reg` / `sub reg, reg` (same register) → 0
//!
//! Anything else writing a GP register clobbers it; `call`, `int` and
//! `into` clobber all GP registers.

use std::collections::BTreeMap;

use crate::{
    Address, GpReg8, GpReg16, Opcode, Operand,
    decoded_instruction::{DataWidth, DecodedInstruction},
    project::{Project, SegmentIdx},
};

#[derive(Default, Copy, Clone, PartialEq, Eq, Debug)]
pub enum Val8 {
    #[default]
    Unknown,
    Const(u8),
}

#[derive(Default, Copy, Clone, PartialEq, Eq, Debug)]
pub enum Val16 {
    #[default]
    Unknown,
    Const(u16),
}

/// Per-register abstract value at a program point. The 8-bit and 16-bit
/// arrays are kept coherent by every writer (see [`RegState::set_gp16`] and
/// [`RegState::set_gp8`]), so a reader can always trust the slot it queries.
#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct RegState {
    /// Indexed by `GpReg16 as usize` — AX, CX, DX, BX, SP, BP, SI, DI.
    pub reg16: [Val16; 8],
    /// Indexed by `GpReg8 as usize` — AL, CL, DL, BL, AH, CH, DH, BH.
    pub reg8: [Val8; 8],
}

impl RegState {
    pub fn get_gp16(&self, r: GpReg16) -> Val16 {
        self.reg16[r as usize]
    }

    pub fn get_gp8(&self, r: GpReg8) -> Val8 {
        self.reg8[r as usize]
    }

    /// Write a 16-bit GP register and update its 8-bit halves (for AX/CX/DX/BX).
    pub fn set_gp16(&mut self, r: GpReg16, v: Val16) {
        self.reg16[r as usize] = v;
        if let Some((lo, hi)) = halves_of(r) {
            self.reg8[lo as usize] = match v {
                Val16::Const(w) => Val8::Const(w as u8),
                Val16::Unknown => Val8::Unknown,
            };
            self.reg8[hi as usize] = match v {
                Val16::Const(w) => Val8::Const((w >> 8) as u8),
                Val16::Unknown => Val8::Unknown,
            };
        }
    }

    /// Write an 8-bit GP register and recompute the containing 16-bit register.
    pub fn set_gp8(&mut self, r: GpReg8, v: Val8) {
        self.reg8[r as usize] = v;
        if let Some((wide, lo, hi)) = parent16_of(r) {
            self.reg16[wide as usize] = match (self.reg8[lo as usize], self.reg8[hi as usize]) {
                (Val8::Const(l), Val8::Const(h)) => Val16::Const(((h as u16) << 8) | l as u16),
                _ => Val16::Unknown,
            };
        }
    }

    fn clobber_gp16(&mut self, r: GpReg16) {
        self.set_gp16(r, Val16::Unknown);
    }

    fn clobber_gp8(&mut self, r: GpReg8) {
        self.set_gp8(r, Val8::Unknown);
    }

    fn clobber_all_gp(&mut self) {
        *self = RegState::default();
    }
}

/// For wide registers that have 8-bit halves, return `(low, high)`.
fn halves_of(r: GpReg16) -> Option<(GpReg8, GpReg8)> {
    match r {
        GpReg16::AX => Some((GpReg8::AL, GpReg8::AH)),
        GpReg16::CX => Some((GpReg8::CL, GpReg8::CH)),
        GpReg16::DX => Some((GpReg8::DL, GpReg8::DH)),
        GpReg16::BX => Some((GpReg8::BL, GpReg8::BH)),
        _ => None,
    }
}

/// For an 8-bit half, return `(parent16, low_half, high_half)`.
fn parent16_of(r: GpReg8) -> Option<(GpReg16, GpReg8, GpReg8)> {
    match r {
        GpReg8::AL | GpReg8::AH => Some((GpReg16::AX, GpReg8::AL, GpReg8::AH)),
        GpReg8::CL | GpReg8::CH => Some((GpReg16::CX, GpReg8::CL, GpReg8::CH)),
        GpReg8::DL | GpReg8::DH => Some((GpReg16::DX, GpReg8::DL, GpReg8::DH)),
        GpReg8::BL | GpReg8::BH => Some((GpReg16::BX, GpReg8::BL, GpReg8::BH)),
    }
}

#[derive(Clone, Debug)]
pub struct IntSite {
    /// Interrupt vector (`int 0x21` → `0x21`, `int 3` → `0x03`).
    pub vector: u8,
    /// Register state immediately before the `int` instruction.
    pub state: RegState,
}

#[derive(Default, Clone, Debug)]
pub struct SimpleConstPropagation {
    /// One entry per `int` / `into` instruction, keyed by its address.
    pub int_sites: BTreeMap<Address, IntSite>,
}

impl SimpleConstPropagation {
    pub fn new() -> Self {
        Self::default()
    }
}

pub fn compute(project: &Project) -> SimpleConstPropagation {
    let mut out = SimpleConstPropagation::default();

    for block in project.blocks.blocks() {
        let mut state = RegState::default();
        let seg = &project.segments[block.seg_idx];
        let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
        let mut ofs = block.start;

        while ofs < block.end {
            let bytes = project.bytes_at_seg(block.seg_idx, ofs);
            let Some(inst) = crate::decode(seg_val, ofs as u16, bytes.iter().copied()) else {
                break;
            };
            let len = inst.bytes.len() as u32;

            transfer(&mut state, &inst, &mut out, (block.seg_idx, ofs));

            ofs += len;
        }
    }

    out
}

fn transfer(
    state: &mut RegState,
    inst: &DecodedInstruction,
    out: &mut SimpleConstPropagation,
    addr: (SegmentIdx, u32),
) {
    match inst.opcode {
        Opcode::Mov => apply_mov(state, inst),

        Opcode::Xor | Opcode::Sub => {
            if !apply_zero_idiom(state, inst) {
                clobber_destinations(state, inst);
            }
        }

        Opcode::Int | Opcode::Into => {
            if let Some(vector) = int_vector(inst) {
                out.int_sites.insert(
                    addr,
                    IntSite {
                        vector,
                        state: state.clone(),
                    },
                );
            }
            state.clobber_all_gp();
        }

        Opcode::Call => {
            state.clobber_all_gp();
        }

        _ => clobber_destinations(state, inst),
    }
}

fn apply_mov(state: &mut RegState, inst: &DecodedInstruction) {
    let Some((dst_i, src_i)) = inst.dst_src() else {
        clobber_destinations(state, inst);
        return;
    };
    let dst = inst.operand(dst_i);
    let src = inst.operand(src_i);

    match (&dst, &src) {
        (
            Operand::Gp16(d),
            Operand::Imm {
                value,
                width: DataWidth::Word,
            },
        ) => state.set_gp16(*d, Val16::Const(*value as u16)),

        (
            Operand::Gp8(d),
            Operand::Imm {
                value,
                width: DataWidth::Byte,
            },
        ) => state.set_gp8(*d, Val8::Const(*value as u8)),

        (Operand::Gp16(d), Operand::Gp16(s)) => {
            let v = state.get_gp16(*s);
            state.set_gp16(*d, v);
        }

        (Operand::Gp8(d), Operand::Gp8(s)) => {
            let v = state.get_gp8(*s);
            state.set_gp8(*d, v);
        }

        _ => clobber_destinations(state, inst),
    }
}

/// `xor reg, reg` and `sub reg, reg` with both operands the same register
/// produce 0. Returns true if the idiom matched and the destination was set.
fn apply_zero_idiom(state: &mut RegState, inst: &DecodedInstruction) -> bool {
    let a = inst.operand(0);
    let b = inst.operand(1);
    match (a, b) {
        (Operand::Gp16(x), Operand::Gp16(y)) if x == y => {
            state.set_gp16(x, Val16::Const(0));
            true
        }
        (Operand::Gp8(x), Operand::Gp8(y)) if x == y => {
            state.set_gp8(x, Val8::Const(0));
            true
        }
        _ => false,
    }
}

fn clobber_destinations(state: &mut RegState, inst: &DecodedInstruction) {
    for i in inst.destinations() {
        match inst.operand(i) {
            Operand::Gp16(r) => state.clobber_gp16(r),
            Operand::Gp8(r) => state.clobber_gp8(r),
            _ => {}
        }
    }
}

fn int_vector(inst: &DecodedInstruction) -> Option<u8> {
    match inst.operand(0) {
        Operand::Const(v) => Some(v as u8),
        Operand::Imm {
            value,
            width: DataWidth::Byte,
        } => Some(value as u8),
        // `into` carries no operand — it always traps to vector 4.
        Operand::None if matches!(inst.opcode, Opcode::Into) => Some(4),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decode;

    fn run(bytes: &[u8]) -> (RegState, Vec<IntSite>) {
        let mut state = RegState::default();
        let mut out = SimpleConstPropagation::default();
        let mut ofs: u16 = 0;
        while (ofs as usize) < bytes.len() {
            let Some(inst) = decode(0, ofs, bytes[ofs as usize..].iter().copied()) else {
                break;
            };
            let len = inst.bytes.len() as u16;
            transfer(
                &mut state,
                &inst,
                &mut out,
                (SegmentIdx::from(0usize), ofs as u32),
            );
            ofs += len;
        }
        let sites: Vec<IntSite> = out.int_sites.into_values().collect();
        (state, sites)
    }

    #[test]
    fn xor_ax_ax_then_int_33() {
        // xor ax, ax = 33 c0 ; int 33h = cd 33
        let (_, sites) = run(&[0x33, 0xc0, 0xcd, 0x33]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].vector, 0x33);
        assert_eq!(sites[0].state.get_gp16(GpReg16::AX), Val16::Const(0));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AL), Val8::Const(0));
    }

    #[test]
    fn mov_ah_imm_then_int_10() {
        // mov ah, 0eh = b4 0e ; int 10h = cd 10
        let (_, sites) = run(&[0xb4, 0x0e, 0xcd, 0x10]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].vector, 0x10);
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0x0e));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AL), Val8::Unknown);
        assert_eq!(sites[0].state.get_gp16(GpReg16::AX), Val16::Unknown);
    }

    #[test]
    fn mov_ax_imm16_then_int_21() {
        // mov ax, 0c06h = b8 06 0c ; int 21h = cd 21
        let (_, sites) = run(&[0xb8, 0x06, 0x0c, 0xcd, 0x21]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].vector, 0x21);
        assert_eq!(sites[0].state.get_gp16(GpReg16::AX), Val16::Const(0x0c06));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0x0c));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AL), Val8::Const(0x06));
    }

    #[test]
    fn mov_ah_4c_then_int_21() {
        // mov ah, 4ch = b4 4c ; int 21h = cd 21
        let (_, sites) = run(&[0xb4, 0x4c, 0xcd, 0x21]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].vector, 0x21);
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0x4c));
    }

    #[test]
    fn int_clobbers_gp_regs() {
        // mov ah, 4ch ; int 21h ; (state after the int)
        let (state, _) = run(&[0xb4, 0x4c, 0xcd, 0x21]);
        assert_eq!(state.get_gp8(GpReg8::AH), Val8::Unknown);
        assert_eq!(state.get_gp16(GpReg16::AX), Val16::Unknown);
    }

    #[test]
    fn writing_8bit_half_invalidates_full_unless_other_half_known() {
        // mov al, 11h ; mov ah, 22h ; int 21h → AX should be Const(0x2211)
        let (_, sites) = run(&[0xb0, 0x11, 0xb4, 0x22, 0xcd, 0x21]);
        assert_eq!(sites[0].state.get_gp16(GpReg16::AX), Val16::Const(0x2211));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AL), Val8::Const(0x11));
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0x22));
    }

    #[test]
    fn sub_same_reg_zeros() {
        // sub bx, bx = 2b db ; int 21h
        let (_, sites) = run(&[0x2b, 0xdb, 0xcd, 0x21]);
        assert_eq!(sites[0].state.get_gp16(GpReg16::BX), Val16::Const(0));
    }

    #[test]
    fn xor_8bit_same_reg_zeros() {
        // xor cl, cl = 32 c9 ; int 21h
        let (_, sites) = run(&[0x32, 0xc9, 0xcd, 0x21]);
        assert_eq!(sites[0].state.get_gp8(GpReg8::CL), Val8::Const(0));
    }

    #[test]
    fn unrelated_write_clobbers_only_destination() {
        // mov ah, 4ch ; mov cl, 1 ; int 21h → AH still 0x4c, CL=1
        let (_, sites) = run(&[0xb4, 0x4c, 0xb1, 0x01, 0xcd, 0x21]);
        assert_eq!(sites[0].state.get_gp8(GpReg8::AH), Val8::Const(0x4c));
        assert_eq!(sites[0].state.get_gp8(GpReg8::CL), Val8::Const(0x01));
    }

    #[test]
    fn int3_short_form_vector_3() {
        // int 3 = cc
        let (_, sites) = run(&[0xcc]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].vector, 3);
    }
}

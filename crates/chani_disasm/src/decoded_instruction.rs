use std::fmt::{Display, Write};

use smallvec::SmallVec;

use crate::{SReg, data_type::DisplayFmt, project::SegmentIdx};

use super::opcode_table::{ArgDir, ArgType, Opcode};

#[derive(Debug)]
pub struct DecodedInstruction {
    pub opcode: Opcode,
    pub bytes: SmallVec<[u8; 16]>,
    pub op_seg: u16,
    pub op_ofs: u16,
    pub modrm: u8,
    pub arg_type: [ArgType; 2],
    pub arg_dir: [ArgDir; 2],
    pub seg_ovr: Option<SReg>,
    pub flag_lock: bool,
    pub flag_f2: bool,
    pub flag_f3: bool,
    pub imm: [u32; 2],
    /// Byte offset within `bytes` at which `imm[i]` begins, or `None` if
    /// `imm[i]` was not fetched from the byte stream (no displacement, no
    /// immediate, register-only operand, etc.). Used to match imm slots
    /// against an external relocation table.
    pub imm_ofs: [Option<u8>; 2],
    /// For each operand slot, the project segment that the imm bytes resolve
    /// to via the EXE relocation table. Set only for `Imm16` operands and for
    /// the seg-half of `IMem32` (far-pointer) operands. `None` when no
    /// relocation applies.
    pub imm_seg: [Option<SegmentIdx>; 2],
    pub has_mem_arg: bool,
}

fn write_imm_fmt<W: Write>(
    w: &mut W,
    v: u32,
    width: DataWidth,
    fmt: DisplayFmt,
) -> std::fmt::Result {
    match fmt {
        DisplayFmt::Default | DisplayFmt::Hex => write_imm(w, v),
        DisplayFmt::Dec => write!(w, "{v}"),
        DisplayFmt::SignedDec => write!(w, "{}", width.sign_extend(v)),
        DisplayFmt::Bin => write!(w, "0b{v:b}"),
        DisplayFmt::Char => {
            let b = v as u8;
            if b.is_ascii_graphic() && b != b'\'' {
                write!(w, "'{}'", b as char)
            } else {
                write_imm(w, v)
            }
        }
    }
}

fn write_imm<W: Write>(w: &mut W, v: u32) -> std::fmt::Result {
    fn most_significant_nybble(n: u32) -> u8 {
        if n == 0 {
            return 0;
        }

        let nybble_position = (31 - n.leading_zeros()) / 4;
        ((n >> (nybble_position * 4)) & 0xf) as u8
    }

    if v < 10 {
        return write!(w, "{v}");
    }
    if most_significant_nybble(v) > 9 {
        write!(w, "0")?;
    }
    write!(w, "{v:x}h")?;

    Ok(())
}

fn is_mem_ref_arg(arg_type: ArgType, modrm: u8) -> bool {
    match arg_type {
        ArgType::IMem8 | ArgType::IMem16 | ArgType::IMem32 => true,
        ArgType::Mem16 | ArgType::Mem32 => true,
        ArgType::RM8 | ArgType::RM16 => modrm < 0xc0,
        _ => false,
    }
}

impl DecodedInstruction {
    pub fn mnemonic(&self) -> &'static str {
        self.opcode.as_str()
    }

    pub fn arg_count(&self) -> usize {
        self.arg_type
            .iter()
            .position(|&arg_type| arg_type == ArgType::None)
            .unwrap_or(2)
    }

    pub fn branches(&self) -> bool {
        matches!(
            self.opcode,
            Opcode::Call
                | Opcode::Ja
                | Opcode::Jb
                | Opcode::Jbe
                | Opcode::Jcxz
                | Opcode::Jg
                | Opcode::Jge
                | Opcode::Jl
                | Opcode::Jle
                | Opcode::Jmp
                | Opcode::Jno
                | Opcode::Jnb
                | Opcode::Jns
                | Opcode::Jnz
                | Opcode::Jo
                | Opcode::Jpe
                | Opcode::Jpo
                | Opcode::Js
                | Opcode::Jz
                | Opcode::Loop
                | Opcode::Loopnz
                | Opcode::Loopz
                | Opcode::Ret
                | Opcode::Retf
                | Opcode::Iret
                | Opcode::Hlt
        )
    }

    pub fn branch_destination(&self) -> Option<(u16, u16)> {
        for i in 0..2 {
            match self.arg_type[i] {
                ArgType::Rel8 => {
                    let inc = self.imm[i] as i8 as i16 as u16;
                    let ofs = self
                        .op_ofs
                        .wrapping_add(self.bytes.len() as u16)
                        .wrapping_add(inc);
                    return Some((self.op_seg, ofs));
                }
                ArgType::Rel16 => {
                    let inc = self.imm[i] as i16 as u16;
                    let ofs = self
                        .op_ofs
                        .wrapping_add(self.bytes.len() as u16)
                        .wrapping_add(inc);
                    return Some((self.op_seg, ofs));
                }
                ArgType::IMem32 => {
                    let ofs = self.imm[i] as u16;
                    let seg = (self.imm[i] >> 16) as u16;
                    return Some((seg, ofs));
                }
                _ => {}
            }
        }
        None
    }

    pub fn stops_control_flow(&self) -> bool {
        matches!(
            self.opcode,
            Opcode::Ret | Opcode::Retf | Opcode::Iret | Opcode::Jmp | Opcode::Hlt
        )
    }

    pub fn mem_dir(&self) -> ArgDir {
        let mem_arg_index = self
            .arg_type
            .iter()
            .copied()
            .enumerate()
            .find(|(_, arg_type)| is_mem_ref_arg(*arg_type, self.modrm))
            .map(|(i, _)| i);

        mem_arg_index
            .map(|i| self.arg_dir[i])
            .unwrap_or(ArgDir::None)
    }

    pub fn writes_to_mem(&self) -> bool {
        let Some(mem_arg_index) = self
            .arg_type
            .iter()
            .copied()
            .enumerate()
            .find(|(_, arg_type)| is_mem_ref_arg(*arg_type, self.modrm))
            .map(|(i, _)| i)
        else {
            return false;
        };

        matches!(self.arg_dir[mem_arg_index], ArgDir::RW | ArgDir::WO)
    }

    pub fn reads_from_mem(&self) -> bool {
        let Some(mem_arg_index) = self
            .arg_type
            .iter()
            .copied()
            .enumerate()
            .find(|(_, arg_type)| is_mem_ref_arg(*arg_type, self.modrm))
            .map(|(i, _)| i)
        else {
            return false;
        };

        matches!(self.arg_dir[mem_arg_index], ArgDir::RW | ArgDir::RO)
    }

    pub fn mem_ref(&self) -> Option<MemRef> {
        (0..2).find_map(|i| self.mem_ref_at(i))
    }

    /// Resolve operand `i` to a memory reference, if it is one. Returns `None`
    /// for non-memory operands (registers, immediates, etc.).
    pub fn mem_ref_at(&self, i: usize) -> Option<MemRef> {
        if i >= 2 {
            return None;
        }
        let arg_type = self.arg_type[i];
        if !is_mem_ref_arg(arg_type, self.modrm) {
            return None;
        }

        let mem_ref = match arg_type {
            ArgType::IMem8 | ArgType::IMem16 => MemRef::Indirect {
                seg: self.seg_ovr.unwrap_or(SReg::DS),
                base: None,
                index: None,
                disp: self.imm[i] as u16,
                width: match arg_type {
                    ArgType::IMem8 => DataWidth::Byte,
                    ArgType::IMem16 => DataWidth::Word,
                    _ => unreachable!(),
                },
            },
            ArgType::IMem32 => MemRef::Direct {
                seg: (self.imm[i] >> 16) as u16,
                seg_idx: self.imm_seg[i],
                ofs: self.imm[i] as u16,
                width: DataWidth::Dword,
            },
            // `Mem16` is a ModRM memory operand (lea/les/lds), not a direct
            // immediate offset like `IMem16`, so it decodes its base/index/disp
            // from the modrm exactly like `RM16`.
            ArgType::RM8 | ArgType::RM16 | ArgType::Mem16 => {
                let modrm = self.modrm;
                let mod_bits = (modrm >> 6) & 0b11;
                let rm = modrm & 0b111;

                let seg = match rm {
                    0b000 => SReg::DS,
                    0b001 => SReg::DS,
                    0b010 => SReg::SS,
                    0b011 => SReg::SS,
                    0b100 => SReg::DS,
                    0b101 => SReg::DS,
                    0b110 if mod_bits == 0 => SReg::DS,
                    0b110 => SReg::SS,
                    0b111 => SReg::DS,
                    _ => unreachable!(),
                };

                let (base, index) = match rm {
                    0b000 => (Some(BaseReg::BX), Some(IndexReg::SI)),
                    0b001 => (Some(BaseReg::BX), Some(IndexReg::DI)),
                    0b010 => (Some(BaseReg::BP), Some(IndexReg::SI)),
                    0b011 => (Some(BaseReg::BP), Some(IndexReg::DI)),
                    0b100 => (None, Some(IndexReg::SI)),
                    0b101 => (None, Some(IndexReg::DI)),
                    0b110 => {
                        if mod_bits == 0 {
                            (None, None)
                        } else {
                            (Some(BaseReg::BP), None)
                        }
                    }
                    0b111 => (Some(BaseReg::BX), None),
                    _ => unreachable!(),
                };

                let disp = match mod_bits {
                    0b00 if rm == 0b110 => self.imm[i] as u16,
                    0b01 => self.imm[i] as u8 as i8 as i16 as u16,
                    0b10 => self.imm[i] as u16,
                    _ => 0,
                };

                MemRef::Indirect {
                    seg: self.seg_ovr.unwrap_or(seg),
                    base,
                    index,
                    disp,
                    width: match arg_type {
                        ArgType::RM8 => DataWidth::Byte,
                        ArgType::RM16 | ArgType::Mem16 => DataWidth::Word,
                        _ => unreachable!(),
                    },
                }
            }
            _ => unreachable!(),
        };

        Some(mem_ref)
    }

    /// Resolve operand `i` to a fully-decoded [`Operand`].
    /// Returns [`Operand::None`] for `i >= arg_count()` or for `ArgType::Inherit`
    /// placeholders (which only appear inside group instruction templates).
    pub fn operand(&self, i: usize) -> Operand {
        if i >= 2 {
            return Operand::None;
        }
        let arg_type = self.arg_type[i];
        match arg_type {
            ArgType::None | ArgType::Inherit => Operand::None,
            ArgType::Const1 => Operand::Const(1),
            ArgType::Const3 => Operand::Const(3),
            ArgType::AL => Operand::Gp8(GpReg8::AL),
            ArgType::CL => Operand::Gp8(GpReg8::CL),
            ArgType::DL => Operand::Gp8(GpReg8::DL),
            ArgType::BL => Operand::Gp8(GpReg8::BL),
            ArgType::AH => Operand::Gp8(GpReg8::AH),
            ArgType::CH => Operand::Gp8(GpReg8::CH),
            ArgType::DH => Operand::Gp8(GpReg8::DH),
            ArgType::BH => Operand::Gp8(GpReg8::BH),
            ArgType::AX => Operand::Gp16(GpReg16::AX),
            ArgType::CX => Operand::Gp16(GpReg16::CX),
            ArgType::DX => Operand::Gp16(GpReg16::DX),
            ArgType::BX => Operand::Gp16(GpReg16::BX),
            ArgType::SP => Operand::Gp16(GpReg16::SP),
            ArgType::BP => Operand::Gp16(GpReg16::BP),
            ArgType::SI => Operand::Gp16(GpReg16::SI),
            ArgType::DI => Operand::Gp16(GpReg16::DI),
            ArgType::ES => Operand::Sreg(SReg::ES),
            ArgType::CS => Operand::Sreg(SReg::CS),
            ArgType::SS => Operand::Sreg(SReg::SS),
            ArgType::DS => Operand::Sreg(SReg::DS),
            ArgType::Reg8 => Operand::Gp8(GpReg8::from_bits(self.modrm >> 3)),
            ArgType::Reg16 => Operand::Gp16(GpReg16::from_bits(self.modrm >> 3)),
            ArgType::SReg => Operand::Sreg(SReg::from_bits(self.modrm >> 3)),
            ArgType::Imm8 => Operand::Imm {
                value: self.imm[i],
                width: DataWidth::Byte,
            },
            ArgType::Imm8Sx => Operand::Imm {
                value: self.imm[i],
                width: DataWidth::Word,
            },
            ArgType::Imm16 => {
                if let Some(idx) = self.imm_seg[i] {
                    Operand::SegRef(idx)
                } else {
                    Operand::Imm {
                        value: self.imm[i],
                        width: DataWidth::Word,
                    }
                }
            }
            ArgType::Rel8 => {
                let inc = self.imm[i] as i8 as i16 as u16;
                let ofs = self
                    .op_ofs
                    .wrapping_add(self.bytes.len() as u16)
                    .wrapping_add(inc);
                Operand::Rel {
                    target: (self.op_seg, ofs),
                }
            }
            ArgType::Rel16 => {
                let inc = self.imm[i] as i16 as u16;
                let ofs = self
                    .op_ofs
                    .wrapping_add(self.bytes.len() as u16)
                    .wrapping_add(inc);
                Operand::Rel {
                    target: (self.op_seg, ofs),
                }
            }
            ArgType::IMem8
            | ArgType::IMem16
            | ArgType::IMem32
            | ArgType::Mem16
            | ArgType::Mem32 => self
                .mem_ref_at(i)
                .map(Operand::Mem)
                .unwrap_or(Operand::None),
            ArgType::RM8 => {
                if (self.modrm >> 6) & 0b11 == 0b11 {
                    Operand::Gp8(GpReg8::from_bits(self.modrm))
                } else {
                    self.mem_ref_at(i)
                        .map(Operand::Mem)
                        .unwrap_or(Operand::None)
                }
            }
            ArgType::RM16 => {
                if (self.modrm >> 6) & 0b11 == 0b11 {
                    Operand::Gp16(GpReg16::from_bits(self.modrm))
                } else {
                    self.mem_ref_at(i)
                        .map(Operand::Mem)
                        .unwrap_or(Operand::None)
                }
            }
        }
    }

    /// For instructions with a clean binary `dst <- src` shape (one operand
    /// `WO`|`RW`, the other `RO`), return `(dst_idx, src_idx)`. Returns
    /// `None` for compares (both `RO`), `xchg` (both `RW`), single-arg or
    /// no-arg instructions.
    pub fn dst_src(&self) -> Option<(usize, usize)> {
        let writes = |i: usize| matches!(self.arg_dir[i], ArgDir::WO | ArgDir::RW);
        let reads = |i: usize| matches!(self.arg_dir[i], ArgDir::RO | ArgDir::RW);
        for dst in 0..2 {
            let src = 1 - dst;
            if writes(dst) && !writes(src) && reads(src) {
                return Some((dst, src));
            }
        }
        None
    }

    /// Iterator over operand indices that are written (`WO` or `RW`).
    pub fn destinations(&self) -> impl Iterator<Item = usize> + '_ {
        (0..2).filter(move |&i| matches!(self.arg_dir[i], ArgDir::WO | ArgDir::RW))
    }

    /// True iff any destination operand resolves to the given GP16 register.
    pub fn writes_to_gp16(&self, r: GpReg16) -> bool {
        self.destinations()
            .any(|i| matches!(self.operand(i), Operand::Gp16(g) if g == r))
    }
}

/// A register operand identity, passed to [`SymbolLookup::lookup_register`] so
/// a renderer can substitute a binding name (e.g. `cx` → `count`).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NamedReg {
    Gp16(GpReg16),
    Gp8(GpReg8),
    Seg(SReg),
}

impl std::fmt::Display for NamedReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedReg::Gp16(r) => write!(f, "{r}"),
            NamedReg::Gp8(r) => write!(f, "{r}"),
            NamedReg::Seg(r) => write!(f, "{r}"),
        }
    }
}

pub trait SymbolLookup {
    /// For `MemRef::Direct { seg, ofs, width }`.
    fn lookup_direct(&self, seg: u16, ofs: u16, width: DataWidth) -> Option<String>;

    /// Resolve a register operand to a binding-derived display name (e.g.
    /// `count`, `skill_sum.lo`) at the address being rendered. Default: none.
    fn lookup_register(&self, _reg: NamedReg) -> Option<String> {
        None
    }

    /// For `MemRef::Indirect { seg, base, index, disp, width }`.
    /// Resolves base/index registers and the segment register internally.
    fn lookup_indirect(
        &self,
        seg: SReg,
        base: Option<BaseReg>,
        index: Option<IndexReg>,
        disp: u16,
        width: DataWidth,
    ) -> Option<String>;

    /// For `Imm8`/`Imm16` — resolves using a pre-configured default segment.
    fn lookup_offset(&self, ofs: u16) -> Option<String>;

    /// Resolve a project segment index to its name. Used to render
    /// relocation-resolved imm16 operands (`Operand::SegRef`) and the
    /// seg-half of relocated far pointers.
    fn lookup_segment(&self, _seg_idx: SegmentIdx) -> Option<String> {
        None
    }
}
pub struct DisplayContext<'a> {
    pub lookup: &'a dyn SymbolLookup,
    /// Per-operand display format hints (index 0 and 1).
    pub arg_fmts: [Option<DisplayFmt>; 2],
}

/// Static mapping from segment registers to known project segment indices.
/// Used by [`ProjectLookup`] to resolve segment registers without a runtime
/// register file. Fields are `None` when the mapping is unknown.
#[derive(Debug, Default, Clone, Copy)]
pub struct SRegMap {
    pub es: Option<SegmentIdx>,
    pub cs: Option<SegmentIdx>,
    pub ss: Option<SegmentIdx>,
    pub ds: Option<SegmentIdx>,
}

impl SRegMap {
    pub fn get(&self, r: SReg) -> Option<SegmentIdx> {
        match r {
            SReg::ES => self.es,
            SReg::CS => self.cs,
            SReg::SS => self.ss,
            SReg::DS => self.ds,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct RegisterFile {
    pub reg: [u16; 13],
}

impl RegisterFile {
    pub fn get_sreg(&self, r: SReg) -> u16 {
        match r {
            SReg::ES => self.reg[8],
            SReg::CS => self.reg[9],
            SReg::SS => self.reg[10],
            SReg::DS => self.reg[11],
        }
    }

    pub fn get_base_reg(&self, r: BaseReg) -> u16 {
        match r {
            BaseReg::BX => self.reg[0],
            BaseReg::BP => self.reg[2],
        }
    }

    pub fn get_index_reg(&self, r: IndexReg) -> u16 {
        match r {
            IndexReg::SI => self.reg[4],
            IndexReg::DI => self.reg[5],
        }
    }
}

/// Render a register operand, substituting a binding name from the lookup when
/// one is available (e.g. `cx` → `count`), otherwise the raw register name.
fn write_reg<W: Write>(w: &mut W, ctx: &DisplayContext, reg: NamedReg) -> std::fmt::Result {
    match ctx.lookup.lookup_register(reg) {
        Some(name) => write!(w, "{name}"),
        None => write!(w, "{reg}"),
    }
}

impl DecodedInstruction {
    pub fn format_opcode<W: Write>(&self, w: &mut W) -> Result<usize, std::fmt::Error> {
        let mut col = 0;

        if self.flag_lock {
            write!(w, "lock ")?;
            col += 5;
        }

        if self.flag_f3 {
            match self.opcode {
                Opcode::Stosb
                | Opcode::Stosw
                | Opcode::Movsb
                | Opcode::Movsw
                | Opcode::Lodsb
                | Opcode::Lodsw => write!(w, "rep ")?,
                Opcode::Cmpsb | Opcode::Cmpsw | Opcode::Scasb | Opcode::Scasw => {
                    write!(w, "repz ")?
                }
                _ => (),
            }
        }
        if self.flag_f2 {
            match self.opcode {
                Opcode::Cmpsb | Opcode::Cmpsw | Opcode::Scasb | Opcode::Scasw => {
                    write!(w, "repnz ")?
                }
                _ => (),
            }
        }

        if let Some(seg_ovr) = self.seg_ovr
            && !self.has_mem_arg
        {
            write!(w, "{seg_ovr}:")?;
            col += 3;
        }

        write!(w, "{}", self.mnemonic())?;
        col += self.mnemonic().len();

        Ok(col)
    }

    pub fn format_arg<W: Write>(
        &self,
        w: &mut W,
        i: usize,
        ctx: &DisplayContext,
    ) -> std::fmt::Result {
        let needs_width_specifier =
            self.has_mem_arg && self.arg_type.iter().any(ArgType::needs_width_specifier);

        if is_mem_ref_arg(self.arg_type[i], self.modrm)
            && let Some(mem_ref) = self.mem_ref()
        {
            let name = match mem_ref {
                MemRef::Direct {
                    seg, ofs, width, ..
                } => ctx.lookup.lookup_direct(seg, ofs, width),
                MemRef::Indirect {
                    seg,
                    base,
                    index,
                    disp,
                    width,
                } => ctx.lookup.lookup_indirect(seg, base, index, disp, width),
            };
            if let Some(name) = name {
                if needs_width_specifier {
                    match self.arg_type[i] {
                        ArgType::RM8 | ArgType::IMem8 => write!(w, "byte ptr ")?,
                        ArgType::RM16 | ArgType::Mem16 | ArgType::IMem16 => write!(w, "word ptr ")?,
                        ArgType::Mem32 => write!(w, "far ptr ")?,
                        _ => {}
                    }
                }
                let has_reg = matches!(
                    mem_ref,
                    MemRef::Indirect { base: Some(_), .. }
                        | MemRef::Indirect { index: Some(_), .. }
                );
                if has_reg {
                    return write!(w, "{name}");
                }
                if let Some(ovr) = self.seg_ovr {
                    write!(w, "{ovr}:")?;
                }
                return write!(w, "[{name}]");
            }
        }

        match self.arg_type[i] {
            ArgType::Inherit => unreachable!(),
            ArgType::None => unreachable!(),
            ArgType::Const1 => write!(w, "1")?,
            ArgType::Const3 => write!(w, "3")?,
            ArgType::AL => write_reg(w, ctx, NamedReg::Gp8(GpReg8::AL))?,
            ArgType::CL => write_reg(w, ctx, NamedReg::Gp8(GpReg8::CL))?,
            ArgType::DL => write_reg(w, ctx, NamedReg::Gp8(GpReg8::DL))?,
            ArgType::BL => write_reg(w, ctx, NamedReg::Gp8(GpReg8::BL))?,
            ArgType::AH => write_reg(w, ctx, NamedReg::Gp8(GpReg8::AH))?,
            ArgType::CH => write_reg(w, ctx, NamedReg::Gp8(GpReg8::CH))?,
            ArgType::DH => write_reg(w, ctx, NamedReg::Gp8(GpReg8::DH))?,
            ArgType::BH => write_reg(w, ctx, NamedReg::Gp8(GpReg8::BH))?,
            ArgType::AX => write_reg(w, ctx, NamedReg::Gp16(GpReg16::AX))?,
            ArgType::CX => write_reg(w, ctx, NamedReg::Gp16(GpReg16::CX))?,
            ArgType::DX => write_reg(w, ctx, NamedReg::Gp16(GpReg16::DX))?,
            ArgType::BX => write_reg(w, ctx, NamedReg::Gp16(GpReg16::BX))?,
            ArgType::SP => write_reg(w, ctx, NamedReg::Gp16(GpReg16::SP))?,
            ArgType::BP => write_reg(w, ctx, NamedReg::Gp16(GpReg16::BP))?,
            ArgType::SI => write_reg(w, ctx, NamedReg::Gp16(GpReg16::SI))?,
            ArgType::DI => write_reg(w, ctx, NamedReg::Gp16(GpReg16::DI))?,
            ArgType::ES => write_reg(w, ctx, NamedReg::Seg(SReg::ES))?,
            ArgType::CS => write_reg(w, ctx, NamedReg::Seg(SReg::CS))?,
            ArgType::SS => write_reg(w, ctx, NamedReg::Seg(SReg::SS))?,
            ArgType::DS => write_reg(w, ctx, NamedReg::Seg(SReg::DS))?,
            ArgType::Reg8 => write_reg(w, ctx, NamedReg::Gp8(GpReg8::from_bits(self.modrm >> 3)))?,
            ArgType::Reg16 => {
                write_reg(w, ctx, NamedReg::Gp16(GpReg16::from_bits(self.modrm >> 3)))?
            }
            ArgType::SReg => write_reg(w, ctx, NamedReg::Seg(SReg::from_bits(self.modrm >> 3)))?,
            ArgType::Imm8 => {
                if let Some(name) = ctx.lookup.lookup_offset(self.imm[i] as u16) {
                    return write!(w, "{name}");
                }
                write_imm_fmt(
                    w,
                    self.imm[i],
                    DataWidth::Byte,
                    ctx.arg_fmts[i].unwrap_or_default(),
                )?;
            }
            ArgType::Imm8Sx => {
                if let Some(name) = ctx.lookup.lookup_offset(self.imm[i] as u16) {
                    return write!(w, "{name}");
                }
                write_imm_fmt(
                    w,
                    self.imm[i],
                    DataWidth::Word,
                    ctx.arg_fmts[i].unwrap_or_default(),
                )?;
            }
            ArgType::Imm16 => {
                if let Some(idx) = self.imm_seg[i]
                    && let Some(name) = ctx.lookup.lookup_segment(idx)
                {
                    return write!(w, "{name}");
                }
                if let Some(name) = ctx.lookup.lookup_offset(self.imm[i] as u16) {
                    return write!(w, "{name}");
                }
                write_imm_fmt(
                    w,
                    self.imm[i],
                    DataWidth::Word,
                    ctx.arg_fmts[i].unwrap_or_default(),
                )?;
            }
            ArgType::Rel8 => {
                let inc = self.imm[i] as i8 as i16 as u16;
                let ofs = self
                    .op_ofs
                    .wrapping_add(self.bytes.len() as u16)
                    .wrapping_add(inc);
                let in_cs = !matches!(self.seg_ovr, Some(s) if s != SReg::CS);
                if in_cs {
                    if let Some(name) = ctx.lookup.lookup_direct(self.op_seg, ofs, DataWidth::Word)
                    {
                        return write!(w, "{name}");
                    }
                }
                write_imm(w, ofs as u32)?;
            }
            ArgType::Rel16 => {
                let inc = self.imm[i] as u16;
                let ofs = self
                    .op_ofs
                    .wrapping_add(self.bytes.len() as u16)
                    .wrapping_add(inc);
                let in_cs = !matches!(self.seg_ovr, Some(s) if s != SReg::CS);
                if in_cs {
                    if let Some(name) = ctx.lookup.lookup_direct(self.op_seg, ofs, DataWidth::Word)
                    {
                        return write!(w, "{name}");
                    }
                }
                write_imm(w, ofs as u32)?;
            }
            ArgType::IMem8 | ArgType::IMem16 => {
                let seg = self.seg_ovr.unwrap_or(SReg::DS);
                let width = if matches!(self.arg_type[i], ArgType::IMem8) {
                    DataWidth::Byte
                } else {
                    DataWidth::Word
                };
                if let Some(name) =
                    ctx.lookup
                        .lookup_indirect(seg, None, None, self.imm[i] as u16, width)
                {
                    return write!(w, "{name}");
                }
                if let Some(ovr) = self.seg_ovr {
                    write!(w, "{ovr}:")?;
                }
                write!(w, "[")?;
                write_imm(w, self.imm[i])?;
                write!(w, "]")?;
            }
            ArgType::IMem32 => {
                let ofs = self.imm[i] as u16;
                let seg = (self.imm[i] >> 16) as u16;
                if let Some(ovr) = self.seg_ovr {
                    write!(w, "{ovr}:")?;
                }
                if let Some(idx) = self.imm_seg[i]
                    && let Some(seg_name) = ctx.lookup.lookup_segment(idx)
                {
                    let ofs_name = ctx.lookup.lookup_offset(ofs);
                    write!(w, "[{seg_name}:")?;
                    if let Some(name) = ofs_name {
                        write!(w, "{name}")?;
                    } else {
                        write_imm(w, ofs as u32)?;
                    }
                    write!(w, "]")?;
                } else {
                    write!(w, "[{seg:04x}:{ofs:04x}]")?;
                }
            }
            ArgType::Mem16 | ArgType::Mem32 | ArgType::RM8 | ArgType::RM16 => {
                let modrm = self.modrm;
                let mod_bits = (modrm >> 6) & 0b11;
                let rm = modrm & 0b111;
                let arg = self.arg_type[i];
                let wd = matches!(arg, ArgType::RM16 | ArgType::Mem16);

                if mod_bits == 0b11 {
                    // Only RM8/RM16 are valid here, Mem16/Mem32 are not
                    if matches!(arg, ArgType::Mem16 | ArgType::Mem32) {
                        write!(w, "invalid")?;
                    } else if wd {
                        write_reg(w, ctx, NamedReg::Gp16(GpReg16::from_bits(rm)))?;
                    } else {
                        write_reg(w, ctx, NamedReg::Gp8(GpReg8::from_bits(rm)))?;
                    }
                } else {
                    if needs_width_specifier {
                        match arg {
                            ArgType::RM8 => write!(w, "byte ptr ")?,
                            ArgType::RM16 | ArgType::Mem16 => write!(w, "word ptr ")?,
                            ArgType::Mem32 => write!(w, "far ptr ")?,
                            _ => {}
                        }
                    }
                    if let Some(ovr) = self.seg_ovr {
                        write!(w, "{ovr}:")?;
                    }
                    write!(w, "[")?;
                    match rm {
                        0b000 => write!(w, "bx+si")?,
                        0b001 => write!(w, "bx+di")?,
                        0b010 => write!(w, "bp+si")?,
                        0b011 => write!(w, "bp+di")?,
                        0b100 => write!(w, "si")?,
                        0b101 => write!(w, "di")?,
                        0b110 => {
                            if mod_bits != 0 {
                                write!(w, "bp")?;
                            } else {
                                write_imm(w, self.imm[i])?;
                            }
                        }
                        0b111 => write!(w, "bx")?,
                        _ => {}
                    }
                    match mod_bits {
                        0b01 => {
                            let disp = self.imm[i] as i8;
                            if disp < 0 {
                                write!(w, "-")?;
                                write_imm(w, (-(disp as i32)) as u32)?;
                            } else if disp > 0 {
                                write!(w, "+")?;
                                write_imm(w, disp as u32)?;
                            }
                        }
                        0b10 => {
                            let disp = self.imm[i] as i16;
                            if disp < 0 {
                                write!(w, "-")?;
                                write_imm(w, (-(disp as i32)) as u32)?;
                            } else if disp > 0 {
                                write!(w, "+")?;
                                write_imm(w, disp as u32)?;
                            }
                        }
                        _ => {}
                    }
                    write!(w, "]")?;
                }
            }
        }

        Ok(())
    }

    pub fn format<W: Write>(&self, mut w: W, ctx: DisplayContext) -> std::fmt::Result {
        let mut col = self.format_opcode(&mut w)?;

        for i in 0..2 {
            if self.arg_type[i] == ArgType::None {
                break;
            }

            if i == 0 {
                loop {
                    write!(w, " ")?;
                    col += 1;
                    if col >= 8 {
                        break;
                    }
                }
            } else {
                write!(w, ", ")?;
            }

            self.format_arg(&mut w, i, &ctx)?;
        }

        Ok(())
    }

    pub fn to_string_opts(&self, ctx: DisplayContext<'_>) -> String {
        struct FormattedInstruction<'a, 'b> {
            instr: &'a DecodedInstruction,
            ctx: DisplayContext<'b>,
        }

        impl<'a, 'b> std::fmt::Display for FormattedInstruction<'a, 'b> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.instr.format(
                    f,
                    DisplayContext {
                        lookup: self.ctx.lookup,
                        arg_fmts: self.ctx.arg_fmts,
                    },
                )
            }
        }

        FormattedInstruction { instr: self, ctx }.to_string()
    }
}

struct NullLookup;

impl SymbolLookup for NullLookup {
    fn lookup_direct(&self, _seg: u16, _ofs: u16, _width: DataWidth) -> Option<String> {
        None
    }
    fn lookup_indirect(
        &self,
        _seg: SReg,
        _base: Option<BaseReg>,
        _index: Option<IndexReg>,
        _disp: u16,
        _width: DataWidth,
    ) -> Option<String> {
        None
    }
    fn lookup_offset(&self, _ofs: u16) -> Option<String> {
        None
    }
}

impl Display for DecodedInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format(
            f,
            DisplayContext {
                lookup: &NullLookup,
                arg_fmts: [None; 2],
            },
        )
    }
}

/// 16-bit general-purpose register, indexed in standard 8086 modrm.reg / modrm.rm order.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum MemRef {
    Direct {
        seg: u16, // Direct segment (paragraph value)
        /// Resolved project segment, set when the seg-half of the far-pointer
        /// imm32 is covered by an EXE relocation entry.
        seg_idx: Option<SegmentIdx>,
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
    /// A 16-bit immediate that the EXE relocation table resolved to a known
    /// project segment. Produced from `Imm16` operands whose imm bytes are
    /// covered by a relocation entry. e.g. `mov ax, seg001`.
    SegRef(SegmentIdx),
    /// A near branch target. `target` is the absolute `(seg, ofs)` after
    /// applying the relative-branch arithmetic.
    Rel {
        target: (u16, u16),
    },
    Mem(MemRef),
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::{DisasmCtx, decode, decode_with_ctx};

    fn dec(bytes: &[u8]) -> DecodedInstruction {
        decode(0, 0, bytes.iter().copied()).expect("decode failed")
    }

    fn dec_with(
        seg: u16,
        ofs: u16,
        bytes: &[u8],
        relocs: &BTreeMap<u32, SegmentIdx>,
    ) -> DecodedInstruction {
        let ctx = DisasmCtx {
            imm_relocations: Some(relocs),
        };
        decode_with_ctx(seg, ofs, bytes.iter().copied(), &ctx).expect("decode failed")
    }

    #[test]
    fn operand_fixed_registers() {
        // inc ax = 0x40 (one-arg, AX is RW)
        let inst = dec(&[0x40]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::AX));
        assert_eq!(inst.operand(1), Operand::None);

        // push si = 0x56
        let inst = dec(&[0x56]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::SI));
    }

    #[test]
    fn operand_reg16_from_modrm() {
        // mov ax, bx = 89 d8  (modrm: mod=11 reg=011 rm=000)
        // 89 = MOV r/m16, r16; reg field encodes the source (BX), rm encodes dst (AX).
        let inst = dec(&[0x89, 0xd8]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::AX)); // RM16
        assert_eq!(inst.operand(1), Operand::Gp16(GpReg16::BX)); // Reg16
    }

    #[test]
    fn operand_reg8_from_modrm() {
        // mov al, bl = 88 d8 (modrm: mod=11 reg=011(BL) rm=000(AL))
        let inst = dec(&[0x88, 0xd8]);
        assert_eq!(inst.operand(0), Operand::Gp8(GpReg8::AL));
        assert_eq!(inst.operand(1), Operand::Gp8(GpReg8::BL));
    }

    #[test]
    fn operand_sreg_from_modrm() {
        // mov ds, ax = 8e d8 (modrm: mod=11 reg=011(DS) rm=000(AX))
        let inst = dec(&[0x8e, 0xd8]);
        assert_eq!(inst.operand(0), Operand::Sreg(SReg::DS));
        assert_eq!(inst.operand(1), Operand::Gp16(GpReg16::AX));
    }

    #[test]
    fn operand_rm16_memory() {
        // mov ax, [bx] = 8b 07 (modrm: mod=00 reg=000(AX) rm=111(BX))
        // 0x8B = MOV r16, r/m16: operand 0 is Reg16 (AX, dst), operand 1 is RM16 ([BX], src).
        let inst = dec(&[0x8b, 0x07]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::AX));
        match inst.operand(1) {
            Operand::Mem(MemRef::Indirect {
                base, index, disp, ..
            }) => {
                assert_eq!(base, Some(BaseReg::BX));
                assert_eq!(index, None);
                assert_eq!(disp, 0);
            }
            other => panic!("expected Mem(Indirect), got {other:?}"),
        }
    }

    #[test]
    fn operand_lea_mem16_decodes_via_modrm() {
        // lea si, [di+14h] = 8d 75 14 (modrm: mod=01 reg=110(SI) rm=101([DI+disp8])).
        // `lea`'s source is a `Mem16` (ModRM memory) operand: its base/index/disp
        // come from the modrm, not from a direct immediate offset.
        let inst = dec(&[0x8d, 0x75, 0x14]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::SI));
        match inst.operand(1) {
            Operand::Mem(MemRef::Indirect {
                base, index, disp, ..
            }) => {
                assert_eq!(base, None);
                assert_eq!(index, Some(IndexReg::DI));
                assert_eq!(disp, 0x14);
            }
            other => panic!("expected Mem(Indirect [di+14h]), got {other:?}"),
        }
    }

    #[test]
    fn operand_immediate() {
        // mov ax, 1234h = b8 34 12
        let inst = dec(&[0xb8, 0x34, 0x12]);
        assert_eq!(inst.operand(0), Operand::Gp16(GpReg16::AX));
        assert_eq!(
            inst.operand(1),
            Operand::Imm {
                value: 0x1234,
                width: DataWidth::Word,
            }
        );
    }

    #[test]
    fn operand_rel_target_matches_branch_destination() {
        // jmp +5 (rel8) = eb 05 at offset 0
        let inst = dec(&[0xeb, 0x05]);
        let bd = inst.branch_destination().unwrap();
        match inst.operand(0) {
            Operand::Rel { target } => assert_eq!(target, bd),
            other => panic!("expected Rel, got {other:?}"),
        }
    }

    #[test]
    fn dst_src_for_mov() {
        // mov ax, bx = 89 d8
        let inst = dec(&[0x89, 0xd8]);
        assert_eq!(inst.dst_src(), Some((0, 1)));
    }

    #[test]
    fn dst_src_for_cmp_is_none() {
        // cmp ax, bx = 39 d8 (both operands RO)
        let inst = dec(&[0x39, 0xd8]);
        assert_eq!(inst.dst_src(), None);
    }

    #[test]
    fn dst_src_for_xchg_is_none() {
        // xchg ax, bx = 87 c3 (both RW)
        let inst = dec(&[0x87, 0xc3]);
        assert_eq!(inst.dst_src(), None);
    }

    #[test]
    fn destinations_for_xchg() {
        // xchg ax, bx = 87 c3 — both operands writable
        let inst = dec(&[0x87, 0xc3]);
        let dests: Vec<usize> = inst.destinations().collect();
        assert_eq!(dests, vec![0, 1]);
    }

    #[test]
    fn destinations_for_cmp_empty() {
        let inst = dec(&[0x39, 0xd8]);
        let dests: Vec<usize> = inst.destinations().collect();
        assert!(dests.is_empty());
    }

    #[test]
    fn destinations_for_mov_just_dst() {
        let inst = dec(&[0x89, 0xd8]);
        let dests: Vec<usize> = inst.destinations().collect();
        assert_eq!(dests, vec![0]);
    }

    #[test]
    fn writes_to_gp16_sp_via_specific_arg() {
        // pop sp = 5c (one-arg form, SP is the destination)
        let inst = dec(&[0x5c]);
        assert!(inst.writes_to_gp16(GpReg16::SP));
        assert!(!inst.writes_to_gp16(GpReg16::AX));
    }

    #[test]
    fn writes_to_gp16_sp_via_modrm_rm() {
        // mov sp, bx = 89 dc (modrm: mod=11 reg=011(BX) rm=100(SP))
        let inst = dec(&[0x89, 0xdc]);
        assert!(inst.writes_to_gp16(GpReg16::SP));
    }

    #[test]
    fn writes_to_gp16_sp_via_immediate_arith() {
        // add sp, 4 = 83 c4 04 (group: mod=11 reg=000(ADD) rm=100(SP))
        let inst = dec(&[0x83, 0xc4, 0x04]);
        assert!(inst.writes_to_gp16(GpReg16::SP));
    }

    #[test]
    fn imm_ofs_for_imm16() {
        // mov ax, 1234h = b8 34 12 — imm16 starts at byte 1
        let inst = dec(&[0xb8, 0x34, 0x12]);
        assert_eq!(inst.imm_ofs[0], None); // AX is fixed-register
        assert_eq!(inst.imm_ofs[1], Some(1));
    }

    #[test]
    fn imm_ofs_for_rm_with_disp16() {
        // mov ax, [bx+1234h] = 8b 87 34 12
        // modrm 87 = mod=10 reg=000(AX) rm=111(BX), then 16-bit disp at byte 2
        let inst = dec(&[0x8b, 0x87, 0x34, 0x12]);
        assert_eq!(inst.imm_ofs[0], None); // Reg16 (AX): no imm bytes
        assert_eq!(inst.imm_ofs[1], Some(2)); // RM16 with 16-bit disp
    }

    #[test]
    fn imm_ofs_none_for_register_only() {
        // mov ax, bx = 89 d8 (mod=11 — both operands are registers)
        let inst = dec(&[0x89, 0xd8]);
        assert_eq!(inst.imm_ofs, [None, None]);
    }

    #[test]
    fn imm_ofs_for_imem16() {
        // mov ax, [1234h] = a1 34 12 — IMem16 at byte 1
        let inst = dec(&[0xa1, 0x34, 0x12]);
        assert_eq!(inst.imm_ofs[0], None);
        assert_eq!(inst.imm_ofs[1], Some(1));
    }

    #[test]
    fn imm_ofs_for_far_call() {
        // call far 1234:5678 = 9a 78 56 34 12 — IMem32 at byte 1
        let inst = dec(&[0x9a, 0x78, 0x56, 0x34, 0x12]);
        assert_eq!(inst.imm_ofs[0], Some(1));
    }

    #[test]
    fn writes_to_gp16_sp_false_when_only_read() {
        // mov ax, sp = 89 e0 (modrm: mod=11 reg=100(SP) rm=000(AX))
        // SP is the source (Reg16, RO); AX is the destination.
        let inst = dec(&[0x89, 0xe0]);
        assert!(!inst.writes_to_gp16(GpReg16::SP));
        assert!(inst.writes_to_gp16(GpReg16::AX));
    }

    #[test]
    fn imm_seg_empty_without_ctx() {
        // mov ax, 1234h = b8 34 12 — without relocations, imm_seg stays None.
        let inst = dec(&[0xb8, 0x34, 0x12]);
        assert_eq!(inst.imm_seg, [None, None]);
    }

    #[test]
    fn imm_seg_populated_for_imm16_when_reloc_matches() {
        // Place a `mov ax, imm16` at linear address 0x100
        // (op_seg=0x10 paragraph, op_ofs=0). Imm bytes start at op_ofs+1=0x101.
        let mut relocs = BTreeMap::new();
        let target = SegmentIdx::from(7);
        relocs.insert(0x101, target);

        let inst = dec_with(0x10, 0, &[0xb8, 0x34, 0x12], &relocs);
        assert_eq!(inst.imm_seg[1], Some(target));
        // operand(1) should now report SegRef.
        assert_eq!(inst.operand(1), Operand::SegRef(target));
    }

    #[test]
    fn imm_seg_not_populated_when_reloc_misses() {
        // Same instruction, but the relocation is at a different address.
        let mut relocs = BTreeMap::new();
        relocs.insert(0x200, SegmentIdx::from(7));

        let inst = dec_with(0x10, 0, &[0xb8, 0x34, 0x12], &relocs);
        assert_eq!(inst.imm_seg, [None, None]);
        // operand(1) falls back to plain Imm.
        assert_eq!(
            inst.operand(1),
            Operand::Imm {
                value: 0x1234,
                width: DataWidth::Word,
            }
        );
    }

    #[test]
    fn imm_seg_for_imem32_seg_half() {
        // call far 1234:5678 = 9a 78 56 34 12. The seg-half (34 12) sits at
        // imm_ofs+2 = 1+2 = 3 within the instruction. With op_seg=0, op_ofs=0,
        // the relocation key is 3.
        let mut relocs = BTreeMap::new();
        let target = SegmentIdx::from(3);
        relocs.insert(3, target);

        let inst = dec_with(0, 0, &[0x9a, 0x78, 0x56, 0x34, 0x12], &relocs);
        assert_eq!(inst.imm_seg[0], Some(target));
        // The IMem32 routes through MemRef::Direct with seg_idx propagated.
        match inst.operand(0) {
            Operand::Mem(MemRef::Direct { seg_idx, .. }) => {
                assert_eq!(seg_idx, Some(target));
            }
            other => panic!("expected Mem(Direct) with seg_idx set, got {other:?}"),
        }
    }

    #[test]
    fn imm_seg_no_match_for_imem32_offset_half() {
        // The offset half of a far ptr is at imm_ofs+0; we should NOT treat
        // a reloc at that key as a seg fixup. Place reloc at offset-half:
        let mut relocs = BTreeMap::new();
        relocs.insert(1, SegmentIdx::from(3));

        let inst = dec_with(0, 0, &[0x9a, 0x78, 0x56, 0x34, 0x12], &relocs);
        // The probe for IMem32 is imm_ofs+2, not imm_ofs+0, so no match.
        assert_eq!(inst.imm_seg[0], None);
    }
}

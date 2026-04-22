use std::fmt::{Display, Write};

use smallvec::SmallVec;

use crate::{BaseReg, DataWidth, IndexReg, MemRef, SReg};

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
    pub has_mem_arg: bool,
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
        let (i, arg_type) = self
            .arg_type
            .iter()
            .copied()
            .enumerate()
            .find(|(_, arg_type)| is_mem_ref_arg(*arg_type, self.modrm))?;

        let mem_ref = match arg_type {
            ArgType::IMem8 | ArgType::IMem16 => MemRef::Indirect {
                seg: self.seg_ovr.unwrap_or(SReg::DS),
                base: None,
                index: None,
                disp: self.imm[i] as u16,
                width: match self.arg_type[i] {
                    ArgType::IMem8 => DataWidth::Byte,
                    ArgType::IMem16 => DataWidth::Word,
                    _ => unreachable!(),
                },
            },
            ArgType::IMem32 => MemRef::Direct {
                seg: (self.imm[i] >> 16) as u16,
                ofs: self.imm[i] as u16,
                width: DataWidth::Dword,
            },
            ArgType::RM8 | ArgType::RM16 => {
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
                    width: match self.arg_type[i] {
                        ArgType::RM8 => DataWidth::Byte,
                        ArgType::RM16 => DataWidth::Word,
                        _ => unreachable!(),
                    },
                }
            }
            _ => {
                unreachable!()
            }
        };

        Some(mem_ref)
    }
}

pub trait SymbolLookup {
    /// For `MemRef::Direct { seg, ofs, width }`.
    fn lookup_direct(&self, seg: u16, ofs: u16, width: DataWidth) -> Option<&str>;

    /// For `MemRef::Indirect { seg, base, index, disp, width }`.
    /// Resolves base/index registers and the segment register internally.
    fn lookup_indirect(
        &self,
        seg: SReg,
        base: Option<BaseReg>,
        index: Option<IndexReg>,
        disp: u16,
        width: DataWidth,
    ) -> Option<&str>;

    /// For `Imm8`/`Imm16` — resolves using a pre-configured default segment.
    fn lookup_offset(&self, ofs: u16) -> Option<&str>;
}

pub struct DisplayContext<'a> {
    pub lookup: &'a dyn SymbolLookup,
}

/// Static mapping from segment registers to known project segment indices.
/// Used by [`ProjectLookup`] to resolve segment registers without a runtime
/// register file. Fields are `None` when the mapping is unknown.
#[derive(Debug, Default, Clone, Copy)]
pub struct SRegMap {
    pub es: Option<usize>,
    pub cs: Option<usize>,
    pub ss: Option<usize>,
    pub ds: Option<usize>,
}

impl SRegMap {
    pub fn get(&self, r: SReg) -> Option<usize> {
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
                MemRef::Direct { seg, ofs, width } => ctx.lookup.lookup_direct(seg, ofs, width),
                MemRef::Indirect {
                    seg,
                    base,
                    index,
                    disp,
                    width,
                } => ctx.lookup.lookup_indirect(seg, base, index, disp, width),
            };
            if let Some(name) = name {
                return write!(w, "{name}");
            }
        }

        match self.arg_type[i] {
            ArgType::Inherit => unreachable!(),
            ArgType::None => unreachable!(),
            ArgType::Const1 => write!(w, "1")?,
            ArgType::Const3 => write!(w, "3")?,
            ArgType::AL => write!(w, "al")?,
            ArgType::CL => write!(w, "cl")?,
            ArgType::DL => write!(w, "dl")?,
            ArgType::BL => write!(w, "bl")?,
            ArgType::AH => write!(w, "ah")?,
            ArgType::CH => write!(w, "ch")?,
            ArgType::DH => write!(w, "dh")?,
            ArgType::BH => write!(w, "bh")?,
            ArgType::AX => write!(w, "ax")?,
            ArgType::CX => write!(w, "cx")?,
            ArgType::DX => write!(w, "dx")?,
            ArgType::BX => write!(w, "bx")?,
            ArgType::SP => write!(w, "sp")?,
            ArgType::BP => write!(w, "bp")?,
            ArgType::SI => write!(w, "si")?,
            ArgType::DI => write!(w, "di")?,
            ArgType::ES => write!(w, "es")?,
            ArgType::CS => write!(w, "cs")?,
            ArgType::SS => write!(w, "ss")?,
            ArgType::DS => write!(w, "ds")?,
            ArgType::Reg8 => {
                let reg = (self.modrm >> 3) & 0b111;
                let s = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
                write!(w, "{}", s[reg as usize])?;
            }
            ArgType::Reg16 => {
                let reg = (self.modrm >> 3) & 0b111;
                let s = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
                write!(w, "{}", s[reg as usize])?;
            }
            ArgType::SReg => {
                let reg = (self.modrm >> 3) & 0b11;
                let s = ["es", "cs", "ss", "ds"];
                write!(w, "{}", s[reg as usize])?;
            }
            ArgType::Imm8 | ArgType::Imm16 => {
                if let Some(name) = ctx.lookup.lookup_offset(self.imm[i] as u16) {
                    return write!(w, "{name}");
                }
                write_imm(w, self.imm[i])?;
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
                write!(w, "[{seg:04x}:{ofs:04x}]")?;
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
                    } else {
                        // str_reg equivalent: use 16-bit or 8-bit reg name
                        let reg_names = if wd {
                            ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
                        } else {
                            ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
                        };
                        write!(w, "{}", reg_names[rm as usize])?;
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
                    },
                )
            }
        }

        FormattedInstruction { instr: self, ctx }.to_string()
    }
}

struct NullLookup;

impl SymbolLookup for NullLookup {
    fn lookup_direct(&self, _seg: u16, _ofs: u16, _width: DataWidth) -> Option<&str> {
        None
    }
    fn lookup_indirect(
        &self,
        _seg: SReg,
        _base: Option<BaseReg>,
        _index: Option<IndexReg>,
        _disp: u16,
        _width: DataWidth,
    ) -> Option<&str> {
        None
    }
    fn lookup_offset(&self, _ofs: u16) -> Option<&str> {
        None
    }
}

impl Display for DecodedInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format(
            f,
            DisplayContext {
                lookup: &NullLookup,
            },
        )
    }
}

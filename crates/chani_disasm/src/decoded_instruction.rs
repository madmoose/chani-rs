use std::fmt::Display;

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

fn write_imm(f: &mut std::fmt::Formatter<'_>, v: u32) -> std::fmt::Result {
    fn most_significant_nybble(n: u32) -> u8 {
        if n == 0 {
            return 0;
        }

        let nybble_position = (31 - n.leading_zeros()) / 4;
        ((n >> (nybble_position * 4)) & 0xf) as u8
    }

    if v < 10 {
        return write!(f, "{v}");
    }
    if most_significant_nybble(v) > 9 {
        write!(f, "0")?;
    }
    write!(f, "{v:x}h")?;

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

pub struct DisplayContext<'a> {
    pub lookup: &'a dyn Fn(u16, u16) -> Option<&'a str>,
    pub register_file: Option<RegisterFile>,
    pub imm_seg: Option<u16>,
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
    pub fn format(&self, f: &mut std::fmt::Formatter<'_>, ctx: DisplayContext) -> std::fmt::Result {
        let mut col = 0;

        // for b in &self.bytes {
        //     write!(f, "{b:02X} ")?;
        //     col += 3;
        // }

        // loop {
        //     if col > 7 + 5 * 3 {
        //         break;
        //     }
        //     write!(f, " ")?;
        //     col += 1;
        // }

        if self.flag_lock {
            write!(f, "lock ")?;
            col += 5;
        }

        if self.flag_f3 {
            match self.opcode {
                Opcode::Stosb
                | Opcode::Stosw
                | Opcode::Movsb
                | Opcode::Movsw
                | Opcode::Lodsb
                | Opcode::Lodsw => write!(f, "rep ")?,
                Opcode::Cmpsb | Opcode::Cmpsw | Opcode::Scasb | Opcode::Scasw => {
                    write!(f, "repz ")?
                }
                _ => (),
            }
        }
        if self.flag_f2 {
            match self.opcode {
                Opcode::Cmpsb | Opcode::Cmpsw | Opcode::Scasb | Opcode::Scasw => {
                    write!(f, "repnz ")?
                }
                _ => (),
            }
        }

        if let Some(seg_ovr) = self.seg_ovr
            && !self.has_mem_arg
        {
            write!(f, "{seg_ovr}:")?;
            col += 3;
        }

        write!(f, "{}", self.mnemonic())?;
        col += self.mnemonic().len();

        let needs_width_specifier =
            self.has_mem_arg && self.arg_type.iter().any(ArgType::needs_width_specifier);

        for i in 0..2 {
            if self.arg_type[i] == ArgType::None {
                break;
            }

            if i == 0 {
                loop {
                    write!(f, " ")?;
                    col += 1;
                    if col >= 8 {
                        break;
                    }
                }
            } else {
                write!(f, ", ")?;
                col += 2;
            }

            if is_mem_ref_arg(self.arg_type[i], self.modrm)
                && let Some(mem_ref) = self.mem_ref()
            {
                let resolved = match mem_ref {
                    MemRef::Direct { seg, ofs, width: _ } => Some((seg, ofs)),
                    MemRef::Indirect {
                        seg,
                        base,
                        index,
                        disp,
                        width: _,
                    } => ctx.register_file.as_ref().map(|rf| {
                        let seg = rf.get_sreg(seg);
                        let base = base.map(|b| rf.get_base_reg(b)).unwrap_or(0);
                        let index = index.map(|i| rf.get_index_reg(i)).unwrap_or(0);
                        let ofs = base.wrapping_add(index).wrapping_add(disp);
                        (seg, ofs)
                    }),
                };

                if let Some((seg, ofs)) = resolved {
                    if let Some(name) = (ctx.lookup)(seg, ofs) {
                        write!(f, "{}", name)?;
                        continue;
                    }
                }
            }

            match self.arg_type[i] {
                ArgType::Inherit => unreachable!(),
                ArgType::None => unreachable!(),
                ArgType::Const1 => write!(f, "1")?,
                ArgType::Const3 => write!(f, "3")?,
                ArgType::AL => write!(f, "al")?,
                ArgType::CL => write!(f, "cl")?,
                ArgType::DL => write!(f, "dl")?,
                ArgType::BL => write!(f, "bl")?,
                ArgType::AH => write!(f, "ah")?,
                ArgType::CH => write!(f, "ch")?,
                ArgType::DH => write!(f, "dh")?,
                ArgType::BH => write!(f, "bh")?,
                ArgType::AX => write!(f, "ax")?,
                ArgType::CX => write!(f, "cx")?,
                ArgType::DX => write!(f, "dx")?,
                ArgType::BX => write!(f, "bx")?,
                ArgType::SP => write!(f, "sp")?,
                ArgType::BP => write!(f, "bp")?,
                ArgType::SI => write!(f, "si")?,
                ArgType::DI => write!(f, "di")?,
                ArgType::ES => write!(f, "es")?,
                ArgType::CS => write!(f, "cs")?,
                ArgType::SS => write!(f, "ss")?,
                ArgType::DS => write!(f, "ds")?,
                ArgType::Reg8 => {
                    let reg = (self.modrm >> 3) & 0b111;
                    let s = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
                    write!(f, "{}", s[reg as usize])?;
                }
                ArgType::Reg16 => {
                    let reg = (self.modrm >> 3) & 0b111;
                    let s = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
                    write!(f, "{}", s[reg as usize])?;
                }
                ArgType::SReg => {
                    let reg = (self.modrm >> 3) & 0b11;
                    let s = ["es", "cs", "ss", "ds"];
                    write!(f, "{}", s[reg as usize])?;
                }
                ArgType::Imm8 | ArgType::Imm16 => {
                    if let Some(seg) = ctx.imm_seg {
                        if let Some(name) = (ctx.lookup)(seg, self.imm[i] as u16) {
                            write!(f, "{name}")?;
                            continue;
                        }
                    }
                    write_imm(f, self.imm[i])?;
                }
                ArgType::Rel8 => {
                    let inc = self.imm[i] as i8 as i16 as u16;
                    let ofs = self
                        .op_ofs
                        .wrapping_add(self.bytes.len() as u16)
                        .wrapping_add(inc);
                    let in_cs = !matches!(self.seg_ovr, Some(s) if s != SReg::CS);
                    if in_cs {
                        if let Some(name) = (ctx.lookup)(self.op_seg, ofs) {
                            write!(f, "{name}")?;
                            continue;
                        }
                    }
                    write_imm(f, ofs as u32)?;
                }
                ArgType::Rel16 => {
                    let inc = self.imm[i] as u16;
                    let ofs = self
                        .op_ofs
                        .wrapping_add(self.bytes.len() as u16)
                        .wrapping_add(inc);
                    let in_cs = !matches!(self.seg_ovr, Some(s) if s != SReg::CS);
                    if in_cs {
                        if let Some(name) = (ctx.lookup)(self.op_seg, ofs) {
                            write!(f, "{name}")?;
                            continue;
                        }
                    }
                    write_imm(f, ofs as u32)?;
                }
                ArgType::IMem8 | ArgType::IMem16 => {
                    if let Some(ovr) = self.seg_ovr {
                        write!(f, "{ovr}:")?;
                    }
                    write!(f, "[")?;
                    write_imm(f, self.imm[i])?;
                    write!(f, "]")?;
                }
                ArgType::IMem32 => {
                    let ofs = self.imm[i] as u16;
                    let seg = (self.imm[i] >> 16) as u16;
                    if let Some(ovr) = self.seg_ovr {
                        write!(f, "{ovr}:")?;
                    }
                    write!(f, "[{seg:04x}:{ofs:04x}]")?;
                }
                ArgType::Mem16 | ArgType::Mem32 | ArgType::RM8 | ArgType::RM16 => {
                    let modrm = self.modrm;
                    let mod_bits = (modrm >> 6) & 0b11;
                    let rm = modrm & 0b111;
                    let arg = self.arg_type[i];
                    let w = matches!(arg, ArgType::RM16 | ArgType::Mem16);
                    let show_mem_width = needs_width_specifier;

                    if mod_bits == 0b11 {
                        // Only RM8/RM16 are valid here, Mem16/Mem32 are not
                        if matches!(arg, ArgType::Mem16 | ArgType::Mem32) {
                            write!(f, "invalid")?;
                        } else {
                            // str_reg equivalent: use 16-bit or 8-bit reg name
                            let reg_names = if w {
                                ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
                            } else {
                                ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
                            };
                            write!(f, "{}", reg_names[rm as usize])?;
                        }
                    } else {
                        if show_mem_width {
                            match arg {
                                ArgType::RM8 => write!(f, "byte ptr ")?,
                                ArgType::RM16 | ArgType::Mem16 => write!(f, "word ptr ")?,
                                ArgType::Mem32 => write!(f, "far ptr ")?,
                                _ => {}
                            }
                        }
                        if let Some(ovr) = self.seg_ovr {
                            write!(f, "{ovr}:")?;
                        }
                        write!(f, "[")?;
                        match rm {
                            0b000 => write!(f, "bx+si")?,
                            0b001 => write!(f, "bx+di")?,
                            0b010 => write!(f, "bp+si")?,
                            0b011 => write!(f, "bp+di")?,
                            0b100 => write!(f, "si")?,
                            0b101 => write!(f, "di")?,
                            0b110 => {
                                if mod_bits != 0 {
                                    write!(f, "bp")?;
                                } else {
                                    write_imm(f, self.imm[i])?;
                                }
                            }
                            0b111 => write!(f, "bx")?,
                            _ => {}
                        }
                        match mod_bits {
                            0b01 => {
                                let disp = self.imm[i] as i8;
                                if disp < 0 {
                                    write!(f, "-")?;
                                    write_imm(f, (-(disp as i32)) as u32)?;
                                } else if disp > 0 {
                                    write!(f, "+")?;
                                    write_imm(f, disp as u32)?;
                                }
                            }
                            0b10 => {
                                let disp = self.imm[i] as i16;
                                if disp < 0 {
                                    write!(f, "-")?;
                                    write_imm(f, (-(disp as i32)) as u32)?;
                                } else if disp > 0 {
                                    write!(f, "+")?;
                                    write_imm(f, disp as u32)?;
                                }
                            }
                            _ => {}
                        }
                        write!(f, "]")?;
                    }
                }
            };
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
                        register_file: self.ctx.register_file,
                        imm_seg: self.ctx.imm_seg,
                    },
                )
            }
        }

        FormattedInstruction { instr: self, ctx }.to_string()
    }
}

impl Display for DecodedInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format(
            f,
            DisplayContext {
                lookup: &|_, _| None,
                register_file: None,
                imm_seg: None,
            },
        )
    }
}

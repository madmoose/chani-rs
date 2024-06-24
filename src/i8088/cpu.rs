use std::mem::swap;

use crate::bus::Bus;
use crate::i8088::StrOp;

use super::bitops::BitOps;
use super::register_file::RegisterFile;
use super::{
    ea, flags::*, sext16, AluOp, Dir, RepMode, FLAG_AF, FLAG_CF, FLAG_PF, FLAG_SF, FLAG_ZF,
};
use super::{
    read_hi, read_lo, sext, ModRM, RegMem,
    Register::*,
    SReg,
    Width::{self, *},
};

#[derive(Debug, Default)]
pub struct Cpu {
    register_file: RegisterFile,
    sreg_ovr: Option<SReg>,
    int_delay: bool,
    rep_mode: RepMode,
    cycles: u64,
}

impl SReg {
    fn from_u8(i: u8) -> SReg {
        match i {
            0b00 => SReg::ES,
            0b01 => SReg::CS,
            0b10 => SReg::SS,
            0b11 => SReg::DS,
            _ => unreachable!(),
        }
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_ax(&mut self, v: u16) {
        self.register_file.set(AX, v);
    }

    pub fn set_al(&mut self, v: u8) {
        self.register_file.set_lo(AX, v);
    }

    pub fn set_ah(&mut self, v: u8) {
        self.register_file.set_hi(AX, v);
    }

    pub fn set_cx(&mut self, v: u16) {
        self.register_file.set(CX, v);
    }

    pub fn set_dx(&mut self, v: u16) {
        self.register_file.set(DX, v);
    }

    pub fn set_bx(&mut self, v: u16) {
        self.register_file.set(BX, v);
    }

    pub fn set_sp(&mut self, v: u16) {
        self.register_file.set(SP, v);
    }

    pub fn set_bp(&mut self, v: u16) {
        self.register_file.set(BP, v);
    }

    pub fn set_di(&mut self, v: u16) {
        self.register_file.set(DI, v);
    }

    pub fn set_si(&mut self, v: u16) {
        self.register_file.set(SI, v);
    }

    pub fn set_es(&mut self, v: u16) {
        self.register_file.set(ES, v);
    }

    pub fn set_cs(&mut self, v: u16) {
        self.register_file.set(CS, v);
    }

    pub fn set_ss(&mut self, v: u16) {
        self.register_file.set(SS, v);
    }

    pub fn set_ds(&mut self, v: u16) {
        self.register_file.set(DS, v);
    }

    pub fn set_ip(&mut self, v: u16) {
        self.register_file.set(IP, v);
    }

    pub fn set_flags(&mut self, v: u16) {
        const RESERVED_ON: u16 = 0b1111_0000_0000_0010;
        const RESERVED_OFF: u16 = 0b1111_1111_1101_0111;

        let flags = (v | RESERVED_ON) & RESERVED_OFF;

        self.register_file.set_flags(flags);
    }

    pub fn get_ax(&self) -> u16 {
        self.register_file.get(AX)
    }

    pub fn get_al(&self) -> u8 {
        self.register_file.get_lo(AX)
    }

    pub fn get_ah(&self) -> u8 {
        self.register_file.get_hi(AX)
    }

    pub fn get_cx(&self) -> u16 {
        self.register_file.get(CX)
    }

    pub fn get_dx(&self) -> u16 {
        self.register_file.get(DX)
    }

    pub fn get_bx(&self) -> u16 {
        self.register_file.get(BX)
    }

    pub fn get_sp(&self) -> u16 {
        self.register_file.get(SP)
    }

    pub fn get_bp(&self) -> u16 {
        self.register_file.get(BP)
    }

    pub fn get_di(&self) -> u16 {
        self.register_file.get(DI)
    }

    pub fn get_si(&self) -> u16 {
        self.register_file.get(SI)
    }

    pub fn get_es(&self) -> u16 {
        self.register_file.get(ES)
    }

    pub fn get_cs(&self) -> u16 {
        self.register_file.get(CS)
    }

    pub fn get_ss(&self) -> u16 {
        self.register_file.get(SS)
    }

    pub fn get_ds(&self) -> u16 {
        self.register_file.get(DS)
    }

    pub fn get_ip(&self) -> u16 {
        self.register_file.get(IP)
    }

    pub fn get_flags(&self) -> u16 {
        self.register_file.get_flags()
    }

    fn get_cf(&self) -> bool {
        self.register_file.get_cf()
    }

    fn get_pf(&self) -> bool {
        self.register_file.get_pf()
    }

    fn get_af(&self) -> bool {
        self.register_file.get_af()
    }

    fn get_zf(&self) -> bool {
        self.register_file.get_zf()
    }

    fn get_sf(&self) -> bool {
        self.register_file.get_sf()
    }

    fn get_df(&self) -> bool {
        self.register_file.get_df()
    }

    fn get_of(&self) -> bool {
        self.register_file.get_of()
    }

    fn set_cf(&mut self, cond: bool) {
        self.register_file.set_cf(cond);
    }

    fn set_pf(&mut self, cond: bool) {
        self.register_file.set_pf(cond);
    }

    fn set_af(&mut self, cond: bool) {
        self.register_file.set_af(cond);
    }

    fn set_zf(&mut self, cond: bool) {
        self.register_file.set_zf(cond);
    }

    fn set_sf(&mut self, cond: bool) {
        self.register_file.set_sf(cond);
    }

    fn set_if(&mut self, cond: bool) {
        self.register_file.set_if(cond);
    }

    fn set_tf(&mut self, cond: bool) {
        self.register_file.set_tf(cond);
    }

    fn set_df(&mut self, cond: bool) {
        self.register_file.set_df(cond);
    }

    fn set_of(&mut self, cond: bool) {
        self.register_file.set_of(cond);
    }

    fn mem_read8(&self, bus: &impl Bus, seg: u16, ofs: u16) -> u8 {
        let ea = ea(seg, ofs);
        bus.mem_read_u8(ea)
    }

    fn mem_read16(&self, bus: &impl Bus, seg: u16, ofs: u16) -> u16 {
        let ea_lo = ea(seg, ofs);
        let ea_hi = ea(seg, ofs.wrapping_add(1));

        let lo = bus.mem_read_u8(ea_lo) as u16;
        let hi = bus.mem_read_u8(ea_hi) as u16;
        (hi << 8) + lo
    }

    fn mem_readw(&self, bus: &impl Bus, seg: u16, ofs: u16, w: Width) -> u16 {
        match w {
            W8 => self.mem_read8(bus, seg, ofs) as u16,
            W16 => self.mem_read16(bus, seg, ofs),
        }
    }

    fn mem_write8(&self, bus: &mut impl Bus, seg: u16, ofs: u16, v: u8) {
        let ea = ea(seg, ofs);
        bus.mem_write_u8(ea, v);
    }

    fn mem_write16(&self, bus: &mut impl Bus, seg: u16, ofs: u16, v: u16) {
        let ea_lo = ea(seg, ofs);
        let ea_hi = ea(seg, ofs.wrapping_add(1));
        bus.mem_write_u8(ea_lo, read_lo(v));
        bus.mem_write_u8(ea_hi, read_hi(v));
    }

    fn mem_writew(&self, bus: &mut impl Bus, seg: u16, ofs: u16, v: u16, w: Width) {
        match w {
            W8 => self.mem_write8(bus, seg, ofs, read_lo(v)),
            W16 => self.mem_write16(bus, seg, ofs, v),
        }
    }

    fn reg_readw_ax(&self, w: Width) -> u16 {
        match w {
            W8 => self.register_file.get_lo(AX) as u16,
            W16 => self.get_ax(),
        }
    }

    fn reg_writew_ax(&mut self, w: Width, v: u16) {
        match w {
            W8 => self.register_file.set_lo(AX, v as u8),
            W16 => self.register_file.set(AX, v),
        }
    }

    fn reg_read8(&self, reg_idx: u8) -> u8 {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.get_lo(AX),
            0b001 => self.register_file.get_lo(CX),
            0b010 => self.register_file.get_lo(DX),
            0b011 => self.register_file.get_lo(BX),
            0b100 => self.register_file.get_hi(AX),
            0b101 => self.register_file.get_hi(CX),
            0b110 => self.register_file.get_hi(DX),
            0b111 => self.register_file.get_hi(BX),
            _ => panic!(),
        }
    }

    fn reg_read16(&self, reg_idx: u8) -> u16 {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.get_ax(),
            0b001 => self.register_file.get(CX),
            0b010 => self.register_file.get(DX),
            0b011 => self.register_file.get(BX),
            0b100 => self.register_file.get(SP),
            0b101 => self.register_file.get(BP),
            0b110 => self.register_file.get(SI),
            0b111 => self.register_file.get(DI),
            _ => panic!(),
        }
    }

    fn reg_readw(&self, reg_idx: u8, w: Width) -> u16 {
        assert!(reg_idx < 0b1000);
        match w {
            W8 => self.reg_read8(reg_idx) as u16,
            W16 => self.reg_read16(reg_idx),
        }
    }

    fn reg_write8(&mut self, reg_idx: u8, v: u8) {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.set_lo(AX, v),
            0b001 => self.register_file.set_lo(CX, v),
            0b010 => self.register_file.set_lo(DX, v),
            0b011 => self.register_file.set_lo(BX, v),
            0b100 => self.register_file.set_hi(AX, v),
            0b101 => self.register_file.set_hi(CX, v),
            0b110 => self.register_file.set_hi(DX, v),
            0b111 => self.register_file.set_hi(BX, v),
            _ => panic!(),
        }
    }

    fn reg_write16(&mut self, reg_idx: u8, v: u16) {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.set(AX, v),
            0b001 => self.register_file.set(CX, v),
            0b010 => self.register_file.set(DX, v),
            0b011 => self.register_file.set(BX, v),
            0b100 => self.register_file.set(SP, v),
            0b101 => self.register_file.set(BP, v),
            0b110 => self.register_file.set(SI, v),
            0b111 => self.register_file.set(DI, v),
            _ => panic!(),
        }
    }

    fn reg_writew(&mut self, reg_idx: u8, v: u16, w: Width) {
        match w {
            W8 => self.reg_write8(reg_idx, v as u8),
            W16 => self.reg_write16(reg_idx, v),
        }
    }

    fn get_sreg_ovr(&self, sreg_def: SReg) -> SReg {
        self.sreg_ovr.unwrap_or(sreg_def)
    }

    fn read_sreg(&self, sreg: SReg) -> u16 {
        match sreg {
            SReg::ES => self.get_es(),
            SReg::CS => self.get_cs(),
            SReg::SS => self.get_ss(),
            SReg::DS => self.get_ds(),
        }
    }

    fn write_sreg(&mut self, sreg: SReg, v: u16) {
        match sreg {
            SReg::ES => self.set_es(v),
            SReg::CS => self.set_cs(v),
            SReg::SS => self.set_ss(v),
            SReg::DS => self.set_ds(v),
        };
    }

    fn read_sreg_ovr(&self, sreg_def: SReg) -> u16 {
        let sreg = self.get_sreg_ovr(sreg_def);
        self.read_sreg(sreg)
    }

    fn fetch8(&mut self, bus: &mut impl Bus) -> u8 {
        let csip = self.register_file.get_csip();
        let v = self.mem_read8(bus, csip.0, csip.1);
        self.set_ip(csip.1.wrapping_add(1));
        v
    }

    fn fetch16(&mut self, bus: &mut impl Bus) -> u16 {
        let csip = self.register_file.get_csip();
        let v = self.mem_read16(bus, csip.0, csip.1);
        self.set_ip(csip.1.wrapping_add(2));
        v
    }

    fn fetchw(&mut self, bus: &mut impl Bus, w: Width) -> u16 {
        match w {
            W8 => self.fetch8(bus) as u16,
            W16 => self.fetch16(bus),
        }
    }

    fn modrm_mem_sw(&mut self, bus: &mut impl Bus, modrm: u8, w: Width) -> ModRM {
        let r#mod = modrm >> 6;
        let rm = modrm & 0b111;

        if r#mod == 0b11 {
            ModRM {
                w,
                rm: RegMem::Reg(rm),
            }
        } else {
            let sreg = match rm {
                0b000 => SReg::DS,
                0b001 => SReg::DS,
                0b010 => SReg::SS,
                0b011 => SReg::SS,
                0b100 => SReg::DS,
                0b101 => SReg::DS,
                0b110 if r#mod == 0 => SReg::DS,
                0b110 => SReg::SS,
                0b111 => SReg::DS,
                _ => unreachable!(),
            };
            let mut ofs = match rm {
                0b000 => self.register_file.get(BX).wrapping_add(self.get_si()),
                0b001 => self.register_file.get(BX).wrapping_add(self.get_di()),
                0b010 => self.register_file.get(BP).wrapping_add(self.get_si()),
                0b011 => self.register_file.get(BP).wrapping_add(self.get_di()),
                0b100 => self.get_si(),
                0b101 => self.get_di(),
                0b110 if r#mod == 0 => self.fetch16(bus),
                0b110 => self.get_bp(),
                0b111 => self.get_bx(),
                _ => unreachable!(),
            };
            ofs = ofs.wrapping_add(match r#mod {
                0b01 => sext(self.fetch8(bus)),
                0b10 => self.fetch16(bus),
                _ => 0,
            });

            ModRM {
                w,
                rm: RegMem::Mem { sreg, ofs },
            }
        }
    }

    fn modrm_reg_sw(&self, modrm: u8, w: Width) -> ModRM {
        let reg = (modrm >> 3) & 0b111;

        ModRM {
            w,
            rm: RegMem::Reg(reg),
        }
    }

    fn read_modrm(&self, bus: &impl Bus, src: ModRM) -> u16 {
        match src.rm {
            RegMem::Reg(reg) => self.reg_readw(reg, src.w),
            RegMem::Mem { sreg, ofs } => {
                let seg = self.read_sreg_ovr(sreg);
                self.mem_readw(bus, seg, ofs, src.w)
            }
        }
    }

    fn write_modrm(&mut self, bus: &mut impl Bus, dst: ModRM, v: u16) {
        match dst.rm {
            RegMem::Reg(reg) => {
                self.reg_writew(reg, v, dst.w);
            }
            RegMem::Mem { sreg, ofs } => {
                self.mem_writew(bus, self.read_sreg_ovr(sreg), ofs, v, dst.w);
            }
        }
    }

    fn push(&mut self, bus: &mut impl Bus, v: u16) {
        let ss = self.get_ss();
        let sp = self.get_sp().wrapping_sub(2);

        self.set_sp(sp);
        self.mem_write16(bus, ss, sp, v);
    }

    fn pop(&mut self, bus: &mut impl Bus) -> u16 {
        let ss = self.get_ss();
        let sp = self.get_sp();
        let r = self.mem_read16(bus, ss, sp);
        self.set_sp(sp.wrapping_add(2));
        r
    }

    fn update_flags_add8(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf8_add(res, dst, src, cf));
        self.set_pf(pf8(res));
        self.set_af(af8(res, dst, src));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(of8_add(res, dst, src));
    }

    fn update_flags_sub8(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf8_sub(res, dst, src, cf));
        self.set_pf(pf8(res));
        self.set_af(af8(res, dst, src));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(of8_sub(res, dst, src));
    }

    fn update_flags_bin8(&mut self, res: u16, _dst: u16, _src: u16) {
        self.set_cf(false);
        self.set_pf(pf8(res));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(false);
    }

    fn update_flags_add16(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf16_add(res, dst, src, cf));
        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn update_flags_sub16(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf16_sub(res, dst, src, cf));
        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src));
    }

    fn update_flags_bin16(&mut self, res: u16, _dst: u16, _src: u16) {
        self.set_cf(false);
        self.set_pf(pf16(res));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(false);
    }

    fn alu_w(&mut self, func: AluOp, a: u16, b: u16, w: Width) -> u16 {
        let c = self.get_cf();
        let mut res = match func {
            AluOp::Add => a.wrapping_add(b),
            AluOp::Or => a | b,
            AluOp::Adc => a.wrapping_add(b).wrapping_add(c as u16),
            AluOp::Sbb => a.wrapping_sub(b).wrapping_sub(c as u16),
            AluOp::And => a & b,
            AluOp::Sub => a.wrapping_sub(b),
            AluOp::Xor => a ^ b,
            AluOp::Cmp => a.wrapping_sub(b),
        };

        if w == W8 {
            res &= 0x00ff;
        }

        match w {
            W8 => {
                match func {
                    AluOp::Add => self.update_flags_add8(res, a, b, false),
                    AluOp::Or => self.update_flags_bin8(res, a, b),
                    AluOp::Adc => self.update_flags_add8(res, a, b, c),
                    AluOp::Sbb => self.update_flags_sub8(res, a, b, c),
                    AluOp::And => self.update_flags_bin8(res, a, b),
                    AluOp::Sub => self.update_flags_sub8(res, a, b, false),
                    AluOp::Xor => self.update_flags_bin8(res, a, b),
                    AluOp::Cmp => self.update_flags_sub8(res, a, b, false),
                };
            }
            W16 => {
                match func {
                    AluOp::Add => self.update_flags_add16(res, a, b, false),
                    AluOp::Or => self.update_flags_bin16(res, a, b),
                    AluOp::Adc => self.update_flags_add16(res, a, b, c),
                    AluOp::Sbb => self.update_flags_sub16(res, a, b, c),
                    AluOp::And => self.update_flags_bin16(res, a, b),
                    AluOp::Sub => self.update_flags_sub16(res, a, b, false),
                    AluOp::Xor => self.update_flags_bin16(res, a, b),
                    AluOp::Cmp => self.update_flags_sub16(res, a, b, false),
                };
            }
        }

        res
    }

    fn op_alu_r_rm(&mut self, bus: &mut impl Bus, op: u8) {
        let modrm = self.fetch8(bus);
        let w = Width::from_op(op);
        let d = Dir::from_op(op);
        let func = AluOp::from((op >> 3) & 0b111);

        let mem = self.modrm_mem_sw(bus, modrm, w);
        let reg: ModRM = self.modrm_reg_sw(modrm, w);

        let mut a = self.read_modrm(bus, reg);
        let mut b = self.read_modrm(bus, mem);
        if matches!(d, Dir::RegToRM) {
            swap(&mut a, &mut b);
        }
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            match d {
                Dir::RegToRM => self.write_modrm(bus, mem, r),
                Dir::RMToReg => self.write_modrm(bus, reg, r),
            }
        }

        self.cycles += 3;
        if mem.is_mem() {
            match d {
                // If writing to mem
                Dir::RegToRM => {
                    if func != AluOp::Cmp {
                        self.cycles += 6
                    }
                }
                // If reading from mem
                Dir::RMToReg => self.cycles += 6,
            }
            self.cycles += 6;
        }
    }

    fn op_alu_a_imm(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let func = AluOp::from((op >> 3) & 0b111);

        let a = self.reg_readw_ax(w);
        let b = self.fetchw(bus, w);
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            self.reg_writew_ax(w, r);
        }

        self.cycles += 4;
    }

    fn op_push_sreg(&mut self, bus: &mut impl Bus, op: u8) {
        let sreg = (op >> 3) & 0b111;
        let v = self.read_sreg(SReg::from_u8(sreg));
        self.push(bus, v);

        self.cycles += 10;
    }

    fn op_pop_sreg(&mut self, bus: &mut impl Bus, op: u8) {
        let sreg = (op >> 3) & 0b111;
        let v = self.pop(bus);
        self.write_sreg(SReg::from_u8(sreg), v);

        self.int_delay = true;

        self.cycles += 8;
    }

    fn op_daa(&mut self) {
        let mut al = self.get_ax() as u8;
        let mut af = self.get_af();
        let mut cf = self.get_cf();
        let old_al = al;

        let al_check = match af {
            true => 0x9F,
            false => 0x99,
        };

        if (old_al & 0x0f) > 9 || af {
            al = al.wrapping_add(0x06);
            af = true;
        } else {
            af = false;
        }

        if (old_al > al_check) || cf {
            al = al.wrapping_add(0x60);
            cf = true;
        } else {
            cf = false;
        }

        self.register_file.set_lo(AX, al);
        self.set_cf(cf);
        self.set_pf(pf8(al as u16));
        self.set_af(af);
        self.set_zf(zf8(al as u16));
        self.set_sf(sf8(al as u16));
    }

    fn op_das(&mut self) {
        let old_al = self.get_ax() as u8;
        let old_af = self.get_af();
        let old_cf = self.get_cf();

        let mut new_al = old_al;
        let mut new_cf = false;
        let mut new_af = false;

        let al_check = match old_af {
            true => 0x9F,
            false => 0x99,
        };

        if (old_al & 0x0f) > 9 || old_af {
            new_al = new_al.wrapping_sub(0x06);
            new_af = true;
        }

        if (old_al > al_check) || old_cf {
            new_al = new_al.wrapping_sub(0x60);
            new_cf = true;
        }

        self.register_file.set_lo(AX, new_al);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_al as u16));
        self.set_af(new_af);
        self.set_zf(zf8(new_al as u16));
        self.set_sf(sf8(new_al as u16));

        self.cycles += 4;
    }

    fn op_aaa(&mut self) {
        let old_ax = self.get_ax();
        let old_af = self.get_af();

        let mut new_ax = old_ax;
        let mut new_cf = false;
        let mut new_af = false;

        if (old_ax & 0x0f) > 0x09 || old_af {
            let ah = read_hi(old_ax).wrapping_add(1) as u16;
            let al = read_lo(old_ax).wrapping_add(6) as u16;
            new_ax = (ah << 8) + al;
            new_af = true;
            new_cf = true;
        }
        new_ax &= 0xff0f;

        self.register_file.set(AX, new_ax);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_ax));
        self.set_af(new_af);
        self.set_zf(zf8(new_ax));
        self.set_sf(sf8(new_ax));

        self.cycles += 4;
    }

    fn op_aas(&mut self) {
        let old_ax = self.get_ax();
        let old_af = self.register_file.get_af();

        let mut new_ax = old_ax;
        let mut new_cf = false;
        let mut new_af = false;

        if (old_ax & 0x0f) > 0x09 || old_af {
            let al = read_lo(old_ax).wrapping_sub(6) as u16;
            let ah = read_hi(old_ax).wrapping_sub(1) as u16;
            new_ax = (ah << 8) + al;
            new_af = true;
            new_cf = true;
        }
        new_ax &= 0xff0f;

        self.register_file.set(AX, new_ax);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_ax));
        self.set_af(new_af);
        self.set_zf(zf8(new_ax));
        self.set_sf(sf8(new_ax));

        self.cycles += 4;
    }

    fn op_inc_reg(&mut self, op: u8) {
        let reg = op & 0b111;
        let dst = self.reg_read16(reg);
        let src = 1;
        let res = dst.wrapping_add(src);

        self.reg_write16(reg, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn op_dec_reg(&mut self, op: u8) {
        let reg = op & 0b111;
        let dst = self.reg_read16(reg);
        let src = 1;
        let res = dst.wrapping_sub(src);

        self.reg_write16(reg, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src))
    }

    fn op_push_reg(&mut self, bus: &mut impl Bus, op: u8) {
        let reg_idx = op & 0b111;

        // On 8086 'push ss' pushes the updated value of ss
        let ss = self.register_file.get(SS);
        let sp = self.register_file.get(SP).wrapping_sub(2);
        self.register_file.set(SP, sp);

        let v = self.reg_read16(reg_idx);

        self.mem_write16(bus, ss, sp, v);

        self.cycles += 11;
    }

    fn op_pop_reg(&mut self, bus: &mut impl Bus, op: u8) {
        let reg_idx = op & 0b111;
        let v = self.pop(bus);

        self.reg_write16(reg_idx, v);

        self.cycles += 8;
    }

    fn op_jcc(&mut self, bus: &mut impl Bus, op: u8) {
        let cond = (op >> 1) & 0b111;
        let neg = (op & 1) != 0;

        let inc = self.fetch8(bus) as i8;

        let mut r = match cond {
            0b000 => self.get_of(),
            0b001 => self.get_cf(),
            0b010 => self.get_zf(),
            0b011 => self.get_cf() | self.get_zf(),
            0b100 => self.get_sf(),
            0b101 => self.get_pf(),
            0b110 => self.get_sf() ^ self.get_of(),
            0b111 => self.get_sf() ^ self.get_of() | self.get_zf(),
            _ => unreachable!(),
        };

        if neg {
            r = !r;
        }

        if r {
            let ip = self.register_file.get(IP);
            let ip = ip.wrapping_add_signed(inc as i16);
            self.register_file.set(IP, ip);
        }

        self.cycles += 4;
        if r {
            self.cycles += 12;
        }
    }

    fn op_grp1_rmw_imm(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);
        let func = AluOp::from((modrm >> 3) & 0b111);
        let mem = self.modrm_mem_sw(bus, modrm, w);

        let imm = match op & 0b11 {
            0b00 => self.fetch8(bus) as u16,
            0b01 => self.fetch16(bus),
            _ => sext(self.fetch8(bus)),
        };

        let a = self.read_modrm(bus, mem);
        let b = imm;
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            self.write_modrm(bus, mem, r);
        }

        self.cycles += 4;
        // If reading from mem
        if mem.is_mem() {
            self.cycles += 6;
        }
        // If writing to mem
        if func != AluOp::Cmp {
            self.cycles += 7;
        }
    }

    fn op_test_rm_r(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);

        let mem = self.modrm_mem_sw(bus, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        let a = self.read_modrm(bus, mem);
        let b = self.read_modrm(bus, reg);

        self.alu_w(AluOp::And, a, b, w);
    }

    fn op_xchg_rm_r(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);

        let mem = self.modrm_mem_sw(bus, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        let a = self.read_modrm(bus, mem);
        let b = self.read_modrm(bus, reg);

        self.write_modrm(bus, reg, a);
        self.write_modrm(bus, mem, b);
    }

    fn op_mov_rm_r(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let d = Dir::from_op(op);
        let modrm = self.fetch8(bus);

        let mem = self.modrm_mem_sw(bus, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        match d {
            Dir::RegToRM => self.write_modrm(bus, mem, self.read_modrm(bus, reg)),
            Dir::RMToReg => self.write_modrm(bus, reg, self.read_modrm(bus, mem)),
        }

        self.cycles += 2;
        if mem.is_mem() {
            // If one argument is mem
            self.cycles += 6;
            if d == Dir::RegToRM {
                // If destination is mem
                self.cycles += 1;
            }
        }
    }

    fn op_mov_rm16_sreg(&mut self, bus: &mut impl Bus, op: u8) {
        let d = Dir::from_op(op);
        let modrm = self.fetch8(bus);
        let sreg_idx = (modrm >> 3) & 0b11;
        let sreg = SReg::from_u8(sreg_idx);

        match d {
            Dir::RegToRM => {
                let v = self.read_sreg(sreg);
                let dst = self.modrm_mem_sw(bus, modrm, W16);
                self.write_modrm(bus, dst, v);
            }
            Dir::RMToReg => {
                let src = self.modrm_mem_sw(bus, modrm, W16);
                let v = self.read_modrm(bus, src);
                self.write_sreg(sreg, v);
            }
        }
    }

    fn op_lea_r16_m16(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let src = self.modrm_mem_sw(bus, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);
        self.write_modrm(bus, dst, src.ofs());

        self.cycles += 2;
    }

    fn op_pop_rm16(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let dst = self.modrm_mem_sw(bus, modrm, W16);
        let v = self.pop(bus);
        self.write_modrm(bus, dst, v);

        self.cycles += 8;
        if dst.is_mem() {
            self.cycles += 11;
        }
    }

    fn op_xchg_ax_r(&mut self, op: u8) {
        let reg_idx = op & 0b111;
        let rv = self.reg_read16(reg_idx);
        let ax = self.reg_read16(0);

        self.reg_write16(0, rv);
        self.reg_write16(reg_idx, ax);
    }

    fn op_cbw(&mut self) {
        let ax = self.get_ax() as u8;
        self.set_ax(sext(ax));
    }

    fn op_cwd(&mut self) {
        let ax = self.get_ax();
        let dx = if ax & 0x8000 != 0 { 0xffff } else { 0x0000 };
        self.set_dx(dx);
    }

    fn op_call_far(&mut self, bus: &mut impl Bus) {
        let ofs = self.fetch16(bus);
        let seg = self.fetch16(bus);

        self.push(bus, self.get_cs());
        self.push(bus, self.get_ip());

        self.set_cs(seg);
        self.set_ip(ofs);

        self.cycles = 28;
    }

    fn op_wait(&self) {
        unimplemented!();
    }

    fn op_pushf(&mut self, bus: &mut impl Bus) {
        let flags = self.get_flags();
        self.push(bus, flags);
    }

    fn op_popf(&mut self, bus: &mut impl Bus) {
        let flags = self.pop(bus);

        self.set_flags(flags);
    }

    fn op_sahf(&mut self) {
        let ah = read_hi(self.get_ax()) as u16;
        self.set_sf((ah & FLAG_SF) != 0);
        self.set_zf((ah & FLAG_ZF) != 0);
        self.set_af((ah & FLAG_AF) != 0);
        self.set_pf((ah & FLAG_PF) != 0);
        self.set_cf((ah & FLAG_CF) != 0);
    }

    fn op_lahf(&mut self) {
        let flags = read_lo(self.get_flags()) as u16;
        let ax = (flags << 8) | (self.get_al() as u16);
        self.set_ax(ax);
    }

    fn op_mov_a_m(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);

        let seg = self.read_sreg_ovr(SReg::DS);
        let ofs = self.fetch16(bus);

        if op & 2 == 0 {
            let v = self.mem_readw(bus, seg, ofs, w);
            self.reg_writew_ax(w, v);
        } else {
            let v = self.reg_readw_ax(w);
            self.mem_writew(bus, seg, ofs, v, w);
        }

        self.cycles += 10;
    }

    #[inline]
    fn strop_delta(&mut self, w: Width) -> i16 {
        let delta = if !self.get_df() { 1 } else { -1 };

        match w {
            W8 => delta,
            W16 => 2 * delta,
        }
    }

    #[inline]
    fn op_strop(&mut self, bus: &mut impl Bus, strop: StrOp, op: u8) {
        let w = Width::from_op(op);
        let delta = self.strop_delta(w);
        let src_sreg = self.get_sreg_ovr(SReg::DS);

        let src_seg = self.read_sreg(src_sreg);
        let dst_seg = self.get_es();

        loop {
            if self.rep_mode != RepMode::None {
                let cx = self.get_cx();

                if cx == 0 {
                    break;
                }

                // TODO: Check interrupt pending

                self.set_cx(cx.wrapping_sub(1));
            }

            let dst_ofs = self.get_di();
            let src_ofs = self.get_si();

            match strop {
                StrOp::Movs => {
                    let v = self.mem_readw(bus, src_seg, src_ofs, w);
                    self.mem_writew(bus, dst_seg, dst_ofs, v, w);
                }
                StrOp::Cmps => {
                    let a = self.mem_readw(bus, dst_seg, dst_ofs, w);
                    let b = self.mem_readw(bus, src_seg, src_ofs, w);

                    self.alu_w(AluOp::Cmp, b, a, w);
                }
                StrOp::Scas => {
                    let a = self.reg_readw_ax(w);
                    let b = self.mem_readw(bus, dst_seg, dst_ofs, w);
                    self.alu_w(AluOp::Cmp, a, b, w);
                }
                StrOp::Lods => {
                    let v = self.mem_readw(bus, src_seg, src_ofs, w);
                    self.reg_writew_ax(w, v);
                }
                StrOp::Stos => {
                    let v = self.reg_readw_ax(w);
                    self.mem_writew(bus, dst_seg, dst_ofs, v, w);
                }
            }

            self.set_si(src_ofs.wrapping_add_signed(delta));
            self.set_di(dst_ofs.wrapping_add_signed(delta));

            if strop == StrOp::Cmps || strop == StrOp::Scas {
                if self.rep_mode == RepMode::Rep && !self.get_zf() {
                    break;
                }
                if self.rep_mode == RepMode::Repne && self.get_zf() {
                    break;
                }
            }
            if self.rep_mode == RepMode::None {
                break;
            }
        }
    }

    fn op_movs(&mut self, bus: &mut impl Bus, op: u8) {
        self.op_strop(bus, StrOp::Movs, op);
    }

    fn op_cmps(&mut self, bus: &mut impl Bus, op: u8) {
        self.op_strop(bus, StrOp::Cmps, op);
    }

    fn op_test_a_imm(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let imm = self.fetchw(bus, w);
        let a = self.reg_readw_ax(w);

        self.alu_w(AluOp::And, a, imm, w);
    }

    fn op_stos(&mut self, bus: &mut impl Bus, op: u8) {
        self.op_strop(bus, StrOp::Stos, op);
    }

    fn op_lods(&mut self, bus: &mut impl Bus, op: u8) {
        self.op_strop(bus, StrOp::Lods, op);
    }

    fn op_scas(&mut self, bus: &mut impl Bus, op: u8) {
        self.op_strop(bus, StrOp::Scas, op);
    }

    fn op_mov_reg_imm(&mut self, bus: &mut impl Bus, op: u8) {
        let reg_idx = op & 0b111;
        let w = if (op & 0b1000) == 0 { W8 } else { W16 };
        let imm = self.fetchw(bus, w);
        self.reg_writew(reg_idx, imm, w);
    }

    fn op_ret_imm16_intraseg(&mut self, bus: &mut impl Bus) {
        let imm = self.fetch16(bus);

        let ip = self.pop(bus);
        self.set_ip(ip);

        let sp = self.get_sp().wrapping_add(imm);
        self.set_sp(sp);
    }

    fn op_ret_intraseg(&mut self, bus: &mut impl Bus) {
        let ip = self.pop(bus);
        self.set_ip(ip);
    }

    fn op_les_r16_m16(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let src = self.modrm_mem_sw(bus, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);

        assert!(src.is_mem());

        let ofs = self.read_modrm(bus, src);
        let seg = self.read_modrm(bus, src.wrapping_add(2));

        self.write_modrm(bus, dst, ofs);
        self.set_es(seg);

        self.cycles += 16;
    }

    fn op_lds_r16_m16(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let src = self.modrm_mem_sw(bus, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);

        assert!(src.is_mem());

        let ofs = self.read_modrm(bus, src);
        let seg = self.read_modrm(bus, src.wrapping_add(2));

        self.write_modrm(bus, dst, ofs);
        self.set_ds(seg);

        self.cycles += 16;
    }

    fn op_mov_m_imm(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);
        let dst = self.modrm_mem_sw(bus, modrm, w);
        let imm = self.fetchw(bus, w);

        self.write_modrm(bus, dst, imm);

        self.cycles += 10;
    }

    fn op_ret_imm16_interseg(&mut self, bus: &mut impl Bus) {
        let ip = self.pop(bus);
        let cs = self.pop(bus);
        let sp_delta = self.fetch16(bus);
        let sp = self.get_sp();

        self.set_cs(cs);
        self.set_ip(ip);
        self.set_sp(sp.wrapping_add(sp_delta));
    }

    fn op_ret_interseg(&mut self, bus: &mut impl Bus) {
        let ip = self.pop(bus);
        let cs = self.pop(bus);

        self.set_cs(cs);
        self.set_ip(ip);
    }

    fn call_int(&mut self, bus: &mut impl Bus, num: u8) {
        let num = num as u16;
        let int_ip = self.mem_read16(bus, 0, 4 * num);
        let int_cs = self.mem_read16(bus, 0, 4 * num + 2);

        let flags = self.get_flags();
        let cs = self.get_cs();
        let ip = self.get_ip();

        self.push(bus, flags);
        self.push(bus, cs);
        self.push(bus, ip);

        self.set_if(false);
        self.set_tf(false);

        self.set_cs(int_cs);
        self.set_ip(int_ip);
    }

    fn op_int_3(&mut self, bus: &mut impl Bus) {
        self.call_int(bus, 3);

        self.cycles += 52;
    }

    fn op_int_imm8(&mut self, bus: &mut impl Bus) {
        let imm = self.fetch8(bus);
        self.call_int(bus, imm);

        self.cycles += 51;
    }

    fn op_into(&mut self, bus: &mut impl Bus) {
        self.cycles += 4;
        if self.get_of() {
            self.cycles += 49;
            self.call_int(bus, 4);
        }
    }

    fn op_iret(&mut self, bus: &mut impl Bus) {
        let ip = self.pop(bus);
        let cs = self.pop(bus);
        let flags = self.pop(bus);

        self.int_delay = true;
        self.set_ip(ip);
        self.set_cs(cs);
        self.set_flags(flags);
    }

    fn op_grp2_rmw(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);
        let func = (modrm >> 3) & 0b111;
        let dst = self.modrm_mem_sw(bus, modrm, w);

        let count = if op & 2 == 0 {
            1
        } else {
            read_lo(self.get_cx())
        };

        if count == 0 {
            return;
        }

        let src = self.read_modrm(bus, dst);
        let mut cf = self.get_cf();

        match w {
            W8 => {
                let res;
                let src = src as u8;
                match func {
                    0b000 => (res, cf) = src.rol(count, cf),
                    0b001 => (res, cf) = src.ror(count, cf),
                    0b010 => (res, cf) = src.rcl(count, cf),
                    0b011 => (res, cf) = src.rcr(count, cf),
                    0b100 => (res, cf) = src.shl(count, cf),
                    0b101 => (res, cf) = src.shr(count, cf),
                    0b110 => (res, cf) = src.setmo(count, cf),
                    0b111 => (res, cf) = src.sar(count, cf),
                    _ => unreachable!(),
                }

                let of = if func == 0b110 {
                    false
                } else {
                    res.msb() != src.msb()
                };
                let res = res as u16;

                self.write_modrm(bus, dst, res);
                self.set_of(of);
                self.set_cf(cf);
                if func >= 0b100 {
                    self.set_zf(zf8(res));
                    self.set_sf(sf8(res));
                    self.set_pf(pf8(res));
                }
            }
            W16 => {
                let res;
                match func {
                    0b000 => (res, cf) = src.rol(count, cf),
                    0b001 => (res, cf) = src.ror(count, cf),
                    0b010 => (res, cf) = src.rcl(count, cf),
                    0b011 => (res, cf) = src.rcr(count, cf),
                    0b100 => (res, cf) = src.shl(count, cf),
                    0b101 => (res, cf) = src.shr(count, cf),
                    0b110 => (res, cf) = src.setmo(count, cf),
                    0b111 => (res, cf) = src.sar(count, cf),
                    _ => unreachable!(),
                }

                let of = if func == 0b110 {
                    false
                } else {
                    res.msb() != src.msb()
                };

                self.write_modrm(bus, dst, res);
                self.set_of(of);
                self.set_cf(cf);
                if func >= 0b100 {
                    self.set_zf(zf16(res));
                    self.set_sf(sf16(res));
                    self.set_pf(pf16(res));
                }
            }
        }
    }

    fn op_aam(&mut self, bus: &mut impl Bus) {
        let imm = self.fetch8(bus);

        if imm == 0 {
            self.call_int(bus, 0);
            self.set_pf(pf8(0));
            self.set_zf(zf8(0));
            self.set_sf(sf8(0));
            return;
        }

        let tmp_al = self.get_al();
        let ah = (tmp_al / imm) as u16;
        let al = (tmp_al % imm) as u16;

        self.set_ax((ah << 8) + al);
        self.set_pf(pf8(al));
        self.set_zf(zf8(al));
        self.set_sf(sf8(al));

        self.cycles += 83;
    }

    fn op_aad(&mut self, bus: &mut impl Bus) {
        let imm = self.fetch8(bus);

        let al = self.get_al();
        let ah = self.get_ah();
        let res = ah.wrapping_mul(imm).wrapping_add(al) as u16;

        self.set_ax(res);
        self.set_af(false);
        self.set_cf(false);
        self.set_of(false);
        self.set_pf(pf8(res));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
    }

    fn op_salc(&mut self) {
        if self.get_cf() {
            self.set_al(0xff);
        } else {
            self.set_al(0);
        }
    }

    fn op_xlat(&mut self, bus: &mut impl Bus) {
        let seg = self.read_sreg_ovr(SReg::DS);
        let ofs = self.get_bx().wrapping_add(self.get_al() as u16);
        let al = self.mem_read8(bus, seg, ofs);
        self.set_al(al);
    }

    fn op_esc(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        _ = self.read_modrm(bus, mem);
    }

    fn op_loopnz(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch8(bus) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 && !self.get_zf() {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_loopz(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch8(bus) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 && self.get_zf() {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_loop(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch8(bus) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_jcxz(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch8(bus) as i8;
        let cx = self.get_cx();

        if cx == 0 {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_in_al_imm8(&mut self, bus: &mut impl Bus) {
        let port = self.fetch8(bus) as u16;
        let v = bus.io_read_u8(port);
        self.set_al(v);
    }

    fn op_in_ax_imm8(&mut self, bus: &mut impl Bus) {
        let port = self.fetch8(bus) as u16;
        let v = bus.io_read_u16(port);
        self.set_ax(v);
    }

    fn op_out_al_imm8(&mut self, bus: &mut impl Bus) {
        let port = self.fetch8(bus) as u16;
        let v = self.get_al();
        bus.io_write_u8(port, v);
    }

    fn op_out_ax_imm8(&mut self, bus: &mut impl Bus) {
        let port = self.fetch8(bus) as u16;
        let v = self.get_ax();
        bus.io_write_u16(port, v);
    }

    fn op_call_near(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch16(bus);
        let ip = self.get_ip();
        self.push(bus, ip);

        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_jmp_near(&mut self, bus: &mut impl Bus) {
        let inc = self.fetch16(bus);
        let ip = self.get_ip();
        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_jmp_far(&mut self, bus: &mut impl Bus) {
        let ofs = self.fetch16(bus);
        let seg = self.fetch16(bus);

        self.set_cs(seg);
        self.set_ip(ofs);
    }

    fn op_jmp_short(&mut self, bus: &mut impl Bus) {
        let inc = sext(self.fetch8(bus));
        let ip = self.get_ip();
        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_in_al_dx(&mut self, bus: &mut impl Bus) {
        let port = self.get_dx();
        let v = bus.io_read_u8(port);
        self.set_al(v);
    }

    fn op_in_ax_dx(&mut self, bus: &mut impl Bus) {
        let port = self.get_dx();
        let v = bus.io_read_u16(port);
        self.set_ax(v);
    }

    fn op_out_al_dx(&mut self, bus: &mut impl Bus) {
        let port = self.get_dx();
        let v = self.get_al();
        bus.io_write_u8(port, v);
    }

    fn op_out_ax_dx(&mut self, bus: &mut impl Bus) {
        let port = self.get_dx();
        let v = self.get_ax();
        bus.io_write_u16(port, v);
    }

    fn op_lock_prefix(&self) {
        unimplemented!();
    }

    fn op_unused(&self) {
        unimplemented!();
    }

    fn op_repne(&mut self) {
        self.rep_mode = RepMode::Repne;
        self.int_delay = true;
    }

    fn op_rep(&mut self) {
        self.rep_mode = RepMode::Rep;
        self.int_delay = true;
    }

    fn op_hlt(&self) {
        unimplemented!();
    }

    fn op_cmc(&mut self) {
        let cf = self.get_cf();
        self.set_cf(!cf);
    }

    fn op_grp3_rmw(&mut self, bus: &mut impl Bus, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(bus);
        let func = (modrm >> 3) & 0b111;
        let mem = self.modrm_mem_sw(bus, modrm, w);

        match func {
            0b000 | 0b001 => {
                // test
                let a = self.read_modrm(bus, mem);
                let b = self.fetchw(bus, w);
                self.alu_w(AluOp::And, a, b, w);
            }
            0b010 => {
                // not
                let v = self.read_modrm(bus, mem);
                self.write_modrm(bus, mem, !v);
            }
            0b011 => {
                // neg
                let v = self.read_modrm(bus, mem);
                let res = self.alu_w(AluOp::Sub, 0, v, w);
                self.write_modrm(bus, mem, res);
            }
            0b100 => {
                // mul
                let b = self.read_modrm(bus, mem);
                match w {
                    W8 => {
                        let ax = (self.get_al() as u16) * b;
                        self.set_ax(ax);
                        let hi_set = read_hi(ax) != 0;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                    W16 => {
                        let dxax = (self.get_ax() as u32) * (b as u32);
                        let dx = (dxax >> 16) as u16;
                        let ax = dxax as u16;
                        self.set_dx(dx);
                        self.set_ax(ax);
                        let hi_set = dx != 0;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                }
            }
            0b101 => {
                // imul
                let b = self.read_modrm(bus, mem);
                match w {
                    W8 => {
                        let a = (self.get_al() as i8) as i16;
                        let b = ((b as u8) as i8) as i16;
                        let xp = a.wrapping_mul(b);
                        let ax = xp as u16;

                        self.set_ax(ax);

                        let hi_set = sext(read_lo(ax)) != ax;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                    W16 => {
                        let a = (self.get_ax() as i16) as i32;
                        let b = b as i16;
                        let dxax = a.wrapping_mul(b as i32) as u32;

                        let dx = (dxax >> 16) as u16;
                        let ax = dxax as u16;
                        self.set_dx(dx);
                        self.set_ax(ax);

                        let hi_set = sext16(ax) != dxax;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                }
            }
            0b110 => {
                // div
                let src = self.read_modrm(bus, mem);
                match w {
                    W8 => {
                        let ax = self.get_ax();
                        let ah = read_hi(ax);

                        // Check for overflow or divide-by-zero
                        if ah as u16 >= src {
                            self.set_sf(sf8(ah as u16));
                            self.set_zf(zf8(ah as u16));
                            self.set_pf(pf8(ah as u16));
                            self.set_cf(false);
                            self.set_af(false);
                            self.set_of(false);
                            self.call_int(bus, 0);
                            return;
                        }

                        let tmp = ax / src;
                        self.set_al(tmp as u8);
                        self.set_ah((ax % src) as u8);
                    }
                    W16 => {
                        let dx = self.get_dx();
                        let ax = self.get_ax();
                        let dxax = ((dx as u32) << 16) + (ax as u32);

                        // Check for overflow or divide-by-zero
                        if dx >= src {
                            self.set_sf(sf8(ax));
                            self.set_zf(zf8(ax));
                            self.set_pf(pf8(ax));
                            self.set_cf(false);
                            self.set_af(false);
                            self.set_of(false);
                            self.call_int(bus, 0);
                            return;
                        }

                        let tmp = dxax / (src as u32);
                        self.set_ax(tmp as u16);
                        self.set_dx((dxax % (src as u32)) as u16);
                    }
                }
            }
            0b111 => {
                // idiv
                match w {
                    W8 => {
                        let tmp_ac = self.get_ax() as i16;
                        let tmp_b = self.read_modrm(bus, mem) as i8;

                        if tmp_b == 0 {
                            self.call_int(bus, 0);
                            return;
                        }

                        let mut quot = tmp_ac / (tmp_b as i16);
                        let rem = tmp_ac % (tmp_b as i16);

                        if self.rep_mode != RepMode::None {
                            quot *= -1;
                        }

                        if quot <= (i8::MIN as i16) || quot > (i8::MAX as i16) {
                            self.call_int(bus, 0);
                            return;
                        }

                        self.set_al(quot as u8);
                        self.set_ah(rem as u8);
                    }
                    W16 => {
                        let dx = self.get_dx();
                        let ax = self.get_ax();
                        let dxax = ((dx as u32) << 16) + (ax as u32);

                        let tmp_ac = dxax as i32;
                        let tmp_b = self.read_modrm(bus, mem) as i16 as i32;

                        if tmp_b == 0 {
                            self.call_int(bus, 0);
                            return;
                        }

                        let mut quot = tmp_ac / tmp_b;
                        let rem = tmp_ac % tmp_b;

                        if self.rep_mode != RepMode::None {
                            quot *= -1;
                        }

                        if quot <= (i16::MIN as i32) || quot > (i16::MAX as i32) {
                            self.call_int(bus, 0);
                            return;
                        }

                        self.set_ax(quot as u16);
                        self.set_dx(rem as u16);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn op_clc(&mut self) {
        self.set_cf(false);
        self.cycles += 2;
    }

    fn op_stc(&mut self) {
        self.set_cf(true);
        self.cycles += 2;
    }

    fn op_cli(&mut self) {
        self.set_if(false);
        self.cycles += 2;
    }

    fn op_sti(&mut self) {
        self.set_if(true);
        self.cycles += 2;
    }

    fn op_cld(&mut self) {
        self.set_df(false);
        self.cycles += 2;
    }

    fn op_std(&mut self) {
        self.set_df(true);
        self.cycles += 2;
    }

    fn op_grp4_rm8(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let subop = (modrm >> 3) & 0b111;
        match subop {
            0b000 => {
                let mem = self.modrm_mem_sw(bus, modrm, W8);
                let dst = self.read_modrm(bus, mem);
                let src = 1;
                let res = dst.wrapping_add(src);

                self.write_modrm(bus, mem, res);
                self.set_pf(pf8(res));
                self.set_af(af8(res, dst, src));
                self.set_zf(zf8(res));
                self.set_sf(sf8(res));
                self.set_of(of8_add(res, dst, src));

                self.cycles += 3;
                if mem.is_mem() {
                    self.cycles += 12;
                }
            }
            0b001 => {
                let mem = self.modrm_mem_sw(bus, modrm, W8);
                let dst = self.read_modrm(bus, mem);
                let src = 1;
                let res = dst.wrapping_sub(src);

                self.write_modrm(bus, mem, res);
                self.set_pf(pf8(res));
                self.set_af(af8(res, dst, src));
                self.set_zf(zf8(res));
                self.set_sf(sf8(res));
                self.set_of(of8_sub(res, dst, src));

                self.cycles += 3;
                if mem.is_mem() {
                    self.cycles += 12;
                }
            }
            _ => unreachable!(),
        }
    }

    fn op_grp5(&mut self, bus: &mut impl Bus) {
        let modrm = self.fetch8(bus);
        let subop = (modrm >> 3) & 0b111;
        match subop {
            0b000 => self.op_grp5_inc_rm16(bus, modrm),
            0b001 => self.op_grp5_dec_rm16(bus, modrm),
            0b010 => self.op_grp5_call_rm16(bus, modrm),
            0b011 => self.op_grp5_call_far(bus, modrm),
            0b100 => self.op_grp5_jmp_rm16(bus, modrm),
            0b101 => self.op_grp5_jmp_far(bus, modrm),
            0b110 | 0b111 => self.op_grp5_push_rm16(bus, modrm),
            _ => unreachable!(),
        }
    }

    fn op_grp5_inc_rm16(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let dst = self.read_modrm(bus, mem);
        let src = 1;
        let res = dst.wrapping_add(src);

        self.write_modrm(bus, mem, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn op_grp5_dec_rm16(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let dst = self.read_modrm(bus, mem);
        let src = 1;
        let res = dst.wrapping_sub(src);

        self.write_modrm(bus, mem, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src));
    }

    fn op_grp5_call_rm16(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let ofs = self.read_modrm(bus, mem);

        self.push(bus, self.get_ip());
        self.set_ip(ofs);
        self.cycles += 16;
        if mem.is_mem() {
            self.cycles += 5;
        }
    }

    fn op_grp5_call_far(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let ofs = self.read_modrm(bus, mem);
        let seg = self.read_modrm(bus, mem.wrapping_add(2));

        self.push(bus, self.get_cs());
        self.push(bus, self.get_ip());

        self.set_cs(seg);
        self.set_ip(ofs);

        self.cycles += 37;
    }

    fn op_grp5_jmp_rm16(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let ofs = self.read_modrm(bus, mem);

        self.set_ip(ofs);
    }

    fn op_grp5_jmp_far(&mut self, bus: &mut impl Bus, modrm: u8) {
        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let ofs = self.read_modrm(bus, mem);
        let seg = self.read_modrm(bus, mem.wrapping_add(2));

        self.set_cs(seg);
        self.set_ip(ofs);
    }

    fn op_grp5_push_rm16(&mut self, bus: &mut impl Bus, modrm: u8) {
        let ss = self.register_file.get(SS);
        let sp = self.register_file.get(SP).wrapping_sub(2);
        self.register_file.set(SP, sp);

        let mem = self.modrm_mem_sw(bus, modrm, W16);
        let v = self.read_modrm(bus, mem);

        self.mem_write16(bus, ss, sp, v);

        self.cycles += 11;
        if mem.is_mem() {
            self.cycles += 7;
        }
    }

    pub fn step(&mut self, bus: &mut impl Bus) {
        let mut op: u8;

        loop {
            op = self.fetch8(bus);

            match op {
                0x26 => self.sreg_ovr = Some(SReg::ES),
                0x2e => self.sreg_ovr = Some(SReg::CS),
                0x36 => self.sreg_ovr = Some(SReg::SS),
                0x3e => self.sreg_ovr = Some(SReg::DS),

                0xf2 => self.rep_mode = RepMode::Repne,
                0xf3 => self.rep_mode = RepMode::Rep,
                _ => break,
            }
        }

        match op {
            0x00..=0x03 => self.op_alu_r_rm(bus, op),
            0x04..=0x05 => self.op_alu_a_imm(bus, op),
            0x06..=0x06 => self.op_push_sreg(bus, op),
            0x07..=0x07 => self.op_pop_sreg(bus, op),
            0x08..=0x0b => self.op_alu_r_rm(bus, op),
            0x0c..=0x0d => self.op_alu_a_imm(bus, op),
            0x0e..=0x0e => self.op_push_sreg(bus, op),
            0x0f..=0x0f => self.op_pop_sreg(bus, op),
            0x10..=0x13 => self.op_alu_r_rm(bus, op),
            0x14..=0x15 => self.op_alu_a_imm(bus, op),
            0x16..=0x16 => self.op_push_sreg(bus, op),
            0x17..=0x17 => self.op_pop_sreg(bus, op),
            0x18..=0x1b => self.op_alu_r_rm(bus, op),
            0x1c..=0x1d => self.op_alu_a_imm(bus, op),
            0x1e..=0x1e => self.op_push_sreg(bus, op),
            0x1f..=0x1f => self.op_pop_sreg(bus, op),
            0x20..=0x23 => self.op_alu_r_rm(bus, op),
            0x24..=0x25 => self.op_alu_a_imm(bus, op),
            0x26..=0x26 => unreachable!(),
            0x27..=0x27 => self.op_daa(),
            0x28..=0x2b => self.op_alu_r_rm(bus, op),
            0x2c..=0x2d => self.op_alu_a_imm(bus, op),
            0x2e..=0x2e => unreachable!(),
            0x2f..=0x2f => self.op_das(),
            0x30..=0x33 => self.op_alu_r_rm(bus, op),
            0x34..=0x35 => self.op_alu_a_imm(bus, op),
            0x36..=0x36 => unreachable!(),
            0x37..=0x37 => self.op_aaa(),
            0x38..=0x3b => self.op_alu_r_rm(bus, op),
            0x3c..=0x3d => self.op_alu_a_imm(bus, op),
            0x3e..=0x3e => unreachable!(),
            0x3f..=0x3f => self.op_aas(),
            0x40..=0x47 => self.op_inc_reg(op),
            0x48..=0x4f => self.op_dec_reg(op),
            0x50..=0x57 => self.op_push_reg(bus, op),
            0x58..=0x5f => self.op_pop_reg(bus, op),
            0x60..=0x7f => self.op_jcc(bus, op),
            0x80..=0x83 => self.op_grp1_rmw_imm(bus, op),
            0x84..=0x85 => self.op_test_rm_r(bus, op),
            0x86..=0x87 => self.op_xchg_rm_r(bus, op),
            0x88..=0x8b => self.op_mov_rm_r(bus, op),
            0x8c => self.op_mov_rm16_sreg(bus, op),
            0x8d => self.op_lea_r16_m16(bus),
            0x8e => self.op_mov_rm16_sreg(bus, op),
            0x8f => self.op_pop_rm16(bus),
            0x90..=0x97 => self.op_xchg_ax_r(op),
            0x98 => self.op_cbw(),
            0x99 => self.op_cwd(),
            0x9a => self.op_call_far(bus),
            0x9b => self.op_wait(),
            0x9c => self.op_pushf(bus),
            0x9d => self.op_popf(bus),
            0x9e => self.op_sahf(),
            0x9f => self.op_lahf(),
            0xa0..=0xa3 => self.op_mov_a_m(bus, op),
            0xa4..=0xa5 => self.op_movs(bus, op),
            0xa6..=0xa7 => self.op_cmps(bus, op),
            0xa8..=0xa9 => self.op_test_a_imm(bus, op),
            0xaa..=0xab => self.op_stos(bus, op),
            0xac..=0xad => self.op_lods(bus, op),
            0xae..=0xaf => self.op_scas(bus, op),
            0xb0..=0xbf => self.op_mov_reg_imm(bus, op),
            0xc0 => self.op_ret_imm16_intraseg(bus),
            0xc1 => self.op_ret_intraseg(bus),
            0xc2 => self.op_ret_imm16_intraseg(bus),
            0xc3 => self.op_ret_intraseg(bus),
            0xc4 => self.op_les_r16_m16(bus),
            0xc5 => self.op_lds_r16_m16(bus),
            0xc6..=0xc7 => self.op_mov_m_imm(bus, op),
            0xc8 => self.op_ret_imm16_interseg(bus),
            0xc9 => self.op_ret_interseg(bus),
            0xca => self.op_ret_imm16_interseg(bus),
            0xcb => self.op_ret_interseg(bus),
            0xcc => self.op_int_3(bus),
            0xcd => self.op_int_imm8(bus),
            0xce => self.op_into(bus),
            0xcf => self.op_iret(bus),
            0xd0..=0xd3 => self.op_grp2_rmw(bus, op),
            0xd4 => self.op_aam(bus),
            0xd5 => self.op_aad(bus),
            0xd6 => self.op_salc(),
            0xd7 => self.op_xlat(bus),
            0xd8 => self.op_esc(bus),
            0xd9 => self.op_esc(bus),
            0xda => self.op_esc(bus),
            0xdb => self.op_esc(bus),
            0xdc => self.op_esc(bus),
            0xdd => self.op_esc(bus),
            0xde => self.op_esc(bus),
            0xdf => self.op_esc(bus),
            0xe0 => self.op_loopnz(bus),
            0xe1 => self.op_loopz(bus),
            0xe2 => self.op_loop(bus),
            0xe3 => self.op_jcxz(bus),
            0xe4 => self.op_in_al_imm8(bus),
            0xe5 => self.op_in_ax_imm8(bus),
            0xe6 => self.op_out_al_imm8(bus),
            0xe7 => self.op_out_ax_imm8(bus),
            0xe8 => self.op_call_near(bus),
            0xe9 => self.op_jmp_near(bus),
            0xea => self.op_jmp_far(bus),
            0xeb => self.op_jmp_short(bus),
            0xec => self.op_in_al_dx(bus),
            0xed => self.op_in_ax_dx(bus),
            0xee => self.op_out_al_dx(bus),
            0xef => self.op_out_ax_dx(bus),
            0xf0 => self.op_lock_prefix(),
            0xf1 => self.op_unused(),
            0xf2 => self.op_repne(),
            0xf3 => self.op_rep(),
            0xf4 => self.op_hlt(),
            0xf5 => self.op_cmc(),
            0xf6..=0xf7 => self.op_grp3_rmw(bus, op),
            0xf8 => self.op_clc(),
            0xf9 => self.op_stc(),
            0xfa => self.op_cli(),
            0xfb => self.op_sti(),
            0xfc => self.op_cld(),
            0xfd => self.op_std(),
            0xfe => self.op_grp4_rm8(bus),
            0xff => self.op_grp5(bus),
        };
    }
}

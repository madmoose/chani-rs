use super::{
    Register, FLAG_AF, FLAG_CF, FLAG_DF, FLAG_IF, FLAG_OF, FLAG_PF, FLAG_SF, FLAG_TF, FLAG_ZF,
};

#[derive(Debug, Default)]
pub struct RegisterFile {
    reg: [u16; 13],
    flags: u16,
}

fn reg_index(reg: Register) -> usize {
    match reg {
        Register::AX => 0,
        Register::CX => 1,
        Register::DX => 2,
        Register::BX => 3,
        Register::SP => 4,
        Register::BP => 5,
        Register::SI => 6,
        Register::DI => 7,
        Register::ES => 8,
        Register::CS => 9,
        Register::SS => 10,
        Register::DS => 11,
        Register::IP => 12,
    }
}

impl RegisterFile {
    #[inline]
    pub fn set(&mut self, reg: Register, v: u16) {
        let idx = reg_index(reg);
        self.reg[idx] = v;
    }

    #[inline]
    pub fn set_lo(&mut self, reg: Register, v: u8) {
        let idx = reg_index(reg);
        self.reg[idx] = (self.reg[idx] & 0xff00) | (v as u16);
    }

    #[inline]
    pub fn set_hi(&mut self, reg: Register, v: u8) {
        let idx = reg_index(reg);
        self.reg[idx] = (self.reg[idx] & 0x00ff) | ((v as u16) << 8);
    }

    #[inline]
    pub fn get(&self, reg: Register) -> u16 {
        let idx = reg_index(reg);
        self.reg[idx]
    }

    #[inline]
    pub fn get_lo(&self, reg: Register) -> u8 {
        let idx = reg_index(reg);
        self.reg[idx] as u8
    }

    #[inline]
    pub fn get_hi(&self, reg: Register) -> u8 {
        let idx = reg_index(reg);
        (self.reg[idx] >> 8) as u8
    }

    #[inline]
    pub fn get_csip(&self) -> (u16, u16) {
        let cs = self.get(Register::CS);
        let ip = self.get(Register::IP);
        (cs, ip)
    }

    #[inline]
    pub fn get_flags(&self) -> u16 {
        self.flags
    }

    #[inline]
    pub fn set_flags(&mut self, v: u16) {
        self.flags = v;
    }

    #[inline]
    pub fn get_cf(&self) -> bool {
        self.flags & FLAG_CF != 0
    }

    #[inline]
    pub fn get_pf(&self) -> bool {
        self.flags & FLAG_PF != 0
    }

    #[inline]
    pub fn get_af(&self) -> bool {
        self.flags & FLAG_AF != 0
    }

    #[inline]
    pub fn get_zf(&self) -> bool {
        self.flags & FLAG_ZF != 0
    }

    #[inline]
    pub fn get_sf(&self) -> bool {
        self.flags & FLAG_SF != 0
    }

    #[inline]
    pub fn get_tf(&self) -> bool {
        self.flags & FLAG_TF != 0
    }

    #[inline]
    pub fn get_if(&self) -> bool {
        self.flags & FLAG_IF != 0
    }

    #[inline]
    pub fn get_df(&self) -> bool {
        self.flags & FLAG_DF != 0
    }

    #[inline]
    pub fn get_of(&self) -> bool {
        self.flags & FLAG_OF != 0
    }

    fn set_flags_cond(&mut self, mask: u16, cond: bool) {
        self.flags = if cond {
            self.flags | mask
        } else {
            self.flags & !mask
        };
    }

    #[inline]
    pub fn set_cf(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_CF, cond);
    }

    #[inline]
    pub fn set_pf(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_PF, cond);
    }

    #[inline]
    pub fn set_af(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_AF, cond);
    }

    #[inline]
    pub fn set_zf(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_ZF, cond);
    }

    #[inline]
    pub fn set_sf(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_SF, cond);
    }

    #[inline]
    pub fn set_tf(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_TF, cond);
    }

    #[inline]
    pub fn set_if(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_IF, cond);
    }

    #[inline]
    pub fn set_df(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_DF, cond);
    }

    #[inline]
    pub fn set_of(&mut self, cond: bool) {
        self.set_flags_cond(FLAG_OF, cond);
    }
}

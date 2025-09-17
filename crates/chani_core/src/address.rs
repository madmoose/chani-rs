use std::{
    fmt,
    ops::{Add, AddAssign},
};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Address {
    pub seg: u16,
    pub ofs: u16,
}

pub const fn addr(seg: u16, ofs: u16) -> Address {
    Address { seg, ofs }
}

impl Address {
    pub fn ea(&self) -> u32 {
        (((self.seg as u32) << 4) + (self.ofs as u32)) & 0xfffff
    }
}

impl PartialEq<(u16, u16)> for Address {
    fn eq(&self, other: &(u16, u16)) -> bool {
        self.seg == other.0 && self.ofs == other.1
    }
}

impl Add<u16> for Address {
    type Output = Address;

    fn add(self, rel_off: u16) -> Address {
        Address {
            seg: self.seg,
            ofs: self.ofs.wrapping_add(rel_off),
        }
    }
}

impl AddAssign<u16> for Address {
    fn add_assign(&mut self, rel_off: u16) {
        self.ofs = self.ofs.wrapping_add(rel_off);
    }
}

impl From<u32> for Address {
    fn from(value: u32) -> Self {
        let ofs = value & 0xffff;
        let seg = (value - ofs) >> 4;
        let address = Self {
            seg: seg as u16,
            ofs: ofs as u16,
        };

        assert!(value == address.ea());

        address
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:04X}:{:04X}", self.seg, self.ofs)
    }
}

pub trait IntoEffectiveAddress: Copy + Clone {
    fn into_ea(self) -> u32;
    fn into_seg_ofs(self) -> Option<(u16, u16)>;
}

impl IntoEffectiveAddress for u32 {
    fn into_ea(self) -> u32 {
        self
    }

    fn into_seg_ofs(self) -> Option<(u16, u16)> {
        None
    }
}

impl IntoEffectiveAddress for Address {
    fn into_ea(self) -> u32 {
        self.ea()
    }

    fn into_seg_ofs(self) -> Option<(u16, u16)> {
        Some((self.seg, self.ofs))
    }
}

impl IntoEffectiveAddress for (u16, u16) {
    fn into_ea(self) -> u32 {
        ((self.0 as u32) << 4) + (self.1 as u32)
    }

    fn into_seg_ofs(self) -> Option<(u16, u16)> {
        Some((self.0, self.1))
    }
}

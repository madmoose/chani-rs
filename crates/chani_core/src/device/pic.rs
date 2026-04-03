use std::any::Any;

use crate::{device::Device, time::Attoseconds};

pub struct Pic {
    irr: Bitset, // Interrupt Request Register
    isr: Bitset, // In-Service Register
    imr: Bitset, // Interrupt Mask Register

    // Operation Control Words
    // owc1: u8,
    // owc2: u8,
    // owc3: u8,

    // Initialization Command Word
    // icw1: u8,
    icw2: u8,
    // icw3: u8,
    // icw4: u8,
}

impl Pic {
    // TODO: Not the actual default state.
    pub fn new() -> Self {
        Self {
            irr: Bitset(0),
            isr: Bitset(0),
            imr: Bitset(0),

            // owc1: 0b0000_0000,
            // owc2: 0b0000_1000,
            // owc3: 0b0000_0000,

            // icw1: 0b0000_0000,
            icw2: 0b0000_0000,
            // icw3: 0b0000_0000,
            // icw4: 0b0000_0000,
        }
    }

    pub fn ir0(&mut self) {
        self.irr.set_bit(0);
    }

    pub fn ir1(&mut self) {
        self.irr.set_bit(1);
    }

    pub fn ir2(&mut self) {
        self.irr.set_bit(2);
    }

    pub fn ir3(&mut self) {
        self.irr.set_bit(3);
    }

    pub fn ir4(&mut self) {
        self.irr.set_bit(4);
    }

    pub fn ir5(&mut self) {
        self.irr.set_bit(5);
    }

    pub fn ir6(&mut self) {
        self.irr.set_bit(6);
    }

    pub fn ir7(&mut self) {
        self.irr.set_bit(7);
    }

    pub fn has_interrupt(self) -> bool {
        !self.masked_irr().is_empty()
    }
    fn masked_irr(&self) -> Bitset {
        self.irr.mask(self.imr)
    }
    fn masked_isr(&self) -> Bitset {
        self.isr.mask(self.imr)
    }

    fn needs_to_raise_interrupt(&self) -> bool {
        self.masked_irr().priority() > self.isr.priority()
    }

    pub fn next_event(&self) -> Option<Attoseconds> {
        if self.needs_to_raise_interrupt() {
            return Some(Attoseconds::from_nanoseconds(300));
        }

        None
    }

    pub fn inta(&mut self) -> u8 {
        let masked_irr = self.masked_irr();
        if masked_irr.is_empty() {
            return 7;
        }

        let level = masked_irr.highest_level();
        self.isr.set_bit(level);
        self.irr.reset_bit(level);

        (self.icw2 & 0b1111_1000) + level
    }
}

impl Default for Pic {
    fn default() -> Self {
        Self::new()
    }
}

impl Device for Pic {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        todo!()
    }

    fn get_name(&self) -> &str {
        todo!()
    }

    fn frequency(&self) -> f64 {
        1.0
    }

    fn read(&mut self, __addr: u16) -> u8 {
        todo!()
    }

    fn write(&mut self, addr: u16, v: u8) {
        assert!(addr < 2);

        const OCW1MASK: u16 = 0b1_0000_0000;
        const OCW1BITS: u16 = 0b1_0000_0000;
        const OCW2MASK: u16 = 0b1_0001_1000;
        const OCW2BITS: u16 = 0b0_0000_0000;
        const OCW3MASK: u16 = 0b1_0001_1000;
        const OCW3BITS: u16 = 0b0_0000_1000;

        const OCW2_R: u8 = 0b1000_0000;
        const OCW2_SL: u8 = 0b0100_0000;
        const OCW2_EOI: u8 = 0b0010_0000;
        const OCW2_LEVEL: u8 = 0b0000_0111;

        let cw = (addr << 8) | (v as u16);

        if (cw & OCW1MASK) == OCW1BITS {
            self.imr = Bitset(v);
        } else if (cw & OCW2MASK) == OCW2BITS {
            let r = (v & OCW2_R) != 0;
            let sl = (v & OCW2_SL) != 0;
            let eoi = (v & OCW2_EOI) != 0;
            let level = v & OCW2_LEVEL;

            assert!(!r, "PIC priority ration not supported");
            assert!(!sl, "PIC specific level EOI not supported");
            assert!(level == 0, "PIC specific level EOI not supported");
            assert!(eoi);

            let active = self.masked_isr().priority();
            self.isr.reset_bit(active);
        } else if (cw & OCW3MASK) == OCW3BITS {
            panic!("PIC OCW3 not supported");
        }
    }

    fn next_cycles(&self) -> u64 {
        todo!()
    }

    fn run(&mut self, _ctx: &mut crate::machine::DeviceMachineContext, _cycles: u64) -> u64 {
        todo!()
    }
}

#[derive(Copy, Clone)]
struct Bitset(u8);

impl Bitset {
    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn set_bit(&mut self, bit: u8) {
        self.0 |= 1 << bit;
    }

    pub fn reset_bit(&mut self, bit: u8) {
        self.0 &= !(1 << bit);
    }

    pub fn priority(self) -> u8 {
        assert!(!self.is_empty());
        7 - self.0.trailing_zeros() as u8
    }

    pub fn highest_level(self) -> u8 {
        assert!(!self.is_empty());
        (1 << self.0.trailing_zeros()) - 1
    }

    pub fn mask(self, mask: Bitset) -> Bitset {
        Bitset(self.0 & !mask.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitset_priority() {
        // IR0 set
        let b = Bitset(0b0000_0001);
        assert_eq!(b.priority(), 7);

        // Second highest bit set
        let b = Bitset(0b0100_0000);
        assert_eq!(b.priority(), 1);

        // Multiple bits set
        let b = Bitset(0b1011_0000);
        assert_eq!(b.priority(), 3);

        // All bits set
        let b = Bitset(0b1111_1111);
        assert_eq!(b.priority(), 7);
    }

    #[test]
    fn test_bitset_highest_level() {
        // Only lowest bit set
        let b = Bitset(0b0000_0001);
        assert_eq!(b.highest_level(), 0);

        // Second bit set
        let b = Bitset(0b0000_0010);
        assert_eq!(b.highest_level(), 1);

        // Third bit set
        let b = Bitset(0b0000_0100);
        assert_eq!(b.highest_level(), 3);

        // Multiple bits set, should return lowest set bit
        let b = Bitset(0b0010_0100);
        assert_eq!(b.highest_level(), 3);

        // All bits set
        let b = Bitset(0b1111_1111);
        assert_eq!(b.highest_level(), 0);
    }

    #[test]
    #[should_panic]
    fn test_highest_level_empty_panics() {
        let b = Bitset(0);
        b.highest_level();
    }

    #[test]
    fn test_set_isr_level() {
        let irr = Bitset(0b0000_0001);
        let mut isr = Bitset(0);
        isr.set_bit(irr.highest_level());
        assert_eq!(isr.0, 0b0000_0001);
    }
}

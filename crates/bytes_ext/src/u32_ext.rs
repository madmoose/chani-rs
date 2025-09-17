/// An extension trait for u32 to get and set the high and low u16 parts.
pub trait U32Ext {
    /// Returns the high 16 bits of the u32 as a u16.
    fn hi(&self) -> u16;

    /// Returns the low 16 bits of the u32 as a u16.
    fn lo(&self) -> u16;

    /// Sets the high 16 bits of the u32.
    fn set_hi(&mut self, half: u16);

    /// Sets the low 16 bits of the u32.
    fn set_lo(&mut self, half: u16);
}

impl U32Ext for u32 {
    /// Returns the high 16 bits of the u32 as a u16.
    fn hi(&self) -> u16 {
        (*self >> 16) as u16
    }

    /// Returns the low 16 bits of the u32 as a u16.
    fn lo(&self) -> u16 {
        *self as u16
    }

    /// Sets the high 16 bits of the u32.
    fn set_hi(&mut self, half: u16) {
        *self = (*self & 0x0000ffff) | ((half as u32) << 16);
    }

    /// Sets the low 16 bits of the u32.
    fn set_lo(&mut self, half: u16) {
        *self = (*self & 0xffff0000) | (half as u32);
    }
}

#[cfg(test)]
mod tests_u32_ext {
    use super::U32Ext;

    #[test]
    fn test_get_halves() {
        let number: u32 = 0xabcd1234;
        assert_eq!(number.hi(), 0xabcd);
        assert_eq!(number.lo(), 0x1234);
    }

    #[test]
    fn test_get_halves_zero() {
        let number: u32 = 0x00000000;
        assert_eq!(number.hi(), 0x0000);
        assert_eq!(number.lo(), 0x0000);
    }

    #[test]
    fn test_get_halves_max_value() {
        let number: u32 = 0xffffffff;
        assert_eq!(number.hi(), 0xffff);
        assert_eq!(number.lo(), 0xffff);
    }

    #[test]
    fn test_set_hi() {
        let mut number: u32 = 0x12345678;
        number.set_hi(0xffff);
        assert_eq!(number, 0xffff5678);
    }

    #[test]
    fn test_set_lo() {
        let mut number: u32 = 0x12345678;
        number.set_lo(0xffff);
        assert_eq!(number, 0x1234ffff);
    }

    #[test]
    fn test_set_both_halves() {
        let mut number: u32 = 0x00000000;
        number.set_hi(0xdead);
        number.set_lo(0xbeef);
        assert_eq!(number, 0xdeadbeef);
    }

    #[test]
    fn test_set_half_to_zero() {
        let mut number: u32 = 0xabcd1234;
        number.set_hi(0x0000);
        assert_eq!(number, 0x00001234);

        number.set_lo(0x0000);
        assert_eq!(number, 0x00000000);
    }
}

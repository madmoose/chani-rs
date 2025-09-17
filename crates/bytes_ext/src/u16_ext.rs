pub trait U16Ext {
    /// Returns the high byte of the u16.
    fn hi(&self) -> u8;

    /// Returns the low byte of the u16.
    fn lo(&self) -> u8;

    /// Sets the high byte of the u16.
    fn set_hi(&mut self, byte: u8);

    /// Sets the low byte of the u16.
    fn set_lo(&mut self, byte: u8);
}

impl U16Ext for u16 {
    /// Returns the high byte of the u16.
    fn hi(&self) -> u8 {
        (*self >> 8) as u8
    }

    /// Returns the low byte of the u16.
    fn lo(&self) -> u8 {
        *self as u8
    }

    /// Sets the high byte of the u16.
    fn set_hi(&mut self, byte: u8) {
        *self = (*self & 0x00ff) | ((byte as u16) << 8);
    }

    /// Sets the low byte of the u16.
    fn set_lo(&mut self, byte: u8) {
        *self = (*self & 0xff00) | (byte as u16);
    }
}

#[cfg(test)]
mod tests_u16_ext {
    use super::U16Ext; // Import the trait

    #[test]
    fn test_get_bytes() {
        let number: u16 = 0xabcd;
        assert_eq!(number.hi(), 0xab);
        assert_eq!(number.lo(), 0xcd);
    }

    #[test]
    fn test_get_bytes_zero() {
        let number: u16 = 0x0000;
        assert_eq!(number.hi(), 0x00);
        assert_eq!(number.lo(), 0x00);
    }

    #[test]
    fn test_get_bytes_max_value() {
        let number: u16 = 0xffff;
        assert_eq!(number.hi(), 0xff);
        assert_eq!(number.lo(), 0xff);
    }

    #[test]
    fn test_set_hi() {
        let mut number: u16 = 0x1234;
        number.set_hi(0xff);
        assert_eq!(number, 0xff34);
    }

    #[test]
    fn test_set_lo() {
        let mut number: u16 = 0x1234;
        number.set_lo(0xff);
        assert_eq!(number, 0x12ff);
    }

    #[test]
    fn test_set_both_bytes() {
        let mut number: u16 = 0x0000;
        number.set_hi(0xde);
        number.set_lo(0xad);
        assert_eq!(number, 0xdead);
    }

    #[test]
    fn test_set_byte_to_zero() {
        let mut number: u16 = 0xabcd;
        number.set_hi(0x00);
        assert_eq!(number, 0x00cd);

        number.set_lo(0x00);
        assert_eq!(number, 0x0000);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RGB666 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl RGB666 {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        assert!(r < 64);
        assert!(g < 64);
        assert!(b < 64);
        Self { r, g, b }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RGB888 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

fn scale_6_bits_to_8_bits(value: u8) -> u8 {
    ((value as u16 * 85 + 10) / 21) as u8
}

impl From<RGB666> for RGB888 {
    fn from(color: RGB666) -> Self {
        RGB888 {
            r: scale_6_bits_to_8_bits(color.r),
            g: scale_6_bits_to_8_bits(color.g),
            b: scale_6_bits_to_8_bits(color.b),
        }
    }
}

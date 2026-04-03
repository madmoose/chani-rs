pub trait PaletteColor: Clone + Copy + Default {
    fn maxval() -> u8;
    fn to_rgb_bytes(&self) -> [u8; 3];
    fn from_rgb_bytes(r: u8, g: u8, b: u8) -> Self;
}

#[derive(Debug, Clone, Copy, Default)]
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

impl PaletteColor for RGB666 {
    fn maxval() -> u8 {
        63
    }

    fn to_rgb_bytes(&self) -> [u8; 3] {
        [self.r, self.g, self.b]
    }

    fn from_rgb_bytes(r: u8, g: u8, b: u8) -> Self {
        Self {
            r: r.min(63),
            g: g.min(63),
            b: b.min(63),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RGB888 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl PaletteColor for RGB888 {
    fn maxval() -> u8 {
        255
    }

    fn to_rgb_bytes(&self) -> [u8; 3] {
        [self.r, self.g, self.b]
    }

    fn from_rgb_bytes(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
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

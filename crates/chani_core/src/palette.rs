use std::fs::File;
use std::io::{BufWriter, Write};
use std::ops::{Index, IndexMut};

use crate::color::{PaletteColor, RGB666, RGB888};

pub type Pal666 = Palette<RGB666>;
pub type Pal888 = Palette<RGB888>;

#[derive(Clone)]
pub struct Palette<C: PaletteColor> {
    data: [C; 256],
}

impl<C: PaletteColor> Default for Palette<C> {
    fn default() -> Self {
        Self {
            data: [C::default(); 256],
        }
    }
}

impl<C: PaletteColor> Index<u8> for Palette<C> {
    type Output = C;

    fn index(&self, index: u8) -> &Self::Output {
        &self.data[index as usize]
    }
}

impl<C: PaletteColor> IndexMut<u8> for Palette<C> {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        &mut self.data[index as usize]
    }
}

impl<C: PaletteColor> From<&[u8; 768]> for Palette<C> {
    fn from(value: &[u8; 768]) -> Self {
        let mut palette = Palette::default();
        for i in 0..256 {
            let offset = i * 3;
            palette[i as u8] =
                C::from_rgb_bytes(value[offset], value[offset + 1], value[offset + 2]);
        }
        palette
    }
}

impl<C: PaletteColor> Palette<C> {
    /// Write the palette as a visual PPM image showing all 256 colors in a 16x16 grid.
    /// Each color is displayed as a square of `cell_size` x `cell_size` pixels.
    pub fn write_ppm(&self, filename: &str, cell_size: usize) -> std::io::Result<()> {
        let file = File::create(filename)?;
        let mut writer = BufWriter::new(file);

        let cols = 16;
        let rows = 16;
        let width = cols * cell_size;
        let height = rows * cell_size;

        writeln!(writer, "P6 {} {} {}", width, height, C::maxval())?;

        for row in 0..rows {
            for _py in 0..cell_size {
                for col in 0..cols {
                    let color_index = (row * cols + col) as u8;
                    let color = self[color_index];
                    let rgb = color.to_rgb_bytes();
                    for _px in 0..cell_size {
                        writer.write_all(&rgb)?;
                    }
                }
            }
        }

        writer.flush()?;
        Ok(())
    }
}

// Conversion between palette types
impl From<Palette<RGB666>> for Palette<RGB888> {
    fn from(pal666: Palette<RGB666>) -> Self {
        let mut pal888 = Palette::<RGB888>::default();
        for i in 0..=255 {
            pal888[i] = pal666[i].into();
        }
        pal888
    }
}

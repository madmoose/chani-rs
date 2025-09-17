#![allow(clippy::collapsible_if)]
#![allow(clippy::identity_op)]
#![allow(clippy::new_without_default)]
#![allow(dead_code)]
#![feature(ascii_char)]
#![feature(bigint_helper_methods)]
#![feature(iter_map_windows)]

pub mod address;
pub mod bios;
pub mod clock;
pub mod color;
pub mod cpu;
pub mod device;
pub mod dos;
pub mod file_system;
pub mod frame;
pub mod input_event;
pub mod machine;
pub mod memory;
pub mod time;

mod key;

use std::{
    fs::{File, create_dir_all},
    io::{BufWriter, Write},
    path::Path,
};

pub use key::Key;

fn write_ppm(pal: &[u8; 768], data: &[u8], w: usize, h: usize, filename: &str) {
    const SCALE_X: usize = 5;
    const SCALE_Y: usize = 6;

    let w = w * SCALE_X;
    let h = h * SCALE_Y;

    let mut frame = vec![0u8; w * h * 3];

    for y in 0..h {
        for x in 0..w {
            let offset = (w / SCALE_X) * (y / SCALE_Y) + (x / SCALE_X);
            let c = data[offset] as usize;

            let offset = w * y + x;
            frame[3 * offset + 0] = pal[3 * c + 0];
            frame[3 * offset + 1] = pal[3 * c + 1];
            frame[3 * offset + 2] = pal[3 * c + 2];
        }
    }

    let dir = Path::new("ppm");
    if !dir.exists() {
        if let Err(e) = create_dir_all(dir) {
            static mut DID_ERROR: bool = false;
            unsafe {
                if !DID_ERROR {
                    println!("\n\nCreate directory ppm/ to dump frames in. Error: {e}\n\n\n");
                    DID_ERROR = true;
                }
            }
            return;
        }
    }

    let file = File::create(filename).expect("Failed to create PPM file");

    let mut writer = BufWriter::new(file);
    let _ = writeln!(writer, "P6 {w} {h} 255");
    let _ = writer.write_all(&frame[..w * h * 3]);
}

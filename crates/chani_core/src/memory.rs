use std::{
    fmt::Write,
    ops::{Index, IndexMut},
};

use crate::address::{Address, IntoEffectiveAddress, addr};

pub struct Memory {
    data: Box<[u8; 1024 * 1024]>,
    logging_enabled: bool,
}

impl Default for Memory {
    fn default() -> Self {
        Memory::new()
    }
}

pub struct Bytes<'a> {
    memory: &'a Memory,
    pos: usize,
}

impl<'a> Bytes<'a> {
    pub fn new(memory: &'a Memory, start_address: u32) -> Self {
        Self {
            memory,
            pos: start_address as usize,
        }
    }
}

impl<'a> Iterator for Bytes<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.memory.data.len() {
            let byte = self.memory.data[self.pos];
            self.pos += 1;
            Some(byte)
        } else {
            None
        }
    }
}

impl Memory {
    pub fn iter_from(&self, start_address: u32) -> Bytes<'_> {
        Bytes::new(self, start_address)
    }
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            data: Box::new([0; 0x100000]),
            logging_enabled: false,
        }
    }

    pub fn enable_logging(&mut self) {
        // self.logging_enabled = true;
    }

    pub fn disable_logging(&mut self) {
        // self.logging_enabled = false;
    }

    pub fn bytes_at_address<T: IntoEffectiveAddress>(&self, address: T) -> Bytes<'_> {
        Bytes::new(self, address.into_ea())
    }

    pub fn bytes_at_seg(&self, seg: u16) -> Bytes<'_> {
        Bytes::new(self, addr(seg, 0).ea())
    }

    pub fn copy<T: IntoEffectiveAddress, U: IntoEffectiveAddress>(
        &mut self,
        dst: T,
        src: U,
        len: u32,
    ) {
        let dst = dst.into_ea() as usize;
        let src = src.into_ea() as usize;
        for i in 0..(len as usize) {
            self.data[dst + i] = self.data[src + i];
        }
    }

    pub fn read_u8(&self, address: Address) -> u8 {
        let ea = address.into_ea() as usize;
        if ea >= self.data.len() {
            panic!("Memory read out of bounds at address {ea:#X}");
        }

        let v = self.data[ea];
        if self.logging_enabled {
            // println!("READ:  byte {address} = {v:#04x}");
        }

        v
    }

    pub fn write_u8(&mut self, address: Address, value: u8) {
        let ea = address.into_ea() as usize;

        // if self.logging_enabled || ea < 1024 {
        //     println!("WRITE: byte {address} <- {value:#04x} ({})", ea / 4);
        // }
        // if address.ea() == addr(0x017e, 0xefdd).ea() {
        //     println!("XXXX WRITE: byte {address} <- {value:#04x}");
        // }

        let pal_base = 0x02bf;
        if address.seg == 0x24c9 && (pal_base..pal_base + 0x300).contains(&address.ofs) {
            let c = address.ofs - pal_base;
            println!(
                "WRITE: {} [{pal_base:04x}][{}][{}] = {:02x}",
                address,
                c / 3,
                c % 3,
                value & 0xff
            );
        }
        let pal_base = pal_base + 0x0300;
        if address.seg == 0x24c9 && (pal_base..pal_base + 0x300).contains(&address.ofs) {
            let c = address.ofs - pal_base;
            println!(
                "WRITE: {} [{pal_base:04x}][{}][{}] = {:02x}",
                address,
                c / 3,
                c % 3,
                value
            );
        }

        self.data[ea] = value;
    }

    pub fn read_u16(&self, address: Address) -> u16 {
        let low = self.data[address.ea() as usize] as u16;
        let high = self.data[(address + 1).ea() as usize] as u16;
        let v = low | (high << 8);

        if self.logging_enabled {
            // println!("READ:  word {address} = {v:#04x}");
        }

        v
    }

    pub fn write_u16(&mut self, address: Address, value: u16) {
        let ea = address.into_ea() as usize;
        if (ea + 1) >= self.data.len() {
            panic!("Memory write out of bounds at address {ea:#X}");
        }

        // if self.logging_enabled || ea < 1024 {
        //     println!("WRITE: word {address} <- {value:#06x} ({}h)", ea / 4);
        // }
        // if address.ea() == addr(0x017e, 0xefdd).ea() {
        //     println!("XXXX WRITE: word {address} <- {value:#06x}");
        // }

        for i in 0..2 {
            let address = address + i;
            let pal_base = 0x02bf;
            if address.seg == 0x24c9 && (pal_base..pal_base + 0x300).contains(&address.ofs) {
                let c = address.ofs - pal_base;
                println!(
                    "WRITE: {} [{pal_base:04x}][{}][{}] = {:02x}",
                    address,
                    c / 3,
                    c % 3,
                    value & 0xff
                );
            }
            let pal_base = pal_base + 0x0300;
            if address.seg == 0x24c9 && (pal_base..pal_base + 0x300).contains(&address.ofs) {
                let c = address.ofs - pal_base;
                println!(
                    "WRITE: {} [{pal_base:04x}][{}][{}] = {:02x}",
                    address,
                    c / 3,
                    c % 3,
                    value >> 8
                );
            }
        }

        self.data[address.ea() as usize] = (value & 0xff) as u8;
        self.data[(address + 1).ea() as usize] = (value >> 8) as u8;
    }

    pub fn write_bytes(&mut self, address: Address, bytes: &[u8]) {
        let ea = address.into_ea() as usize;
        let end = ea + bytes.len();
        if end > self.data.len() {
            panic!("Memory write out of bounds at address {ea:#X}");
        }
        self.data[ea..end].copy_from_slice(bytes);
    }

    pub fn read_bytes(&self, address: Address, bytes: &mut [u8]) {
        let ea = address.into_ea() as usize;
        let end = ea + bytes.len();
        if end > self.data.len() {
            panic!("Memory read out of bounds at address {ea:#X}");
        }
        bytes.copy_from_slice(&self.data[ea..end]);
    }

    pub fn hexdump(&self, start_address: Address, length: u32) {
        const BYTES_PER_LINE: u32 = 32;

        let start_ea = start_address.into_ea();
        let end_address = start_ea + length;

        let mut line = String::with_capacity(80);

        for address in (start_ea..end_address).step_by(BYTES_PER_LINE as usize) {
            let len = BYTES_PER_LINE.min(end_address - address);

            write!(
                &mut line,
                "{:04x}:{:04x}:",
                start_address.seg,
                start_address.ofs as u32 + (address - start_ea)
            )
            .unwrap();

            for i in 0..len {
                if i == BYTES_PER_LINE / 2 {
                    write!(&mut line, " ").unwrap();
                }
                write!(&mut line, " {:02X}", self.data[(address + i) as usize]).unwrap();
            }
            for i in len..BYTES_PER_LINE {
                if i == BYTES_PER_LINE / 2 {
                    write!(&mut line, " ").unwrap();
                }
                write!(&mut line, "   ").unwrap();
            }

            write!(&mut line, "  ").unwrap();
            for i in 0..len {
                let b = self.data[(address + i) as usize];
                let c = if b.is_ascii_graphic() { b as char } else { '.' };
                write!(&mut line, "{c}").unwrap();
            }
            writeln!(&mut line).unwrap();

            print!("{line}");

            line.clear();
        }
    }
}

impl<T: IntoEffectiveAddress> Index<T> for Memory {
    type Output = u8;

    fn index(&self, index: T) -> &Self::Output {
        let ea = index.into_ea() as usize;
        &self.data[ea]
    }
}

impl<T: IntoEffectiveAddress> IndexMut<T> for Memory {
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        let ea = index.into_ea() as usize;
        &mut self.data[ea]
    }
}

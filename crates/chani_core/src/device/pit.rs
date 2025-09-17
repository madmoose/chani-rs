use std::any::Any;

use bytes_ext::U16Ext;

use crate::device::Device;

pub struct Counter {
    pub activated: bool,
    pub counting_element: u32,
    pub output_latch: u16,
    pub count_register: u16,
    pub is_latched: bool,
    pub r: u8,
    pub w: u8,
    pub mode: u8,
    pub bcd: bool,
    pub out: u8,
}

impl Default for Counter {
    fn default() -> Self {
        Self {
            activated: false,
            counting_element: 0x10000,
            output_latch: 0,
            count_register: 0,
            is_latched: false,
            r: 0,
            w: 0,
            mode: 0,
            bcd: false,
            out: 0,
        }
    }
}

impl Counter {
    pub fn write(&mut self, v: u8) {
        match self.w {
            0b00 => return,
            0b01 => {
                self.count_register.set_lo(v);
                self.w = 0b00;
            }
            0b10 => {
                self.count_register.set_hi(v);
                self.w = 0b00;
            }
            0b11 => {
                self.count_register.set_lo(v);
                self.w = 0b10;
            }
            _ => panic!("unreachable"),
        }
        if self.w == 0b00 {
            self.activated = true;
            self.counting_element = self.count_register as u32;
            if self.counting_element == 0 {
                self.counting_element = 0x10000;
            }
        }
    }

    pub fn read(&mut self) -> u8 {
        match self.r {
            0b00 => 0,
            0b01 => {
                self.r = 0b00;
                self.output_latch.lo()
            }
            0b10 => {
                self.r = 0b00;
                self.output_latch.hi()
            }
            0b11 => {
                self.r = 0b10;
                self.output_latch.lo()
            }
            _ => panic!("unreachable"),
        }
    }

    pub fn run_cycles(&mut self, cycles: u64) -> u64 {
        assert!(cycles <= (u32::MAX as u64));
        self.counting_element = self.counting_element.saturating_sub(cycles as u32);
        cycles
    }
}

pub struct Pit {
    counter: [Counter; 3],
}

impl Pit {
    pub fn new() -> Self {
        let mut pit = Self {
            counter: [Counter::default(), Counter::default(), Counter::default()],
        };
        pit.counter[0].activated = true;
        pit
    }
}

impl Default for Pit {
    fn default() -> Self {
        Self::new()
    }
}

impl Device for Pit {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_name(&self) -> &str {
        "i8254_pit"
    }

    fn frequency(&self) -> f64 {
        105.0 / 88.0 // 1.1931818181818181
    }

    fn read(&mut self, addr: u16) -> u8 {
        assert!(addr <= 0b11);
        self.counter[addr as usize].read()
    }

    fn write(&mut self, addr: u16, w: u8) {
        println!("i8254_pit write addr: {addr:02X} w: {w:02X}");

        assert!(addr <= 0b11);
        if addr < 0b11 {
            self.counter[addr as usize].write(w);
        } else {
            let sc = (w >> 6) & 0b11;
            if sc < 3 {
                let rw = (w >> 4) & 0b11;
                let mut mode = (w >> 1) & 0b111;
                let bcd = (w & 1) != 0;

                let counter = &mut self.counter[sc as usize];

                if rw == 0 {
                    counter.is_latched = true;
                } else {
                    if (mode & 0b010) != 0 {
                        mode |= 0b011;
                    }

                    counter.r = rw;
                    counter.w = rw;
                    counter.mode = mode;
                    counter.bcd = bcd;

                    counter.out = 0b111110 >> mode;
                }
            } else {
                // Read-back command
            }
        }
    }

    fn next_cycles(&self) -> u64 {
        self.counter
            .iter()
            .filter(|c| c.activated)
            .map(|c| c.counting_element as u64)
            .min()
            .unwrap_or(u64::MAX)
    }

    fn run(&mut self, ctx: &mut crate::machine::DeviceMachineContext, cycles: u64) -> u64 {
        let cycles = std::cmp::min(cycles, self.next_cycles());

        for c in &mut self.counter {
            c.run_cycles(cycles);
        }

        if self.counter[0].counting_element == 0 {
            ctx.cpu.raise_intr(8);
            self.counter[0].counting_element = self.counter[0].count_register as u32;
            if self.counter[0].counting_element == 0 {
                self.counter[0].counting_element = 0x10000;
            }
        }
        cycles
    }
}

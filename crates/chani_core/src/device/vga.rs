use crate::address::Address;
use crate::color::RGB666;
use crate::device::Device;
use crate::frame::{Frame, FrameCacheSync, FrameSender};
use crate::machine::DeviceMachineContext;
use crate::memory::Memory;
use std::any::Any;
use std::fs::{File, create_dir_all};
use std::io::{BufWriter, Write};
use std::path::Path;

pub struct DACRam {
    data: [u8; 0x300],
}

impl DACRam {
    pub fn new() -> Self {
        Self { data: [0; 0x300] }
    }

    pub fn get(&self, index: usize) -> u8 {
        assert!(index < self.data.len());
        self.data[index]
    }

    pub fn set(&mut self, index: usize, value: u8) {
        assert!(index < self.data.len());
        self.data[index] = value;
    }

    pub fn get_color(&self, palette_index: u8) -> RGB666 {
        let base_index = (palette_index as usize) * 3;

        let r = self.data[base_index + 0] & 0x3f;
        let g = self.data[base_index + 1] & 0x3f;
        let b = self.data[base_index + 2] & 0x3f;

        RGB666::new(r, g, b)
    }
}

pub struct Vga {
    frame_sender: Option<FrameSender>,
    frame_cache: FrameCacheSync,
    frames_rendered: usize,

    pixel_frequency: f64,
    memory_map: u32,
    h_visible_area: i32,
    h_front_porch: i32,
    h_sync_pulse: i32,
    h_back_porch: i32,
    h_double: bool,
    v_visible_area: i32,
    v_front_porch: i32,
    v_sync_pulse: i32,
    v_back_porch: i32,
    v_double: bool,
    h_total: i32,
    v_total: i32,
    total_pels: i32,
    v_sync_pels: i32,
    current_pel: i32,
    crtc_addr: u8,
    crtc_regs: [u8; 0x19],
    input_status_1_reg: u8,
    attr_addr: u8,
    attr_regs: [u8; 0x25],
    input_status_0_reg: u8,
    seq_addr: u8,
    seq_regs: [u8; 0x05],
    dac_state: u8,
    dac_addr: u16,
    dac_ram: DACRam,
    feat_ctrl_reg: u8,
    misc_output_reg: u8,
    gfx_addr: u8,
    gfx_regs: [u8; 0x09],
    plane: [[u8; 0x10000]; 4],
}

impl Vga {
    pub fn new(frame_cache: FrameCacheSync) -> Self {
        let h_visible_area = 640;
        let h_front_porch = 16;
        let h_sync_pulse = 96;
        let h_back_porch = 48;
        let v_visible_area = 400;
        let v_front_porch = 12;
        let v_sync_pulse = 2;
        let v_back_porch = 35;
        let h_total = h_visible_area + h_front_porch + h_sync_pulse + h_back_porch;
        let v_total = v_visible_area + v_front_porch + v_sync_pulse + v_back_porch;
        let total_pels = h_total * v_total;
        let v_sync_pels = h_total * v_sync_pulse;
        let mut vga = Self {
            frame_sender: None,
            frame_cache,
            frames_rendered: 0,
            pixel_frequency: 25.175,
            memory_map: 0xa0000,
            h_visible_area,
            h_front_porch,
            h_sync_pulse,
            h_back_porch,
            h_double: true,
            v_visible_area,
            v_front_porch,
            v_sync_pulse,
            v_back_porch,
            v_double: true,
            h_total,
            v_total,
            total_pels,
            v_sync_pels,
            current_pel: 0,
            crtc_addr: 0,
            crtc_regs: [0; 0x19],
            input_status_1_reg: 0,
            attr_addr: 0,
            attr_regs: [0; 0x25],
            input_status_0_reg: 0,
            seq_addr: 0,
            seq_regs: [0; 0x05],
            dac_state: 0,
            dac_addr: 0,
            dac_ram: DACRam::new(),
            feat_ctrl_reg: 0,
            misc_output_reg: 0,
            gfx_addr: 0,
            gfx_regs: [0; 0x09],
            plane: [[0; 0x10000]; 4],
        };
        vga.set_mode(0x13);
        vga
    }

    pub fn set_mode(&mut self, mode: i32) {
        // TODO: Implement full mode setting logic

        println!("Vga::set_mode: Setting mode {mode}");

        match mode {
            2 | 3 => {
                self.pixel_frequency = 28.322;
                self.h_visible_area = 720;
                self.h_front_porch = 18;
                self.h_sync_pulse = 108;
                self.h_back_porch = 54;
                self.v_visible_area = 400;
                self.v_front_porch = 13;
                self.v_sync_pulse = 2;
                self.v_back_porch = 34;
                self.h_double = false;
                self.v_double = false;
                self.memory_map = 0xb8000;
            }
            0x0d | 0x13 => {
                self.pixel_frequency = 25.175;
                self.h_visible_area = 640;
                self.h_front_porch = 16;
                self.h_sync_pulse = 96;
                self.h_back_porch = 48;
                self.v_visible_area = 400;
                self.v_front_porch = 12;
                self.v_sync_pulse = 2;
                self.v_back_porch = 35;
                self.h_double = true;
                self.v_double = true;
                self.memory_map = 0xa0000;
            }
            _ => {
                // TODO: Support more modes
            }
        }
        self.h_total =
            self.h_visible_area + self.h_front_porch + self.h_sync_pulse + self.h_back_porch;
        self.v_total =
            self.v_visible_area + self.v_front_porch + self.v_sync_pulse + self.v_back_porch;
        self.total_pels = self.h_total * self.v_total;
        self.v_sync_pels = self.h_total * self.v_sync_pulse;
    }

    pub fn is_frame_ready(&self) -> bool {
        self.current_pel == self.v_sync_pels
    }

    pub fn complete_frame(&mut self, memory: &Memory) {
        if let Some(frame_sink) = self.frame_sender.as_ref() {
            let w = 320;
            let h = 200;
            let addr: Address = self.memory_map.into();

            let mut frame = Frame::new(w, h);

            for y in 0..h {
                for x in 0..w {
                    let offset = w * y + x;
                    let c = memory.read_u8(addr + offset as u16);
                    let color = self.dac_ram.get_color(c);
                    frame.set_rgb666(x, y, color);
                }
            }

            frame_sink.send(frame).unwrap();
        }
    }

    pub fn next_cycles(&self) -> u64 {
        let total_pels = self.h_total * self.v_total;
        let v_sync_pels = self.h_total * self.v_sync_pulse;
        let mut current_pel = self.current_pel;
        while current_pel >= total_pels {
            current_pel -= total_pels;
        }
        let next = if current_pel < v_sync_pels {
            (v_sync_pels - current_pel) as u64
        } else {
            (total_pels - current_pel) as u64
        };
        // println!("Vga::next_cycles: Total PELs = {total_pels}");
        // println!("Vga::next_cycles: Next cycles = {next}");

        let _ = next;

        next
    }

    pub fn run_cycles(&mut self, cycles: u64) -> u64 {
        self.current_pel += cycles as i32;
        while self.current_pel >= self.total_pels {
            self.current_pel -= self.total_pels;
        }
        if self.current_pel < self.v_sync_pels {
            (self.v_sync_pels - self.current_pel) as u64
        } else {
            (self.total_pels - self.current_pel) as u64
        }
    }
}

impl Vga {
    pub fn set_frame_sender(&mut self, receiver: FrameSender) {
        self.frame_sender = Some(receiver);
    }

    pub fn get_pal_888(&self) -> [u8; 768] {
        let mut pal = [0u8; 768];
        for i in 0..256 {
            let color = self.dac_ram.get_color(i as u8);
            pal[i * 3 + 0] = ((255 * (color.r as u16)) / 63) as u8;
            pal[i * 3 + 1] = ((255 * (color.g as u16)) / 63) as u8;
            pal[i * 3 + 2] = ((255 * (color.b as u16)) / 63) as u8;
        }
        pal
    }

    pub fn write_ppm(&mut self, memory: &Memory, addr: Address, w: usize, h: usize) {
        let mut frame = vec![0u8; w * h * 3];

        for y in 0..h {
            for x in 0..w {
                let offset = w * y + x;
                let c = memory.read_u8(addr + offset as u16);
                let color = self.dac_ram.get_color(c);
                frame[3 * offset + 0] = ((255 * (color.r as u16)) / 63) as u8;
                frame[3 * offset + 1] = ((255 * (color.g as u16)) / 63) as u8;
                frame[3 * offset + 2] = ((255 * (color.b as u16)) / 63) as u8;
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

        let filename = format!("ppm/frame-{:05}.ppm", self.frames_rendered);
        let file = File::create(&filename).expect("Failed to create PPM file");

        // if frame_number % 70 == 0 {
        //     println!("Dumping framebuffer to {filename}");
        // }

        let mut writer = BufWriter::new(file);
        let _ = writeln!(writer, "P6 {w} {h} 255");
        let _ = writer.write_all(&frame[..w * h * 3]);

        self.frames_rendered += 1;
    }
}

impl Device for Vga {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_name(&self) -> &str {
        "VGA"
    }

    fn frequency(&self) -> f64 {
        self.pixel_frequency
    }

    fn read(&mut self, addr: u16) -> u8 {
        let addr = addr + 0x3c0;

        let mut v = 0u8;

        match addr {
            0x3b4 | 0x3d4 => {
                v = self.crtc_addr;
            }
            0x3b5 | 0x3d5 => {
                v = self.crtc_regs[self.crtc_addr as usize];
            }
            0x3ba | 0x3da => {
                v = self.input_status_1_reg;
                self.attr_regs[0x24] &= 0x7f;
            }
            0x3c0 => {
                v = self.attr_addr;
            }
            0x3c1 => {
                v = self.attr_regs[self.attr_addr as usize];
            }
            0x3c2 => {
                v = self.input_status_0_reg;
            }
            0x3c4 => {
                v = self.seq_addr;
            }
            0x3c5 => {
                v = self.seq_regs[self.seq_addr as usize];
            }
            0x3c7 => {
                v = self.dac_state;
            }
            0x3c8 => {
                v = (self.dac_addr / 3) as u8;
            }
            0x3c9 => {
                v = self.dac_ram.get(self.dac_addr as usize);
                self.dac_addr = (self.dac_addr + 1) % 0x300;
            }
            0x3ca => {
                v = self.feat_ctrl_reg;
            }
            0x3cc => {
                v = self.misc_output_reg;
            }
            0x3ce => {
                v = self.gfx_addr;
            }
            0x3cf => {
                v = self.gfx_regs[self.gfx_addr as usize];
            }
            _ => {}
        }

        v
    }

    fn write(&mut self, addr: u16, v: u8) {
        let addr = addr + 0x3c0;

        // println!("Vga::write: {addr:#x} <- {v:#04x}");

        match addr {
            0x3b4 | 0x3d4 => {
                self.crtc_addr = v;
            }
            0x3b5 | 0x3d5 => {
                self.crtc_regs[self.crtc_addr as usize] = v;
            }
            0x3c0 => {
                // attr_writing_to_data() is typically: (attr_regs[0x24] & 0x80) == 0
                if (self.attr_regs[0x24] & 0x80) == 0 {
                    self.attr_regs[self.attr_addr as usize] = v;
                } else {
                    self.attr_addr = v;
                }
                self.attr_regs[0x24] ^= 0x80;
            }
            0x3c4 => {
                self.seq_addr = v;
            }
            0x3c5 => {
                self.seq_regs[self.seq_addr as usize] = v;
            }
            0x3c7 => {
                self.dac_addr = 3 * (v as u16);
                self.dac_state = 0b00;
            }
            0x3c8 => {
                self.dac_addr = 3 * (v as u16);
                self.dac_state = 0b11;
            }
            0x3c9 => {
                self.dac_ram.set(self.dac_addr as usize, v);
                self.dac_addr = (self.dac_addr + 1) % 0x300;
            }
            0x3ba | 0x3da => {
                self.feat_ctrl_reg = v;
            }
            0x3c2 => {
                self.misc_output_reg = v;
            }
            0x3ce => {
                self.gfx_addr = v;
            }
            0x3cf => {
                self.gfx_regs[self.gfx_addr as usize] = v;
            }
            _ => {}
        }
    }

    fn next_cycles(&self) -> u64 {
        self.next_cycles()
    }

    fn run(&mut self, _ctx: &mut DeviceMachineContext, cycles: u64) -> u64 {
        self.run_cycles(cycles)
    }
}

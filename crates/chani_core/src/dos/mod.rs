mod dos_alloc;
mod dos_exec;
mod dos_file;
mod environment;
mod error;
mod int21_dos;
mod int33_mouse;
mod mcb;
mod pdb;

use std::fmt::Display;

pub use dos_alloc::*;
pub use dos_exec::*;
pub use dos_file::*;
use mcb::MemoryControlBlock;

use crate::{
    address::{Address, addr},
    cpu::Cpu,
    file_system::Fd,
    machine::{DosMachineContext, Machine, install_interrupt_handler_callback},
    memory::Memory,
};

const MAX_PATH: usize = 260;

pub fn read_path<'a>(
    path: &'a mut [u8; MAX_PATH],
    memory: &Memory,
    address: Address,
) -> Option<&'a [u8]> {
    let mut len: usize = 0;
    loop {
        if len == MAX_PATH {
            return None;
        }
        let b = memory.read_u8(addr(address.seg, address.ofs + (len as u16)));
        path[len] = b;
        if b == 0 {
            break;
        }
        len += 1;
    }
    Some(&path[0..len])
}

#[derive(Default)]
struct UserRegs {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    si: u16,
    di: u16,
    bp: u16,
    ds: u16,
    es: u16,
    flags: u16,
}

impl Display for UserRegs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "UserRegs {{ ax: {:04X}, bx: {:04X}, cx: {:04X}, dx: {:04X}, si: {:04X}, di: {:04X}, bp: {:04X}, ds: {:04X}, es: {:04X}, flags: {:04X} }}",
            self.ax,
            self.bx,
            self.cx,
            self.dx,
            self.si,
            self.di,
            self.bp,
            self.ds,
            self.es,
            self.flags
        )
    }
}

fn readlo(reg: u16) -> u8 {
    (reg & 0x0f) as u8
}
fn writelo(reg: &mut u16, v: u8) {
    *reg = (*reg & 0xf0) | (v as u16);
}

impl UserRegs {
    pub fn get_al(&self) -> u8 {
        readlo(self.ax)
    }

    pub fn set_al(&mut self, v: u8) {
        writelo(&mut self.ax, v);
    }

    pub fn get_dl(&self) -> u8 {
        readlo(self.dx)
    }

    pub fn set_dl(&mut self, v: u8) {
        writelo(&mut self.dx, v);
    }

    pub fn get_cxdx_as_u32(&self) -> u32 {
        ((self.cx as u32) << 16) | (self.dx as u32)
    }

    pub fn set_cxdx(&mut self, v: u32) {
        self.cx = (v >> 16) as u16;
        self.dx = (v & 0xffff) as u16;
    }

    pub fn set_dxax_as_u32(&mut self, v: u32) {
        self.dx = (v >> 16) as u16;
        self.ax = (v & 0xffff) as u16;
    }

    pub fn get_dsdx_as_addr(&self) -> Address {
        addr(self.ds, self.dx)
    }

    pub fn set_dsdx(&mut self, address: Address) {
        self.ds = address.seg;
        self.dx = address.ofs;
    }
}

#[derive(Default)]
pub struct Dos {
    initial_mcb_seg: u16,
    allocation_strategy: u8, // 0 = first fit, 1 = best fit, 2 = last fit
    in_dos: i16,
    ctrl_break: bool,
    user_regs: UserRegs,
    files: Vec<Option<Fd>>,
    pub current_directory: Vec<u8>,
}

impl Dos {
    pub fn new() -> Self {
        Dos {
            current_directory: Vec::with_capacity(67),
            ..Dos::default()
        }
    }

    pub fn install(&mut self, cpu: &mut Cpu, memory: &mut Memory) {
        self.install_interrupt_handlers(cpu, memory);

        // Initialize the DOS memory control block (MCB)
        // The initial MCB covers the memory from 0x0080 to 0xa000
        let mut mcb = MemoryControlBlock {
            seg: 0x0080,
            signature: mcb::Z,
            pid: 8,
            size_in_paragraphs: 0xa000 - 0x0080 - 1,
        };
        // split writes the MCB to memory
        mcb.split(memory, 0x199)
            .expect("Failed to split initial MCB");

        self.initial_mcb_seg = mcb.seg;

        if !self.validate_mcb_chain(memory) {
            panic!("Invalid MCB chain detected");
        };
    }

    fn install_interrupt_handlers(&self, cpu: &mut Cpu, memory: &mut Memory) {
        fn dos_int21_callback(machine: &mut Machine, _address: Address) {
            let (dos, mut ctx) = machine.get_dos_and_context();
            dos.int21(&mut ctx);
        }

        fn dos_int33_callback(machine: &mut Machine, _address: Address) {
            let (dos, mut ctx) = machine.get_dos_and_context();
            dos.int33(&mut ctx);
        }

        install_interrupt_handler_callback(cpu, memory, 0x21, dos_int21_callback);
        install_interrupt_handler_callback(cpu, memory, 0x33, dos_int33_callback);
    }

    fn stc(&mut self) {
        self.user_regs.flags |= 0x001;
    }

    fn clc(&mut self) {
        self.user_regs.flags &= !0x001;
    }

    fn save_user_state(&mut self, cpu: &Cpu) {
        self.user_regs = UserRegs {
            ax: cpu.get_ax(),
            bx: cpu.get_bx(),
            cx: cpu.get_cx(),
            dx: cpu.get_dx(),
            si: cpu.get_si(),
            di: cpu.get_di(),
            bp: cpu.get_bp(),
            ds: cpu.get_ds(),
            es: cpu.get_es(),
            flags: cpu.get_flags(),
        }
    }

    fn restore_user_state(&self, cpu: &mut Cpu) {
        cpu.set_ax(self.user_regs.ax);
        cpu.set_bx(self.user_regs.bx);
        cpu.set_cx(self.user_regs.cx);
        cpu.set_dx(self.user_regs.dx);
        cpu.set_si(self.user_regs.si);
        cpu.set_di(self.user_regs.di);
        cpu.set_bp(self.user_regs.bp);
        cpu.set_ds(self.user_regs.ds);
        cpu.set_es(self.user_regs.es);
        // Does not restore flags register
    }

    pub fn validate_mcb_chain(&self, memory: &Memory) -> bool {
        let mut seg = self.initial_mcb_seg;
        println!("\nValidating MCB chain starting from segment {seg:#06X}");

        loop {
            let Some(mcb) = mcb::read(memory, seg) else {
                return false;
            };
            if mcb.is_last() {
                return true;
            }
            seg = mcb.next_seg();
        }
    }

    pub fn int33(&mut self, ctx: &mut DosMachineContext) {
        // let ss = ctx.cpu.get_ss();
        // let sp = ctx.cpu.get_sp();
        // let ip = ctx.memory.read_u16(addr(ss, sp)) - 2;
        // let cs = ctx.memory.read_u16(addr(ss, sp + 2));

        // eprintln!("DOS int 33h callback executed at address {}", addr(cs, ip),);
        let ax = ctx.cpu.get_ax();

        let func = match ax {
            0x0000 => Dos::int33_0000_reset_driver_and_read_status,
            0x0001 => Dos::int33_0001_show_mouse_cursor,
            0x0002 => Dos::int33_0002_hide_mouse_cursor,
            0x0003 => Dos::int33_0003_return_position_and_button_status,
            0x0004 => Dos::int33_0004_position_mouse_cursor,
            0x0005 => Dos::int33_0005_return_button_press_data,
            0x0006 => Dos::int33_0006_return_button_release_data,
            0x0007 => Dos::int33_0007_define_horizontal_cursor_range,
            0x0008 => Dos::int33_0008_define_vertical_cursor_range,
            0x0009 => Dos::int33_0009_define_graphics_cursor,
            0x000a => Dos::int33_000a_define_text_cursor,
            0x000b => Dos::int33_000b_read_motion_counters,
            0x000c => Dos::int33_000c_define_interrupt_subroutine_parameters,
            0x000d => Dos::int33_000d_light_pen_emulation_on,
            0x000e => Dos::int33_000e_light_pen_emulation_off,
            0x000f => Dos::int33_000f_define_mickey_pixel_ratio,
            0x0010 => Dos::int33_0010_define_screen_region_for_updating,
            _ => {
                println!("Unknown DOS int 33h function: {ax:#02x}");
                todo!();
            }
        };

        func(self, ctx);

        // println!("DOS int 33h function {ah:#02x} executed successfully.\n");

        ctx.cpu.iret(ctx.memory);
    }
}

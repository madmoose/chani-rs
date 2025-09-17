use crate::{cpu::Cpu, machine::install_interrupt_handler_callback, memory::Memory};

mod int08_system_timer;
mod int09_keyboard;
mod int10_video;

pub struct Bios {
    // uint16_ref_t keyboard_buffer_head;
    // uint16_ref_t keyboard_buffer_tail;
    // uint16_t     keyboard_buffer_begin;
    // uint16_ref_t keyboard_buffer[16];
    // uint16_t     keyboard_buffer_end;

    // uint16_ref_t timer_lo;
    // uint16_ref_t timer_hi;
    // uint16_ref_t timer_ofl;
    cursor_pos_x: u8,
    cursor_pos_y: u8,
    cols: u8,
}

impl Bios {
    pub fn new() -> Self {
        Self {
            cursor_pos_x: 0,
            cursor_pos_y: 0,
            cols: 80,
        }
    }

    pub fn install(&mut self, cpu: &mut Cpu, memory: &mut Memory) {
        install_interrupt_handler_callback(cpu, memory, 0x08, |machine, _address| {
            // println!("int08_system_timer");
            let (bios, mut ctx) = machine.get_bios_and_context();
            bios.int08_system_timer(&mut ctx);
        });

        install_interrupt_handler_callback(cpu, memory, 0x09, |machine, _address| {
            let (bios, mut ctx) = machine.get_bios_and_context();
            bios.int09_keyboard(&mut ctx);
        });

        install_interrupt_handler_callback(cpu, memory, 0x10, |machine, _address| {
            let (bios, mut ctx) = machine.get_bios_and_context();
            bios.int10_video(&mut ctx);
        });
    }
}

impl Default for Bios {
    fn default() -> Self {
        Self::new()
    }
}

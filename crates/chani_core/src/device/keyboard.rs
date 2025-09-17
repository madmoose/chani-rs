use std::any::Any;
use std::collections::VecDeque;

use crate::Key;
use crate::device::Device;

const I8042_STATUS_OUTPUT_BUFFER_FULL: u8 = 0x01;

#[derive(Clone)]
pub struct KeySequence {
    pub key: Key,
    pub make_sequence: &'static [u8],
    pub break_sequence: &'static [u8],
}

// PS/2 Scan Code Set 1 mapping
// egui::Key mapped to PS/2 scan codes
const SCAN_CODE_SET_1: &[KeySequence] = &[
    // Letters A-Z
    KeySequence {
        key: Key::A,
        make_sequence: &[0x1E],
        break_sequence: &[0x9E],
    },
    KeySequence {
        key: Key::B,
        make_sequence: &[0x30],
        break_sequence: &[0xB0],
    },
    KeySequence {
        key: Key::C,
        make_sequence: &[0x2E],
        break_sequence: &[0xAE],
    },
    KeySequence {
        key: Key::D,
        make_sequence: &[0x20],
        break_sequence: &[0xA0],
    },
    KeySequence {
        key: Key::E,
        make_sequence: &[0x12],
        break_sequence: &[0x92],
    },
    KeySequence {
        key: Key::F,
        make_sequence: &[0x21],
        break_sequence: &[0xA1],
    },
    KeySequence {
        key: Key::G,
        make_sequence: &[0x22],
        break_sequence: &[0xA2],
    },
    KeySequence {
        key: Key::H,
        make_sequence: &[0x23],
        break_sequence: &[0xA3],
    },
    KeySequence {
        key: Key::I,
        make_sequence: &[0x17],
        break_sequence: &[0x97],
    },
    KeySequence {
        key: Key::J,
        make_sequence: &[0x24],
        break_sequence: &[0xA4],
    },
    KeySequence {
        key: Key::K,
        make_sequence: &[0x25],
        break_sequence: &[0xA5],
    },
    KeySequence {
        key: Key::L,
        make_sequence: &[0x26],
        break_sequence: &[0xA6],
    },
    KeySequence {
        key: Key::M,
        make_sequence: &[0x32],
        break_sequence: &[0xB2],
    },
    KeySequence {
        key: Key::N,
        make_sequence: &[0x31],
        break_sequence: &[0xB1],
    },
    KeySequence {
        key: Key::O,
        make_sequence: &[0x18],
        break_sequence: &[0x98],
    },
    KeySequence {
        key: Key::P,
        make_sequence: &[0x19],
        break_sequence: &[0x99],
    },
    KeySequence {
        key: Key::Q,
        make_sequence: &[0x10],
        break_sequence: &[0x90],
    },
    KeySequence {
        key: Key::R,
        make_sequence: &[0x13],
        break_sequence: &[0x93],
    },
    KeySequence {
        key: Key::S,
        make_sequence: &[0x1F],
        break_sequence: &[0x9F],
    },
    KeySequence {
        key: Key::T,
        make_sequence: &[0x14],
        break_sequence: &[0x94],
    },
    KeySequence {
        key: Key::U,
        make_sequence: &[0x16],
        break_sequence: &[0x96],
    },
    KeySequence {
        key: Key::V,
        make_sequence: &[0x2F],
        break_sequence: &[0xAF],
    },
    KeySequence {
        key: Key::W,
        make_sequence: &[0x11],
        break_sequence: &[0x91],
    },
    KeySequence {
        key: Key::X,
        make_sequence: &[0x2D],
        break_sequence: &[0xAD],
    },
    KeySequence {
        key: Key::Y,
        make_sequence: &[0x15],
        break_sequence: &[0x95],
    },
    KeySequence {
        key: Key::Z,
        make_sequence: &[0x2C],
        break_sequence: &[0xAC],
    },
    // Numbers 0-9
    KeySequence {
        key: Key::Num0,
        make_sequence: &[0x0B],
        break_sequence: &[0x8B],
    },
    KeySequence {
        key: Key::Num1,
        make_sequence: &[0x02],
        break_sequence: &[0x82],
    },
    KeySequence {
        key: Key::Num2,
        make_sequence: &[0x03],
        break_sequence: &[0x83],
    },
    KeySequence {
        key: Key::Num3,
        make_sequence: &[0x04],
        break_sequence: &[0x84],
    },
    KeySequence {
        key: Key::Num4,
        make_sequence: &[0x05],
        break_sequence: &[0x85],
    },
    KeySequence {
        key: Key::Num5,
        make_sequence: &[0x06],
        break_sequence: &[0x86],
    },
    KeySequence {
        key: Key::Num6,
        make_sequence: &[0x07],
        break_sequence: &[0x87],
    },
    KeySequence {
        key: Key::Num7,
        make_sequence: &[0x08],
        break_sequence: &[0x88],
    },
    KeySequence {
        key: Key::Num8,
        make_sequence: &[0x09],
        break_sequence: &[0x89],
    },
    KeySequence {
        key: Key::Num9,
        make_sequence: &[0x0A],
        break_sequence: &[0x8A],
    },
    // Special keys
    KeySequence {
        key: Key::Space,
        make_sequence: &[0x39],
        break_sequence: &[0xB9],
    },
    KeySequence {
        key: Key::Enter,
        make_sequence: &[0x1C],
        break_sequence: &[0x9C],
    },
    KeySequence {
        key: Key::Escape,
        make_sequence: &[0x01],
        break_sequence: &[0x81],
    },
    KeySequence {
        key: Key::Backspace,
        make_sequence: &[0x0E],
        break_sequence: &[0x8E],
    },
    KeySequence {
        key: Key::Tab,
        make_sequence: &[0x0F],
        break_sequence: &[0x8F],
    },
    // Arrow keys (extended keys with E0 prefix)
    KeySequence {
        key: Key::ArrowUp,
        make_sequence: &[0xE0, 0x48],
        break_sequence: &[0xE0, 0xC8],
    },
    KeySequence {
        key: Key::ArrowDown,
        make_sequence: &[0xE0, 0x50],
        break_sequence: &[0xE0, 0xD0],
    },
    KeySequence {
        key: Key::ArrowLeft,
        make_sequence: &[0xE0, 0x4B],
        break_sequence: &[0xE0, 0xCB],
    },
    KeySequence {
        key: Key::ArrowRight,
        make_sequence: &[0xE0, 0x4D],
        break_sequence: &[0xE0, 0xCD],
    },
    // Function keys
    KeySequence {
        key: Key::F1,
        make_sequence: &[0x3B],
        break_sequence: &[0xBB],
    },
    KeySequence {
        key: Key::F2,
        make_sequence: &[0x3C],
        break_sequence: &[0xBC],
    },
    KeySequence {
        key: Key::F3,
        make_sequence: &[0x3D],
        break_sequence: &[0xBD],
    },
    KeySequence {
        key: Key::F4,
        make_sequence: &[0x3E],
        break_sequence: &[0xBE],
    },
    KeySequence {
        key: Key::F5,
        make_sequence: &[0x3F],
        break_sequence: &[0xBF],
    },
    KeySequence {
        key: Key::F6,
        make_sequence: &[0x40],
        break_sequence: &[0xC0],
    },
    KeySequence {
        key: Key::F7,
        make_sequence: &[0x41],
        break_sequence: &[0xC1],
    },
    KeySequence {
        key: Key::F8,
        make_sequence: &[0x42],
        break_sequence: &[0xC2],
    },
    KeySequence {
        key: Key::F9,
        make_sequence: &[0x43],
        break_sequence: &[0xC3],
    },
    KeySequence {
        key: Key::F10,
        make_sequence: &[0x44],
        break_sequence: &[0xC4],
    },
];

pub struct Keyboard {
    data_output_buffer: u8,
    status: u8,
    next_event: u64,
    buffer: VecDeque<u8>,
}

impl Keyboard {
    pub fn new() -> Self {
        Self {
            data_output_buffer: 0,
            status: 0,
            next_event: u64::MAX,
            buffer: VecDeque::new(),
        }
    }

    pub fn key_down(&mut self, key: Key) {
        for element in SCAN_CODE_SET_1 {
            if element.key == key {
                for &value in element.make_sequence {
                    self.buffer.push_back(value);
                }
                self.next_event = 0;
                break;
            }
        }
    }

    pub fn key_up(&mut self, key: Key) {
        for element in SCAN_CODE_SET_1 {
            if element.key == key {
                for &value in element.break_sequence {
                    self.buffer.push_back(value);
                }
                self.next_event = 0;
                break;
            }
        }
    }
}

impl Device for Keyboard {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_name(&self) -> &str {
        "Keyboard"
    }

    fn frequency(&self) -> f64 {
        // Return frequency in MHz (e.g., 1.0 for 1MHz)
        1.0
    }

    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x00 => {
                if self.status & I8042_STATUS_OUTPUT_BUFFER_FULL != 0 {
                    self.status &= !I8042_STATUS_OUTPUT_BUFFER_FULL;
                    self.next_event = (1000.0 * self.frequency()) as u64;
                }
                self.data_output_buffer
            }
            0x04 => self.status,
            _ => {
                // Unhandled I/O read
                0
            }
        }
    }

    fn write(&mut self, _addr: u16, _v: u8) {
        // Unhandled I/O write - keyboard is typically input-only
    }

    fn next_cycles(&self) -> u64 {
        self.next_event
    }

    fn run(&mut self, ctx: &mut crate::machine::DeviceMachineContext, cycles: u64) -> u64 {
        if self.next_event == u64::MAX {
            return cycles;
        }

        self.next_event = self.next_event.saturating_sub(cycles);

        if self.next_event == 0 {
            if let Some(data) = self.buffer.pop_front() {
                self.data_output_buffer = data;
                self.status |= I8042_STATUS_OUTPUT_BUFFER_FULL;
                ctx.cpu.raise_intr(9);
                self.next_event = (1000.0 * self.frequency()) as u64;
            } else {
                self.next_event = u64::MAX;
            }
        }

        cycles
    }
}

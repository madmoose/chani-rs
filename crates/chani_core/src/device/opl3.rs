use crate::device::Device;

pub struct OPL3 {
    write_index: [u8; 2],
    /// When true, every I/O write to ports 0x388-0x38B is logged to stdout
    /// in the "OPL3: I/O write at port 0xXXX <- VV" format that the
    /// reverse-engineered driver also emits. Toggled by the
    /// `--dump-opl3-writes` flag on the emulate-* binaries.
    pub dump_writes: bool,
}

impl OPL3 {
    pub fn new() -> Self {
        OPL3 {
            write_index: [0; 2],
            dump_writes: false,
        }
    }
}

impl Device for OPL3 {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_name(&self) -> &str {
        "OPL3"
    }

    fn frequency(&self) -> f64 {
        14.31818
    }

    fn write(&mut self, addr: u16, v: u8) {
        if self.dump_writes {
            println!("OPL3: I/O write at port {:#X} <- {:02X}", 0x388 + addr, v);
        }

        if addr % 2 == 0 {
            self.write_index[addr as usize / 2] = v;
        } else if addr % 2 == 1 {
            // println!(
            //     "WRITE {}{:02X}: {:02X}",
            //     addr >> 1,
            //     self.write_index[addr as usize / 2],
            //     v
            // );
            // println!(
            //     "\tOPL3: I/O write [{}][{:02X}] = {:02X}",
            //     addr as usize / 2,
            //     self.write_index[addr as usize / 2],
            //     v
            // );
        }
    }

    fn read(&mut self, addr: u16) -> u8 {
        // println!("OPL3: I/O read at port {:#X}", 0x388 + addr);
        if addr == 0 {
            // println!("\tOPL3: I/O read status");
        }
        0
    }

    fn next_cycles(&self) -> u64 {
        u64::MAX
    }

    fn run(&mut self, _ctx: &mut crate::machine::DeviceMachineContext, _cycles: u64) -> u64 {
        0
    }
}

use super::Bus;

pub struct TestBus<'a> {
    pub ram: &'a mut [u8],
}

impl Bus for TestBus<'_> {
    fn mem_read_u8(&self, ea: u32) -> u8 {
        self.ram[ea as usize]
    }

    fn mem_write_u8(&mut self, ea: u32, v: u8) {
        self.ram[ea as usize] = v;
    }

    fn io_read_u8(&self, _port: u16) -> u8 {
        0xff
    }

    fn io_write_u8(&mut self, _port: u16, _v: u8) {}

    fn io_read_u16(&self, _port: u16) -> u16 {
        0xffff
    }

    fn io_write_u16(&mut self, _port: u16, _v: u16) {}
}

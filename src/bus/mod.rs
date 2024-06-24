pub mod ibm5150_bus;
pub mod test_bus;

pub trait Bus {
    fn mem_read_u8(&self, ea: u32) -> u8;
    fn mem_write_u8(&mut self, ea: u32, v: u8);

    fn io_read_u8(&self, port: u16) -> u8;
    fn io_write_u8(&mut self, port: u16, v: u8);

    fn io_read_u16(&self, port: u16) -> u16;
    fn io_write_u16(&mut self, port: u16, v: u16);
}

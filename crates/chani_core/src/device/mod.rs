use std::any::Any;

pub mod keyboard;
pub mod pic;
pub mod pit;
pub mod vga;

pub trait Device: Send {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn get_name(&self) -> &str;

    fn frequency(&self) -> f64;

    fn read(&mut self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, v: u8);

    fn next_cycles(&self) -> u64;
    fn run(&mut self, ctx: &mut crate::machine::DeviceMachineContext, cycles: u64) -> u64;
}

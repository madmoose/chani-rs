use crate::{address::addr, machine::DosMachineContext};

impl super::Bios {
    pub(super) fn int09_keyboard(&mut self, ctx: &mut DosMachineContext) {
        let ss = ctx.cpu.get_ss();
        let sp = ctx.cpu.get_sp();
        let ip = ctx.memory.read_u16(addr(ss, sp)) - 2;
        let cs = ctx.memory.read_u16(addr(ss, sp + 2));

        println!("BIOS int 09h executed at address {}", addr(cs, ip));

        ctx.cpu.cli();
        ctx.cpu.perform_iret(ctx.memory);
    }
}

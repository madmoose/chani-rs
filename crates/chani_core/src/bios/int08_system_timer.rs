use crate::{
    address::{Address, addr},
    machine::DosMachineContext,
};

const TIMER_LO: Address = addr(0x0040, 0x006c);
const TIMER_HI: Address = addr(0x0040, 0x006e);
const TIMER_OFL: Address = addr(0x0040, 0x0070);

impl super::Bios {
    pub(super) fn int08_system_timer(&mut self, ctx: &mut DosMachineContext) {
        let mut timer_lo = ctx.memory.read_u16(TIMER_LO);
        let mut timer_hi = ctx.memory.read_u16(TIMER_HI);

        timer_lo = timer_lo.wrapping_add(1);
        ctx.memory.write_u16(TIMER_LO, timer_lo);

        if timer_lo == 0 {
            timer_hi = timer_hi.wrapping_add(1);
            ctx.memory.write_u16(TIMER_HI, timer_hi);
        }

        if timer_hi == 0x0018 && timer_lo == 0x00b0 {
            timer_lo = 0;
            timer_hi = 0;

            ctx.memory.write_u16(TIMER_LO, timer_lo);
            ctx.memory.write_u16(TIMER_HI, timer_hi);
            ctx.memory.write_u16(TIMER_OFL, 1);
        }

        // TODO: Call int 1c

        ctx.cpu.perform_iret(ctx.memory);
    }
}

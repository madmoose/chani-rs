use crate::{dos::Dos, machine::DosMachineContext};

impl Dos {
    pub(super) fn int33_0000_reset_driver_and_read_status(&mut self, ctx: &mut DosMachineContext) {
        ctx.cpu.set_ax(0xffff);
        ctx.cpu.set_bx(3);
    }

    pub(super) fn int33_0001_show_mouse_cursor(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0002_hide_mouse_cursor(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0003_return_position_and_button_status(
        &mut self,
        ctx: &mut DosMachineContext,
    ) {
        ctx.cpu.set_bx(0); // Mouse button status
        ctx.cpu.set_cx(0); // Mouse column
        ctx.cpu.set_dx(0); // Mouse row
        // todo!();
    }

    pub(super) fn int33_0004_position_mouse_cursor(&mut self, _ctx: &mut DosMachineContext) {
        // todo!();
    }

    pub(super) fn int33_0005_return_button_press_data(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0006_return_button_release_data(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0007_define_horizontal_cursor_range(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        // todo!();
    }

    pub(super) fn int33_0008_define_vertical_cursor_range(&mut self, _ctx: &mut DosMachineContext) {
        // todo!();
    }

    pub(super) fn int33_0009_define_graphics_cursor(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_000a_define_text_cursor(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_000b_read_motion_counters(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_000c_define_interrupt_subroutine_parameters(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_000d_light_pen_emulation_on(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_000e_light_pen_emulation_off(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_000f_define_mickey_pixel_ratio(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0010_define_screen_region_for_updating(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }
}

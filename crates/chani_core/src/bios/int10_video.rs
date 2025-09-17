use crate::{address::addr, machine::DosMachineContext};

impl super::Bios {
    pub(super) fn int10_video(&mut self, ctx: &mut DosMachineContext) {
        let ah = ctx.cpu.get_ah();
        match ah {
            0x00 => self.int10_00_set_video_mode(ctx),
            0x01 => self.int10_01_set_text_mode_cursor_shape(ctx),
            0x02 => self.int10_02_set_cursor_position(ctx),
            0x03 => self.int10_03_get_cursor_position_and_shape(ctx),
            0x04 => self.int10_04_read_light_pen_position(ctx),
            0x05 => self.int10_05_select_active_display_page(ctx),
            0x06 => self.int10_06_scroll_up_window(ctx),
            0x07 => self.int10_07_scroll_down_window(ctx),
            0x08 => self.int10_08_read_character_and_attribute_at_cursor_position(ctx),
            0x09 => self.int10_09_write_character_and_attribute_at_cursor_position(ctx),
            0x0a => self.int10_0a_write_character_only_at_cursor_position(ctx),
            0x0b => self.int10_0b_set_color(ctx),
            0x0c => self.int10_0c_write_graphics_pixel(ctx),
            0x0d => self.int10_0d_read_graphics_pixel(ctx),
            0x0e => self.int10_0e_teletype_output(ctx),
            0x0f => self.int10_0f_get_current_video_mode(ctx),
            0x11 => self.int10_11_change_text_mode_character_set(ctx),
            0x13 => self.int10_13_write_string(ctx),
            0x1a => self.int10_1a_video_display_combination(ctx),
            _ => {
                println!("Unimplemented int10,{ah:02x}");
                panic!("Unimplemented int10h function");
            }
        }
        ctx.cpu.perform_iret(ctx.memory);
    }

    fn int10_00_set_video_mode(&mut self, ctx: &mut DosMachineContext) {
        let mode = ctx.cpu.get_al();
        println!("INT10: Set video mode {mode:x}");

        ctx.devices.vga_mut().set_mode(mode as i32);
    }

    fn int10_01_set_text_mode_cursor_shape(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_01_set_text_mode_cursor_shape");
    }

    fn int10_02_set_cursor_position(&mut self, ctx: &mut DosMachineContext) {
        self.cursor_pos_x = ctx.cpu.get_dl();
        self.cursor_pos_y = ctx.cpu.get_dh();
    }

    fn int10_03_get_cursor_position_and_shape(&mut self, ctx: &mut DosMachineContext) {
        ctx.cpu.set_cx(0);
        ctx.cpu.set_dx(0);
    }

    fn int10_04_read_light_pen_position(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_04_read_light_pen_position");
    }

    fn int10_05_select_active_display_page(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_05_select_active_display_page");
    }

    fn int10_06_scroll_up_window(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_06_scroll_up_window");
    }

    fn int10_07_scroll_down_window(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_07_scroll_down_window");
    }

    fn int10_08_read_character_and_attribute_at_cursor_position(
        &mut self,
        ctx: &mut DosMachineContext,
    ) {
        ctx.cpu.set_ax(0);
    }

    fn int10_09_write_character_and_attribute_at_cursor_position(
        &mut self,
        ctx: &mut DosMachineContext,
    ) {
        let c = ctx.cpu.get_al();
        for i in 0..ctx.cpu.get_cx() {
            let addr = addr(
                0xb800,
                2 * (self.cols as u16 * self.cursor_pos_y as u16 + self.cursor_pos_x as u16 + i),
            );
            ctx.memory.write_u8(addr, c);
        }
    }

    fn int10_0a_write_character_only_at_cursor_position(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_0a_write_character_only_at_cursor_position");
    }

    fn int10_0b_set_color(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_0b_set_color");
    }

    fn int10_0c_write_graphics_pixel(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_0c_write_graphics_pixel");
    }

    fn int10_0d_read_graphics_pixel(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_0d_read_graphics_pixel");
    }

    fn int10_0e_teletype_output(&mut self, ctx: &mut DosMachineContext) {
        let c = ctx.cpu.get_al();
        print!("{}", if c.is_ascii_graphic() { c as char } else { '.' });
    }

    fn int10_0f_get_current_video_mode(&mut self, ctx: &mut DosMachineContext) {
        ctx.cpu.set_ax((80 << 8) | 3);
        ctx.cpu.set_bx(0);
    }

    fn int10_11_change_text_mode_character_set(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_11_change_text_mode_character_set");
    }

    fn int10_13_write_string(&mut self, _ctx: &mut DosMachineContext) {
        unimplemented!("int10_13_write_string");
    }

    fn int10_1a_video_display_combination(&mut self, ctx: &mut DosMachineContext) {
        match ctx.cpu.get_al() {
            0 => {
                ctx.cpu.set_al(0x1a);
                ctx.cpu.set_bl(0x08); // VGA with analog color display
            }
            sub => {
                println!("int10_1a_video_display_combination: Unhandled subfunction {sub}");
                panic!("Unhandled int10_1a subfunction");
            }
        }
    }
}

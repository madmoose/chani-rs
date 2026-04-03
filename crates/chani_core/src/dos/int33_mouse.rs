use crate::{dos::Dos, machine::DosMachineContext};

impl Dos {
    pub fn int33(&mut self, ctx: &mut DosMachineContext) {
        // let ss = ctx.cpu.get_ss();
        // let sp = ctx.cpu.get_sp();
        // let ip = ctx.memory.read_u16(addr(ss, sp)) - 2;
        // let cs = ctx.memory.read_u16(addr(ss, sp + 2));

        // eprintln!("DOS int 33h callback executed at address {}", addr(cs, ip),);
        let ax = ctx.cpu.get_ax();

        let func = match ax {
            0x0000 => Dos::int33_0000_reset_driver_and_read_status,
            0x0001 => Dos::int33_0001_show_mouse_cursor,
            0x0002 => Dos::int33_0002_hide_mouse_cursor,
            0x0003 => Dos::int33_0003_return_position_and_button_status,
            0x0004 => Dos::int33_0004_position_mouse_cursor,
            0x0005 => Dos::int33_0005_return_button_press_data,
            0x0006 => Dos::int33_0006_return_button_release_data,
            0x0007 => Dos::int33_0007_define_horizontal_cursor_range,
            0x0008 => Dos::int33_0008_define_vertical_cursor_range,
            0x0009 => Dos::int33_0009_define_graphics_cursor,
            0x000a => Dos::int33_000a_define_text_cursor,
            0x000b => Dos::int33_000b_read_motion_counters,
            0x000c => Dos::int33_000c_define_interrupt_subroutine_parameters,
            0x000d => Dos::int33_000d_light_pen_emulation_on,
            0x000e => Dos::int33_000e_light_pen_emulation_off,
            0x000f => Dos::int33_000f_define_mickey_pixel_ratio,
            0x0010 => Dos::int33_0010_define_screen_region_for_updating,
            0x0012 => Dos::int33_0012_set_large_graphics_cursor_block,
            0x0013 => Dos::int33_0013_define_double_speed_threshold,
            0x0014 => Dos::int33_0014_exchange_interrupt_subroutines,
            0x0015 => Dos::int33_0015_return_driver_storage_requirements,
            0x0016 => Dos::int33_0016_save_driver_state,
            0x0017 => Dos::int33_0017_restore_driver_state,
            0x0018 => Dos::int33_0018_set_alternate_mouse_user_handler,
            0x0019 => Dos::int33_0019_return_user_alternate_interrupt_vector,
            0x001a => Dos::int33_001a_set_mouse_sensitivity,
            0x001b => Dos::int33_001b_return_mouse_sensitivity,
            0x001c => Dos::int33_001c_set_interrupt_rate,
            0x001d => Dos::int33_001d_define_display_page_number,
            0x001e => Dos::int33_001e_return_display_page_number,
            0x001f => Dos::int33_001f_disable_mouse_driver,
            0x0020 => Dos::int33_0020_enable_mouse_driver,
            0x0021 => Dos::int33_0021_software_reset,
            0x0022 => Dos::int33_0022_set_language_for_messages,
            0x0023 => Dos::int33_0023_get_language_for_messages,
            0x0024 => Dos::int33_0024_get_software_version_mouse_type_and_irq_number,
            0x0025 => Dos::int33_0025_get_general_driver_information,
            0x0026 => Dos::int33_0026_get_maximum_virtual_coordinates,
            0x0027 => Dos::int33_0027_get_screen_cursor_masks_and_mickey_counts,
            0x0028 => Dos::int33_0028_set_video_mode,
            0x0029 => Dos::int33_0029_enumerate_video_modes,
            0x002a => Dos::int33_002a_get_cursor_hot_spot,
            0x002b => Dos::int33_002b_load_acceleration_profiles,
            0x002c => Dos::int33_002c_get_acceleration_profiles,
            0x002d => Dos::int33_002d_select_acceleration_profile,
            0x002e => Dos::int33_002e_set_acceleration_profile_names,
            0x002f => Dos::int33_002f_mouse_hardware_reset,
            0x0030 => Dos::int33_0030_get_set_ballpoint_information,
            0x0031 => Dos::int33_0031_get_current_minimum_maximum_virtual_coordinates,
            0x0032 => Dos::int33_0032_get_active_advanced_functions,
            0x0033 => Dos::int33_0033_get_switch_settings_and_acceleration_profile_data,
            0x0034 => Dos::int33_0034_get_initialization_file,
            0x0035 => Dos::int33_0035_lcd_screen_large_pointer_support,
            0x004d => Dos::int33_004d_return_pointer_to_copyright_string,
            0x006d => Dos::int33_006d_get_version_string,
            _ => {
                println!("Unknown DOS int 33h function: {ax:#02x}");
                todo!();
            }
        };

        func(self, ctx);

        // println!("DOS int 33h function {ah:#02x} executed successfully.\n");

        ctx.cpu.iret(ctx.memory);
    }

    pub(super) fn int33_0000_reset_driver_and_read_status(&mut self, ctx: &mut DosMachineContext) {
        println!("int33_0000_reset_driver_and_read_status");
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
        ctx.cpu.set_cx(self.mouse.x & 0xfffe); // Mouse column
        ctx.cpu.set_dx(self.mouse.y); // Mouse row
        ctx.cpu.set_bx(self.mouse.buttons); // Mouse button status
        // println!(
        //     "int33_0003_return_position_and_button_status: {:?} {:08x}",
        //     self.mouse,
        //     ctx.cpu.get_cxdx()
        // );
    }

    pub(super) fn int33_0004_position_mouse_cursor(&mut self, ctx: &mut DosMachineContext) {
        self.mouse.x = ctx.cpu.get_cx(); // Mouse column
        self.mouse.y = ctx.cpu.get_dx(); // Mouse row
        println!("int33_0004_position_mouse_cursor");
    }

    pub(super) fn int33_0005_return_button_press_data(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0006_return_button_release_data(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0007_define_horizontal_cursor_range(
        &mut self,
        ctx: &mut DosMachineContext,
    ) {
        println!(
            "int33_0007_define_horizontal_cursor_range: {} - {}",
            ctx.cpu.get_cx(),
            ctx.cpu.get_dx()
        );
    }

    pub(super) fn int33_0008_define_vertical_cursor_range(&mut self, ctx: &mut DosMachineContext) {
        println!(
            "int33_0008_define_vertical_cursor_range: {} - {}",
            ctx.cpu.get_cx(),
            ctx.cpu.get_dx()
        );
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
        // todo!();
    }

    pub(super) fn int33_0010_define_screen_region_for_updating(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0012_set_large_graphics_cursor_block(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0013_define_double_speed_threshold(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        println!("unimplemented: int33_0013_define_double_speed_threshold");
    }

    pub(super) fn int33_0014_exchange_interrupt_subroutines(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0015_return_driver_storage_requirements(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0016_save_driver_state(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0017_restore_driver_state(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0018_set_alternate_mouse_user_handler(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0019_return_user_alternate_interrupt_vector(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_001a_set_mouse_sensitivity(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_001b_return_mouse_sensitivity(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_001c_set_interrupt_rate(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_001d_define_display_page_number(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_001e_return_display_page_number(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_001f_disable_mouse_driver(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0020_enable_mouse_driver(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0021_software_reset(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0022_set_language_for_messages(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0023_get_language_for_messages(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0024_get_software_version_mouse_type_and_irq_number(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0025_get_general_driver_information(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0026_get_maximum_virtual_coordinates(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0027_get_screen_cursor_masks_and_mickey_counts(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0028_set_video_mode(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0029_enumerate_video_modes(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_002a_get_cursor_hot_spot(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_002b_load_acceleration_profiles(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_002c_get_acceleration_profiles(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_002d_select_acceleration_profile(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_002e_set_acceleration_profile_names(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_002f_mouse_hardware_reset(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0030_get_set_ballpoint_information(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0031_get_current_minimum_maximum_virtual_coordinates(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0032_get_active_advanced_functions(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0033_get_switch_settings_and_acceleration_profile_data(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_0034_get_initialization_file(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }

    pub(super) fn int33_0035_lcd_screen_large_pointer_support(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_004d_return_pointer_to_copyright_string(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) {
        todo!();
    }

    pub(super) fn int33_006d_get_version_string(&mut self, _ctx: &mut DosMachineContext) {
        todo!();
    }
}

use std::io::SeekFrom;

use bytes_ext::U16Ext;

use crate::{
    address::addr,
    dos::{Dos, MAX_PATH, OpenMode, error::Error, read_path},
    machine::DosMachineContext,
};

use super::dos_alloc::AllocationError;

type SyscallResult = Result<(), super::error::Error>;

impl Dos {
    pub fn int21(&mut self, ctx: &mut DosMachineContext) {
        let ah = ctx.cpu.get_ah();

        // let ss = ctx.cpu.get_ss();
        // let sp = ctx.cpu.get_sp();
        // let ip = ctx.memory.read_u16(addr(ss, sp)) - 2;
        // let cs = ctx.memory.read_u16(addr(ss, sp + 2));

        // println!(
        //     "DOS int 21h ah={:02x} '{}' executed at address {}",
        //     ah,
        //     int21_name(ah),
        //     addr(cs, ip)
        // );

        self.in_dos += 1;
        self.save_user_state(ctx.cpu);

        let func = match ah {
            0x00 => Dos::int21_00_program_terminate,
            0x01 => Dos::int21_01_character_input,
            0x02 => Dos::int21_02_character_output,
            0x03 => Dos::int21_03_auxiliary_input,
            0x04 => Dos::int21_04_auxiliary_output,
            0x05 => Dos::int21_05_printer_output,
            0x06 => Dos::int21_06_direct_console_io,
            0x07 => Dos::int21_07_direct_console_input_without_echo,
            0x08 => Dos::int21_08_console_input_without_echo,
            0x09 => Dos::int21_09_display_string,
            0x0a => Dos::int21_0a_buffered_keyboard_input,
            0x0b => Dos::int21_0b_get_input_status,
            0x0c => Dos::int21_0c_flush_input_buffer_and_input,
            0x0d => Dos::int21_0d_disk_reset,
            0x0e => Dos::int21_0e_set_default_drive,
            0x0f => Dos::int21_0f_open_file,
            0x10 => Dos::int21_10_close_file,
            0x11 => Dos::int21_11_find_first_file,
            0x12 => Dos::int21_12_find_next_file,
            0x13 => Dos::int21_13_delete_file,
            0x14 => Dos::int21_14_sequential_read,
            0x15 => Dos::int21_15_sequential_write,
            0x16 => Dos::int21_16_create_or_truncate_file,
            0x17 => Dos::int21_17_rename_file,
            0x18 => Dos::int21_18_reserved,
            0x19 => Dos::int21_19_get_default_drive,
            0x1a => Dos::int21_1a_set_disk_transfer_address,
            0x1b => Dos::int21_1b_get_allocation_info_for_default_drive,
            0x1c => Dos::int21_1c_get_allocation_info_for_specified_drive,
            0x1d => Dos::int21_1d_reserved,
            0x1e => Dos::int21_1e_reserved,
            0x1f => Dos::int21_1f_get_disk_parameter_block_for_default_drive,
            0x20 => Dos::int21_20_reserved,
            0x21 => Dos::int21_21_random_read,
            0x22 => Dos::int21_22_random_write,
            0x23 => Dos::int21_23_get_file_size_in_records,
            0x24 => Dos::int21_24_set_random_record_number,
            0x25 => Dos::int21_25_set_interrupt_vector,
            0x26 => Dos::int21_26_create_psp,
            0x27 => Dos::int21_27_random_block_read,
            0x28 => Dos::int21_28_random_block_write,
            0x29 => Dos::int21_29_parse_filename,
            0x2a => Dos::int21_2a_get_date,
            0x2b => Dos::int21_2b_set_date,
            0x2c => Dos::int21_2c_get_time,
            0x2d => Dos::int21_2d_set_time,
            0x2e => Dos::int21_2e_set_verify_flag,
            0x2f => Dos::int21_2f_get_disk_transfer_address,
            0x30 => Dos::int21_30_get_dos_version,
            0x31 => Dos::int21_31_terminate_and_stay_resident,
            0x32 => Dos::int21_32_get_disk_parameter_block_for_specified_drive,
            0x33 => Dos::int21_33_get_or_set_ctrl_break,
            0x34 => Dos::int21_34_get_indos_flag_pointer,
            0x35 => Dos::int21_35_get_interrupt_vector,
            0x36 => Dos::int21_36_get_free_disk_space,
            0x37 => Dos::int21_37_get_or_set_switch_character,
            0x38 => Dos::int21_38_get_or_set_country_info,
            0x39 => Dos::int21_39_create_subdirectory,
            0x3a => Dos::int21_3a_remove_subdirectory,
            0x3b => Dos::int21_3b_change_current_directory,
            0x3c => Dos::int21_3c_create_or_truncate_file,
            0x3d => Dos::int21_3d_open_file,
            0x3e => Dos::int21_3e_close_file,
            0x3f => Dos::int21_3f_read_file_or_device,
            0x40 => Dos::int21_40_write_file_or_device,
            0x41 => Dos::int21_41_delete_file,
            0x42 => Dos::int21_42_move_file_pointer,
            0x43 => Dos::int21_43_get_or_set_file_attributes,
            0x44 => Dos::int21_44_io_control_for_devices,
            0x45 => Dos::int21_45_duplicate_handle,
            0x46 => Dos::int21_46_redirect_handle,
            0x47 => Dos::int21_47_get_current_directory,
            0x48 => Dos::int21_48_allocate_memory,
            0x49 => Dos::int21_49_release_memory,
            0x4a => Dos::int21_4a_reallocate_memory,
            0x4b => Dos::int21_4b_execute_program,
            0x4c => Dos::int21_4c_terminate_with_return_code,
            0x4d => Dos::int21_4d_get_program_return_code,
            0x4e => Dos::int21_4e_find_first_file,
            0x4f => Dos::int21_4f_find_next_file,
            0x50 => Dos::int21_50_set_current_psp,
            0x51 => Dos::int21_51_get_current_psp,
            0x52 => Dos::int21_52_get_dos_internal_pointers,
            0x53 => Dos::int21_53_create_disk_parameter_block,
            0x54 => Dos::int21_54_get_verify_flag,
            0x55 => Dos::int21_55_create_program_psp,
            0x56 => Dos::int21_56_rename_file,
            0x57 => Dos::int21_57_get_or_set_file_date_and_time,
            0x58 => Dos::int21_58_get_or_set_allocation_strategy,
            0x59 => Dos::int21_59_get_extended_error_info,
            0x5a => Dos::int21_5a_create_unique_file,
            0x5b => Dos::int21_5b_create_new_file,
            0x5c => Dos::int21_5c_lock_or_unlock_file,
            0x5d => Dos::int21_5d_file_sharing_functions,
            0x5e => Dos::int21_5e_network_functions,
            0x5f => Dos::int21_5f_network_redirection_functions,
            0x60 => Dos::int21_60_qualify_filename,
            0x61 => Dos::int21_61_reserved,
            0x62 => Dos::int21_62_get_current_psp,
            0x63 => Dos::int21_63_get_dbcs_lead_byte_table_pointer,
            0x64 => Dos::int21_64_set_wait_for_external_event_flag,
            0x65 => Dos::int21_65_get_extended_country_info,
            0x66 => Dos::int21_66_get_or_set_code_page,
            0x67 => Dos::int21_67_set_handle_count,
            0x68 => Dos::int21_68_commit_file,
            0x69 => Dos::int21_69_get_or_set_media_id,
            0x6a => Dos::int21_6a_commit_file,
            0x6b => Dos::int21_6b_reserved,
            0x6c => Dos::int21_6c_extended_open_create_file,
            _ => {
                println!("Unknown DOS int 21h function: {ah:#02x}");
                todo!();
            }
        };

        let res = func(self, ctx);

        match res {
            Ok(()) => {
                // Clear error indicator in the carry flag
                self.clc();

                self.return_from_dos(ctx);

                // println!("DOS int 21h function {ah:#02x} executed successfully.\n");
            }
            Err(error_code) => {
                // Set error indicator in the carry flag
                self.stc();
                self.user_regs.ax = error_code.to_dos_error_code();

                self.return_from_dos(ctx);

                // println!("DOS int 21h function {ah:#02x} executed with error code {error_code:?}.");
            }
        }
    }

    fn return_from_dos(&mut self, ctx: &mut DosMachineContext<'_>) {
        self.in_dos -= 1;
        self.restore_user_state(ctx.cpu);

        // Write the updated flags to the stack, iret will move the flags to the flags register
        let flags_addr = addr(ctx.cpu.get_ss(), ctx.cpu.get_sp() + 4);
        ctx.memory.write_u16(flags_addr, self.user_regs.flags);

        ctx.cpu.iret(ctx.memory);
        ctx.cpu.sti();
    }

    pub(super) fn int21_00_program_terminate(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_01_character_input(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_02_character_output(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_03_auxiliary_input(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_04_auxiliary_output(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_05_printer_output(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_06_direct_console_io(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_07_direct_console_input_without_echo(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_08_console_input_without_echo(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_09_display_string(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0a_buffered_keyboard_input(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0b_get_input_status(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0c_flush_input_buffer_and_input(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0d_disk_reset(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0e_set_default_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_0f_open_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_10_close_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_11_find_first_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_12_find_next_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_13_delete_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_14_sequential_read(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_15_sequential_write(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_16_create_or_truncate_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_17_rename_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_18_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_19_get_default_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        self.user_regs.ax.set_lo(2);

        Ok(())
    }

    pub(super) fn int21_1a_set_disk_transfer_address(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_1b_get_allocation_info_for_default_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_1c_get_allocation_info_for_specified_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_1d_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_1e_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_1f_get_disk_parameter_block_for_default_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_20_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_21_random_read(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_22_random_write(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_23_get_file_size_in_records(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_24_set_random_record_number(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_25_set_interrupt_vector(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_26_create_psp(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_27_random_block_read(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_28_random_block_write(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_29_parse_filename(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2a_get_date(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2b_set_date(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2c_get_time(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2d_set_time(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2e_set_verify_flag(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_2f_get_disk_transfer_address(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_30_get_dos_version(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_31_terminate_and_stay_resident(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_32_get_disk_parameter_block_for_specified_drive(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_33_get_or_set_ctrl_break(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        match self.user_regs.get_al() {
            0x00 => {
                self.user_regs.set_dl(self.ctrl_break as u8);
                Ok(())
            }
            0x01 => {
                self.ctrl_break = self.user_regs.get_dl() != 0;
                Ok(())
            }
            _ => {
                panic!(
                    "Unhandled function {} in int21_33_get_or_set_ctrl_break",
                    self.user_regs.get_al()
                )
            }
        }
    }

    pub(super) fn int21_34_get_indos_flag_pointer(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_35_get_interrupt_vector(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let num = self.user_regs.get_al();
        println!("num: {num:#x}");

        let offset = 4 * num as u16;
        println!("offset: = {:#x}", offset);

        // IVT is at segment 0, offset = 4*num
        self.user_regs.bx = ctx.memory.read_u16(addr(0, offset));
        self.user_regs.es = ctx.memory.read_u16(addr(0, offset + 2));
        ctx.memory.hexdump(addr(0, 0), 256 * 4);

        Ok(())
    }

    pub(super) fn int21_36_get_free_disk_space(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_37_get_or_set_switch_character(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_38_get_or_set_country_info(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_39_create_subdirectory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_3a_remove_subdirectory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_3b_change_current_directory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_3c_create_or_truncate_file(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let path_addr = self.user_regs.get_dsdx_as_addr();
        let mut path = [0u8; MAX_PATH];

        let Some(path) = read_path(&mut path, ctx.memory, path_addr) else {
            return Err(Error::FileNotFound);
        };

        // TODO: Truncate file
        let fd = self.open(ctx, path, OpenMode::ReadWrite)?;

        self.user_regs.ax = fd + 2;
        Ok(())
    }

    pub(super) fn int21_3d_open_file(&mut self, ctx: &mut DosMachineContext) -> SyscallResult {
        let path_addr = self.user_regs.get_dsdx_as_addr();
        let mut path = [0u8; MAX_PATH];

        let Some(path) = read_path(&mut path, ctx.memory, path_addr) else {
            return Err(Error::FileNotFound);
        };

        let fd = self.open(ctx, path, OpenMode::ReadWrite)?;

        self.user_regs.ax = fd + 2;
        Ok(())
    }

    pub(super) fn int21_3e_close_file(&mut self, ctx: &mut DosMachineContext) -> SyscallResult {
        let fd = self.user_regs.bx - 2;

        self.close(ctx, fd)?;

        Ok(())
    }

    pub(super) fn int21_3f_read_file_or_device(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let fd = ctx.cpu.get_bx() - 2;
        let bytes_to_read = ctx.cpu.get_cx();
        let dst = addr(ctx.cpu.get_ds(), ctx.cpu.get_dx());

        let bytes_read = self.read(ctx, fd, dst, bytes_to_read as u32)?;

        // ctx.memory.hexdump(dst, bytes_read);

        self.user_regs.ax = bytes_read as u16;

        Ok(())
    }

    pub(super) fn int21_40_write_file_or_device(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let fd = ctx.cpu.get_bx() - 2;
        let bytes_to_write = ctx.cpu.get_cx();
        let src = addr(ctx.cpu.get_ds(), ctx.cpu.get_dx());

        let bytes_written = self.write(ctx, fd, src, bytes_to_write as u32)?;

        // ctx.memory.hexdump(src, bytes_written);

        self.user_regs.ax = bytes_written as u16;

        Ok(())
    }

    pub(super) fn int21_41_delete_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_42_move_file_pointer(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let origin = self.user_regs.get_al();
        let fd = self.user_regs.bx - 2;
        let pos = self.user_regs.get_cxdx_as_u32();

        // println!("int21_42_move_file_pointer: origin: {origin}, fd: {fd}, pos: {pos}");

        let position = match origin {
            0 => SeekFrom::Start(pos as u64),
            1 => SeekFrom::Current(pos as i32 as i64),
            2 => SeekFrom::End(pos as i32 as i64),
            _ => return Err(Error::InvalidFunction),
        };

        let new_location = ctx
            .file_system_manager
            .seek(fd, position)
            .map_err(|_| Error::InvalidHandle)?;

        // println!("New file position: {new_location}");

        self.user_regs.set_dxax_as_u32(new_location as u32);

        Ok(())
    }

    pub(super) fn int21_43_get_or_set_file_attributes(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_44_io_control_for_devices(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_45_duplicate_handle(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_46_redirect_handle(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_47_get_current_directory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_48_allocate_memory(
        &mut self,
        ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        let requested_paragraphs = ctx.cpu.get_bx();

        match self.allocate(ctx.memory, requested_paragraphs) {
            Ok(_) => todo!(),
            Err(AllocationError::NotEnoughMemory { largest_available }) => {
                self.user_regs.bx = largest_available;
                Err(super::error::Error::NotEnoughMemory)
            }
            Err(AllocationError::MemoryArenaTrashed) => Err(super::error::Error::ArenaTrashed),
        }
    }

    pub(super) fn int21_49_release_memory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4a_reallocate_memory(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4b_execute_program(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4c_terminate_with_return_code(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4d_get_program_return_code(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4e_find_first_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_4f_find_next_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_50_set_current_psp(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_51_get_current_psp(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_52_get_dos_internal_pointers(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_53_create_disk_parameter_block(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_54_get_verify_flag(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_55_create_program_psp(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_56_rename_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_57_get_or_set_file_date_and_time(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_58_get_or_set_allocation_strategy(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_59_get_extended_error_info(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5a_create_unique_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5b_create_new_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5c_lock_or_unlock_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5d_file_sharing_functions(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5e_network_functions(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_5f_network_redirection_functions(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_60_qualify_filename(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_61_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_62_get_current_psp(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_63_get_dbcs_lead_byte_table_pointer(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_64_set_wait_for_external_event_flag(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_65_get_extended_country_info(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_66_get_or_set_code_page(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_67_set_handle_count(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_68_commit_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_69_get_or_set_media_id(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_6a_commit_file(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_6b_reserved(&mut self, _ctx: &mut DosMachineContext) -> SyscallResult {
        todo!();
    }

    pub(super) fn int21_6c_extended_open_create_file(
        &mut self,
        _ctx: &mut DosMachineContext,
    ) -> SyscallResult {
        todo!();
    }
}

fn int21_name(ah: u8) -> &'static str {
    match ah {
        0x00 => "int21_00_program_terminate",
        0x01 => "int21_01_character_input",
        0x02 => "int21_02_character_output",
        0x03 => "int21_03_auxiliary_input",
        0x04 => "int21_04_auxiliary_output",
        0x05 => "int21_05_printer_output",
        0x06 => "int21_06_direct_console_io",
        0x07 => "int21_07_direct_console_input_without_echo",
        0x08 => "int21_08_console_input_without_echo",
        0x09 => "int21_09_display_string",
        0x0a => "int21_0a_buffered_keyboard_input",
        0x0b => "int21_0b_get_input_status",
        0x0c => "int21_0c_flush_input_buffer_and_input",
        0x0d => "int21_0d_disk_reset",
        0x0e => "int21_0e_set_default_drive",
        0x0f => "int21_0f_open_file",
        0x10 => "int21_10_close_file",
        0x11 => "int21_11_find_first_file",
        0x12 => "int21_12_find_next_file",
        0x13 => "int21_13_delete_file",
        0x14 => "int21_14_sequential_read",
        0x15 => "int21_15_sequential_write",
        0x16 => "int21_16_create_or_truncate_file",
        0x17 => "int21_17_rename_file",
        0x18 => "int21_18_reserved",
        0x19 => "int21_19_get_default_drive",
        0x1a => "int21_1a_set_disk_transfer_address",
        0x1b => "int21_1b_get_allocation_info_for_default_drive",
        0x1c => "int21_1c_get_allocation_info_for_specified_drive",
        0x1d => "int21_1d_reserved",
        0x1e => "int21_1e_reserved",
        0x1f => "int21_1f_get_disk_parameter_block_for_default_drive",
        0x20 => "int21_20_reserved",
        0x21 => "int21_21_random_read",
        0x22 => "int21_22_random_write",
        0x23 => "int21_23_get_file_size_in_records",
        0x24 => "int21_24_set_random_record_number",
        0x25 => "int21_25_set_interrupt_vector",
        0x26 => "int21_26_create_psp",
        0x27 => "int21_27_random_block_read",
        0x28 => "int21_28_random_block_write",
        0x29 => "int21_29_parse_filename",
        0x2a => "int21_2a_get_date",
        0x2b => "int21_2b_set_date",
        0x2c => "int21_2c_get_time",
        0x2d => "int21_2d_set_time",
        0x2e => "int21_2e_set_verify_flag",
        0x2f => "int21_2f_get_disk_transfer_address",
        0x30 => "int21_30_get_dos_version",
        0x31 => "int21_31_terminate_and_stay_resident",
        0x32 => "int21_32_get_disk_parameter_block_for_specified_drive",
        0x33 => "int21_33_get_or_set_ctrl_break",
        0x34 => "int21_34_get_indos_flag_pointer",
        0x35 => "int21_35_get_interrupt_vector",
        0x36 => "int21_36_get_free_disk_space",
        0x37 => "int21_37_get_or_set_switch_character",
        0x38 => "int21_38_get_or_set_country_info",
        0x39 => "int21_39_create_subdirectory",
        0x3a => "int21_3a_remove_subdirectory",
        0x3b => "int21_3b_change_current_directory",
        0x3c => "int21_3c_create_or_truncate_file",
        0x3d => "int21_3d_open_file",
        0x3e => "int21_3e_close_file",
        0x3f => "int21_3f_read_file_or_device",
        0x40 => "int21_40_write_file_or_device",
        0x41 => "int21_41_delete_file",
        0x42 => "int21_42_move_file_pointer",
        0x43 => "int21_43_get_or_set_file_attributes",
        0x44 => "int21_44_io_control_for_devices",
        0x45 => "int21_45_duplicate_handle",
        0x46 => "int21_46_redirect_handle",
        0x47 => "int21_47_get_current_directory",
        0x48 => "int21_48_allocate_memory",
        0x49 => "int21_49_release_memory",
        0x4a => "int21_4a_reallocate_memory",
        0x4b => "int21_4b_execute_program",
        0x4c => "int21_4c_terminate_with_return_code",
        0x4d => "int21_4d_get_program_return_code",
        0x4e => "int21_4e_find_first_file",
        0x4f => "int21_4f_find_next_file",
        0x50 => "int21_50_set_current_psp",
        0x51 => "int21_51_get_current_psp",
        0x52 => "int21_52_get_dos_internal_pointers",
        0x53 => "int21_53_create_disk_parameter_block",
        0x54 => "int21_54_get_verify_flag",
        0x55 => "int21_55_create_program_psp",
        0x56 => "int21_56_rename_file",
        0x57 => "int21_57_get_or_set_file_date_and_time",
        0x58 => "int21_58_get_or_set_allocation_strategy",
        0x59 => "int21_59_get_extended_error_info",
        0x5a => "int21_5a_create_unique_file",
        0x5b => "int21_5b_create_new_file",
        0x5c => "int21_5c_lock_or_unlock_file",
        0x5d => "int21_5d_file_sharing_functions",
        0x5e => "int21_5e_network_functions",
        0x5f => "int21_5f_network_redirection_functions",
        0x60 => "int21_60_qualify_filename",
        0x61 => "int21_61_reserved",
        0x62 => "int21_62_get_current_psp",
        0x63 => "int21_63_get_dbcs_lead_byte_table_pointer",
        0x64 => "int21_64_set_wait_for_external_event_flag",
        0x65 => "int21_65_get_extended_country_info",
        0x66 => "int21_66_get_or_set_code_page",
        0x67 => "int21_67_set_handle_count",
        0x68 => "int21_68_commit_file",
        0x69 => "int21_69_get_or_set_media_id",
        0x6a => "int21_6a_commit_file",
        0x6b => "int21_6b_reserved",
        0x6c => "int21_6c_extended_open_create_file",
        _ => "unknown",
    }
}

use std::io::SeekFrom;

use crate::{
    address::{Address, IntoEffectiveAddress, addr},
    machine::DosMachineContext,
};

use super::{Dos, error::Error};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpenMode {
    ReadOnly,
    WriteOnly,
    ReadWrite,
}

const ATTR_READ_ONLY: u8 = 0x01;
const ATTR_HIDDEN: u8 = 0x02;
const ATTR_SYSTEM: u8 = 0x04;
const ATTR_VOLUME_ID: u8 = 0x08;
const ATTR_DIRECTORY: u8 = 0x10;
const ATTR_ARCHIVE: u8 = 0x20;
const ATTR_DEVICE: u8 = 0x40;
const ATTR_ALL: u8 = ATTR_HIDDEN | ATTR_SYSTEM | ATTR_DIRECTORY;
const ATTR_IGNORE: u8 = ATTR_READ_ONLY | ATTR_ARCHIVE | ATTR_DEVICE;
const ATTR_CHANGEABLE: u8 = ATTR_READ_ONLY | ATTR_HIDDEN | ATTR_SYSTEM | ATTR_ARCHIVE;

impl Dos {
    pub fn create(&mut self, _path: &[u8], _attribute: u8) {
        todo!();
    }

    pub fn open(
        &mut self,
        ctx: &mut DosMachineContext,
        path: &[u8],
        _mode: OpenMode,
    ) -> Result<u16, Error> {
        ctx.file_system_manager
            .open(path)
            .map_err(|_| Error::FileNotFound)
    }

    pub fn close(&mut self, ctx: &mut DosMachineContext, fd: u16) -> Result<(), Error> {
        ctx.file_system_manager
            .close(fd)
            .map_err(|_| Error::InvalidHandle)
    }

    pub fn read_bytes(
        &mut self,
        ctx: &mut DosMachineContext,
        fd: u16,
        buffer: &mut [u8],
    ) -> Result<u32, Error> {
        let bytes_to_read = buffer.len();

        let bytes_read = ctx
            .file_system_manager
            .read(fd, &mut buffer[0..bytes_to_read])
            .map_err(|_| Error::InvalidHandle)?;

        Ok(bytes_read as u32)
    }

    pub fn read(
        &mut self,
        ctx: &mut DosMachineContext,
        fd: u16,
        address: Address,
        bytes_to_read: u32,
    ) -> Result<u32, Error> {
        let total_bytes_to_read = bytes_to_read as usize;
        let mut address_ea = address.into_ea();
        let mut buffer = [0u8; 4096];
        let mut total_bytes_read = 0;

        while total_bytes_read != total_bytes_to_read {
            let bytes_remaining = total_bytes_to_read - total_bytes_read;
            let bytes_to_read = usize::min(buffer.len(), bytes_remaining);

            let bytes_read = ctx
                .file_system_manager
                .read(fd, &mut buffer[0..bytes_to_read])
                .map_err(|_| Error::InvalidHandle)?;

            if bytes_read == 0 {
                break;
            }

            let seg = (address_ea >> 4) as u16;
            let ofs = (address_ea & 0xf) as u16;
            ctx.memory
                .write_bytes(addr(seg, ofs), &buffer[0..bytes_read]);

            address_ea += bytes_read as u32;

            total_bytes_read += bytes_read;
        }

        // ctx.memory.hexdump(address, total_bytes_read as u32);

        Ok(total_bytes_read as u32)
    }

    pub fn write(
        &mut self,
        ctx: &mut DosMachineContext,
        fd: u16,
        address: Address,
        bytes_to_write: u32,
    ) -> Result<u32, Error> {
        let total_bytes_to_write = bytes_to_write as usize;
        let mut address_ea = address.into_ea();
        let mut buffer = [0u8; 4096];
        let mut total_bytes_written = 0;

        while total_bytes_written != total_bytes_to_write {
            let bytes_remaining = total_bytes_to_write - total_bytes_written;
            let bytes_to_write = usize::min(buffer.len(), bytes_remaining);

            ctx.memory
                .read_bytes(Address::from(address_ea), &mut buffer[0..bytes_to_write]);

            let bytes_written = ctx
                .file_system_manager
                .write(fd, &buffer[0..bytes_to_write])
                .map_err(|_| Error::InvalidHandle)?;

            if bytes_written == 0 {
                break;
            }

            address_ea += bytes_written as u32;

            total_bytes_written += bytes_written;
        }

        Ok(total_bytes_written as u32)
    }

    pub fn seek(
        &mut self,
        ctx: &mut DosMachineContext,
        fd: u16,
        position: SeekFrom,
    ) -> Result<u64, Error> {
        ctx.file_system_manager
            .seek(fd, position)
            .map_err(|_| Error::InvalidHandle)
    }
}

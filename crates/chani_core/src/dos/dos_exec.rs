use std::io::{self, Cursor, Read, SeekFrom};

use bytes_ext::ReadBytesExt;

use crate::{
    address::addr,
    dos::{
        dos_alloc::AllocationError, dos_file::OpenMode, environment::get_environment_length,
        pdb::PDB_SIZE,
    },
    machine::DosMachineContext,
    memory::Memory,
};

use super::Dos;

pub enum ExecFunction {
    LoadAndExecute,
    Load,
    LoadOverlay,
}

pub struct LoadArgs {
    pub environ: u16,
    pub com_line: u16,
    // pub fcb_5c: u16,
    // pub fcb_6c: u16,
}

pub struct LoadReturnArgs {
    pub sp: u16,
    pub ss: u16,
    pub ip: u16,
    pub cs: u16,
}

pub struct LoadOverlayArgs {
    pub load_addr: u16,
    pub reloc_fac: u16,
}

#[derive(Debug)]
pub enum ExecError {
    InvalidFunction,
    BadFormat,
    BadEnvironment,
    NotEnoughMemory,
    FileNotFound,
}

const VALID_SIGNATURES: [u16; 2] = [0x5a4d, 0x4d5a];

struct ExeHeader {
    signature: u16,   // must contain 4D5A  (yay zibo!)
    len_mod_512: u16, // low 9 bits of length
    pages: u16,       // number of 512b pages in file
    rle_count: u16,   // count of reloc entries
    par_dir: u16,     // number of paragraphs before image
    min_bss: u16,     // minimum number of para of BSS
    max_bss: u16,     // max number of para of BSS
    ss: u16,          // stack of image
    sp: u16,          // SP of image
    chksum: u16,      // checksum  of file (ignored)
    ip: u16,          // IP of entry
    cs: u16,          // CS of entry
    rle_table: u16,   // byte offset of reloc table
    iov: u16,
    sym_tab: u32,
}

impl ExeHeader {
    const SIZE: usize = 32;

    fn read(buf: &[u8]) -> std::io::Result<Self> {
        fn read_u16<R: Read>(r: &mut R) -> std::io::Result<u16> {
            let mut buf = [0u8; 2];
            r.read_exact(&mut buf)?;
            Ok(u16::from_le_bytes(buf))
        }
        fn read_u32<R: Read>(r: &mut R) -> std::io::Result<u32> {
            let mut buf = [0u8; 4];
            r.read_exact(&mut buf)?;
            Ok(u32::from_le_bytes(buf))
        }

        if buf.len() < ExeHeader::SIZE {
            return Err(io::Error::from(io::ErrorKind::InvalidData));
        }

        let r = &mut Cursor::new(buf);

        let signature = read_u16(r)?;
        if !VALID_SIGNATURES.contains(&signature) {
            return Err(io::Error::from(io::ErrorKind::InvalidData));
        }

        Ok(ExeHeader {
            signature,
            len_mod_512: read_u16(r)?,
            pages: read_u16(r)?,
            rle_count: read_u16(r)?,
            par_dir: read_u16(r)?,
            min_bss: read_u16(r)?,
            max_bss: read_u16(r)?,
            ss: read_u16(r)?,
            sp: read_u16(r)?,
            chksum: read_u16(r)?,
            ip: read_u16(r)?,
            cs: read_u16(r)?,
            rle_table: read_u16(r)?,
            iov: read_u16(r)?,
            sym_tab: read_u32(r)?,
        })
    }

    // Writes the header fields to stdout in hex
    pub fn dump(&self) {
        println!("ExeHeader:");
        println!("  Signature: 0x{:04X}", self.signature);
        println!("  Length mod 512: {}", self.len_mod_512);
        println!("  Pages: {}", self.pages);
        println!("  Reloc count: {}", self.rle_count);
        println!("  Paragraphs before image: {}", self.par_dir);
        println!("  Min BSS: {}", self.min_bss);
        println!("  Max BSS: {}", self.max_bss);
        println!("  Stack segment: {}", self.ss);
        println!("  Stack pointer: {}", self.sp);
        println!("  Checksum: {}", self.chksum);
        println!("  Instruction pointer: {}", self.ip);
        println!("  Code segment: {}", self.cs);
        println!("  Reloc table offset: {}", self.rle_table);
    }
}

pub type ExecEnv = u16;

pub enum ExecArgs<'a> {
    Seg(u16),
    Str(&'a str),
}

impl Dos {
    pub fn exec_load_and_execute(
        &mut self,
        ctx: &mut DosMachineContext,
        path: &[u8],
        env: ExecEnv,
        args: ExecArgs<'_>,
    ) -> Result<(), ExecError> {
        let load_result = self.exec_load_without_executing(ctx, path, env, args)?;

        ctx.cpu.set_cx(0xff);
        ctx.cpu.set_dx(0x0000);
        ctx.cpu.set_di(0x0800);
        ctx.cpu.set_bp(0x091c);
        ctx.cpu.set_if(true);
        ctx.cpu.set_si(0x570c);

        ctx.cpu.set_sp(load_result.sp);
        ctx.cpu.set_ss(load_result.ss);
        ctx.cpu.set_ip(load_result.ip);
        ctx.cpu.set_cs(load_result.cs);

        Ok(())
    }

    /// Loads an executable file without executing it.
    ///
    /// TODO: Check the errors returned.
    /// TODO: Close the exe file if loading fails
    pub fn exec_load_without_executing(
        &mut self,
        ctx: &mut DosMachineContext,
        path: &[u8],
        env: ExecEnv,
        args: ExecArgs<'_>,
    ) -> Result<LoadReturnArgs, ExecError> {
        ctx.memory.disable_logging();

        let _new_env = if env != 0 {
            let old_env = env;
            let Some(env_len) = get_environment_length(ctx.memory, old_env) else {
                return Err(ExecError::BadEnvironment);
            };
            let env_len_paras = (env_len + 15) >> 4;

            let new_env = self
                .allocate(ctx.memory, env_len_paras)
                .map_err(|_| ExecError::NotEnoughMemory)?;

            // Copy the environment
            for i in 0..env_len {
                ctx.memory[(new_env, i)] = ctx.memory[(old_env, i)];
            }
            new_env
        } else {
            // todo!();
            0
        };

        let fd = self
            .open(ctx, path, OpenMode::ReadOnly)
            .map_err(|_| ExecError::FileNotFound)?;

        let mut buffer = [0u8; ExeHeader::SIZE];
        let bytes_read = self
            .read_bytes(ctx, fd, &mut buffer)
            .map_err(|_| ExecError::BadFormat)? as usize;

        // If we read the correct number of bytes, parse the exe header,
        // otherwise try to load as a com file.
        //
        // ExeHeader::read checks the buf size, and the exe signature.
        if let Ok(header) = ExeHeader::read(&buffer[0..bytes_read]) {
            // header.dump();
            let load_high = header.min_bss == 0 && header.max_bss == 0;
            let image_size_paras = (header.pages << 5) - header.par_dir;
            println!("image_size_paras: {image_size_paras}");

            let paras_available = match self.allocate(ctx.memory, 0xffff) {
                Ok(_) => panic!("Should have failed"),
                Err(AllocationError::MemoryArenaTrashed) => panic!("Arena trashed"),
                Err(AllocationError::NotEnoughMemory { largest_available }) => {
                    largest_available as u32
                }
            };

            let psp_and_image_size = image_size_paras as u32 + (PDB_SIZE >> 4);

            let paras_required = psp_and_image_size + header.min_bss as u32;
            if paras_required > paras_available {
                return Err(ExecError::NotEnoughMemory);
            }

            let alloc_paras = if !load_high {
                u32::min(psp_and_image_size + header.max_bss as u32, paras_available)
            } else {
                paras_available
            } as u16;

            let alloc_seg = self.allocate(ctx.memory, alloc_paras)
                .unwrap_or_else(|_| panic!("DOS::exec_load_without_executing: Max allocation size was previously checked to be {paras_available} paras but failed to allocate {alloc_paras} paras."));

            let _psp_seg = if !load_high {
                alloc_seg
            } else {
                alloc_seg + alloc_paras - (psp_and_image_size as u16)
            };
            let psp_seg = 0x017e - 0x10;
            let image_seg = psp_seg + 0x10;

            match args {
                ExecArgs::Seg(_seg) => {
                    todo!();
                }
                ExecArgs::Str(str) => {
                    if let Some(ascii_str) = str.as_ascii() {
                        assert!(ascii_str.len() < 127);

                        ctx.memory
                            .write_u8(addr(psp_seg, 0x80), ascii_str.len() as u8);

                        for (ofs, &c) in ascii_str.iter().enumerate() {
                            ctx.memory
                                .write_u8(addr(psp_seg, 0x81 + ofs as u16), c as u8);
                        }

                        ctx.memory
                            .write_u8(addr(psp_seg, 0x81 + (ascii_str.len() as u16)), 0x0d);
                    }
                }
            }

            let pos = SeekFrom::Start((header.par_dir as u64) << 4);
            self.seek(ctx, fd, pos).map_err(|_| ExecError::BadFormat)?;

            let mut load_bytes = (image_size_paras as u32) << 4;
            if header.len_mod_512 != 0 {
                load_bytes = load_bytes - 512 + (header.len_mod_512 as u32);
            }

            println!("load_bytes: {load_bytes:#x}");
            self.read(ctx, fd, addr(image_seg, 0), load_bytes)
                .map_err(|_| ExecError::BadFormat)?;

            // Apply relocations in chunks
            const RLE_ENTRY_SIZE: usize = 4;
            const RLE_ENTRIES_PER_CHUNK: usize = 32;
            const RLE_CHUNK_SIZE: usize = RLE_ENTRY_SIZE * RLE_ENTRIES_PER_CHUNK;
            let rle_count = header.rle_count as usize;
            for i in (0..rle_count).step_by(RLE_ENTRIES_PER_CHUNK) {
                let mut rle_buffer = [0u8; RLE_CHUNK_SIZE];

                let seek_offset = header.rle_table as u64 + (i * RLE_ENTRY_SIZE) as u64;

                self.seek(ctx, fd, SeekFrom::Start(seek_offset))
                    .expect("Failed to seek to reloc table");

                self.read_bytes(ctx, fd, &mut rle_buffer)
                    .expect("Failed to read reloc table");

                let mut c = Cursor::new(rle_buffer);
                for _ in 0..(usize::min(RLE_ENTRIES_PER_CHUNK, rle_count - i)) {
                    let ofs = c.read_le_u16().unwrap();
                    let seg = c.read_le_u16().unwrap();
                    let rel_addr = addr(seg + image_seg, ofs);

                    let v = ctx.memory.read_u16(rel_addr);

                    // println!(
                    //     "relocating: {}: {:04x} -> {:04x}",
                    //     addr(seg, ofs),
                    //     v,
                    //     v + image_seg
                    // );

                    ctx.memory.write_u16(rel_addr, v + image_seg);
                }
            }

            ctx.memory.write_u16(addr(psp_seg, 2), 0x9fff);

            ctx.cpu.set_ds(psp_seg);
            ctx.cpu.set_es(psp_seg);

            ctx.memory.enable_logging();

            return Ok(LoadReturnArgs {
                sp: header.sp,
                ss: header.ss + image_seg,
                ip: header.ip,
                cs: header.cs + image_seg,
            });
        }

        todo!();
    }

    pub fn exec_load_overlay(
        &mut self,
        _memory: &mut Memory,
        _args: LoadOverlayArgs,
    ) -> Result<(), ExecError> {
        todo!();
    }
}

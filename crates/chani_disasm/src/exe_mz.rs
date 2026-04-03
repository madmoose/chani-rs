use std::io::{self, Cursor, Read, Seek, SeekFrom, Write};

use bytes_ext::ReadBytesExt;

pub const SIG_MZ: u16 = 0x4d5a;
pub const SIG_NE: u16 = 0x4e45;

#[derive(Debug, Default, Clone)]
pub struct ExeMzHeader {
    pub magic: u16,      // Magic number
    pub cblp: u16,       // Bytes on last page of file
    pub cp: u16,         // Pages in file
    pub crlc: u16,       // Relocations
    pub cparhdr: u16,    // Size of header in paragraphs
    pub minalloc: u16,   // Minimum extra paragraphs needed
    pub maxalloc: u16,   // Maximum extra paragraphs needed
    pub ss: u16,         // Initial (relative) SS value
    pub sp: u16,         // Initial SP value
    pub csum: u16,       // Checksum
    pub ip: u16,         // Initial IP value
    pub cs: u16,         // Initial (relative) CS value
    pub lfarlc: u16,     // File address of relocation table
    pub ovno: u16,       // Overlay number
    pub res: [u16; 4],   // Reserved words
    pub oemid: u16,      // OEM identifier (for oeminfo)
    pub oeminfo: u16,    // OEM information; oemid specific
    pub res2: [u16; 10], // Reserved words
    pub lfanew: u32,     // File address of new exe header
}

fn remaining(r: &Cursor<&[u8]>) -> usize {
    r.get_ref().len().saturating_sub(r.position() as usize)
}

impl ExeMzHeader {
    pub fn load(&mut self, r: &mut Cursor<&[u8]>) -> io::Result<()> {
        if remaining(r) < 28 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Not a valid executable: Size too small",
            ));
        }

        self.magic = r.read_be_u16()?;

        if self.magic != SIG_MZ && self.magic != SIG_NE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Not a valid executable: Header magic not MZ or NE.",
            ));
        }

        self.cblp = r.read_le_u16()?;
        self.cp = r.read_le_u16()?;
        self.crlc = r.read_le_u16()?;
        self.cparhdr = r.read_le_u16()?;
        self.minalloc = r.read_le_u16()?;
        self.maxalloc = r.read_le_u16()?;
        self.ss = r.read_le_u16()?;
        self.sp = r.read_le_u16()?;
        self.csum = r.read_le_u16()?;
        self.ip = r.read_le_u16()?;
        self.cs = r.read_le_u16()?;
        self.lfarlc = r.read_le_u16()?;
        self.ovno = r.read_le_u16()?;

        if self.lfarlc == 0x40 {
            if remaining(r) < 36 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Not a valid executable: Size too small",
                ));
            }
            for i in 0..4 {
                self.res[i] = r.read_le_u16()?;
            }
            self.oemid = r.read_le_u16()?;
            self.oeminfo = r.read_le_u16()?;
            for i in 0..10 {
                self.res2[i] = r.read_le_u16()?;
            }
            self.lfanew = r.read_le_u32()?;
        }

        Ok(())
    }

    pub fn save(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_all(&self.magic.to_be_bytes())?;
        w.write_all(&self.cblp.to_le_bytes())?;
        w.write_all(&self.cp.to_le_bytes())?;
        w.write_all(&self.crlc.to_le_bytes())?;
        w.write_all(&self.cparhdr.to_le_bytes())?;
        w.write_all(&self.minalloc.to_le_bytes())?;
        w.write_all(&self.maxalloc.to_le_bytes())?;
        w.write_all(&self.ss.to_le_bytes())?;
        w.write_all(&self.sp.to_le_bytes())?;
        w.write_all(&self.csum.to_le_bytes())?;
        w.write_all(&self.ip.to_le_bytes())?;
        w.write_all(&self.cs.to_le_bytes())?;
        w.write_all(&self.lfarlc.to_le_bytes())?;
        w.write_all(&self.ovno.to_le_bytes())?;

        if self.lfarlc == 0x40 {
            for v in &self.res {
                w.write_all(&v.to_le_bytes())?;
            }
            w.write_all(&self.oemid.to_le_bytes())?;
            w.write_all(&self.oeminfo.to_le_bytes())?;
            for v in &self.res2 {
                w.write_all(&v.to_le_bytes())?;
            }
            w.write_all(&self.lfanew.to_le_bytes())?;
        }

        Ok(())
    }

    pub fn dump(&self) {
        println!("DOS header");
        println!("magic     = {:04x}", self.magic);
        println!("cblp      = {:04x}", self.cblp);
        println!("cp        = {:04x}", self.cp);
        println!("crlc      = {:04x}", self.crlc);
        println!("cparhdr   = {:04x}", self.cparhdr);
        println!("minalloc  = {:04x}", self.minalloc);
        println!("maxalloc  = {:04x}", self.maxalloc);
        println!("ss        = {:04x}", self.ss);
        println!("sp        = {:04x}", self.sp);
        println!("csum      = {:04x}", self.csum);
        println!("ip        = {:04x}", self.ip);
        println!("cs        = {:04x}", self.cs);
        println!("lfarlc    = {:04x}", self.lfarlc);
        println!("ovno      = {:04x}", self.ovno);

        if self.lfarlc == 0x40 {
            for (i, v) in self.res.iter().enumerate() {
                println!("res[{}]    = {:04x}", i, v);
            }
            println!("oemid     = {:04x}", self.oemid);
            println!("oeminfo   = {:04x}", self.oeminfo);
            for (i, v) in self.res2.iter().enumerate() {
                println!("res2[{}]   = {:04x}", i, v);
            }
            println!("lfanew    = {:08x}", self.lfanew);
        }
        println!();
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExeMzRelocation {
    pub ofs: u16,
    pub seg: u16,
}

#[derive(Debug, Default, Clone)]
pub struct ExeMz {
    pub name: String,
    pub head: ExeMzHeader,
    pub relocations: Vec<ExeMzRelocation>,
    pub image: Vec<u8>,
}

impl ExeMz {
    pub fn load(data: &[u8], name: String) -> io::Result<Self> {
        let mut r = Cursor::new(data);
        let mut head = ExeMzHeader::default();
        head.load(&mut r)?;

        let image_size = {
            let mut size = 512i64 * head.cp as i64;
            if head.cblp != 0 {
                size += head.cblp as i64 - 512;
            }
            size.max(0) as usize
        };

        let mut image = vec![0u8; image_size];
        r.seek(SeekFrom::Start(16 * head.cparhdr as u64))?;
        let to_read = image_size.min(remaining(&r));
        r.read_exact(&mut image[..to_read])?;

        let mut relocations = Vec::with_capacity(head.crlc as usize);
        r.seek(SeekFrom::Start(head.lfarlc as u64))?;
        for _ in 0..head.crlc {
            let ofs = r.read_le_u16()?;
            let seg = r.read_le_u16()?;
            relocations.push(ExeMzRelocation { ofs, seg });
        }

        Ok(ExeMz {
            name,
            head,
            relocations,
            image,
        })
    }
}

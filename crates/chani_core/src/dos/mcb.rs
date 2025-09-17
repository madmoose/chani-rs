use std::fmt::Display;

use crate::{address::addr, memory::Memory};

pub const M: u8 = 0x4d;
pub const Z: u8 = 0x5a;

#[derive(Debug)]
pub enum MCBError {
    InvalidSignature,
}

#[derive(Debug)]
pub struct MemoryControlBlock {
    pub seg: u16,
    pub signature: u8,
    pub pid: u16,
    pub size_in_paragraphs: u16,
}

impl Display for MemoryControlBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "MCB {{ seg: {:#06X}, signature: {}, pid: {}, size_in_paragraphs: {:#06X} }}",
            self.seg, self.signature as char, self.pid, self.size_in_paragraphs
        )
    }
}

#[must_use]
pub fn read(memory: &Memory, seg: u16) -> Option<MemoryControlBlock> {
    let signature = memory.read_u8(addr(seg, 0));
    let pid = memory.read_u16(addr(seg, 1));
    let size_in_paragraphs = memory.read_u16(addr(seg, 3));
    let seg = memory.read_u16(addr(seg, 5));

    let mcb = MemoryControlBlock {
        signature,
        pid,
        size_in_paragraphs,
        seg,
    };

    if !mcb.is_valid() {
        return None;
    }

    Some(mcb)
}

impl MemoryControlBlock {
    #[must_use]
    fn has_valid_signature(&self) -> bool {
        self.signature == M || self.signature == Z
    }

    #[must_use]
    fn is_valid(&self) -> bool {
        let next_seg = self.data_seg().wrapping_add(self.size_in_paragraphs);
        self.has_valid_signature() && next_seg > self.seg
    }

    pub fn is_last(&self) -> bool {
        self.signature == Z
    }

    pub fn set_is_last(&mut self, is_last: bool) {
        if is_last {
            self.signature = Z;
        } else {
            self.signature = M;
        }
    }

    pub fn is_free(&self) -> bool {
        self.pid == 0
    }

    pub fn set_is_free(&mut self) {
        self.pid = 0;
    }

    pub fn data_seg(&self) -> u16 {
        self.seg + 1
    }

    pub fn next_seg(&self) -> u16 {
        self.data_seg() + self.size_in_paragraphs
    }

    #[must_use]
    fn read_next(&self, memory: &Memory) -> Option<MemoryControlBlock> {
        if self.is_last() {
            return None;
        }
        let next_seg = self.data_seg() + self.size_in_paragraphs;
        read(memory, next_seg)
    }

    #[must_use]
    pub fn split(&mut self, memory: &mut Memory, paras: u16) -> Option<MemoryControlBlock> {
        assert!(
            paras < self.size_in_paragraphs,
            "Cannot split MCB into more paragraphs than it has"
        );

        let old_size_in_paragraphs = self.size_in_paragraphs;
        self.size_in_paragraphs = paras;

        let new_seg = self.data_seg() + paras;

        let new_mcb = MemoryControlBlock {
            seg: new_seg,
            signature: self.signature,
            pid: 0,
            size_in_paragraphs: old_size_in_paragraphs - paras - 1,
        };
        self.set_is_last(false);

        // Write the updated MCB
        self.write(memory);
        // Write the new MCB
        new_mcb.write(memory);

        Some(new_mcb)
    }

    pub fn coalesce_free_blocks(&mut self, memory: &mut Memory) -> Option<()> {
        loop {
            if self.is_last() {
                return Some(());
            }

            let next = self.read_next(memory)?;
            if !next.is_free() {
                return Some(());
            }

            self.combine(memory, next)?;
        }
    }

    pub fn combine(&mut self, memory: &mut Memory, next: MemoryControlBlock) -> Option<()> {
        assert!(!self.is_last(), "Cannot combine the last MCB");
        assert!(self.next_seg() == next.seg, "Next MCB is not contiguous");

        self.size_in_paragraphs += next.size_in_paragraphs + 1;
        self.set_is_last(next.is_last());

        self.write(memory);

        Some(())
    }

    pub fn write(&self, memory: &mut Memory) {
        memory.write_u8(addr(self.seg, 0), self.signature);
        memory.write_u16(addr(self.seg, 1), self.pid);
        memory.write_u16(addr(self.seg, 3), self.size_in_paragraphs);
        memory.write_u16(addr(self.seg, 5), self.seg);
    }
}

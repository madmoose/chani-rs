use crate::memory::Memory;

use super::{Dos, mcb};

#[derive(Debug)]
pub enum AllocationError {
    NotEnoughMemory { largest_available: u16 },
    MemoryArenaTrashed,
}

impl Dos {
    pub fn allocate(
        &mut self,
        memory: &mut Memory,
        requested_paragraphs: u16,
    ) -> Result<u16, AllocationError> {
        let mut largest_available = 0;
        let mut first_fit_seg: u16 = 0;
        let mut best_fit_seg: u16 = 0;
        let mut best_fit_size: u16 = 0;
        let mut last_fit_seg: u16 = 0;

        let mut seg = self.initial_mcb_seg;

        loop {
            let mut mcb = mcb::read(memory, seg).ok_or(AllocationError::MemoryArenaTrashed)?;
            if mcb.is_free() {
                mcb.coalesce_free_blocks(memory)
                    .ok_or(AllocationError::MemoryArenaTrashed)?;

                let mcb_size = mcb.size_in_paragraphs;

                largest_available = u16::max(largest_available, mcb_size);

                if mcb_size >= requested_paragraphs {
                    if first_fit_seg == 0 {
                        first_fit_seg = mcb.seg;
                    }

                    if best_fit_seg == 0 || mcb_size < best_fit_size {
                        best_fit_size = mcb_size;
                        best_fit_seg = mcb.seg;
                    }

                    last_fit_seg = mcb.seg;
                }

                if mcb.is_last() {
                    break;
                }
            }
            seg = mcb.next_seg();
        }

        if largest_available < requested_paragraphs {
            return Err(AllocationError::NotEnoughMemory { largest_available });
        }

        let seg = match self.allocation_strategy {
            0 => first_fit_seg, // First fit
            1 => best_fit_seg,  // Best fit
            _ => last_fit_seg,  // Last fit
        };

        // We've already walked the MCB chain, so unwrap should always succeed.
        let mut mcb = mcb::read(memory, seg).unwrap();

        // If the located MCB is bigger than the requested size, we need to split it.
        if mcb.size_in_paragraphs > requested_paragraphs {
            if self.allocation_strategy < 2 {
                mcb.split(memory, requested_paragraphs).unwrap();
            } else {
                // If strategy is Last Fit then split such that the new allocation fits at the end,
                // and use the latter block.
                mcb = mcb
                    .split(memory, mcb.size_in_paragraphs - requested_paragraphs - 1)
                    .unwrap();
            }
        }

        Ok(mcb.seg)
    }
}

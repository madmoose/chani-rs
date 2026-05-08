use std::collections::{BTreeMap, BTreeSet};

use smallvec::SmallVec;

use crate::{Address, Opcode, decode, project::{Project, SegmentIdx}};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub seg_idx: SegmentIdx,
    pub start: u32,
    pub end: u32, // exclusive — first byte past last instruction
    pub successors: SmallVec<[Address; 2]>,
    pub predecessors: Vec<Address>,
}

#[derive(Debug, Default, Clone)]
pub struct BasicBlockMap {
    blocks: BTreeMap<Address, BasicBlock>,
}

impl BasicBlockMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, block: BasicBlock) {
        self.blocks.insert((block.seg_idx, block.start), block);
    }

    pub fn block_at(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<&BasicBlock> {
        self.blocks.get(&(seg_idx, ofs))
    }

    pub fn block_at_mut(&mut self, seg_idx: SegmentIdx, ofs: u32) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&(seg_idx, ofs))
    }

    /// Returns the block whose range `[start, end)` contains `ofs`.
    pub fn block_containing(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<&BasicBlock> {
        self.blocks
            .range((seg_idx, 0)..=(seg_idx, ofs))
            .next_back()
            .map(|(_, b)| b)
            .filter(|b| ofs < b.end)
    }

    pub fn blocks(&self) -> impl Iterator<Item = &BasicBlock> {
        self.blocks.values()
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Block-level CFG walk from a function entry. Follows all successors of
    /// non-Call blocks, but for blocks ending in `Call` follows only the
    /// fall-through edge (returning to the next instruction) rather than the
    /// callee. Returns the sorted list of block-start addresses reachable
    /// under those rules.
    pub fn function_blocks(&self, project: &Project, entry: Address) -> Vec<Address> {
        let mut visited: BTreeSet<Address> = BTreeSet::new();
        let mut queue: Vec<Address> = vec![entry];

        while let Some(addr) = queue.pop() {
            if !visited.insert(addr) {
                continue;
            }
            let Some(block) = self.block_at(addr.0, addr.1) else {
                continue;
            };

            let last_ofs = project.segments[block.seg_idx]
                .addr_attributes
                .prev(block.end)
                .filter(|&p| p >= block.start)
                .unwrap_or(block.start);

            let seg = &project.segments[block.seg_idx];
            let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
            let last_is_call = decode(
                seg_val,
                last_ofs as u16,
                project.bytes_at_seg(block.seg_idx, last_ofs).iter().copied(),
            )
            .is_some_and(|i| i.opcode == Opcode::Call);

            for &succ in &block.successors {
                if last_is_call {
                    if succ == (block.seg_idx, block.end) {
                        queue.push(succ);
                    }
                } else {
                    queue.push(succ);
                }
            }
        }

        let mut addrs: Vec<Address> = visited.into_iter().collect();
        addrs.sort();
        addrs
    }
}

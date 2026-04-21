use std::collections::BTreeMap;

use smallvec::SmallVec;

pub type Addr = (usize, u32); // (segment index, offset)

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub seg_idx: usize,
    pub start: u32,
    pub end: u32, // exclusive — first byte past last instruction
    pub successors: SmallVec<[Addr; 2]>,
    pub predecessors: Vec<Addr>,
}

#[derive(Debug, Default, Clone)]
pub struct BasicBlockMap {
    blocks: BTreeMap<Addr, BasicBlock>,
}

impl BasicBlockMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, block: BasicBlock) {
        self.blocks.insert((block.seg_idx, block.start), block);
    }

    pub fn block_at(&self, seg_idx: usize, ofs: u32) -> Option<&BasicBlock> {
        self.blocks.get(&(seg_idx, ofs))
    }

    pub fn block_at_mut(&mut self, seg_idx: usize, ofs: u32) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&(seg_idx, ofs))
    }

    /// Returns the block whose range `[start, end)` contains `ofs`.
    pub fn block_containing(&self, seg_idx: usize, ofs: u32) -> Option<&BasicBlock> {
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
}

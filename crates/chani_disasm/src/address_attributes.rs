const ATTR_OP: u8 = 1; // First byte of an instruction
const ATTR_OP_CONT: u8 = 2; // Continuation bytes of an instruction
const ATTR_DATA: u8 = 4; // Marked as data
const ATTR_DATA_CONT: u8 = 8; // Continuation bytes of data
const ATTR_FLOW: u8 = 16; // The immediately preceding decoded instruction does not stop control flow
const ATTR_OP_STOPS_FLOW: u8 = 32; // This instruction stops control flow (ret, jmp, hlt, etc.)
const ATTR_BLOCK_START: u8 = 64; // First instruction of a basic block

#[derive(Debug, Clone, Default)]
pub struct AddressAttributes {
    /// Offset subtracted from every incoming `ofs` before indexing `attrs`.
    /// 0 for EXE-backed segments; `load_offset` for bin/com-backed segments.
    base: u32,
    attrs: Vec<u8>,
}

impl AddressAttributes {
    pub fn new(size: usize) -> Self {
        Self {
            base: 0,
            attrs: vec![0u8; size],
        }
    }

    pub fn new_with_base(base: u32, size: usize) -> Self {
        Self {
            base,
            attrs: vec![0u8; size],
        }
    }

    #[inline]
    fn idx(&self, ofs: u32) -> Option<usize> {
        let i = (ofs).checked_sub(self.base)? as usize;
        if i < self.attrs.len() { Some(i) } else { None }
    }

    pub fn mark_as_code(&mut self, ofs: u32, len: u32) {
        if len == 0 {
            return;
        }
        if let Some(i) = self.idx(ofs) {
            self.attrs[i] |= ATTR_OP;
        }
        for k in 1..len {
            if let Some(i) = self.idx(ofs.wrapping_add(k)) {
                self.attrs[i] |= ATTR_OP_CONT;
            }
        }
    }

    pub fn unmark_as_code(&mut self, ofs: u32) {
        let Some(i) = self.idx(ofs) else { return };
        self.attrs[i] &= !ATTR_OP;
        let mut j = i + 1;
        while j < self.attrs.len() && (self.attrs[j] & ATTR_OP_CONT) != 0 {
            self.attrs[j] &= !ATTR_OP_CONT;
            j += 1;
        }
    }

    pub fn mark_as_data(&mut self, ofs: u32, len: u32) {
        if len == 0 {
            return;
        }
        if let Some(i) = self.idx(ofs) {
            self.attrs[i] |= ATTR_DATA;
        }
        for k in 1..len {
            if let Some(i) = self.idx(ofs.wrapping_add(k)) {
                self.attrs[i] |= ATTR_DATA_CONT;
            }
        }
    }

    pub fn mark_as_flow(&mut self, ofs: u32) {
        if let Some(i) = self.idx(ofs) {
            self.attrs[i] |= ATTR_FLOW;
        }
    }

    pub fn mark_as_stops_flow(&mut self, ofs: u32) {
        if let Some(i) = self.idx(ofs) {
            self.attrs[i] |= ATTR_OP_STOPS_FLOW;
        }
    }

    pub fn stops_flow(&self, ofs: u32) -> bool {
        self.idx(ofs)
            .is_some_and(|i| self.attrs[i] & ATTR_OP_STOPS_FLOW != 0)
    }

    pub fn is_unmarked(&self, ofs: u32, len: u32) -> bool {
        const CLASSIFICATION_BITS: u8 = ATTR_OP | ATTR_OP_CONT | ATTR_DATA | ATTR_DATA_CONT;
        (0..len).all(|k| {
            self.idx(ofs.wrapping_add(k))
                .is_none_or(|i| self.attrs[i] & CLASSIFICATION_BITS == 0)
        })
    }

    pub fn is_code(&self, ofs: u32) -> bool {
        self.is_op(ofs) || self.is_op_cont(ofs)
    }

    pub fn is_op(&self, ofs: u32) -> bool {
        self.idx(ofs).is_some_and(|i| self.attrs[i] & ATTR_OP != 0)
    }

    pub fn is_op_cont(&self, ofs: u32) -> bool {
        self.idx(ofs)
            .is_some_and(|i| self.attrs[i] & ATTR_OP_CONT != 0)
    }

    pub fn is_flow(&self, ofs: u32) -> bool {
        self.idx(ofs)
            .is_some_and(|i| self.attrs[i] & ATTR_FLOW != 0)
    }

    /// Find next offset that is op or data or unmarked
    pub fn next(&self, ofs: u32) -> Option<u32> {
        let start = self.idx(ofs)?;
        for i in (start + 1)..self.attrs.len() {
            let a = self.attrs[i];
            // Skip pure continuation bytes (middle of an instruction or data item).
            if (a & (ATTR_OP_CONT | ATTR_DATA_CONT)) != 0 && (a & (ATTR_OP | ATTR_DATA)) == 0 {
                continue;
            }
            return Some(self.base.wrapping_add(i as u32));
        }
        None
    }

    /// Find the previous offset that is op or data or unmarked
    pub fn prev(&self, ofs: u32) -> Option<u32> {
        let end = self.idx(ofs)?;
        for i in (0..end).rev() {
            let a = self.attrs[i];
            // Skip pure continuation bytes (middle of an instruction or data item).
            if (a & (ATTR_OP_CONT | ATTR_DATA_CONT)) != 0 && (a & (ATTR_OP | ATTR_DATA)) == 0 {
                continue;
            }
            return Some(self.base.wrapping_add(i as u32));
        }
        None
    }

    /// Returns the length of the instruction starting at `ofs` (1 + consecutive CONT bytes).
    /// Returns 0 if `ofs` is not an OP byte.
    pub fn op_len(&self, ofs: u32) -> u32 {
        if !self.is_op(ofs) {
            return 0;
        }
        let mut len = 1;
        while self.is_op_cont(ofs.wrapping_add(len)) {
            len += 1;
        }
        len
    }

    pub fn is_data(&self, ofs: u32) -> bool {
        self.idx(ofs)
            .is_some_and(|i| self.attrs[i] & (ATTR_DATA | ATTR_DATA_CONT) != 0)
    }

    pub fn mark_as_block_start(&mut self, ofs: u32) {
        if let Some(i) = self.idx(ofs) {
            self.attrs[i] |= ATTR_BLOCK_START;
        }
    }

    pub fn is_block_start(&self, ofs: u32) -> bool {
        self.idx(ofs)
            .is_some_and(|i| self.attrs[i] & ATTR_BLOCK_START != 0)
    }

    pub fn base(&self) -> u32 {
        self.base
    }
}

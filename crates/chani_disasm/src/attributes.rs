const ATTR_OP: u8 = 1; // First byte of an instruction
const ATTR_CONT: u8 = 2; // Continuation bytes of an instruction
const ATTR_FLOW: u8 = 4; // Reachable via fallthrough from a branching instruction
const ATTR_DATA: u8 = 8; // Marked as data

#[derive(Debug, Clone, Default)]
pub struct Attributes {
    attrs: Vec<u8>,
}

impl Attributes {
    pub fn new(size: usize) -> Self {
        Self {
            attrs: vec![0u8; size],
        }
    }

    pub fn mark_as_code(&mut self, ofs: u16, len: usize) {
        if len == 0 {
            return;
        }
        if let Some(b) = self.attrs.get_mut(ofs as usize) {
            *b |= ATTR_OP;
        }
        for i in 1..len {
            if let Some(b) = self.attrs.get_mut(ofs as usize + i) {
                *b |= ATTR_CONT;
            }
        }
    }

    pub fn unmark_as_code(&mut self, ofs: u16) {
        let idx = ofs as usize;
        if idx >= self.attrs.len() {
            return;
        }
        self.attrs[idx] &= !ATTR_OP;
        let mut i = idx + 1;
        while i < self.attrs.len() && (self.attrs[i] & ATTR_CONT) != 0 {
            self.attrs[i] &= !ATTR_CONT;
            i += 1;
        }
    }

    pub fn mark_as_data(&mut self, ofs: u16, len: usize) {
        for i in 0..len {
            if let Some(b) = self.attrs.get_mut(ofs as usize + i) {
                *b |= ATTR_DATA;
            }
        }
    }

    pub fn mark_as_flow(&mut self, ofs: u16) {
        if let Some(b) = self.attrs.get_mut(ofs as usize) {
            *b |= ATTR_FLOW;
        }
    }

    pub fn is_unmarked(&self, ofs: u16, len: usize) -> bool {
        (0..len).all(|i| self.attrs.get(ofs as usize + i).map_or(true, |&b| b == 0))
    }

    pub fn is_code(&self, ofs: u16) -> bool {
        self.is_op(ofs) || self.is_cont(ofs)
    }

    pub fn is_op(&self, ofs: u16) -> bool {
        self.attrs
            .get(ofs as usize)
            .map_or(false, |&b| b & ATTR_OP != 0)
    }

    pub fn is_cont(&self, ofs: u16) -> bool {
        self.attrs
            .get(ofs as usize)
            .map_or(false, |&b| b & ATTR_CONT != 0)
    }

    pub fn is_flow(&self, ofs: u16) -> bool {
        self.attrs
            .get(ofs as usize)
            .map_or(false, |&b| b & ATTR_FLOW != 0)
    }

    /// Returns the length of the instruction starting at `ofs` (1 + consecutive CONT bytes).
    /// Returns 0 if `ofs` is not an OP byte.
    pub fn op_len(&self, ofs: u16) -> usize {
        if !self.is_op(ofs) {
            return 0;
        }
        let mut len = 1;
        while self.is_cont(ofs.wrapping_add(len as u16)) {
            len += 1;
        }
        len
    }

    pub fn is_data(&self, ofs: u16) -> bool {
        self.attrs
            .get(ofs as usize)
            .map_or(false, |&b| b & ATTR_DATA != 0)
    }
}

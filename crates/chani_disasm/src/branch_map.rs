use std::collections::{BTreeMap, BTreeSet};

pub type Addr = (usize, u16); // (segment index, offset)

#[derive(Debug, Default)]
pub struct BranchMap {
    forward: BTreeMap<Addr, BTreeSet<Addr>>,
    backward: BTreeMap<Addr, BTreeSet<Addr>>,
}

impl BranchMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, from: Addr, to: Addr) {
        self.forward.entry(from).or_default().insert(to);
        self.backward.entry(to).or_default().insert(from);
    }

    /// Addresses this instruction branches to.
    pub fn targets(&self, from: Addr) -> impl Iterator<Item = Addr> + '_ {
        self.forward
            .get(&from)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }

    /// Addresses that branch to this address.
    pub fn sources(&self, to: Addr) -> impl Iterator<Item = Addr> + '_ {
        self.backward
            .get(&to)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }

    /// All unique branch target addresses across all edges.
    pub fn all_targets(&self) -> impl Iterator<Item = Addr> + '_ {
        self.backward.keys().copied()
    }

    pub fn has_target(&self, from: Addr) -> bool {
        self.forward.contains_key(&from)
    }

    pub fn has_source(&self, to: Addr) -> bool {
        self.backward.contains_key(&to)
    }
}

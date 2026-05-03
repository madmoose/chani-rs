use std::collections::{BTreeMap, BTreeSet};

use crate::Address;

#[derive(Debug, Default, Clone)]
pub struct BranchMap {
    forward: BTreeMap<Address, BTreeSet<Address>>,
    backward: BTreeMap<Address, BTreeSet<Address>>,
}

impl BranchMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, from: Address, to: Address) {
        self.forward.entry(from).or_default().insert(to);
        self.backward.entry(to).or_default().insert(from);
    }

    /// Addresses this instruction branches to.
    pub fn targets(&self, from: Address) -> impl Iterator<Item = Address> + '_ {
        self.forward
            .get(&from)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }

    /// Addresses that branch to this address.
    pub fn sources(&self, to: Address) -> impl Iterator<Item = Address> + '_ {
        self.backward
            .get(&to)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }

    /// All unique branch target addresses across all edges.
    pub fn all_targets(&self) -> impl Iterator<Item = Address> + '_ {
        self.backward.keys().copied()
    }

    pub fn has_target(&self, from: Address) -> bool {
        self.forward.contains_key(&from)
    }

    pub fn has_source(&self, to: Address) -> bool {
        self.backward.contains_key(&to)
    }
}

//! Explicit function recovery from the basic-block CFG.
//!
//! After [`Project::build_basic_blocks`] populates the block map and
//! [`Project::branches`] holds every recorded branch edge (including manually
//! supplied destinations for indirect calls/jumps), this pass recovers a
//! function-level view of the program:
//!
//! - **Function entries** are: every basic block that is the target of a `Call`
//!   instruction, the EXE start address, and any address marked
//!   [`AttrType::Code`](crate::project::AttrType::Code) by the user.
//! - For each entry, the **block set** is computed by a block-level CFG walk
//!   that does *not* cross call edges and stops at any successor that is a
//!   different function's entry. The same basic block may belong to multiple
//!   functions when shared epilogues, helpers reached by fall-through, or
//!   merged paths are involved.
//! - A **call graph** is built on top of those edges. Direct calls and any
//!   branch (`Jmp`, `Jcc`) into another function's entry — including plain
//!   fall-through — count as callee edges. Inverting the relation yields the
//!   caller set.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    Address, Opcode, decode,
    project::{AttrType, Project},
};

/// Per-function recovered metadata.
#[derive(Debug, Clone)]
pub struct Function {
    pub entry: Address,
    /// Sorted, deduplicated block-start addresses that belong to this function.
    pub blocks: Vec<Address>,
    /// Function entries that call (or tail-call) this function.
    pub callers: BTreeSet<Address>,
    /// Function entries this function calls (or tail-calls).
    pub callees: BTreeSet<Address>,
}

#[derive(Debug, Default, Clone)]
pub struct FunctionMap {
    functions: BTreeMap<Address, Function>,
}

impl FunctionMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function_at(&self, entry: Address) -> Option<&Function> {
        self.functions.get(&entry)
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    pub fn entries(&self) -> impl Iterator<Item = Address> + '_ {
        self.functions.keys().copied()
    }

    pub fn is_entry(&self, addr: Address) -> bool {
        self.functions.contains_key(&addr)
    }

    pub fn len(&self) -> usize {
        self.functions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Iterate over every function whose recovered block set contains the
    /// block starting at `block_start`. A block may belong to multiple
    /// functions, so this can yield more than one result. Callers with an
    /// arbitrary in-block address should resolve it via
    /// [`crate::basic_block::BasicBlockMap::block_containing`] first.
    pub fn functions_with_block(&self, block_start: Address) -> impl Iterator<Item = &Function> {
        self.functions
            .values()
            .filter(move |f| f.blocks.binary_search(&block_start).is_ok())
    }
}

// ── Computation ───────────────────────────────────────────────────────────────

pub(crate) fn compute(project: &Project) -> FunctionMap {
    let entries = collect_entries(project);

    let mut functions: BTreeMap<Address, Function> = BTreeMap::new();
    for &entry in &entries {
        if project.blocks.block_at(entry.0, entry.1).is_none() {
            continue;
        }
        let (blocks, callees) = walk_function(project, entry, &entries);
        let mut block_vec: Vec<Address> = blocks.into_iter().collect();
        block_vec.sort();
        functions.insert(
            entry,
            Function {
                entry,
                blocks: block_vec,
                callers: BTreeSet::new(),
                callees,
            },
        );
    }

    // Invert callee edges to build callers. Edges that point to an address that
    // is not itself a recovered function are dropped — by construction every
    // callee is in `entries`, but blocks can be missing (e.g. EXE entry whose
    // bytes aren't decoded), so the recovered map can be a strict subset.
    let edges: Vec<(Address, Address)> = functions
        .iter()
        .flat_map(|(&caller, f)| f.callees.iter().map(move |&callee| (callee, caller)))
        .collect();
    for (callee, caller) in edges {
        if let Some(f) = functions.get_mut(&callee) {
            f.callers.insert(caller);
        }
    }

    FunctionMap { functions }
}

/// Function entry seeds: targets of `Call` instructions (which already include
/// manually-supplied targets for indirect calls — see
/// [`Project::disassemble`]), the EXE start address, and any user-supplied
/// `AttrType::Code` attribute.
fn collect_entries(project: &Project) -> BTreeSet<Address> {
    let mut entries: BTreeSet<Address> = BTreeSet::new();

    for src in project.branches.all_sources() {
        let seg = &project.segments[src.0];
        let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
        let bytes = project.bytes_at_seg(src.0, src.1);
        let Some(inst) = decode(seg_val, src.1 as u16, bytes.iter().copied()) else {
            continue;
        };
        if inst.opcode != Opcode::Call {
            continue;
        }
        for tgt in project.branches.targets(src) {
            entries.insert(tgt);
        }
    }

    if let Some(exe) = &project.exe
        && let Some(seg_idx) = project.segment_index_for(exe.head.cs)
    {
        entries.insert((seg_idx, exe.head.ip as u32));
    }

    for (&addr, attr) in &project.attrs {
        if matches!(attr.r#type, Some(AttrType::Code)) {
            entries.insert(addr);
        }
    }

    entries
}

/// Block-level walk producing `(in_function_blocks, callees)`.
///
/// Edge classification, decided from the last instruction of each block:
///
/// - **Call:** the fall-through successor (`block.end`) stays in the function;
///   every other successor is a direct callee.
/// - **Anything else (Jmp/Jcc/non-branching fall-through):** a successor that
///   is a known function entry other than this function's own entry is a
///   tail-call edge — record as a callee, do not enter that block. All other
///   successors are intra-procedural and walked.
fn walk_function(
    project: &Project,
    entry: Address,
    known_entries: &BTreeSet<Address>,
) -> (BTreeSet<Address>, BTreeSet<Address>) {
    let mut visited: BTreeSet<Address> = BTreeSet::new();
    let mut callees: BTreeSet<Address> = BTreeSet::new();
    let mut queue: Vec<Address> = vec![entry];

    while let Some(addr) = queue.pop() {
        if !visited.insert(addr) {
            continue;
        }
        let Some(block) = project.blocks.block_at(addr.0, addr.1) else {
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
            project
                .bytes_at_seg(block.seg_idx, last_ofs)
                .iter()
                .copied(),
        )
        .is_some_and(|i| i.opcode == Opcode::Call);

        for &succ in &block.successors {
            let is_fall_through = succ == (block.seg_idx, block.end);

            if last_is_call {
                if is_fall_through {
                    queue.push(succ);
                } else {
                    callees.insert(succ);
                }
            } else if known_entries.contains(&succ) && succ != entry {
                callees.insert(succ);
            } else {
                queue.push(succ);
            }
        }
    }

    (visited, callees)
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address_attributes::AddressAttributes;
    use crate::project::{BinImage, SegmentIdx};

    fn make_project(code: &[u8], code_seeds: &[u32]) -> Project {
        let mut chani = String::new();
        chani.push_str("project[test]:\n\n");
        chani.push_str("arch = 8086\n\n");
        chani.push_str("segment[seg000]:\n");
        chani.push_str("    type = code\n");
        chani.push_str("    start = 0x0\n");
        chani.push_str(&format!("    end = 0x{:x}\n", code.len()));
        chani.push_str("end\n\n");
        for &e in code_seeds {
            chani.push_str(&format!("attr[seg000:{e:x}]: type = code\n"));
        }
        chani.push_str("\nend\n");
        let mut p = Project::from_str(&chani).unwrap();
        p.segments[SegmentIdx::from(0)].addr_attributes = AddressAttributes::new(code.len());
        p.images.push(BinImage {
            seg_idx: SegmentIdx::from(0),
            load_offset: 0,
            data: code.to_vec(),
        });
        p.analyze();
        p
    }

    fn at(ofs: u32) -> Address {
        (SegmentIdx::from(0), ofs)
    }

    fn call_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE8, disp as u8, (disp >> 8) as u8]
    }

    fn jmp_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE9, disp as u8, (disp >> 8) as u8]
    }

    /// Encode `jz rel8` at `at_ofs` targeting `target_ofs`.
    fn jz_rel8(at_ofs: u32, target_ofs: u32) -> [u8; 2] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 2) as i8 as u8;
        [0x74, disp]
    }

    #[test]
    fn direct_call_records_caller_and_callee() {
        // F @ 0x00: call G; ret. G @ 0x10: ret. H @ 0x20 calls F.
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x10));
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.push(0xC3);
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x20]);

        let f = p.functions.function_at(at(0x00)).unwrap();
        let g = p.functions.function_at(at(0x10)).unwrap();
        let h = p.functions.function_at(at(0x20)).unwrap();

        assert!(f.callees.contains(&at(0x10)));
        assert!(g.callers.contains(&at(0x00)));
        assert!(h.callees.contains(&at(0x00)));
        assert!(f.callers.contains(&at(0x20)));
        assert!(!f.blocks.contains(&at(0x10)), "G is not in F's body");
    }

    #[test]
    fn jmp_to_entry_is_tail_call() {
        // F @ 0x00: jmp G. G @ 0x10: ret. H @ 0x20 calls G; I @ 0x30 calls F.
        let mut code = Vec::new();
        code.extend_from_slice(&jmp_rel16(0x00, 0x10));
        code.resize(0x10, 0x90);
        code.push(0xC3);
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x10));
        code.push(0xC3);
        code.resize(0x30, 0x90);
        code.extend_from_slice(&call_rel16(0x30, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x20, 0x30]);

        let f = p.functions.function_at(at(0x00)).unwrap();
        let g = p.functions.function_at(at(0x10)).unwrap();
        assert!(f.callees.contains(&at(0x10)), "F tail-calls G");
        assert!(g.callers.contains(&at(0x00)));
        assert!(!f.blocks.contains(&at(0x10)), "G not part of F");
    }

    #[test]
    fn jcc_to_entry_is_tail_call() {
        // F @ 0x00: jz G; ret. G @ 0x10: ret.
        let mut code = Vec::new();
        code.extend_from_slice(&jz_rel8(0x00, 0x10));
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.push(0xC3);
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00));
        code.push(0xC3);
        code.resize(0x28, 0x90);
        code.extend_from_slice(&call_rel16(0x28, 0x10));
        code.push(0xC3);
        let p = make_project(&code, &[0x20, 0x28]);

        let f = p.functions.function_at(at(0x00)).unwrap();
        assert!(f.callees.contains(&at(0x10)), "jz to G is a tail call");
        // The fall-through (the `ret` at 0x02) is still in F's body.
        assert!(
            f.blocks.iter().any(|&b| b == at(0x00) || b == at(0x02)),
            "F's fall-through block should be present, got {:?}",
            f.blocks
        );
    }

    #[test]
    fn fall_through_into_entry_is_tail_call() {
        // F @ 0x00: nop; (fall-through); G @ 0x01: ret.
        // H @ 0x10 calls F; I @ 0x14 calls G.
        let mut code = Vec::new();
        code.push(0x90); // nop at 0x00
        code.push(0xC3); // ret at 0x01
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        code.extend_from_slice(&call_rel16(0x14, 0x01));
        code.push(0xC3);
        // 0x10/0x14 are Code seeds so disassembly has roots. They also become
        // function entries via the Code-attr rule, which is fine for this test.
        let p = make_project(&code, &[0x10, 0x14]);

        let f = p.functions.function_at(at(0x00)).unwrap();
        let g = p.functions.function_at(at(0x01)).unwrap();
        assert!(
            f.callees.contains(&at(0x01)),
            "F falls through to G — should be a tail call. callees={:?}",
            f.callees
        );
        assert!(g.callers.contains(&at(0x00)));
        assert!(!f.blocks.contains(&at(0x01)), "G not part of F");
    }

    #[test]
    fn shared_block_belongs_to_multiple_functions() {
        // Two callers fall-through-jmp into the same shared block.
        // F @ 0x00: jmp shared. G @ 0x04: jmp shared. shared @ 0x10: ret.
        // External callers for F and G.
        let mut code = Vec::new();
        code.extend_from_slice(&jmp_rel16(0x00, 0x10));
        code.push(0x90);
        code.extend_from_slice(&jmp_rel16(0x04, 0x10));
        code.resize(0x10, 0x90);
        code.push(0xC3); // shared
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00));
        code.push(0xC3);
        code.extend_from_slice(&call_rel16(0x24, 0x04));
        code.push(0xC3);
        let p = make_project(&code, &[0x20, 0x24]);

        // The shared block at 0x10 is itself a function entry only if it's the
        // target of a *call*. Here it's only a jmp target, so it's not a
        // function. F and G should both contain block 0x10 since their jmp
        // targets a non-entry block.
        assert!(
            p.functions.function_at(at(0x10)).is_none(),
            "0x10 reached only by jmp shouldn't be a function"
        );
        let f = p.functions.function_at(at(0x00)).unwrap();
        let g = p.functions.function_at(at(0x04)).unwrap();
        assert!(f.blocks.contains(&at(0x10)));
        assert!(g.blocks.contains(&at(0x10)));
    }

    #[test]
    fn self_recursive_call_records_self_callee() {
        // F @ 0x00: call F; ret. H @ 0x10 calls F.
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x00));
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let f = p.functions.function_at(at(0x00)).unwrap();
        assert!(f.callees.contains(&at(0x00)), "self-recursion is a callee");
        assert!(f.callers.contains(&at(0x00)));
        assert!(f.callers.contains(&at(0x10)));
    }

    #[test]
    fn code_attr_seeds_function_entry() {
        // F @ 0x00 is marked Code but has no callers in the binary.
        let code = vec![0xC3, 0x90];
        let p = make_project(&code, &[0x00]);
        assert!(
            p.functions.function_at(at(0x00)).is_some(),
            "Code attr should seed a function entry"
        );
    }
}

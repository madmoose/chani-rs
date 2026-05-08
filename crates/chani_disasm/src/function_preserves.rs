//! Per-function analysis: does this function preserve DS, ES, and SS on every
//! return path?
//!
//! Function entries are direct `Call` destinations. For each entry the pass
//! runs an intra-procedural forward dataflow tracking, for each segment
//! register, whether the live value is still equal to the value at function
//! entry. An abstract stack of [`Slot`] tokens lets `push <sreg>` /
//! `pop <sreg>` pairs be matched: if the popped slot is the matching
//! `EntryDs/Es/Ss` token, the register is restored to its entry value.
//!
//! Calls to other known functions consult the current fixpoint estimate of
//! the callee. A standard worklist computes the greatest fixpoint over the
//! call graph (optimistic init at TOP, monotone descent).
//!
//! Tail calls — both `jmp <known-entry>` and structural fall-through into a
//! known entry — are modeled as returns that route through the tail-callee:
//! the function preserves R iff the local state still has R equal to entry
//! AND the tail-callee preserves R.
//!
//! CS is not tracked: near calls and returns are hardware-balanced, so any
//! CS value before a call is already retained by `seg_dataflow`. Far calls
//! and far returns are likewise hardware-balanced for properly structured
//! functions.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    Address, Opcode, decode,
    decoded_instruction::DecodedInstruction,
    opcode_table::{ArgDir, ArgType},
    project::{Project, SegmentIdx},
};

// ── Public types ──────────────────────────────────────────────────────────────

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FunctionPreserves {
    pub ds: bool,
    pub es: bool,
    pub ss: bool,
}

impl FunctionPreserves {
    pub const TOP: Self = Self { ds: true, es: true, ss: true };
    pub const BOTTOM: Self = Self { ds: false, es: false, ss: false };

    pub fn meet(self, other: Self) -> Self {
        Self {
            ds: self.ds && other.ds,
            es: self.es && other.es,
            ss: self.ss && other.ss,
        }
    }
}

pub type FunctionPreservesMap = BTreeMap<Address, FunctionPreserves>;

// ── Internal abstract state ───────────────────────────────────────────────────

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Slot {
    EntryDs,
    EntryEs,
    EntrySs,
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum StackState {
    Tracked(Vec<Slot>),
    Invalid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct AbsState {
    ds_eq_entry: bool,
    es_eq_entry: bool,
    ss_eq_entry: bool,
    stack: StackState,
}

impl AbsState {
    fn initial() -> Self {
        Self {
            ds_eq_entry: true,
            es_eq_entry: true,
            ss_eq_entry: true,
            stack: StackState::Tracked(Vec::new()),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        let stack = match (&self.stack, &other.stack) {
            (StackState::Tracked(a), StackState::Tracked(b)) if a == b => {
                StackState::Tracked(a.clone())
            }
            _ => StackState::Invalid,
        };
        Self {
            ds_eq_entry: self.ds_eq_entry && other.ds_eq_entry,
            es_eq_entry: self.es_eq_entry && other.es_eq_entry,
            ss_eq_entry: self.ss_eq_entry && other.ss_eq_entry,
            stack,
        }
    }

    fn push_slot(&mut self, slot: Slot) {
        if let StackState::Tracked(s) = &mut self.stack {
            s.push(slot);
        }
    }

    fn pop_slot(&mut self) -> Slot {
        match &mut self.stack {
            StackState::Tracked(s) => s.pop().unwrap_or(Slot::Other),
            StackState::Invalid => Slot::Other,
        }
    }

    fn invalidate_stack(&mut self) {
        self.stack = StackState::Invalid;
    }

    fn stack_is_empty(&self) -> bool {
        matches!(&self.stack, StackState::Tracked(s) if s.is_empty())
    }
}

// ── Argument helpers ──────────────────────────────────────────────────────────

fn sreg_from_modrm_bits(bits: u8) -> u8 {
    bits & 3
}

/// Return the `Slot` token corresponding to a segment-register operand,
/// based on the operand's `ArgType` (specific) or modrm bits (`ArgType::SReg`).
/// Returns `None` if the operand is not a segment register.
fn sreg_slot_from_arg(arg: ArgType, modrm: u8) -> Option<SRegKind> {
    match arg {
        ArgType::DS => Some(SRegKind::Ds),
        ArgType::ES => Some(SRegKind::Es),
        ArgType::SS => Some(SRegKind::Ss),
        ArgType::CS => Some(SRegKind::Cs),
        ArgType::SReg => Some(match sreg_from_modrm_bits((modrm >> 3) & 3) {
            0 => SRegKind::Es,
            1 => SRegKind::Cs,
            2 => SRegKind::Ss,
            _ => SRegKind::Ds,
        }),
        _ => None,
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum SRegKind {
    Ds,
    Es,
    Ss,
    Cs,
}

/// Returns true if any operand of the instruction writes to SP.
fn writes_to_sp(inst: &DecodedInstruction) -> bool {
    let mod_bits = (inst.modrm >> 6) & 3;
    for i in 0..2 {
        if inst.arg_type[i] == ArgType::None {
            continue;
        }
        if !matches!(inst.arg_dir[i], ArgDir::WO | ArgDir::RW) {
            continue;
        }
        match inst.arg_type[i] {
            ArgType::SP => return true,
            ArgType::Reg16 => {
                if (inst.modrm >> 3) & 7 == 4 {
                    return true;
                }
            }
            ArgType::RM16 => {
                if mod_bits == 0b11 && (inst.modrm & 7) == 4 {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

fn call_dest(project: &Project, inst: &DecodedInstruction) -> Option<Address> {
    let (seg, ofs) = inst.branch_destination()?;
    let seg_idx = project.segment_index_for(seg)?;
    Some((seg_idx, ofs as u32))
}

// ── Transfer function ─────────────────────────────────────────────────────────

fn apply_transfer(
    project: &Project,
    inst: &DecodedInstruction,
    state: &mut AbsState,
    estimates: &FunctionPreservesMap,
) {
    let modrm = inst.modrm;

    match inst.opcode {
        Opcode::Push => {
            let slot = match sreg_slot_from_arg(inst.arg_type[0], modrm) {
                Some(SRegKind::Ds) if state.ds_eq_entry => Slot::EntryDs,
                Some(SRegKind::Es) if state.es_eq_entry => Slot::EntryEs,
                Some(SRegKind::Ss) if state.ss_eq_entry => Slot::EntrySs,
                _ => Slot::Other,
            };
            state.push_slot(slot);
        }

        Opcode::Pop => {
            let popped = state.pop_slot();
            match sreg_slot_from_arg(inst.arg_type[0], modrm) {
                Some(SRegKind::Ds) => state.ds_eq_entry = matches!(popped, Slot::EntryDs),
                Some(SRegKind::Es) => state.es_eq_entry = matches!(popped, Slot::EntryEs),
                Some(SRegKind::Ss) => state.ss_eq_entry = matches!(popped, Slot::EntrySs),
                Some(SRegKind::Cs) => {} // unusual; CS not tracked
                None => {} // pop to gp/mem — already discarded
            }
            if writes_to_sp(inst) {
                state.invalidate_stack();
            }
        }

        Opcode::Pushf => {
            state.push_slot(Slot::Other);
        }

        Opcode::Popf => {
            let _ = state.pop_slot();
        }

        Opcode::Mov | Opcode::Xchg => {
            // Clear eq flags for any segment-register destination.
            for i in 0..2 {
                if matches!(inst.arg_dir[i], ArgDir::WO | ArgDir::RW) {
                    if let Some(k) = sreg_slot_from_arg(inst.arg_type[i], modrm) {
                        match k {
                            SRegKind::Ds => state.ds_eq_entry = false,
                            SRegKind::Es => state.es_eq_entry = false,
                            SRegKind::Ss => state.ss_eq_entry = false,
                            SRegKind::Cs => {}
                        }
                    }
                }
            }
            if writes_to_sp(inst) {
                state.invalidate_stack();
            }
        }

        Opcode::Lds => {
            state.ds_eq_entry = false;
        }
        Opcode::Les => {
            state.es_eq_entry = false;
        }

        Opcode::Call => match call_dest(project, inst) {
            Some(addr) => {
                let est = estimates.get(&addr).copied().unwrap_or(FunctionPreserves::BOTTOM);
                state.ds_eq_entry &= est.ds;
                state.es_eq_entry &= est.es;
                state.ss_eq_entry &= est.ss;
            }
            None => {
                // Indirect call: clobber.
                state.ds_eq_entry = false;
                state.es_eq_entry = false;
                state.ss_eq_entry = false;
            }
        },

        Opcode::Int | Opcode::Into => {
            // Conservative for the general case; SS is hardware-restored by iret.
            state.ds_eq_entry = false;
            state.es_eq_entry = false;
        }

        _ => {
            // Catch any other instruction that might alter SP (add, sub, inc, dec, lea, …).
            if writes_to_sp(inst) {
                state.invalidate_stack();
            }
        }
    }
}

fn transfer_block(
    project: &Project,
    block_seg: SegmentIdx,
    start: u32,
    end: u32,
    entry_state: &AbsState,
    estimates: &FunctionPreservesMap,
) -> AbsState {
    let mut state = entry_state.clone();
    let seg = &project.segments[block_seg];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
    let mut ofs = start;

    while ofs < end {
        let bytes = project.bytes_at_seg(block_seg, ofs);
        let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else {
            break;
        };
        let len = inst.bytes.len() as u32;
        apply_transfer(project, &inst, &mut state, estimates);
        ofs += len;
    }

    state
}

// ── Function-entry enumeration ────────────────────────────────────────────────

fn collect_call_entries(project: &Project) -> BTreeSet<Address> {
    let mut entries = BTreeSet::new();
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

    // EXE entry point (`start`) is seeded by the OS loader, never reached via
    // a Call in the program text — but it's structurally a function and the
    // analysis should know about it.
    if let Some(exe) = &project.exe
        && let Some(seg_idx) = project.segment_index_for(exe.head.cs)
    {
        entries.insert((seg_idx, exe.head.ip as u32));
    }

    entries
}

// ── Per-function CFG walk ─────────────────────────────────────────────────────

/// Decode the last instruction of a block.
fn last_instruction(project: &Project, block_seg: SegmentIdx, block_start: u32, block_end: u32) -> Option<DecodedInstruction> {
    let last_ofs = project.segments[block_seg]
        .addr_attributes
        .prev(block_end)
        .filter(|&p| p >= block_start)
        .unwrap_or(block_start);
    let seg = &project.segments[block_seg];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
    decode(
        seg_val,
        last_ofs as u16,
        project.bytes_at_seg(block_seg, last_ofs).iter().copied(),
    )
}

/// Set of basic-block start addresses that belong to function `entry`.
/// Stops at any successor that is a known function entry other than `entry`
/// (those edges become tail calls handled at the dataflow level). Does not
/// follow Call edges (only the fall-through side of a Call-terminated block).
fn walk_function_blocks(
    project: &Project,
    entry: Address,
    known_entries: &BTreeSet<Address>,
) -> BTreeSet<Address> {
    let mut visited: BTreeSet<Address> = BTreeSet::new();
    let mut queue: Vec<Address> = vec![entry];

    while let Some(addr) = queue.pop() {
        if !visited.insert(addr) {
            continue;
        }
        let Some(block) = project.blocks.block_at(addr.0, addr.1) else {
            continue;
        };

        let last_inst = last_instruction(project, block.seg_idx, block.start, block.end);
        let last_is_call = last_inst.as_ref().is_some_and(|i| i.opcode == Opcode::Call);

        for &succ in &block.successors {
            if last_is_call && succ != (block.seg_idx, block.end) {
                continue; // call edge — not in-function
            }
            if known_entries.contains(&succ) && succ != entry {
                continue; // tail-call edge — not in-function
            }
            queue.push(succ);
        }
    }

    visited
}

// ── Intra-procedural pass ─────────────────────────────────────────────────────

fn run_intra(
    project: &Project,
    entry: Address,
    in_func_blocks: &BTreeSet<Address>,
    known_entries: &BTreeSet<Address>,
    estimates: &FunctionPreservesMap,
    callers: &mut BTreeMap<Address, BTreeSet<Address>>,
) -> FunctionPreserves {
    let mut block_in: BTreeMap<Address, AbsState> = BTreeMap::new();
    block_in.insert(entry, AbsState::initial());
    let mut worklist: VecDeque<Address> = VecDeque::from([entry]);
    let mut visited: BTreeSet<Address> = BTreeSet::new();

    let mut result = FunctionPreserves::TOP;

    while let Some(b_addr) = worklist.pop_front() {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else {
            continue;
        };
        let entry_state = match block_in.get(&b_addr) {
            Some(s) => s.clone(),
            None => continue,
        };
        visited.insert(b_addr);

        let exit_state = transfer_block(
            project,
            block.seg_idx,
            block.start,
            block.end,
            &entry_state,
            estimates,
        );

        let last_inst = last_instruction(project, block.seg_idx, block.start, block.end);
        let last_op = last_inst.as_ref().map(|i| i.opcode);
        let last_is_call = last_op == Some(Opcode::Call);

        // Per-return contribution.
        match last_op {
            Some(Opcode::Ret) => {
                result.ds &= exit_state.ds_eq_entry;
                result.es &= exit_state.es_eq_entry;
                result.ss &= exit_state.ss_eq_entry;
            }
            Some(Opcode::Retf) => {
                result.ds &= exit_state.ds_eq_entry;
                result.es &= exit_state.es_eq_entry;
                result.ss &= exit_state.ss_eq_entry && exit_state.stack_is_empty();
            }
            Some(Opcode::Iret) => {
                result.ds &= exit_state.ds_eq_entry;
                result.es &= exit_state.es_eq_entry;
                result.ss &= exit_state.ss_eq_entry;
            }
            Some(Opcode::Hlt) => {
                // No contribution.
            }
            Some(Opcode::Jmp) if block.successors.is_empty() => {
                // Indirect jmp with no resolvable destination — treat as tail call to unknown.
                result.ds = false;
                result.es = false;
                result.ss = false;
            }
            _ => {}
        }

        // Edge classification & propagation.
        for &succ in &block.successors {
            if last_is_call && succ != (block.seg_idx, block.end) {
                continue; // ignore call edge to callee
            }
            if in_func_blocks.contains(&succ) {
                let new_state = match block_in.get(&succ) {
                    Some(prev) => prev.meet(&exit_state),
                    None => exit_state.clone(),
                };
                let changed = block_in.get(&succ) != Some(&new_state);
                if changed {
                    block_in.insert(succ, new_state);
                    worklist.push_back(succ);
                }
            } else if known_entries.contains(&succ) && succ != entry {
                // Tail call to a known function.
                callers.entry(succ).or_default().insert(entry);
                let est = estimates.get(&succ).copied().unwrap_or(FunctionPreserves::BOTTOM);
                result.ds &= exit_state.ds_eq_entry && est.ds;
                result.es &= exit_state.es_eq_entry && est.es;
                result.ss &= exit_state.ss_eq_entry && est.ss;
            } else {
                // Unknown destination outside the function — conservative.
                result.ds = false;
                result.es = false;
                result.ss = false;
            }
        }
    }

    // Record direct callees (for the call-graph caller index) by re-decoding.
    for &b_addr in &visited {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else { continue };
        let seg = &project.segments[block.seg_idx];
        let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
        let mut ofs = block.start;
        while ofs < block.end {
            let bytes = project.bytes_at_seg(block.seg_idx, ofs);
            let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else { break };
            if inst.opcode == Opcode::Call {
                if let Some(addr) = call_dest(project, &inst) {
                    callers.entry(addr).or_default().insert(entry);
                }
            }
            ofs += inst.bytes.len() as u32;
        }
    }

    result
}

// ── Public entry point ────────────────────────────────────────────────────────

pub fn compute(project: &Project) -> FunctionPreservesMap {
    let known_entries = collect_call_entries(project);

    let mut estimates: FunctionPreservesMap = BTreeMap::new();
    let mut blocks_per_func: BTreeMap<Address, BTreeSet<Address>> = BTreeMap::new();
    let mut analyzable: BTreeSet<Address> = BTreeSet::new();

    for &e in &known_entries {
        if project.blocks.block_at(e.0, e.1).is_some() {
            estimates.insert(e, FunctionPreserves::TOP);
            let bs = walk_function_blocks(project, e, &known_entries);
            blocks_per_func.insert(e, bs);
            analyzable.insert(e);
        } else {
            estimates.insert(e, FunctionPreserves::BOTTOM);
        }
    }

    let mut callers: BTreeMap<Address, BTreeSet<Address>> = BTreeMap::new();
    let mut worklist: VecDeque<Address> = VecDeque::from_iter(analyzable.iter().copied());
    let mut in_worklist: BTreeSet<Address> = analyzable.clone();

    while let Some(e) = worklist.pop_front() {
        in_worklist.remove(&e);
        let Some(in_func_blocks) = blocks_per_func.get(&e) else { continue };
        let new = run_intra(project, e, in_func_blocks, &known_entries, &estimates, &mut callers);
        let prev = estimates.get(&e).copied().unwrap_or(FunctionPreserves::TOP);
        let merged = prev.meet(new);
        if merged != prev {
            estimates.insert(e, merged);
            for &caller in callers.get(&e).into_iter().flatten() {
                if analyzable.contains(&caller) && !in_worklist.contains(&caller) {
                    worklist.push_back(caller);
                    in_worklist.insert(caller);
                }
            }
        }
    }

    estimates
}

// ── Clobber trace ─────────────────────────────────────────────────────────────

/// One instruction's contribution to a function failing to preserve a segment
/// register or the abstract stack shape.
#[derive(Clone, Debug)]
pub struct ClobberRecord {
    pub addr: Address,
    pub clobbers_ds: bool,
    pub clobbers_es: bool,
    pub clobbers_ss: bool,
    pub stack_invalidated: bool,
    pub reason: String,
}

impl ClobberRecord {
    fn is_empty(&self) -> bool {
        !(self.clobbers_ds || self.clobbers_es || self.clobbers_ss || self.stack_invalidated)
    }
}

/// Re-run the per-function intra-procedural pass for `entry` and return every
/// instruction that downgrades preservation for DS/ES/SS or invalidates the
/// abstract stack. Uses the project's already-computed `function_preserves`
/// estimates for callee lookups.
///
/// Returns an empty vector when `entry` is not a known function entry or has
/// no decoded body.
pub fn trace_function_clobbers(project: &Project, entry: Address) -> Vec<ClobberRecord> {
    if project.blocks.block_at(entry.0, entry.1).is_none() {
        return Vec::new();
    }
    let known_entries: BTreeSet<Address> = project.function_preserves.keys().copied().collect();
    let in_func_blocks = walk_function_blocks(project, entry, &known_entries);
    let estimates = &project.function_preserves;

    // 1. Forward dataflow: compute per-block entry states.
    let mut block_in: BTreeMap<Address, AbsState> = BTreeMap::new();
    block_in.insert(entry, AbsState::initial());
    let mut worklist: VecDeque<Address> = VecDeque::from([entry]);

    while let Some(b_addr) = worklist.pop_front() {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else { continue };
        let entry_state = match block_in.get(&b_addr) {
            Some(s) => s.clone(),
            None => continue,
        };
        let exit_state = transfer_block(
            project,
            block.seg_idx,
            block.start,
            block.end,
            &entry_state,
            estimates,
        );

        let last_inst = last_instruction(project, block.seg_idx, block.start, block.end);
        let last_is_call = last_inst.as_ref().map(|i| i.opcode) == Some(Opcode::Call);

        for &succ in &block.successors {
            if last_is_call && succ != (block.seg_idx, block.end) {
                continue;
            }
            if in_func_blocks.contains(&succ) {
                let new_state = match block_in.get(&succ) {
                    Some(prev) => prev.meet(&exit_state),
                    None => exit_state.clone(),
                };
                if block_in.get(&succ) != Some(&new_state) {
                    block_in.insert(succ, new_state);
                    worklist.push_back(succ);
                }
            }
        }
    }

    // 2. Walk every block instruction-by-instruction, recording transitions.
    let mut records: Vec<ClobberRecord> = Vec::new();

    for &b_addr in &in_func_blocks {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else { continue };
        let mut state = match block_in.get(&b_addr) {
            Some(s) => s.clone(),
            None => continue,
        };

        let seg = &project.segments[block.seg_idx];
        let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
        let mut ofs = block.start;
        let mut last_ofs = block.start;
        let mut last_inst_opt: Option<DecodedInstruction> = None;

        while ofs < block.end {
            let bytes = project.bytes_at_seg(block.seg_idx, ofs);
            let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else { break };
            let len = inst.bytes.len() as u32;

            let before = state.clone();
            apply_transfer(project, &inst, &mut state, estimates);

            let clobbers_ds = before.ds_eq_entry && !state.ds_eq_entry;
            let clobbers_es = before.es_eq_entry && !state.es_eq_entry;
            let clobbers_ss = before.ss_eq_entry && !state.ss_eq_entry;
            let stack_invalidated = matches!(before.stack, StackState::Tracked(_))
                && matches!(state.stack, StackState::Invalid);

            if clobbers_ds || clobbers_es || clobbers_ss || stack_invalidated {
                let reason = explain_clobber(
                    project,
                    &inst,
                    &before,
                    estimates,
                    clobbers_ds,
                    clobbers_es,
                    clobbers_ss,
                    stack_invalidated,
                );
                records.push(ClobberRecord {
                    addr: (block.seg_idx, ofs),
                    clobbers_ds,
                    clobbers_es,
                    clobbers_ss,
                    stack_invalidated,
                    reason,
                });
            }

            last_ofs = ofs;
            last_inst_opt = Some(inst);
            ofs += len;
        }

        // Tail-call edge: any successor that's a known entry other than `entry`
        // and not in our in-function block set.
        for &succ in &block.successors {
            let last_is_call =
                last_inst_opt.as_ref().map(|i| i.opcode) == Some(Opcode::Call);
            if last_is_call && succ != (block.seg_idx, block.end) {
                continue;
            }
            if in_func_blocks.contains(&succ) {
                continue;
            }
            // out-of-function edge
            let (rec_ds, rec_es, rec_ss, reason) = if known_entries.contains(&succ) && succ != entry
            {
                let est = estimates.get(&succ).copied().unwrap_or(FunctionPreserves::BOTTOM);
                let ds = state.ds_eq_entry && !est.ds;
                let es = state.es_eq_entry && !est.es;
                let ss = state.ss_eq_entry && !est.ss;
                let name = project.name_at(succ.0, succ.1).map(|s| s.to_owned()).unwrap_or_else(
                    || format!("{}:{:04x}", project.segments[succ.0].name, succ.1),
                );
                let mut what = Vec::new();
                if !est.ds { what.push("DS"); }
                if !est.es { what.push("ES"); }
                if !est.ss { what.push("SS"); }
                let what_str = if what.is_empty() { "nothing".to_string() } else { what.join(", ") };
                (ds, es, ss, format!("tail call to {name}: callee clobbers {what_str}"))
            } else {
                let ds = state.ds_eq_entry;
                let es = state.es_eq_entry;
                let ss = state.ss_eq_entry;
                (ds, es, ss, "tail call to unknown destination".to_string())
            };
            if rec_ds || rec_es || rec_ss {
                records.push(ClobberRecord {
                    addr: (block.seg_idx, last_ofs),
                    clobbers_ds: rec_ds,
                    clobbers_es: rec_es,
                    clobbers_ss: rec_ss,
                    stack_invalidated: false,
                    reason,
                });
            }
        }
    }

    records.sort_by_key(|r| r.addr);
    records.retain(|r| !r.is_empty());
    records
}

#[allow(clippy::too_many_arguments)]
fn explain_clobber(
    project: &Project,
    inst: &DecodedInstruction,
    before: &AbsState,
    estimates: &FunctionPreservesMap,
    clobbers_ds: bool,
    clobbers_es: bool,
    clobbers_ss: bool,
    stack_invalidated: bool,
) -> String {
    match inst.opcode {
        Opcode::Mov | Opcode::Xchg => {
            let mut parts = Vec::new();
            if clobbers_ds { parts.push("DS"); }
            if clobbers_es { parts.push("ES"); }
            if clobbers_ss { parts.push("SS"); }
            if !parts.is_empty() {
                return format!("direct write to {}", parts.join("/"));
            }
            if stack_invalidated {
                return "writes SP — abstract stack invalidated".into();
            }
            "writes a tracked register".into()
        }
        Opcode::Lds => "loads DS from memory".into(),
        Opcode::Les => "loads ES from memory".into(),
        Opcode::Pop => {
            if stack_invalidated {
                "pop sp — abstract stack invalidated".into()
            } else {
                let mut parts = Vec::new();
                if clobbers_ds { parts.push("DS"); }
                if clobbers_es { parts.push("ES"); }
                if clobbers_ss { parts.push("SS"); }
                if parts.is_empty() {
                    "pop".into()
                } else {
                    format!("pop {}: top-of-stack slot does not match entry value",
                        parts.join("/"))
                }
            }
        }
        Opcode::Call => match call_dest(project, inst) {
            Some(addr) => {
                let est = estimates.get(&addr).copied().unwrap_or(FunctionPreserves::BOTTOM);
                let name = project.name_at(addr.0, addr.1).map(|s| s.to_owned()).unwrap_or_else(
                    || format!("{}:{:04x}", project.segments[addr.0].name, addr.1),
                );
                let mut what = Vec::new();
                if !est.ds { what.push("DS"); }
                if !est.es { what.push("ES"); }
                if !est.ss { what.push("SS"); }
                let what_str = if what.is_empty() { "nothing".into() } else { what.join(", ") };
                format!("call {name}: callee clobbers {what_str}")
            }
            None => "indirect call: clobbers DS, ES, SS".into(),
        },
        Opcode::Int | Opcode::Into => "int: clobbers DS, ES (conservative)".into(),
        _ => {
            if stack_invalidated {
                "writes SP — abstract stack invalidated".into()
            } else {
                let mut parts = Vec::new();
                if clobbers_ds { parts.push("DS"); }
                if clobbers_es { parts.push("ES"); }
                if clobbers_ss { parts.push("SS"); }
                let _ = before; // suppress warning
                format!("clobbers {}", parts.join("/"))
            }
        }
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address_attributes::AddressAttributes;
    use crate::project::BinImage;

    /// Build a minimal in-memory project from raw bytes loaded at offset 0
    /// of `seg000`, with `AttrType::Code` markers seeding disassembly.
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
        // Reset addr_attributes to the right size in case the parser used a
        // different one.
        p.segments[SegmentIdx::from(0)].addr_attributes =
            AddressAttributes::new(code.len());
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

    /// Encode `call rel16` at `at_ofs` targeting `target_ofs`.
    fn call_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE8, disp as u8, (disp >> 8) as u8]
    }

    /// Encode `jmp rel16` at `at_ofs` targeting `target_ofs`.
    fn jmp_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE9, disp as u8, (disp >> 8) as u8]
    }

    #[test]
    fn push_pop_ds_preserves_ds() {
        // F @ 0x00: push ds; pop ds; ret.
        // Caller @ 0x10: call F; ret.
        let mut code = vec![
            0x1E, // push ds
            0x1F, // pop ds
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3); // ret
        let p = make_project(&code, &[0x10]);
        assert_eq!(
            p.function_preserves.get(&at(0x00)).copied(),
            Some(FunctionPreserves::TOP),
            "push ds; pop ds; ret should preserve everything"
        );
    }

    #[test]
    fn mov_ds_clobbers_ds() {
        // F @ 0x00: mov ds, ax; ret.
        let mut code = vec![
            0x8E, 0xD8, // mov ds, ax (modrm: mod=11 reg=011(DS) rm=000(AX))
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ds, "mov ds, ax should clobber DS");
        assert!(pres.es, "ES untouched");
        assert!(pres.ss, "SS untouched");
    }

    #[test]
    fn lds_clobbers_ds() {
        // F @ 0x00: lds ax, [bx]; ret.
        let mut code = vec![
            0xC5, 0x07, // lds ax, [bx] (modrm: mod=00 reg=000(AX) rm=111(BX))
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ds);
        assert!(pres.es);
    }

    #[test]
    fn push_ds_pop_es_pop_ds_clobbers_ds() {
        // Mismatched slots: push ds, pop es, pop ds — DS not preserved.
        let mut code = vec![
            0x1E, // push ds
            0x07, // pop es
            0x1F, // pop ds
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ds, "DS popped from non-EntryDs slot");
        assert!(!pres.es, "ES popped from non-EntryEs slot");
    }

    #[test]
    fn mov_ss_clobbers_ss() {
        // F @ 0x00: mov ss, ax; mov sp, bx; ret.
        let mut code = vec![
            0x8E, 0xD0, // mov ss, ax (reg=010 SS, rm=000 AX)
            0x89, 0xDC, // mov sp, bx (rm=100 SP, reg=011 BX)
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ss, "mov ss, ax should clobber SS");
        assert!(pres.ds);
        assert!(pres.es);
    }

    #[test]
    fn callee_ds_clobber_propagates() {
        // F @ 0x00: call G; ret. G @ 0x10: mov ds, ax; ret.
        // F should inherit ds:false from G.
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x10)); // call G
        code.push(0xC3); // ret
        code.resize(0x10, 0x90);
        code.extend_from_slice(&[0x8E, 0xD8, 0xC3]); // G: mov ds, ax; ret
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00)); // outer caller calls F
        code.push(0xC3);
        let p = make_project(&code, &[0x20]);
        let f = p.function_preserves.get(&at(0x00)).copied().unwrap();
        let g = p.function_preserves.get(&at(0x10)).copied().unwrap();
        assert!(!g.ds);
        assert!(!f.ds, "F should inherit DS-clobber from G");
    }

    #[test]
    fn tail_call_inherits_callee() {
        // F @ 0x00: jmp G (tail call). G @ 0x10: mov ds, ax; ret.
        // H @ 0x20 calls G (so G is in known_entries).
        // I @ 0x30 calls F (so F is reachable).
        let mut code = Vec::new();
        code.extend_from_slice(&jmp_rel16(0x00, 0x10)); // jmp G
        code.resize(0x10, 0x90);
        code.extend_from_slice(&[0x8E, 0xD8, 0xC3]); // G: mov ds, ax; ret
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x10)); // call G
        code.push(0xC3);
        code.resize(0x30, 0x90);
        code.extend_from_slice(&call_rel16(0x30, 0x00)); // call F
        code.push(0xC3);
        let p = make_project(&code, &[0x20, 0x30]);
        let f = p.function_preserves.get(&at(0x00)).copied().unwrap();
        let g = p.function_preserves.get(&at(0x10)).copied().unwrap();
        assert!(!g.ds);
        assert!(!f.ds, "F tail-calls G and should inherit DS-clobber");
    }

    #[test]
    fn indirect_call_clobbers_all() {
        // F @ 0x00: call ax; ret.
        let mut code = vec![
            0xFF, 0xD0, // call ax (modrm: 11 010 000)
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ds);
        assert!(!pres.es);
        assert!(!pres.ss);
    }

    #[test]
    fn add_sp_invalidates_stack() {
        // F @ 0x00: push ds; add sp, 4; pop ds; ret.
        let mut code = vec![
            0x1E,             // push ds
            0x83, 0xC4, 0x04, // add sp, 4 (group, /0=ADD, rm=100=SP)
            0x1F,             // pop ds
            0xC3,             // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let pres = p.function_preserves.get(&at(0x00)).copied().unwrap();
        assert!(!pres.ds, "stack invalidation must downgrade pop ds to non-equal");
    }

    #[test]
    fn mutual_recursion_converges_to_top() {
        // F @ 0x00: call G; ret. G @ 0x10: call F; ret. Both pure.
        // External caller H @ 0x20: call F; ret.
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x10)); // F: call G
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00)); // G: call F
        code.push(0xC3);
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00)); // H: call F
        code.push(0xC3);
        let p = make_project(&code, &[0x20]);
        let f = p.function_preserves.get(&at(0x00)).copied().unwrap();
        let g = p.function_preserves.get(&at(0x10)).copied().unwrap();
        assert_eq!(f, FunctionPreserves::TOP);
        assert_eq!(g, FunctionPreserves::TOP);
    }
}

use std::collections::{BTreeMap, VecDeque};

use crate::{
    Address, GpReg16, Operand, SReg, SRegMap, SmallString,
    basic_block::BasicBlock,
    decode,
    decoded_instruction::DecodedInstruction,
    opcode_table::Opcode,
    project::{Project, SegmentIdx},
};

// ── Abstract value ────────────────────────────────────────────────────────────

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SegVal {
    /// A specific project segment index is known.
    Known(SegmentIdx),
    /// No information — could be anything.
    Unknown,
}

impl SegVal {
    fn join(&self, other: &Self) -> Self {
        if self == other {
            self.clone()
        } else {
            SegVal::Unknown
        }
    }
}

// ── Abstract state ────────────────────────────────────────────────────────────

/// Abstract register state at a program point.
///
/// `sregs` are indexed by `SReg as usize` (ES=0, CS=1, SS=2, DS=3).
/// `gpregs` are indexed by `GpReg16 as usize` (AX=0..DI=7).
/// GP registers are tracked only as conduits for segment values.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AbstractState {
    pub sregs: [SegVal; 4],
    pub gpregs: [SegVal; 8],
}

impl AbstractState {
    pub fn all_unknown() -> Self {
        Self {
            sregs: [
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
            ],
            gpregs: [
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
                SegVal::Unknown,
            ],
        }
    }

    pub fn join(&self, other: &Self) -> Self {
        let sregs = std::array::from_fn(|i| self.sregs[i].join(&other.sregs[i]));
        let gpregs = std::array::from_fn(|i| self.gpregs[i].join(&other.gpregs[i]));
        Self { sregs, gpregs }
    }

    fn sreg_idx(r: SReg) -> usize {
        match r {
            SReg::ES => 0,
            SReg::CS => 1,
            SReg::SS => 2,
            SReg::DS => 3,
        }
    }

    fn sreg_idx_from_str(s: &SmallString) -> Option<usize> {
        match s.as_str() {
            "es" => Some(0),
            "cs" => Some(1),
            "ss" => Some(2),
            "ds" => Some(3),
            _ => None,
        }
    }

    pub fn get_sreg(&self, r: SReg) -> &SegVal {
        &self.sregs[Self::sreg_idx(r)]
    }

    pub fn set_sreg(&mut self, r: SReg, v: SegVal) {
        self.sregs[Self::sreg_idx(r)] = v;
    }

    fn get_gpreg(&self, r: GpReg16) -> &SegVal {
        &self.gpregs[r as usize]
    }

    fn set_gpreg(&mut self, r: GpReg16, v: SegVal) {
        self.gpregs[r as usize] = v;
    }

    pub fn to_sreg_map(&self) -> SRegMap {
        fn known(v: &SegVal) -> Option<SegmentIdx> {
            if let SegVal::Known(idx) = v {
                Some(*idx)
            } else {
                None
            }
        }

        SRegMap {
            es: known(&self.sregs[0]),
            cs: known(&self.sregs[1]),
            ss: known(&self.sregs[2]),
            ds: known(&self.sregs[3]),
        }
    }
}

// ── Dataflow result ───────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub struct SegDataflow {
    pub block_entry: BTreeMap<Address, AbstractState>,
}

impl SegDataflow {
    pub fn new() -> Self {
        Self {
            block_entry: BTreeMap::new(),
        }
    }

    pub fn entry_state(&self, seg_idx: SegmentIdx, block_start: u32) -> Option<&AbstractState> {
        self.block_entry.get(&(seg_idx, block_start))
    }

    /// Abstract state immediately before the instruction at `(seg_idx, ofs)`.
    /// Re-runs the transfer function from the containing block's entry state up to `ofs`.
    pub fn state_at(
        &self,
        project: &Project,
        seg_idx: SegmentIdx,
        ofs: u32,
    ) -> Option<AbstractState> {
        let block = project.blocks.block_containing(seg_idx, ofs)?;
        let entry = self.entry_state(block.seg_idx, block.start)?;
        Some(transfer_block_until(project, block, entry, ofs))
    }
}

// ── Operand → abstract value helpers ──────────────────────────────────────────

/// Read the abstract segment value of an operand (for moves / xchg / push).
/// Returns Unknown for non-register operands and for 8-bit registers (which
/// the analysis does not track).
fn read_seg_val(state: &AbstractState, inst: &DecodedInstruction, i: usize) -> SegVal {
    match inst.operand(i) {
        Operand::Sreg(r) => state.get_sreg(r).clone(),
        Operand::Gp16(r) => state.get_gpreg(r).clone(),
        _ => SegVal::Unknown,
    }
}

/// Write an abstract value to a destination operand. Memory destinations are
/// silently ignored (we don't track memory).
fn write_seg_val(state: &mut AbstractState, inst: &DecodedInstruction, i: usize, val: SegVal) {
    match inst.operand(i) {
        Operand::Sreg(r) => state.set_sreg(r, val),
        Operand::Gp16(r) => state.set_gpreg(r, val),
        _ => {}
    }
}

// ── Transfer function ─────────────────────────────────────────────────────────

/// Run the transfer function from `block.start` up to (but not including) `stop_before`.
/// Pass `block.end` for the full-block exit state.
fn transfer_block_until(
    project: &Project,
    block: &BasicBlock,
    entry: &AbstractState,
    stop_before: u32,
) -> AbstractState {
    let mut state = entry.clone();
    let mut abstract_stack: Vec<SegVal> = Vec::new();

    let seg = &project.segments[block.seg_idx];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
    let mut ofs = block.start;

    while ofs < block.end && ofs < stop_before {
        apply_attr_assumes(&mut state, project, block.seg_idx, ofs);

        let bytes = project.bytes_at_seg(block.seg_idx, ofs);
        let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else {
            break;
        };

        let len = inst.bytes.len() as u32;

        match inst.opcode {
            Opcode::Push => {
                abstract_stack.push(read_seg_val(&state, &inst, 0));
            }

            Opcode::Pop => {
                let val = abstract_stack.pop().unwrap_or(SegVal::Unknown);
                write_seg_val(&mut state, &inst, 0, val);
            }

            Opcode::Mov => {
                if let Some((dst, src)) = inst.dst_src() {
                    let v = read_seg_val(&state, &inst, src);
                    write_seg_val(&mut state, &inst, dst, v);
                }
            }

            Opcode::Xchg => {
                let a = read_seg_val(&state, &inst, 0);
                let b = read_seg_val(&state, &inst, 1);
                write_seg_val(&mut state, &inst, 0, b);
                write_seg_val(&mut state, &inst, 1, a);
            }

            Opcode::Lds => {
                state.set_sreg(SReg::DS, SegVal::Unknown);
                write_seg_val(&mut state, &inst, 0, SegVal::Unknown);
            }

            Opcode::Les => {
                state.set_sreg(SReg::ES, SegVal::Unknown);
                write_seg_val(&mut state, &inst, 0, SegVal::Unknown);
            }

            // Near calls preserve CS; consult the per-function preservation
            // analysis for DS/ES/SS, and assume all GP regs are clobbered.
            Opcode::Call => {
                clobber_call_with_preserves(&mut state, project, &inst);
            }

            // Interrupts: handler returns via iret, restoring CS/SS/flags from stack,
            // but DS/ES/GP may have changed.
            Opcode::Int | Opcode::Into => {
                clobber_call(&mut state);
            }

            // Stack-pointer manipulation invalidates the abstract stack.
            Opcode::Pushf | Opcode::Popf => {
                abstract_stack.clear();
            }

            // Any other opcode: clobber destination GP registers.
            _ => {
                for i in inst.destinations() {
                    if let Operand::Gp16(r) = inst.operand(i) {
                        state.set_gpreg(r, SegVal::Unknown);
                    }
                }
            }
        }

        ofs += len;
    }

    apply_attr_assumes(&mut state, project, block.seg_idx, stop_before);

    state
}

fn apply_attr_assumes(state: &mut AbstractState, project: &Project, seg_idx: SegmentIdx, ofs: u32) {
    let Some(attr) = project.attr_at(seg_idx, ofs) else {
        return;
    };

    for (sreg_name, seg_name) in &attr.assume {
        let Some(sreg) = AbstractState::sreg_idx_from_str(sreg_name) else {
            continue;
        };
        if let Some(target_idx) = project.segments.iter().position(|s| &s.name == seg_name) {
            state.sregs[sreg] = SegVal::Known(SegmentIdx::from(target_idx));
        }
    }
}

fn transfer_block(project: &Project, block: &BasicBlock, entry: &AbstractState) -> AbstractState {
    transfer_block_until(project, block, entry, block.end)
}

/// Conservatively model a call/int: CS is preserved (near code), DS/ES and all GP regs go Unknown.
fn clobber_call(state: &mut AbstractState) {
    state.set_sreg(SReg::DS, SegVal::Unknown);
    state.set_sreg(SReg::ES, SegVal::Unknown);
    for r in &[
        GpReg16::AX,
        GpReg16::CX,
        GpReg16::DX,
        GpReg16::BX,
        GpReg16::SP,
        GpReg16::BP,
        GpReg16::SI,
        GpReg16::DI,
    ] {
        state.set_gpreg(*r, SegVal::Unknown);
    }
}

/// Model a `Call` instruction using the per-function preservation analysis.
/// Falls back to [`clobber_call`] when the callee is indirect or unknown.
fn clobber_call_with_preserves(
    state: &mut AbstractState,
    project: &Project,
    inst: &crate::DecodedInstruction,
) {
    // GP regs are always assumed caller-save.
    for r in &[
        GpReg16::AX,
        GpReg16::CX,
        GpReg16::DX,
        GpReg16::BX,
        GpReg16::SP,
        GpReg16::BP,
        GpReg16::SI,
        GpReg16::DI,
    ] {
        state.set_gpreg(*r, SegVal::Unknown);
    }

    let preserves = inst
        .branch_destination()
        .and_then(|(seg, ofs)| {
            let seg_idx = project.segment_index_for(seg)?;
            project.function_preserves.get(&(seg_idx, ofs as u32)).copied()
        });

    match preserves {
        Some(p) => {
            if !p.ds {
                state.set_sreg(SReg::DS, SegVal::Unknown);
            }
            if !p.es {
                state.set_sreg(SReg::ES, SegVal::Unknown);
            }
            if !p.ss {
                state.set_sreg(SReg::SS, SegVal::Unknown);
            }
        }
        None => {
            // Indirect or unknown — clobber DS, ES, SS conservatively.
            state.set_sreg(SReg::DS, SegVal::Unknown);
            state.set_sreg(SReg::ES, SegVal::Unknown);
            state.set_sreg(SReg::SS, SegVal::Unknown);
        }
    }
}

// ── Fixed-point iteration ─────────────────────────────────────────────────────

pub fn compute(project: &Project) -> SegDataflow {
    let mut df = SegDataflow::new();
    let mut worklist: VecDeque<Address> = VecDeque::new();

    // Seed: every block that has no CFG predecessors gets an initial state
    // with CS = Known(seg_idx) and any segment-level `assume` values applied.
    for block in project.blocks.blocks() {
        if block.predecessors.is_empty() {
            let mut state = AbstractState::all_unknown();
            state.set_sreg(SReg::CS, SegVal::Known(block.seg_idx));
            for (sreg_name, seg_name) in project.segments[block.seg_idx].assume.iter() {
                let Some(sreg) = AbstractState::sreg_idx_from_str(sreg_name) else {
                    continue;
                };
                if let Some(target_idx) = project.segments.iter().position(|s| &s.name == seg_name) {
                    state.sregs[sreg] = SegVal::Known(SegmentIdx::from(target_idx));
                }
            }
            df.block_entry
                .entry((block.seg_idx, block.start))
                .and_modify(|e| *e = e.join(&state))
                .or_insert(state);
            worklist.push_back((block.seg_idx, block.start));
        }
    }

    // Propagate until stable.
    while let Some((seg_idx, start)) = worklist.pop_front() {
        let Some(block) = project.blocks.block_at(seg_idx, start) else {
            continue;
        };

        let entry = df
            .block_entry
            .get(&(seg_idx, start))
            .cloned()
            .unwrap_or_else(AbstractState::all_unknown);

        let exit = transfer_block(project, block, &entry);

        let successors: Vec<Address> = block.successors.to_vec();
        for (succ_seg, succ_ofs) in successors {
            let changed = match df.block_entry.get_mut(&(succ_seg, succ_ofs)) {
                Some(old) => {
                    let new = old.join(&exit);
                    if new != *old {
                        *old = new;
                        true
                    } else {
                        false
                    }
                }
                None => {
                    df.block_entry.insert((succ_seg, succ_ofs), exit.clone());
                    true
                }
            };
            if changed {
                worklist.push_back((succ_seg, succ_ofs));
            }
        }
    }

    df
}

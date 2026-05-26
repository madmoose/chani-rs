use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    Address, DisasmCtx, GpReg16, Operand, SReg, SRegMap, SmallString,
    basic_block::BasicBlock,
    decode_with_ctx,
    decoded_instruction::DecodedInstruction,
    function_summary::{ExitVal, RegId},
    opcode_table::Opcode,
    project::{Project, SegmentIdx},
};

/// Decode the instruction at `(seg_idx, ofs)` in `project`, consulting the
/// EXE relocation table so that Imm16 operands resolve to `Operand::SegRef`
/// for relocated segment-paragraph constants (e.g. `mov ax, seg001`).
fn decode_in_project(
    project: &Project,
    seg_idx: SegmentIdx,
    ofs: u32,
) -> Option<DecodedInstruction> {
    let seg = &project.segments[seg_idx];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
    let ctx = DisasmCtx {
        imm_relocations: Some(&project.imm_relocations),
    };
    decode_with_ctx(
        seg_val,
        ofs as u16,
        project.bytes_at_seg(seg_idx, ofs).iter().copied(),
        &ctx,
    )
}

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

    fn get(&self, r: RegId) -> SegVal {
        match r {
            RegId::Sreg(s) => self.get_sreg(s).clone(),
            RegId::Gp16(g) => self.get_gpreg(g).clone(),
        }
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

/// Read the abstract segment value of an operand. Handles registers and the
/// `SegRef` immediate produced when the EXE relocation table resolves an Imm16
/// to a project segment (e.g. `mov ax, seg001`).
fn read_seg_val(state: &AbstractState, inst: &DecodedInstruction, i: usize) -> SegVal {
    match inst.operand(i) {
        Operand::Sreg(r) => state.get_sreg(r).clone(),
        Operand::Gp16(r) => state.get_gpreg(r).clone(),
        Operand::SegRef(idx) => SegVal::Known(idx),
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

/// Result of transferring through a basic block.
///
/// For most blocks `fall_through` is the only output. For Call-terminated
/// blocks `pre_call` additionally carries the state at the call instruction
/// (used as the entry state of the callee).
#[derive(Clone, Debug)]
struct BlockTransfer {
    fall_through: AbstractState,
    pre_call: Option<AbstractState>,
}

/// Run the transfer function from `block.start` up to (but not including) `stop_before`.
/// Pass `block.end` for the full-block exit state.
fn transfer_block_until(
    project: &Project,
    block: &BasicBlock,
    entry: &AbstractState,
    stop_before: u32,
) -> AbstractState {
    let mut state = entry.clone();
    // CS is determined by the hosting segment.
    state.set_sreg(SReg::CS, SegVal::Known(block.seg_idx));
    let mut abstract_stack: Vec<SegVal> = Vec::new();

    let mut ofs = block.start;

    while ofs < block.end && ofs < stop_before {
        apply_attr_assumes(&mut state, project, block.seg_idx, ofs);

        let Some(inst) = decode_in_project(project, block.seg_idx, ofs) else {
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

            // Near calls: apply the callee's function summary as a state
            // transformer over the caller's pre-call state.
            Opcode::Call => {
                apply_call_summary(&mut state, project, &inst);
            }

            // Interrupts: consult the int description for the specific
            // (vector, condition) — if found, only registers in its `returns`
            // clause are clobbered. Otherwise fall back to clobbering DS/ES
            // and all GP regs (SS is hardware-restored by iret).
            Opcode::Int | Opcode::Into => {
                apply_int(&mut state, project, block.seg_idx, ofs);
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

/// Transfer through a block, returning both the fall-through exit state and,
/// for call-terminated blocks, the state at the call instruction (which is
/// what the callee sees on entry).
fn transfer_block(project: &Project, block: &BasicBlock, entry: &AbstractState) -> BlockTransfer {
    // Locate the last instruction's offset to detect Call termination.
    let last_ofs = project.segments[block.seg_idx]
        .addr_attributes
        .prev(block.end)
        .filter(|&p| p >= block.start)
        .unwrap_or(block.start);

    let last_inst = decode_in_project(project, block.seg_idx, last_ofs);

    let last_is_call = last_inst.as_ref().is_some_and(|i| i.opcode == Opcode::Call);

    if last_is_call {
        // Pre-call state: run transfer up to but not including the call.
        let pre_call = transfer_block_until(project, block, entry, last_ofs);
        // Full exit state including the call's transfer (summary application).
        let fall_through = transfer_block_until(project, block, entry, block.end);
        BlockTransfer {
            fall_through,
            pre_call: Some(pre_call),
        }
    } else {
        let fall_through = transfer_block_until(project, block, entry, block.end);
        BlockTransfer {
            fall_through,
            pre_call: None,
        }
    }
}

/// Conservatively model an int or indirect/unknown call. CS is preserved by
/// hardware. SS is preserved too: interrupts are hardware-balanced through
/// `iret` and indirect callees are assumed to follow the standard SS-preserving
/// calling convention. DS, ES and all GP regs go Unknown.
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

/// Resolve a callee's [`ExitVal`] for one register against the caller's
/// pre-call state.
fn resolve_exit(ev: &ExitVal, pre_call: &AbstractState) -> SegVal {
    match ev {
        ExitVal::Entry(r) => pre_call.get(*r),
        ExitVal::Known(s) => SegVal::Known(*s),
        ExitVal::Unknown => SegVal::Unknown,
    }
}

/// Apply a direct-call callee summary to the caller's pre-call state. If the
/// call target is indirect or has no summary, fall back to a conservative
/// clobber.
fn apply_call_summary(state: &mut AbstractState, project: &Project, inst: &DecodedInstruction) {
    let summary = inst.branch_destination().and_then(|(seg, ofs)| {
        let seg_idx = project.segment_index_for(seg)?;
        project.function_summary.get(&(seg_idx, ofs as u32))
    });

    let Some(summary) = summary else {
        clobber_call(state);
        return;
    };

    let pre = state.clone();
    let new_sregs: [SegVal; 4] = std::array::from_fn(|i| resolve_exit(&summary.sregs[i], &pre));
    let new_gpregs: [SegVal; 8] = std::array::from_fn(|i| resolve_exit(&summary.gpregs[i], &pre));
    state.sregs = new_sregs;
    state.gpregs = new_gpregs;
}

/// Model an `int` / `into` instruction. When `simple_const_propagation`
/// recorded a state at this address and that state matches an
/// `IntDescription` in `project.int_descriptions`, only the registers
/// named by the description's `returns` clause are clobbered. Otherwise
/// fall back to [`clobber_call`].
fn apply_int(state: &mut AbstractState, project: &Project, seg_idx: SegmentIdx, ofs: u32) {
    let description = project
        .simple_const_propagation
        .int_sites
        .get(&(seg_idx, ofs))
        .and_then(|site| project.int_descriptions.find(site.vector, &site.state));

    let Some(description) = description else {
        clobber_call(state);
        return;
    };

    let (sregs, gpregs) = description.clobbered_regs();
    for s in sregs {
        state.set_sreg(s, SegVal::Unknown);
    }
    for g in gpregs {
        state.set_gpreg(g, SegVal::Unknown);
    }
}

// ── Fixed-point iteration ─────────────────────────────────────────────────────

pub fn compute(project: &Project) -> SegDataflow {
    let mut df = SegDataflow::new();
    let mut worklist: VecDeque<Address> = VecDeque::new();

    // Seed: every block that has no CFG predecessors gets an initial state
    // with any segment-level `assume` values applied.
    for block in project.blocks.blocks() {
        if block.predecessors.is_empty() {
            let mut state = AbstractState::all_unknown();
            for (sreg_name, seg_name) in project.segments[block.seg_idx].assume.iter() {
                let Some(sreg) = AbstractState::sreg_idx_from_str(sreg_name) else {
                    continue;
                };
                if let Some(target_idx) = project.segments.iter().position(|s| &s.name == seg_name)
                {
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

        let transfer = transfer_block(project, block, &entry);

        let successors: Vec<Address> = block.successors.to_vec();
        // For call-terminated blocks, every recorded branch target at the
        // call site is a callee edge — those successors receive the pre-call
        // state; the structural fall-through receives the summary-applied
        // state.
        let call_targets: BTreeSet<Address> = if transfer.pre_call.is_some() {
            match last_instruction_ofs(project, block) {
                Some(call_ofs) => project.branches.targets((seg_idx, call_ofs)).collect(),
                None => BTreeSet::new(),
            }
        } else {
            BTreeSet::new()
        };

        for succ in successors {
            let state_for_succ = if let Some(pre_call) = transfer.pre_call.as_ref()
                && call_targets.contains(&succ)
            {
                pre_call.clone()
            } else {
                transfer.fall_through.clone()
            };

            let changed = match df.block_entry.get_mut(&succ) {
                Some(old) => {
                    let new = old.join(&state_for_succ);
                    if new != *old {
                        *old = new;
                        true
                    } else {
                        false
                    }
                }
                None => {
                    df.block_entry.insert(succ, state_for_succ);
                    true
                }
            };
            if changed {
                worklist.push_back(succ);
            }
        }
    }

    df
}

fn last_instruction_ofs(project: &Project, block: &BasicBlock) -> Option<u32> {
    project.segments[block.seg_idx]
        .addr_attributes
        .prev(block.end)
        .filter(|&p| p >= block.start)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::GpReg16;
    use crate::address_attributes::AddressAttributes;
    use crate::project::BinImage;
    use crate::project::SegmentIdx;

    fn make_project(
        chani_extra: &str,
        seg0_code: &[u8],
        seg1_code: &[u8],
        relocs: &[(u32, u32)], // (linear byte addr in seg0, target segment index)
    ) -> Project {
        let mut chani = String::new();
        chani.push_str("project[test]:\n\n");
        chani.push_str("arch = 8086\n\n");
        chani.push_str("segment[seg000]:\n");
        chani.push_str("    type = code\n");
        chani.push_str("    start = 0x0\n");
        chani.push_str(&format!("    end = 0x{:x}\n", seg0_code.len()));
        chani.push_str("end\n\n");
        chani.push_str("segment[seg001]:\n");
        chani.push_str("    type = data\n");
        chani.push_str("    start = 0x1000\n");
        chani.push_str(&format!(
            "    end = 0x{:x}\n",
            0x1000 + seg1_code.len().max(1)
        ));
        chani.push_str("end\n\n");
        chani.push_str(chani_extra);
        chani.push_str("\nend\n");
        let mut p = Project::from_str(&chani).unwrap();
        p.segments[SegmentIdx::from(0)].addr_attributes = AddressAttributes::new(seg0_code.len());
        p.segments[SegmentIdx::from(1)].addr_attributes =
            AddressAttributes::new(seg1_code.len().max(1));
        p.images.push(BinImage {
            seg_idx: SegmentIdx::from(0),
            load_offset: 0,
            data: seg0_code.to_vec(),
        });
        if !seg1_code.is_empty() {
            p.images.push(BinImage {
                seg_idx: SegmentIdx::from(1),
                load_offset: 0x1000,
                data: seg1_code.to_vec(),
            });
        }
        for &(linear, tgt) in relocs {
            p.imm_relocations
                .insert(linear, SegmentIdx::from(tgt as usize));
        }
        p.analyze();
        p
    }

    fn call_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE8, disp as u8, (disp >> 8) as u8]
    }

    #[test]
    fn callee_summary_propagates_entry_value_to_caller() {
        // F @ 0x00: mov ds, ax ; ret           — summary.ds = Entry(AX).
        // caller @ 0x10: (assumed DS=seg001) mov ax, ds ; call F ; mov bx, 0 ; ret.
        // The caller's pre-call AX is Known(seg001) (copied from DS), so the
        // call summary resolves Entry(AX) → Known(seg001). DS should remain
        // Known(seg001) after the call.
        let mut code = vec![0x8E, 0xD8, 0xC3]; // mov ds, ax; ret
        code.resize(0x10, 0x90);
        code.extend_from_slice(&[0x8C, 0xD8]); // mov ax, ds (8C /r, modrm 11 011 000)
        code.extend_from_slice(&call_rel16(0x12, 0x00)); // call F (3 bytes)
        code.extend_from_slice(&[0xBB, 0x00, 0x00]); // mov bx, 0
        code.push(0xC3); // ret

        let extra = "attr[seg000:0]: type = code\n\
                     attr[seg000:10]: type = code; assume = ds:seg001\n";
        let p = make_project(extra, &code, &[], &[]);

        // State at the `mov bx, 0` (which is after the call).
        let state = p
            .seg_dataflow
            .state_at(&p, SegmentIdx::from(0), 0x15)
            .unwrap();
        assert_eq!(state.sregs[3], SegVal::Known(SegmentIdx::from(1))); // DS=seg001
    }

    #[test]
    fn callee_entry_sees_pre_call_state() {
        // F @ 0x00: ret  (does nothing — summary is TOP)
        // caller @ 0x10: assume DS=seg001 at entry, then call F.
        // F's body entry state must show DS = Known(seg001), not Unknown.
        let mut code = vec![0xC3]; // ret
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00)); // call F
        code.push(0xC3);

        // Apply assume on the caller's first block via attribute.
        let extra = "attr[seg000:0]: type = code\n\
                     attr[seg000:10]: type = code; assume = ds:seg001\n";
        let p = make_project(extra, &code, &[], &[]);

        // Callee entry: block at 0x00.
        let state = p
            .seg_dataflow
            .entry_state(SegmentIdx::from(0), 0x00)
            .unwrap();
        assert_eq!(state.sregs[3], SegVal::Known(SegmentIdx::from(1))); // DS=seg001
    }

    #[test]
    fn caller_fall_through_after_callee_clobber() {
        // F @ 0x00: mov ax, 5 ; mov ds, ax ; ret  — clobbers DS, exit value not a
        // recognized segment paragraph (no relocation on the `mov ax, 5`).
        // caller @ 0x10: assume DS=seg001 at entry, then call F, then ret.
        // After the call, DS should be Unknown (callee returns Unknown DS).
        let mut code = vec![0xB8, 0x05, 0x00, 0x8E, 0xD8, 0xC3];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);

        let extra = "attr[seg000:0]: type = code\n\
                     attr[seg000:10]: type = code; assume = ds:seg001\n";
        let p = make_project(extra, &code, &[], &[]);

        // State at the ret in the caller block.
        let state = p
            .seg_dataflow
            .state_at(&p, SegmentIdx::from(0), 0x13)
            .unwrap();
        assert_eq!(state.sregs[3], SegVal::Unknown);
        // Sanity: callee entry still sees DS=seg001 because the pre-call state
        // is now what flows on the call edge.
        let _ = GpReg16::AX;
        let entry = p
            .seg_dataflow
            .entry_state(SegmentIdx::from(0), 0x00)
            .unwrap();
        assert_eq!(entry.sregs[3], SegVal::Known(SegmentIdx::from(1)));
    }
}

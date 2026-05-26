//! Per-function summary: how does each segment / GP register at every return
//! path of this function relate to its value at entry?
//!
//! For each register the summary records one of:
//! - `Entry(R)` — at every return path, this register equals the entry value
//!   of register `R`. `R` is normally the same register (preservation) but can
//!   also be another register (e.g. `mov es, ds; ret` yields `ES = Entry(DS)`).
//! - `Known(SegmentIdx)` — at every return path, this register equals the
//!   paragraph of a specific project segment.
//! - `Unknown` — value varies across return paths.
//!
//! The pass runs an intra-procedural forward dataflow using `BodyVal`
//! (same lattice as `ExitVal`). At entry each register holds `Entry(self)`;
//! transfer functions propagate values through moves, push/pop, and recursive
//! call-summary application. Returns meet their per-register `BodyVal` into
//! the function's summary. A standard call-graph worklist drives the greatest
//! fixpoint (optimistic init at TOP, monotone meet toward `Unknown`).
//!
//! Tail calls — both `jmp <known-entry>` and structural fall-through into a
//! known entry — are modeled as routing the current state through the tail
//! callee's summary, then returning.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    Address, DisasmCtx, GpReg16, Opcode, Operand, SReg, decode_with_ctx,
    decoded_instruction::DecodedInstruction,
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

// ── Register identifier ───────────────────────────────────────────────────────

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegId {
    Sreg(SReg),
    Gp16(GpReg16),
}

impl RegId {
    pub fn as_str(self) -> String {
        match self {
            RegId::Sreg(s) => s.as_str().to_string(),
            RegId::Gp16(g) => g.as_str().to_string(),
        }
    }
}

// ── Exit-value lattice ────────────────────────────────────────────────────────

/// Per-return-path summary for one register.
///
/// Lattice (TOP → BOTTOM):
/// `Entry(self)` (preservation) and `Known(s)` are incomparable concrete
/// values; their meet with anything different is `Unknown`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExitVal {
    /// Value equals the entry value of `source`.
    Entry(RegId),
    /// Value is the paragraph of this project segment.
    Known(SegmentIdx),
    /// Value varies across return paths.
    Unknown,
}

impl ExitVal {
    pub fn meet(&self, other: &Self) -> Self {
        if self == other {
            self.clone()
        } else {
            ExitVal::Unknown
        }
    }
}

// ── Function summary ──────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionSummary {
    /// Indexed by `SReg as usize` via [`sreg_idx`].
    pub sregs: [ExitVal; 4],
    /// Indexed by `GpReg16 as usize` (AX=0..DI=7).
    pub gpregs: [ExitVal; 8],
}

impl FunctionSummary {
    /// TOP — every register preserves its entry value.
    pub fn top() -> Self {
        Self {
            sregs: [
                ExitVal::Entry(RegId::Sreg(SReg::ES)),
                ExitVal::Entry(RegId::Sreg(SReg::CS)),
                ExitVal::Entry(RegId::Sreg(SReg::SS)),
                ExitVal::Entry(RegId::Sreg(SReg::DS)),
            ],
            gpregs: [
                ExitVal::Entry(RegId::Gp16(GpReg16::AX)),
                ExitVal::Entry(RegId::Gp16(GpReg16::CX)),
                ExitVal::Entry(RegId::Gp16(GpReg16::DX)),
                ExitVal::Entry(RegId::Gp16(GpReg16::BX)),
                ExitVal::Entry(RegId::Gp16(GpReg16::SP)),
                ExitVal::Entry(RegId::Gp16(GpReg16::BP)),
                ExitVal::Entry(RegId::Gp16(GpReg16::SI)),
                ExitVal::Entry(RegId::Gp16(GpReg16::DI)),
            ],
        }
    }

    /// BOTTOM — nothing is known about any register.
    pub fn bottom() -> Self {
        Self {
            sregs: [
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
            ],
            gpregs: [
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
                ExitVal::Unknown,
            ],
        }
    }

    pub fn meet(&self, other: &Self) -> Self {
        let sregs = std::array::from_fn(|i| self.sregs[i].meet(&other.sregs[i]));
        let gpregs = std::array::from_fn(|i| self.gpregs[i].meet(&other.gpregs[i]));
        Self { sregs, gpregs }
    }

    pub fn get_sreg(&self, r: SReg) -> &ExitVal {
        &self.sregs[sreg_idx(r)]
    }

    pub fn get_gp16(&self, r: GpReg16) -> &ExitVal {
        &self.gpregs[r as usize]
    }

    pub fn get(&self, r: RegId) -> &ExitVal {
        match r {
            RegId::Sreg(s) => self.get_sreg(s),
            RegId::Gp16(g) => self.get_gp16(g),
        }
    }
}

pub type FunctionSummaryMap = BTreeMap<Address, FunctionSummary>;

pub fn sreg_idx(r: SReg) -> usize {
    match r {
        SReg::ES => 0,
        SReg::CS => 1,
        SReg::SS => 2,
        SReg::DS => 3,
    }
}

// ── Body abstract state ───────────────────────────────────────────────────────

type BodyVal = ExitVal;

#[derive(Clone, Debug, PartialEq, Eq)]
enum BodyStack {
    Tracked(Vec<BodyVal>),
    Invalid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct BodyState {
    sregs: [BodyVal; 4],
    gpregs: [BodyVal; 8],
    stack: BodyStack,
}

impl BodyState {
    fn entry() -> Self {
        Self {
            sregs: FunctionSummary::top().sregs,
            gpregs: FunctionSummary::top().gpregs,
            stack: BodyStack::Tracked(Vec::new()),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        let stack = match (&self.stack, &other.stack) {
            (BodyStack::Tracked(a), BodyStack::Tracked(b)) if a == b => {
                BodyStack::Tracked(a.clone())
            }
            _ => BodyStack::Invalid,
        };
        Self {
            sregs: std::array::from_fn(|i| self.sregs[i].meet(&other.sregs[i])),
            gpregs: std::array::from_fn(|i| self.gpregs[i].meet(&other.gpregs[i])),
            stack,
        }
    }

    fn get_sreg(&self, r: SReg) -> BodyVal {
        self.sregs[sreg_idx(r)].clone()
    }

    fn set_sreg(&mut self, r: SReg, v: BodyVal) {
        self.sregs[sreg_idx(r)] = v;
    }

    fn get_gp16(&self, r: GpReg16) -> BodyVal {
        self.gpregs[r as usize].clone()
    }

    fn set_gp16(&mut self, r: GpReg16, v: BodyVal) {
        self.gpregs[r as usize] = v;
    }

    fn get(&self, r: RegId) -> BodyVal {
        match r {
            RegId::Sreg(s) => self.get_sreg(s),
            RegId::Gp16(g) => self.get_gp16(g),
        }
    }

    fn push_slot(&mut self, v: BodyVal) {
        if let BodyStack::Tracked(s) = &mut self.stack {
            s.push(v);
        }
    }

    fn pop_slot(&mut self) -> BodyVal {
        match &mut self.stack {
            BodyStack::Tracked(s) => s.pop().unwrap_or(BodyVal::Unknown),
            BodyStack::Invalid => BodyVal::Unknown,
        }
    }

    fn invalidate_stack(&mut self) {
        self.stack = BodyStack::Invalid;
    }

    fn stack_is_empty(&self) -> bool {
        matches!(&self.stack, BodyStack::Tracked(s) if s.is_empty())
    }
}

// ── Operand → BodyVal helpers ─────────────────────────────────────────────────

/// Read an operand's `BodyVal`. Handles registers and the `SegRef` immediate
/// produced when the EXE relocation table resolves an Imm16 to a project
/// segment. Returns `Unknown` for everything else (plain immediates, memory,
/// etc. — the body pass does not model memory).
fn read_body_val(state: &BodyState, inst: &DecodedInstruction, i: usize) -> BodyVal {
    match inst.operand(i) {
        Operand::Sreg(r) => state.get_sreg(r),
        Operand::Gp16(r) => state.get_gp16(r),
        Operand::SegRef(idx) => BodyVal::Known(idx),
        _ => BodyVal::Unknown,
    }
}

fn write_body_val(state: &mut BodyState, inst: &DecodedInstruction, i: usize, v: BodyVal) {
    match inst.operand(i) {
        Operand::Sreg(r) => state.set_sreg(r, v),
        Operand::Gp16(r) => state.set_gp16(r, v),
        _ => {}
    }
}

// ── Resolving an Entry token in a caller context ──────────────────────────────

/// Apply a callee summary to the caller's pre-call `BodyState`. Updates the
/// state in place to reflect the callee's exit values, resolving every
/// `Entry(R)` against the caller's pre-call value of `R`.
fn apply_callee_summary_in_body(state: &mut BodyState, summary: &FunctionSummary) {
    let pre = state.clone();
    let resolve = |ev: &ExitVal| -> BodyVal {
        match ev {
            ExitVal::Entry(r) => pre.get(*r),
            ExitVal::Known(s) => BodyVal::Known(*s),
            ExitVal::Unknown => BodyVal::Unknown,
        }
    };

    for i in 0..4 {
        state.sregs[i] = resolve(&summary.sregs[i]);
    }
    for i in 0..8 {
        state.gpregs[i] = resolve(&summary.gpregs[i]);
    }
}

/// Conservative model of an unknown call or interrupt: clobbers DS, ES, and
/// every GP register. SS is left as-is because `int`/`iret` is hardware-
/// balanced and direct unknown callees are assumed to follow the standard
/// SS-preserving calling convention.
fn clobber_unknown_call(state: &mut BodyState) {
    state.set_sreg(SReg::DS, BodyVal::Unknown);
    state.set_sreg(SReg::ES, BodyVal::Unknown);
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
        state.set_gp16(*r, BodyVal::Unknown);
    }
}

// ── Transfer function ─────────────────────────────────────────────────────────

fn call_dest(project: &Project, inst: &DecodedInstruction) -> Option<Address> {
    let (seg, ofs) = inst.branch_destination()?;
    let seg_idx = project.segment_index_for(seg)?;
    Some((seg_idx, ofs as u32))
}

fn apply_transfer(
    project: &Project,
    inst: &DecodedInstruction,
    addr: Address,
    state: &mut BodyState,
    estimates: &FunctionSummaryMap,
) {
    match inst.opcode {
        Opcode::Push => {
            let v = read_body_val(state, inst, 0);
            state.push_slot(v);
        }

        Opcode::Pop => {
            let v = state.pop_slot();
            write_body_val(state, inst, 0, v);
            if inst.writes_to_gp16(GpReg16::SP) {
                state.invalidate_stack();
            }
        }

        Opcode::Pushf => {
            state.push_slot(BodyVal::Unknown);
        }

        Opcode::Popf => {
            let _ = state.pop_slot();
        }

        Opcode::Mov => {
            if let Some((dst, src)) = inst.dst_src() {
                let v = read_body_val(state, inst, src);
                write_body_val(state, inst, dst, v);
            }
            if inst.writes_to_gp16(GpReg16::SP) {
                state.invalidate_stack();
            }
        }

        Opcode::Xchg => {
            let a = read_body_val(state, inst, 0);
            let b = read_body_val(state, inst, 1);
            write_body_val(state, inst, 0, b);
            write_body_val(state, inst, 1, a);
            if inst.writes_to_gp16(GpReg16::SP) {
                state.invalidate_stack();
            }
        }

        Opcode::Lds => {
            state.set_sreg(SReg::DS, BodyVal::Unknown);
            write_body_val(state, inst, 0, BodyVal::Unknown);
        }

        Opcode::Les => {
            state.set_sreg(SReg::ES, BodyVal::Unknown);
            write_body_val(state, inst, 0, BodyVal::Unknown);
        }

        Opcode::Call => match call_dest(project, inst) {
            Some(addr) => match estimates.get(&addr) {
                Some(summary) => apply_callee_summary_in_body(state, summary),
                None => clobber_unknown_call(state),
            },
            None => clobber_unknown_call(state),
        },

        Opcode::Int | Opcode::Into => {
            // Consult the int description for this specific (vector, condition)
            // — when found, only registers named in its `returns` clause are
            // clobbered (the rest are preserved). Falls back to a full
            // unknown-call clobber when no description matches. SS is left
            // alone in either case (hardware-restored by iret).
            apply_int(state, project, addr);
        }

        _ => {
            // Any other write to a tracked register goes to Unknown.
            for i in inst.destinations() {
                match inst.operand(i) {
                    Operand::Sreg(r) => state.set_sreg(r, BodyVal::Unknown),
                    Operand::Gp16(r) => state.set_gp16(r, BodyVal::Unknown),
                    _ => {}
                }
            }
            if inst.writes_to_gp16(GpReg16::SP) {
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
    entry_state: &BodyState,
    estimates: &FunctionSummaryMap,
) -> BodyState {
    let mut state = entry_state.clone();
    let mut ofs = start;

    while ofs < end {
        let Some(inst) = decode_in_project(project, block_seg, ofs) else {
            break;
        };
        let len = inst.bytes.len() as u32;
        apply_transfer(project, &inst, (block_seg, ofs), &mut state, estimates);
        ofs += len;
    }

    state
}

/// Model an `int` / `into` instruction. When `simple_const_propagation`
/// recorded a state at this address and that state matches an
/// `IntDescription` in `project.int_descriptions`, only the registers
/// named by the description's `returns` clause are clobbered (everything
/// else is preserved — including `Entry(self)` slots). Falls back to a
/// full unknown-call clobber when no description matches.
fn apply_int(state: &mut BodyState, project: &Project, addr: Address) {
    let description = project
        .simple_const_propagation
        .int_sites
        .get(&addr)
        .and_then(|site| project.int_descriptions.find(site.vector, &site.state));

    let Some(description) = description else {
        clobber_unknown_call(state);
        return;
    };

    let (sregs, gpregs) = description.clobbered_regs();
    for s in sregs {
        state.set_sreg(s, BodyVal::Unknown);
    }
    for g in gpregs {
        state.set_gp16(g, BodyVal::Unknown);
    }
}

// ── Function-entry enumeration ────────────────────────────────────────────────

fn collect_call_entries(project: &Project) -> BTreeSet<Address> {
    let mut entries = BTreeSet::new();
    for src in project.branches.all_sources() {
        let Some(inst) = decode_in_project(project, src.0, src.1) else {
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

fn last_instruction(
    project: &Project,
    block_seg: SegmentIdx,
    block_start: u32,
    block_end: u32,
) -> Option<DecodedInstruction> {
    let last_ofs = project.segments[block_seg]
        .addr_attributes
        .prev(block_end)
        .filter(|&p| p >= block_start)
        .unwrap_or(block_start);
    decode_in_project(project, block_seg, last_ofs)
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
    estimates: &FunctionSummaryMap,
    callers: &mut BTreeMap<Address, BTreeSet<Address>>,
) -> FunctionSummary {
    let mut block_in: BTreeMap<Address, BodyState> = BTreeMap::new();
    block_in.insert(entry, BodyState::entry());
    let mut worklist: VecDeque<Address> = VecDeque::from([entry]);
    let mut visited: BTreeSet<Address> = BTreeSet::new();

    // Accumulator: None = no return contributions seen yet. The very first
    // return seeds the result; subsequent returns meet into it. This way
    // the result is the meet of all observed return contributions (and not
    // meet-with-TOP, which would collapse a single observed `Entry(AX)` to
    // `Unknown`).
    let mut result: Option<FunctionSummary> = None;
    let absorb = |result: &mut Option<FunctionSummary>, contribution: FunctionSummary| {
        *result = Some(match result.take() {
            Some(r) => r.meet(&contribution),
            None => contribution,
        });
    };

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

        match last_op {
            Some(Opcode::Ret) => {
                absorb(&mut result, project_return(&exit_state, false));
            }
            Some(Opcode::Retf) => {
                absorb(&mut result, project_return(&exit_state, true));
            }
            Some(Opcode::Iret) => {
                absorb(&mut result, project_return(&exit_state, false));
            }
            Some(Opcode::Hlt) => {
                // No contribution.
            }
            Some(Opcode::Jmp) if block.successors.is_empty() => {
                // Indirect jmp with no resolvable destination — treat as tail
                // call to unknown.
                absorb(&mut result, FunctionSummary::bottom());
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
                if block_in.get(&succ) != Some(&new_state) {
                    block_in.insert(succ, new_state);
                    worklist.push_back(succ);
                }
            } else if known_entries.contains(&succ) && succ != entry {
                // Tail call to a known function: route through callee summary
                // and treat as a return contribution.
                callers.entry(succ).or_default().insert(entry);
                let mut after_tail = exit_state.clone();
                match estimates.get(&succ) {
                    Some(summary) => apply_callee_summary_in_body(&mut after_tail, summary),
                    None => clobber_unknown_call(&mut after_tail),
                }
                absorb(&mut result, project_return(&after_tail, false));
            } else {
                // Unknown destination outside the function — conservative.
                absorb(&mut result, FunctionSummary::bottom());
            }
        }
    }

    // Record direct callees (for the call-graph caller index) by re-decoding.
    for &b_addr in &visited {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else {
            continue;
        };
        let mut ofs = block.start;
        while ofs < block.end {
            let Some(inst) = decode_in_project(project, block.seg_idx, ofs) else {
                break;
            };
            if inst.opcode == Opcode::Call
                && let Some(addr) = call_dest(project, &inst)
            {
                callers.entry(addr).or_default().insert(entry);
            }
            ofs += inst.bytes.len() as u32;
        }
    }

    // A function with no observed return paths (only halts / unresolved
    // indirect jmps that were modeled as bottom contributions) gets TOP.
    // Vacuously: the never-returned exit value is consistent with every
    // possible specification, so claiming preservation is harmless for any
    // caller (whose continuation would never execute anyway). Matches the
    // legacy function_preserves behavior on non-returning functions.
    result.unwrap_or_else(FunctionSummary::top)
}

/// Project a body state at a return point to a per-return-path contribution.
fn project_return(state: &BodyState, needs_stack_empty: bool) -> FunctionSummary {
    let mut contribution = FunctionSummary {
        sregs: std::array::from_fn(|i| state.sregs[i].clone()),
        gpregs: std::array::from_fn(|i| state.gpregs[i].clone()),
    };
    if needs_stack_empty && !state.stack_is_empty() {
        // The stack shape doesn't match entry — `retf` would pop garbage as CS
        // before SS would be restored. Conservatively downgrade SS.
        contribution.sregs[sreg_idx(SReg::SS)] = ExitVal::Unknown;
    }
    contribution
}

// ── Public entry point ────────────────────────────────────────────────────────

pub fn compute(project: &Project) -> FunctionSummaryMap {
    let known_entries = collect_call_entries(project);

    let mut estimates: FunctionSummaryMap = BTreeMap::new();
    let mut blocks_per_func: BTreeMap<Address, BTreeSet<Address>> = BTreeMap::new();
    let mut analyzable: BTreeSet<Address> = BTreeSet::new();

    for &e in &known_entries {
        if project.blocks.block_at(e.0, e.1).is_some() {
            estimates.insert(e, FunctionSummary::top());
            let bs = walk_function_blocks(project, e, &known_entries);
            blocks_per_func.insert(e, bs);
            analyzable.insert(e);
        } else {
            estimates.insert(e, FunctionSummary::bottom());
        }
    }

    let mut callers: BTreeMap<Address, BTreeSet<Address>> = BTreeMap::new();
    let mut worklist: VecDeque<Address> = VecDeque::from_iter(analyzable.iter().copied());
    let mut in_worklist: BTreeSet<Address> = analyzable.clone();

    while let Some(e) = worklist.pop_front() {
        in_worklist.remove(&e);
        let Some(in_func_blocks) = blocks_per_func.get(&e) else {
            continue;
        };
        let new = run_intra(
            project,
            e,
            in_func_blocks,
            &known_entries,
            &estimates,
            &mut callers,
        );
        // Greatest-fixpoint iteration over a finite-height lattice: the new
        // value is the result of run_intra given current callee estimates.
        // We replace rather than meet — `meet` would collapse a lateral
        // refinement (e.g. TOP `Entry(DS)` → discovered `Entry(AX)`) to
        // `Unknown`. Termination is guaranteed because the lattice has a
        // finite number of states per register, and re-queueing only happens
        // on a structural change.
        let prev = estimates.get(&e).cloned();
        if prev.as_ref() != Some(&new) {
            estimates.insert(e, new);
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

// ── Rendering helpers ─────────────────────────────────────────────────────────

/// Render a function summary as one or two comment lines for the disassembly
/// listing.
///
/// - "preserves: DS, ES, SS" (or "-") lists segregs whose exit value is
///   `Entry(self)`.
/// - "exit: ds = seg001, es = entry(ax)" appears only when at least one
///   segreg has a non-`Entry(self)` exit that is concrete (Known) or names
///   another entry register.
pub fn render_summary_comment_lines(project: &Project, summary: &FunctionSummary) -> Vec<String> {
    let mut lines = Vec::new();

    let segregs = [SReg::DS, SReg::ES, SReg::SS];

    let mut exit_parts: Vec<String> = Vec::new();
    for &r in &segregs {
        let v = summary.get_sreg(r);
        let lhs = r.as_str();
        let rhs = match v {
            ExitVal::Entry(r2) => format!("entry({})", r2.as_str()),
            ExitVal::Known(idx) => project.segments[*idx].name.clone(),
            ExitVal::Unknown => continue,
        };
        exit_parts.push(format!("{lhs} = {rhs}"));
    }
    if !exit_parts.is_empty() {
        lines.push(format!("exit: {}", exit_parts.join(", ")));
    }

    lines
}

// ── Clobber trace ─────────────────────────────────────────────────────────────

/// One instruction's contribution to a function downgrading some register's
/// `BodyVal` away from `Entry(self)`, or invalidating the abstract stack.
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

fn is_self_entry(v: &BodyVal, r: RegId) -> bool {
    matches!(v, BodyVal::Entry(s) if *s == r)
}

/// Re-run the per-function intra-procedural pass for `entry` and return every
/// instruction that downgrades any segreg's `BodyVal` away from `Entry(self)`
/// or invalidates the abstract stack. Uses the project's already-computed
/// `function_summary` estimates for callee lookups.
pub fn trace_function_clobbers(project: &Project, entry: Address) -> Vec<ClobberRecord> {
    if project.blocks.block_at(entry.0, entry.1).is_none() {
        return Vec::new();
    }
    let known_entries: BTreeSet<Address> = project.function_summary.keys().copied().collect();
    let in_func_blocks = walk_function_blocks(project, entry, &known_entries);
    let estimates = &project.function_summary;

    // 1. Forward dataflow: compute per-block entry states.
    let mut block_in: BTreeMap<Address, BodyState> = BTreeMap::new();
    block_in.insert(entry, BodyState::entry());
    let mut worklist: VecDeque<Address> = VecDeque::from([entry]);

    while let Some(b_addr) = worklist.pop_front() {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else {
            continue;
        };
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

    // 2. Walk each block instruction-by-instruction recording transitions.
    let mut records: Vec<ClobberRecord> = Vec::new();

    for &b_addr in &in_func_blocks {
        let Some(block) = project.blocks.block_at(b_addr.0, b_addr.1) else {
            continue;
        };
        let mut state = match block_in.get(&b_addr) {
            Some(s) => s.clone(),
            None => continue,
        };

        let mut ofs = block.start;
        let mut last_ofs = block.start;
        let mut last_inst_opt: Option<DecodedInstruction> = None;

        while ofs < block.end {
            let Some(inst) = decode_in_project(project, block.seg_idx, ofs) else {
                break;
            };
            let len = inst.bytes.len() as u32;

            let before = state.clone();
            apply_transfer(project, &inst, (block.seg_idx, ofs), &mut state, estimates);

            let was_ds = is_self_entry(&before.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS));
            let was_es = is_self_entry(&before.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES));
            let was_ss = is_self_entry(&before.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS));
            let now_ds = is_self_entry(&state.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS));
            let now_es = is_self_entry(&state.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES));
            let now_ss = is_self_entry(&state.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS));

            let clobbers_ds = was_ds && !now_ds;
            let clobbers_es = was_es && !now_es;
            let clobbers_ss = was_ss && !now_ss;
            let stack_invalidated = matches!(before.stack, BodyStack::Tracked(_))
                && matches!(state.stack, BodyStack::Invalid);

            if clobbers_ds || clobbers_es || clobbers_ss || stack_invalidated {
                let reason = explain_clobber(
                    project,
                    &inst,
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

        // Tail-call edge to a known function outside the body.
        for &succ in &block.successors {
            let last_is_call = last_inst_opt.as_ref().map(|i| i.opcode) == Some(Opcode::Call);
            if last_is_call && succ != (block.seg_idx, block.end) {
                continue;
            }
            if in_func_blocks.contains(&succ) {
                continue;
            }
            let (rec_ds, rec_es, rec_ss, reason) = if known_entries.contains(&succ) && succ != entry
            {
                let est = estimates
                    .get(&succ)
                    .cloned()
                    .unwrap_or_else(FunctionSummary::bottom);
                let after_ds = match &est.sregs[sreg_idx(SReg::DS)] {
                    ExitVal::Entry(r) => state.get(*r),
                    ExitVal::Known(s) => BodyVal::Known(*s),
                    ExitVal::Unknown => BodyVal::Unknown,
                };
                let after_es = match &est.sregs[sreg_idx(SReg::ES)] {
                    ExitVal::Entry(r) => state.get(*r),
                    ExitVal::Known(s) => BodyVal::Known(*s),
                    ExitVal::Unknown => BodyVal::Unknown,
                };
                let after_ss = match &est.sregs[sreg_idx(SReg::SS)] {
                    ExitVal::Entry(r) => state.get(*r),
                    ExitVal::Known(s) => BodyVal::Known(*s),
                    ExitVal::Unknown => BodyVal::Unknown,
                };
                let ds = is_self_entry(&state.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS))
                    && !is_self_entry(&after_ds, RegId::Sreg(SReg::DS));
                let es = is_self_entry(&state.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES))
                    && !is_self_entry(&after_es, RegId::Sreg(SReg::ES));
                let ss = is_self_entry(&state.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS))
                    && !is_self_entry(&after_ss, RegId::Sreg(SReg::SS));
                let name = project
                    .name_at(succ.0, succ.1)
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("{}:{:04x}", project.segments[succ.0].name, succ.1));
                let mut what = Vec::new();
                if !is_self_entry(&est.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS)) {
                    what.push("DS");
                }
                if !is_self_entry(&est.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES)) {
                    what.push("ES");
                }
                if !is_self_entry(&est.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS)) {
                    what.push("SS");
                }
                let what_str = if what.is_empty() {
                    "nothing".to_string()
                } else {
                    what.join(", ")
                };
                (
                    ds,
                    es,
                    ss,
                    format!("tail call to {name}: callee clobbers {what_str}"),
                )
            } else {
                let ds = is_self_entry(&state.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS));
                let es = is_self_entry(&state.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES));
                let ss = is_self_entry(&state.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS));
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
    estimates: &FunctionSummaryMap,
    clobbers_ds: bool,
    clobbers_es: bool,
    clobbers_ss: bool,
    stack_invalidated: bool,
) -> String {
    match inst.opcode {
        Opcode::Mov | Opcode::Xchg => {
            let mut parts = Vec::new();
            if clobbers_ds {
                parts.push("DS");
            }
            if clobbers_es {
                parts.push("ES");
            }
            if clobbers_ss {
                parts.push("SS");
            }
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
                if clobbers_ds {
                    parts.push("DS");
                }
                if clobbers_es {
                    parts.push("ES");
                }
                if clobbers_ss {
                    parts.push("SS");
                }
                if parts.is_empty() {
                    "pop".into()
                } else {
                    format!(
                        "pop {}: top-of-stack slot does not match entry value",
                        parts.join("/")
                    )
                }
            }
        }
        Opcode::Call => match call_dest(project, inst) {
            Some(addr) => {
                let est = estimates
                    .get(&addr)
                    .cloned()
                    .unwrap_or_else(FunctionSummary::bottom);
                let name = project
                    .name_at(addr.0, addr.1)
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("{}:{:04x}", project.segments[addr.0].name, addr.1));
                let mut what = Vec::new();
                if !is_self_entry(&est.sregs[sreg_idx(SReg::DS)], RegId::Sreg(SReg::DS)) {
                    what.push("DS");
                }
                if !is_self_entry(&est.sregs[sreg_idx(SReg::ES)], RegId::Sreg(SReg::ES)) {
                    what.push("ES");
                }
                if !is_self_entry(&est.sregs[sreg_idx(SReg::SS)], RegId::Sreg(SReg::SS)) {
                    what.push("SS");
                }
                let what_str = if what.is_empty() {
                    "nothing".into()
                } else {
                    what.join(", ")
                };
                format!("call {name}: callee clobbers {what_str}")
            }
            None => "indirect call: clobbers DS, ES, SS".into(),
        },
        Opcode::Int | Opcode::Into => "int: clobbers DS, ES, SS (conservative)".into(),
        _ => {
            if stack_invalidated {
                "writes SP — abstract stack invalidated".into()
            } else {
                let mut parts = Vec::new();
                if clobbers_ds {
                    parts.push("DS");
                }
                if clobbers_es {
                    parts.push("ES");
                }
                if clobbers_ss {
                    parts.push("SS");
                }
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

    fn ds_idx() -> usize {
        sreg_idx(SReg::DS)
    }
    fn es_idx() -> usize {
        sreg_idx(SReg::ES)
    }
    fn ss_idx() -> usize {
        sreg_idx(SReg::SS)
    }

    #[test]
    fn push_pop_ds_preserves_ds() {
        let mut code = vec![
            0x1E, // push ds
            0x1F, // pop ds
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        assert_eq!(s.sregs[ds_idx()], ExitVal::Entry(RegId::Sreg(SReg::DS)));
        assert_eq!(s.sregs[es_idx()], ExitVal::Entry(RegId::Sreg(SReg::ES)));
        assert_eq!(s.sregs[ss_idx()], ExitVal::Entry(RegId::Sreg(SReg::SS)));
    }

    #[test]
    fn mov_ds_ax_clobbers_ds() {
        // F @ 0x00: mov ds, ax; ret.
        let mut code = vec![
            0x8E, 0xD8, // mov ds, ax (modrm: mod=11 reg=011(DS) rm=000(AX))
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        // ds at exit = entry value of ax (the source register).
        assert_eq!(s.sregs[ds_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
        assert_eq!(s.sregs[es_idx()], ExitVal::Entry(RegId::Sreg(SReg::ES)));
        assert_eq!(s.sregs[ss_idx()], ExitVal::Entry(RegId::Sreg(SReg::SS)));
    }

    #[test]
    fn lds_clobbers_ds() {
        let mut code = vec![
            0xC5, 0x07, // lds ax, [bx]
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        assert_eq!(s.sregs[ds_idx()], ExitVal::Unknown);
        assert_eq!(s.sregs[es_idx()], ExitVal::Entry(RegId::Sreg(SReg::ES)));
    }

    #[test]
    fn push_ds_pop_es_pop_ds_clobbers_both() {
        // push ds, pop es, pop ds — ES gets Entry(DS), DS gets Unknown
        // (popped from an empty/unmatched stack slot).
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
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        assert_eq!(s.sregs[es_idx()], ExitVal::Entry(RegId::Sreg(SReg::DS)));
        assert_eq!(s.sregs[ds_idx()], ExitVal::Unknown);
    }

    #[test]
    fn mov_ss_ax_yields_entry_ax() {
        // mov ss, ax; mov sp, bx; ret  — SS at exit = Entry(AX), stack invalidated.
        let mut code = vec![
            0x8E, 0xD0, // mov ss, ax
            0x89, 0xDC, // mov sp, bx
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        assert_eq!(s.sregs[ss_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
    }

    #[test]
    fn callee_clobber_propagates() {
        // F @ 0x00: call G; ret. G @ 0x10: mov ds, ax; ret.
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x10));
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.extend_from_slice(&[0x8E, 0xD8, 0xC3]); // mov ds, ax; ret
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x20]);
        let f = p.function_summary.get(&at(0x00)).cloned().unwrap();
        let g = p.function_summary.get(&at(0x10)).cloned().unwrap();
        assert_eq!(g.sregs[ds_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
        // F's DS exit value is also Entry(AX): F's body resolves G's Entry(AX)
        // against F's pre-call AX, which is Entry(AX) — so it stays Entry(AX).
        assert_eq!(f.sregs[ds_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
    }

    #[test]
    fn tail_call_inherits_callee() {
        let mut code = Vec::new();
        code.extend_from_slice(&jmp_rel16(0x00, 0x10)); // jmp G
        code.resize(0x10, 0x90);
        code.extend_from_slice(&[0x8E, 0xD8, 0xC3]); // G: mov ds, ax; ret
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x10));
        code.push(0xC3);
        code.resize(0x30, 0x90);
        code.extend_from_slice(&call_rel16(0x30, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x20, 0x30]);
        let f = p.function_summary.get(&at(0x00)).cloned().unwrap();
        let g = p.function_summary.get(&at(0x10)).cloned().unwrap();
        assert_eq!(g.sregs[ds_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
        assert_eq!(f.sregs[ds_idx()], ExitVal::Entry(RegId::Gp16(GpReg16::AX)));
    }

    #[test]
    fn indirect_call_clobbers_ds_es() {
        let mut code = vec![
            0xFF, 0xD0, // call ax
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        assert_eq!(s.sregs[ds_idx()], ExitVal::Unknown);
        assert_eq!(s.sregs[es_idx()], ExitVal::Unknown);
        // SS is assumed preserved across indirect calls by convention.
        assert_eq!(s.sregs[ss_idx()], ExitVal::Entry(RegId::Sreg(SReg::SS)));
    }

    #[test]
    fn add_sp_invalidates_stack() {
        let mut code = vec![
            0x1E, // push ds
            0x83, 0xC4, 0x04, // add sp, 4
            0x1F, // pop ds
            0xC3, // ret
        ];
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x10]);
        let s = p.function_summary.get(&at(0x00)).cloned().unwrap();
        // DS popped from invalidated stack — Unknown.
        assert_eq!(s.sregs[ds_idx()], ExitVal::Unknown);
    }

    #[test]
    fn mutual_recursion_converges_to_top() {
        let mut code = Vec::new();
        code.extend_from_slice(&call_rel16(0x00, 0x10));
        code.push(0xC3);
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00));
        code.push(0xC3);
        code.resize(0x20, 0x90);
        code.extend_from_slice(&call_rel16(0x20, 0x00));
        code.push(0xC3);
        let p = make_project(&code, &[0x20]);
        let f = p.function_summary.get(&at(0x00)).cloned().unwrap();
        let g = p.function_summary.get(&at(0x10)).cloned().unwrap();
        assert_eq!(f, FunctionSummary::top());
        assert_eq!(g, FunctionSummary::top());
    }
}

use std::collections::{BTreeMap, VecDeque};

use crate::{
    SReg, SRegMap,
    basic_block::BasicBlock,
    decode,
    opcode_table::{ArgDir, ArgType, Opcode},
    project::Project,
};

// ── Abstract value ────────────────────────────────────────────────────────────

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SegVal {
    /// A specific project segment index is known.
    Known(usize),
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

// ── GP register index ─────────────────────────────────────────────────────────

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum GpReg16 {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
}

impl GpReg16 {
    fn from_bits(bits: u8) -> Self {
        match bits & 7 {
            0 => Self::AX,
            1 => Self::CX,
            2 => Self::DX,
            3 => Self::BX,
            4 => Self::SP,
            5 => Self::BP,
            6 => Self::SI,
            _ => Self::DI,
        }
    }

    fn from_arg_type(arg: ArgType) -> Option<Self> {
        match arg {
            ArgType::AX => Some(Self::AX),
            ArgType::CX => Some(Self::CX),
            ArgType::DX => Some(Self::DX),
            ArgType::BX => Some(Self::BX),
            ArgType::SP => Some(Self::SP),
            ArgType::BP => Some(Self::BP),
            ArgType::SI => Some(Self::SI),
            ArgType::DI => Some(Self::DI),
            _ => None,
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
        fn known(v: &SegVal) -> Option<usize> {
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
    pub block_entry: BTreeMap<(usize, u32), AbstractState>,
}

impl SegDataflow {
    pub fn new() -> Self {
        Self {
            block_entry: BTreeMap::new(),
        }
    }

    pub fn entry_state(&self, seg_idx: usize, block_start: u32) -> Option<&AbstractState> {
        self.block_entry.get(&(seg_idx, block_start))
    }

    /// Abstract state immediately before the instruction at `(seg_idx, ofs)`.
    /// Re-runs the transfer function from the containing block's entry state up to `ofs`.
    pub fn state_at(&self, project: &Project, seg_idx: usize, ofs: u32) -> Option<AbstractState> {
        let block = project.blocks.block_containing(seg_idx, ofs)?;
        let entry = self.entry_state(block.seg_idx, block.start)?;
        Some(transfer_block_until(project, block, entry, ofs))
    }
}

// ── Helper: classify arg as SReg ─────────────────────────────────────────────

fn sreg_from_modrm_bits(bits: u8) -> SReg {
    match bits & 3 {
        0 => SReg::ES,
        1 => SReg::CS,
        2 => SReg::SS,
        _ => SReg::DS,
    }
}

fn sreg_from_arg_type(arg: ArgType) -> Option<SReg> {
    match arg {
        ArgType::ES => Some(SReg::ES),
        ArgType::CS => Some(SReg::CS),
        ArgType::SS => Some(SReg::SS),
        ArgType::DS => Some(SReg::DS),
        _ => None,
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
        let bytes = project.bytes_at_seg(block.seg_idx, ofs);
        let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else {
            break;
        };

        let len = inst.bytes.len() as u32;
        let modrm = inst.modrm;
        let mod_bits = (modrm >> 6) & 3;

        match inst.opcode {
            Opcode::Push => {
                let val = match inst.arg_type[0] {
                    ArgType::SReg => {
                        let r = sreg_from_modrm_bits((modrm >> 3) & 3);
                        state.get_sreg(r).clone()
                    }
                    arg if sreg_from_arg_type(arg).is_some() => {
                        state.get_sreg(sreg_from_arg_type(arg).unwrap()).clone()
                    }
                    ArgType::Reg16 => {
                        let r = GpReg16::from_bits((modrm >> 3) & 7);
                        state.get_gpreg(r).clone()
                    }
                    arg if let Some(r) = GpReg16::from_arg_type(arg) => state.get_gpreg(r).clone(),
                    _ => SegVal::Unknown,
                };
                abstract_stack.push(val);
            }

            Opcode::Pop => {
                let val = abstract_stack.pop().unwrap_or(SegVal::Unknown);
                match inst.arg_type[0] {
                    ArgType::SReg => {
                        let r = sreg_from_modrm_bits((modrm >> 3) & 3);
                        state.set_sreg(r, val);
                    }
                    arg if sreg_from_arg_type(arg).is_some() => {
                        state.set_sreg(sreg_from_arg_type(arg).unwrap(), val);
                    }
                    ArgType::RM16 if mod_bits == 0b11 => {
                        let r = GpReg16::from_bits(modrm & 7);
                        state.set_gpreg(r, val);
                    }
                    arg if GpReg16::from_arg_type(arg).is_some() => {
                        state.set_gpreg(GpReg16::from_arg_type(arg).unwrap(), val);
                    }
                    _ => {} // pop to memory — discard
                }
            }

            Opcode::Mov => {
                // Find which arg is dest (WO/RW) and which is src (RO).
                let dest_idx = inst
                    .arg_dir
                    .iter()
                    .position(|d| matches!(d, ArgDir::WO | ArgDir::RW));
                let src_idx = dest_idx.map(|i| 1 - i);

                if let (Some(dst), Some(src)) = (dest_idx, src_idx) {
                    let src_val = read_arg_val(&state, inst.arg_type[src], modrm);
                    write_arg_val(
                        &mut state,
                        inst.arg_type[dst],
                        inst.arg_dir[dst],
                        modrm,
                        src_val,
                    );
                }
            }

            Opcode::Xchg => {
                let a = read_arg_val(&state, inst.arg_type[0], modrm);
                let b = read_arg_val(&state, inst.arg_type[1], modrm);
                write_arg_val(&mut state, inst.arg_type[0], inst.arg_dir[0], modrm, b);
                write_arg_val(&mut state, inst.arg_type[1], inst.arg_dir[1], modrm, a);
            }

            Opcode::Lds => {
                state.set_sreg(SReg::DS, SegVal::Unknown);
                if mod_bits == 0b11 {
                    // Technically invalid encoding, but be safe.
                    state.set_gpreg(GpReg16::from_bits((modrm >> 3) & 7), SegVal::Unknown);
                } else if let Some(r) = GpReg16::from_arg_type(inst.arg_type[0]) {
                    state.set_gpreg(r, SegVal::Unknown);
                }
            }

            Opcode::Les => {
                state.set_sreg(SReg::ES, SegVal::Unknown);
                if mod_bits == 0b11 {
                    state.set_gpreg(GpReg16::from_bits((modrm >> 3) & 7), SegVal::Unknown);
                } else if let Some(r) = GpReg16::from_arg_type(inst.arg_type[0]) {
                    state.set_gpreg(r, SegVal::Unknown);
                }
            }

            // Near calls preserve CS; callee may freely modify DS, ES, and GP regs.
            Opcode::Call => {
                clobber_call(&mut state);
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
                for i in 0..2 {
                    if matches!(inst.arg_dir[i], ArgDir::WO | ArgDir::RW) {
                        clobber_gp_arg(&mut state, inst.arg_type[i], modrm);
                    }
                }
            }
        }

        ofs += len;
    }

    state
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

/// Set a destination GP register to Unknown for "other" instructions.
fn clobber_gp_arg(state: &mut AbstractState, arg: ArgType, modrm: u8) {
    let mod_bits = (modrm >> 6) & 3;
    match arg {
        ArgType::RM16 if mod_bits == 0b11 => {
            state.set_gpreg(GpReg16::from_bits(modrm & 7), SegVal::Unknown);
        }
        ArgType::Reg16 => {
            state.set_gpreg(GpReg16::from_bits((modrm >> 3) & 7), SegVal::Unknown);
        }
        arg if GpReg16::from_arg_type(arg).is_some() => {
            state.set_gpreg(GpReg16::from_arg_type(arg).unwrap(), SegVal::Unknown);
        }
        _ => {}
    }
}

/// Read the abstract segment value of an argument (for moves/xchg source side).
/// Returns Unknown for anything that isn't a simple register reference.
fn read_arg_val(state: &AbstractState, arg: ArgType, modrm: u8) -> SegVal {
    let mod_bits = (modrm >> 6) & 3;
    match arg {
        ArgType::SReg => {
            let r = sreg_from_modrm_bits((modrm >> 3) & 3);
            state.get_sreg(r).clone()
        }
        arg if let Some(r) = sreg_from_arg_type(arg) => state.get_sreg(r).clone(),
        ArgType::RM16 if mod_bits == 0b11 => state.get_gpreg(GpReg16::from_bits(modrm & 7)).clone(),
        ArgType::Reg16 => state
            .get_gpreg(GpReg16::from_bits((modrm >> 3) & 7))
            .clone(),
        arg if let Some(r) = GpReg16::from_arg_type(arg) => state.get_gpreg(r).clone(),
        _ => SegVal::Unknown,
    }
}

/// Write an abstract value to a destination argument.
/// Memory destinations are silently ignored (we don't track memory).
fn write_arg_val(state: &mut AbstractState, arg: ArgType, dir: ArgDir, modrm: u8, val: SegVal) {
    if !matches!(dir, ArgDir::WO | ArgDir::RW) {
        return;
    }
    let mod_bits = (modrm >> 6) & 3;
    match arg {
        ArgType::SReg => {
            let r = sreg_from_modrm_bits((modrm >> 3) & 3);
            state.set_sreg(r, val);
        }
        arg if let Some(r) = sreg_from_arg_type(arg) => state.set_sreg(r, val),
        ArgType::RM16 if mod_bits == 0b11 => {
            state.set_gpreg(GpReg16::from_bits(modrm & 7), val);
        }
        ArgType::Reg16 => {
            state.set_gpreg(GpReg16::from_bits((modrm >> 3) & 7), val);
        }
        arg if let Some(r) = GpReg16::from_arg_type(arg) => state.set_gpreg(r, val),
        _ => {} // memory destination or untracked — ignore
    }
}

// ── Fixed-point iteration ─────────────────────────────────────────────────────

pub fn compute(project: &Project) -> SegDataflow {
    let mut df = SegDataflow::new();
    let mut worklist: VecDeque<(usize, u32)> = VecDeque::new();

    // Seed: every block that has no CFG predecessors gets an initial state
    // with CS = Known(seg_idx) — it is executing in that segment.
    for block in project.blocks.blocks() {
        if block.predecessors.is_empty() {
            let mut state = AbstractState::all_unknown();
            state.set_sreg(SReg::CS, SegVal::Known(block.seg_idx));
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

        let successors: Vec<(usize, u32)> = block.successors.to_vec();
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

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    Address, DecodedInstruction, GpReg16, Opcode, Operand, SReg,
    basic_block::BasicBlock,
    decode,
    project::{Project, SegmentIdx},
};

#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub enum RegVal {
    #[default]
    Unknown,
    Segment(SReg),
    VirtualSegment(SegmentIdx),
    Constant(u16),
}

impl RegVal {
    fn join(a: &Self, b: &Self) -> Self {
        if a == b { *a } else { RegVal::Unknown }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct State {
    pub sregs: [RegVal; 4],
    pub gpregs: [RegVal; 8],
}

impl State {
    fn join(a: &Self, b: &Self) -> Self {
        let sregs = std::array::from_fn(|i| RegVal::join(&a.sregs[i], &b.sregs[i]));
        let gpregs = std::array::from_fn(|i| RegVal::join(&a.gpregs[i], &b.gpregs[i]));
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

    pub fn get_sreg(&self, r: SReg) -> &RegVal {
        &self.sregs[Self::sreg_idx(r)]
    }

    pub fn set_sreg(&mut self, r: SReg, v: RegVal) {
        self.sregs[Self::sreg_idx(r)] = v;
    }

    fn get_gpreg(&self, r: GpReg16) -> &RegVal {
        &self.gpregs[r as usize]
    }

    fn set_gpreg(&mut self, r: GpReg16, v: RegVal) {
        self.gpregs[r as usize] = v;
    }
}

#[derive(Debug, Default, Clone)]
pub struct RegDataflow {
    pub block_entry: BTreeMap<Address, State>,
}

fn read_seg_val(state: &State, inst: &DecodedInstruction, i: usize) -> RegVal {
    match inst.operand(i) {
        Operand::Sreg(r) => *state.get_sreg(r),
        Operand::Gp16(r) => *state.get_gpreg(r),
        _ => RegVal::Unknown,
    }
}

fn write_seg_val(state: &mut State, inst: &DecodedInstruction, i: usize, val: RegVal) {
    match inst.operand(i) {
        Operand::Sreg(r) => state.set_sreg(r, val),
        Operand::Gp16(r) => state.set_gpreg(r, val),
        _ => {}
    }
}

fn transfer_block(project: &Project, block: &BasicBlock, entry: &State) -> State {
    let mut state = entry.clone();
    let mut stack: Vec<RegVal> = Vec::new();

    let seg = &project.segments[block.seg_idx];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;

    let mut ofs = block.start;
    while ofs < block.end {
        let bytes = project.bytes_at_seg(block.seg_idx, ofs);
        let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) else {
            break;
        };
        let len = inst.bytes.len() as u32;

        match inst.opcode {
            Opcode::Push => {
                stack.push(read_seg_val(&state, &inst, 0));
            }

            Opcode::Pop => {
                let val = stack.pop().unwrap_or(RegVal::Unknown);
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
                state.set_sreg(SReg::DS, RegVal::Unknown);
                write_seg_val(&mut state, &inst, 0, RegVal::Unknown);
            }

            Opcode::Les => {
                state.set_sreg(SReg::ES, RegVal::Unknown);
                write_seg_val(&mut state, &inst, 0, RegVal::Unknown);
            }

            // Near calls preserve CS; consult the per-function preservation
            // analysis for DS/ES/SS, and assume all GP regs are clobbered.
            Opcode::Call => {
                // clobber_call_with_preserves(&mut state, project, &inst);
            }

            // Interrupts: handler returns via iret, restoring CS/SS/flags from stack,
            // but DS/ES/GP may have changed.
            Opcode::Int | Opcode::Into => {
                // clobber_call(&mut state);
            }

            // Stack-pointer manipulation invalidates the abstract stack.
            Opcode::Pushf | Opcode::Popf => {
                stack.clear();
            }

            // Any other opcode: clobber destination GP registers.
            _ => {
                for i in inst.destinations() {
                    if let Operand::Gp16(r) = inst.operand(i) {
                        state.set_gpreg(r, RegVal::Unknown);
                    }
                }
            }
        }

        ofs += len;
    }

    state
}

pub fn compute(project: &Project) -> RegDataflow {
    let mut df = RegDataflow::default();
    let mut worklist = BTreeSet::<Address>::new();

    // Seed: every block that has no CFG predecessors gets an initial state
    // with CS = Segment(seg_idx) and any segment-level `assume` values applied.
    for block in project.blocks.blocks() {
        if block.predecessors.is_empty() {
            let mut state = State::default();

            state.set_sreg(SReg::CS, RegVal::VirtualSegment(block.seg_idx));

            df.block_entry
                .entry((block.seg_idx, block.start))
                .and_modify(|e| *e = State::join(e, &state))
                .or_insert(state);

            worklist.insert((block.seg_idx, block.start));
        }
    }

    // Propagate until stable.
    while let Some((seg_idx, start)) = worklist.pop_first() {
        let Some(block) = project.blocks.block_at(seg_idx, start) else {
            continue;
        };

        let entry = df
            .block_entry
            .get(&(seg_idx, start))
            .cloned()
            .unwrap_or_else(State::default);

        let exit = transfer_block(project, block, &entry);

        let successors: Vec<Address> = block.successors.to_vec();
        for (succ_seg, succ_ofs) in successors {
            let changed = match df.block_entry.get_mut(&(succ_seg, succ_ofs)) {
                Some(old) => {
                    let new = State::join(old, &exit);
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
                worklist.insert((block.seg_idx, block.start));
            }
        }
    }

    df
}

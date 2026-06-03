//! Type propagation — turns `let`/`fn` bindings into per-address register/stack
//! types.
//!
//! Each binding is a *seed*: "at this PC, this location holds this type." A
//! forward data-flow pass (mirroring [`crate::seg_dataflow`]) carries the seed
//! along the CFG, copying types through register-to-register moves and dropping
//! them where a location is reloaded with an unrelated value. The result lets
//! the listing renderer rewrite `[si+3]` → `troop->occupation` and `cx` →
//! `count`.
//!
//! Seeds are applied at the *entry* to their address (before that address's
//! instruction transfer), so a `fn` signature types the entry state and a `let`
//! placed at an instruction types the value flowing into it. To re-type a
//! register after a reassignment, place the `let` at the address of the
//! instruction *following* the reassignment.

use std::collections::{BTreeMap, VecDeque};

use crate::binding::{Binding, Location};
use crate::data_type::DataType;
use crate::function_summary::{ExitVal, RegId};
use crate::{
    Address, DisasmCtx, GpReg8, GpReg16, Operand, SReg, SmallString,
    basic_block::BasicBlock,
    decode_with_ctx,
    decoded_instruction::DecodedInstruction,
    opcode_table::Opcode,
    project::{Project, SegmentIdx},
};

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

/// A typed value living in some location, carrying the seeding binding's name.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypedVal {
    Typed {
        name: Option<SmallString>,
        ty: DataType,
    },
    /// No type information.
    Unknown,
}

impl TypedVal {
    fn join(&self, other: &Self) -> Self {
        if self == other {
            self.clone()
        } else {
            TypedVal::Unknown
        }
    }

    fn is_known(&self) -> bool {
        matches!(self, TypedVal::Typed { .. })
    }
}

// ── Abstract state ────────────────────────────────────────────────────────────

/// Per-location type state at a program point. GP registers are tracked at both
/// 16-bit and 8-bit granularity; an 8-bit write narrows its parent 16-bit slot
/// and vice versa.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeState {
    gp16: [TypedVal; 8],
    gp8: [TypedVal; 8],
    sregs: [TypedVal; 4],
    flags: [TypedVal; 6],
    stack: BTreeMap<i32, TypedVal>,
}

impl TypeState {
    pub fn all_unknown() -> Self {
        Self {
            gp16: std::array::from_fn(|_| TypedVal::Unknown),
            gp8: std::array::from_fn(|_| TypedVal::Unknown),
            sregs: std::array::from_fn(|_| TypedVal::Unknown),
            flags: std::array::from_fn(|_| TypedVal::Unknown),
            stack: BTreeMap::new(),
        }
    }

    fn join(&self, other: &Self) -> Self {
        let gp16 = std::array::from_fn(|i| self.gp16[i].join(&other.gp16[i]));
        let gp8 = std::array::from_fn(|i| self.gp8[i].join(&other.gp8[i]));
        let sregs = std::array::from_fn(|i| self.sregs[i].join(&other.sregs[i]));
        let flags = std::array::from_fn(|i| self.flags[i].join(&other.flags[i]));
        // Stack slots: keep only entries present and equal in both.
        let mut stack = BTreeMap::new();
        for (k, v) in &self.stack {
            if let Some(o) = other.stack.get(k) {
                let j = v.join(o);
                if j.is_known() {
                    stack.insert(*k, j);
                }
            }
        }
        Self {
            gp16,
            gp8,
            sregs,
            flags,
            stack,
        }
    }

    /// Read the type at a location.
    pub fn get(&self, loc: Location) -> TypedVal {
        match loc {
            Location::Gp16(r) => self.gp16[r as usize].clone(),
            Location::Gp8(r) => self.gp8[r as usize].clone(),
            Location::Seg(r) => self.sregs[sreg_idx(r)].clone(),
            Location::Flag(f) => self.flags[f.idx()].clone(),
            Location::Stack(off) => self.stack.get(&off).cloned().unwrap_or(TypedVal::Unknown),
        }
    }

    fn set(&mut self, loc: Location, val: TypedVal) {
        match loc {
            Location::Gp16(r) => {
                self.gp16[r as usize] = val;
                // A 16-bit write narrows any overlapping 8-bit halves.
                for h in gp8_halves(r) {
                    self.gp8[*h as usize] = TypedVal::Unknown;
                }
            }
            Location::Gp8(r) => {
                self.gp8[r as usize] = val;
                // An 8-bit write narrows the parent 16-bit register.
                self.gp16[gp16_parent(r) as usize] = TypedVal::Unknown;
            }
            Location::Seg(r) => self.sregs[sreg_idx(r)] = val,
            Location::Flag(f) => self.flags[f.idx()] = val,
            Location::Stack(off) => {
                if val.is_known() {
                    self.stack.insert(off, val);
                } else {
                    self.stack.remove(&off);
                }
            }
        }
    }

    /// Clobber every register/flag — the conservative model for an indirect
    /// call or interrupt with no usable summary.
    fn clobber_all_regs(&mut self) {
        self.gp16 = std::array::from_fn(|_| TypedVal::Unknown);
        self.gp8 = std::array::from_fn(|_| TypedVal::Unknown);
        self.sregs = std::array::from_fn(|_| TypedVal::Unknown);
        self.flags = std::array::from_fn(|_| TypedVal::Unknown);
    }

    /// Clobber the registers a callee does not preserve. A register the
    /// summary reports as exiting with its entry value (`Entry(self)`) keeps
    /// its type; everything else is dropped. Flags are always dropped.
    fn clobber_for_call(&mut self, summary: &crate::function_summary::FunctionSummary) {
        for (i, reg) in GP16_BY_IDX.into_iter().enumerate() {
            let preserved =
                matches!(summary.gpregs[i], ExitVal::Entry(RegId::Gp16(r)) if r as usize == i);
            if !preserved {
                self.gp16[i] = TypedVal::Unknown;
                // The two 8-bit halves go with their 16-bit parent.
                for h in gp8_halves(reg) {
                    self.gp8[*h as usize] = TypedVal::Unknown;
                }
            }
        }
        for (i, sreg) in [SReg::ES, SReg::CS, SReg::SS, SReg::DS]
            .into_iter()
            .enumerate()
        {
            let preserved = matches!(summary.sregs[i], ExitVal::Entry(RegId::Sreg(r)) if r == sreg);
            if !preserved {
                self.sregs[i] = TypedVal::Unknown;
            }
        }
        self.flags = std::array::from_fn(|_| TypedVal::Unknown);
    }
}

/// 16-bit registers in `GpReg16 as usize` order.
const GP16_BY_IDX: [GpReg16; 8] = [
    GpReg16::AX,
    GpReg16::CX,
    GpReg16::DX,
    GpReg16::BX,
    GpReg16::SP,
    GpReg16::BP,
    GpReg16::SI,
    GpReg16::DI,
];

fn sreg_idx(r: SReg) -> usize {
    match r {
        SReg::ES => 0,
        SReg::CS => 1,
        SReg::SS => 2,
        SReg::DS => 3,
    }
}

/// The two 8-bit halves overlapping a 16-bit register (empty for SP/BP/SI/DI).
fn gp8_halves(r: GpReg16) -> &'static [GpReg8] {
    match r {
        GpReg16::AX => &[GpReg8::AL, GpReg8::AH],
        GpReg16::CX => &[GpReg8::CL, GpReg8::CH],
        GpReg16::DX => &[GpReg8::DL, GpReg8::DH],
        GpReg16::BX => &[GpReg8::BL, GpReg8::BH],
        _ => &[],
    }
}

/// The 16-bit register an 8-bit register is a half of.
pub fn gp16_parent(r: GpReg8) -> GpReg16 {
    match r {
        GpReg8::AL | GpReg8::AH => GpReg16::AX,
        GpReg8::CL | GpReg8::CH => GpReg16::CX,
        GpReg8::DL | GpReg8::DH => GpReg16::DX,
        GpReg8::BL | GpReg8::BH => GpReg16::BX,
    }
}

/// True for the high half (AH/CH/DH/BH).
pub fn gp8_is_high(r: GpReg8) -> bool {
    matches!(r, GpReg8::AH | GpReg8::CH | GpReg8::DH | GpReg8::BH)
}

/// Map an operand to its binding location, if it is a register.
fn operand_loc(op: Operand) -> Option<Location> {
    match op {
        Operand::Gp16(r) => Some(Location::Gp16(r)),
        Operand::Gp8(r) => Some(Location::Gp8(r)),
        Operand::Sreg(r) => Some(Location::Seg(r)),
        _ => None,
    }
}

// ── Dataflow result ───────────────────────────────────────────────────────────

#[derive(Clone, Debug, Default)]
pub struct TypeProp {
    block_entry: BTreeMap<Address, TypeState>,
}

impl TypeProp {
    pub fn new() -> Self {
        Self::default()
    }

    /// Type state immediately before the instruction at `(seg_idx, ofs)`.
    /// Re-runs the transfer from the containing block's entry up to `ofs`.
    pub fn state_at(&self, project: &Project, seg_idx: SegmentIdx, ofs: u32) -> Option<TypeState> {
        let block = project.blocks.block_containing(seg_idx, ofs)?;
        let entry = self.block_entry.get(&(block.seg_idx, block.start))?;
        Some(transfer_block_until(project, block, entry, ofs))
    }

    /// The type bound to `loc` immediately before the instruction at
    /// `(seg_idx, ofs)`, if any.
    pub fn type_at(
        &self,
        project: &Project,
        seg_idx: SegmentIdx,
        ofs: u32,
        loc: Location,
    ) -> Option<TypedVal> {
        let v = self.state_at(project, seg_idx, ofs)?.get(loc);
        v.is_known().then_some(v)
    }
}

// ── Seeds ───────────────────────────────────────────────────────────────────

/// Apply the `let`/`fn` seeds attached to `(seg_idx, ofs)` to `state`.
fn apply_seeds(state: &mut TypeState, project: &Project, seg_idx: SegmentIdx, ofs: u32) {
    let Some(attr) = project.attr_at(seg_idx, ofs) else {
        return;
    };
    let seed = |state: &mut TypeState, b: &Binding| {
        state.set(
            b.loc,
            TypedVal::Typed {
                name: b.name.clone(),
                ty: b.ty.clone(),
            },
        );
    };
    if let Some(signature) = &attr.signature {
        for b in signature {
            seed(state, b);
        }
    }
    for b in &attr.lets {
        seed(state, b);
    }
}

// ── Transfer function ─────────────────────────────────────────────────────────

/// Run the transfer from `block.start` up to (but not including) `stop_before`.
fn transfer_block_until(
    project: &Project,
    block: &BasicBlock,
    entry: &TypeState,
    stop_before: u32,
) -> TypeState {
    let mut state = entry.clone();
    let mut abstract_stack: Vec<TypedVal> = Vec::new();

    let mut ofs = block.start;
    while ofs < block.end && ofs < stop_before {
        apply_seeds(&mut state, project, block.seg_idx, ofs);

        let Some(inst) = decode_in_project(project, block.seg_idx, ofs) else {
            break;
        };
        let len = inst.bytes.len() as u32;

        match inst.opcode {
            Opcode::Push => {
                let v = operand_loc(inst.operand(0))
                    .map(|l| state.get(l))
                    .unwrap_or(TypedVal::Unknown);
                abstract_stack.push(v);
            }
            Opcode::Pop => {
                let v = abstract_stack.pop().unwrap_or(TypedVal::Unknown);
                if let Some(loc) = operand_loc(inst.operand(0)) {
                    state.set(loc, v);
                }
            }
            // Register-to-register moves copy the type; any other move source
            // (memory, immediate) narrows the destination.
            Opcode::Mov => {
                if let Some((dst, src)) = inst.dst_src()
                    && let Some(dst_loc) = operand_loc(inst.operand(dst))
                {
                    let v = match operand_loc(inst.operand(src)) {
                        Some(src_loc) => state.get(src_loc),
                        None => TypedVal::Unknown,
                    };
                    state.set(dst_loc, v);
                }
            }
            Opcode::Xchg => {
                let a_loc = operand_loc(inst.operand(0));
                let b_loc = operand_loc(inst.operand(1));
                let a = a_loc.map(|l| state.get(l)).unwrap_or(TypedVal::Unknown);
                let b = b_loc.map(|l| state.get(l)).unwrap_or(TypedVal::Unknown);
                if let Some(l) = a_loc {
                    state.set(l, b);
                }
                if let Some(l) = b_loc {
                    state.set(l, a);
                }
            }
            // `lds`/`les` load a fresh far pointer, dropping the destination's
            // (and the loaded segment's) type.
            Opcode::Lds => {
                state.set(Location::Seg(SReg::DS), TypedVal::Unknown);
                if let Some(loc) = operand_loc(inst.operand(0)) {
                    state.set(loc, TypedVal::Unknown);
                }
            }
            Opcode::Les => {
                state.set(Location::Seg(SReg::ES), TypedVal::Unknown);
                if let Some(loc) = operand_loc(inst.operand(0)) {
                    state.set(loc, TypedVal::Unknown);
                }
            }
            // A direct call drops the types of every register the callee does
            // not preserve (per its function summary); a preserved register
            // (e.g. a `si` the callee leaves untouched) keeps its type. An
            // indirect call or interrupt with no summary clobbers everything.
            Opcode::Call => {
                let summary = inst.branch_destination().and_then(|(seg, ofs)| {
                    let seg_idx = project.segment_index_for(seg)?;
                    project.function_summary.get(&(seg_idx, ofs as u32))
                });
                match summary {
                    Some(summary) => state.clobber_for_call(summary),
                    None => state.clobber_all_regs(),
                }
            }
            Opcode::Int | Opcode::Into => {
                state.clobber_all_regs();
            }
            Opcode::Pushf | Opcode::Popf => {
                abstract_stack.clear();
            }
            // Arithmetic and other in-place opcodes preserve the destination's
            // type: an `inout` accumulator (`add dl, …`; `inc cx`) stays the
            // same logical value. A type is only dropped when a location is
            // *reloaded* with an unrelated value (a `mov`/`pop`/`lds` above) or
            // re-pinned by a `let` seed.
            _ => {}
        }

        ofs += len;
    }

    apply_seeds(&mut state, project, block.seg_idx, stop_before);
    state
}

fn transfer_block(project: &Project, block: &BasicBlock, entry: &TypeState) -> TypeState {
    transfer_block_until(project, block, entry, block.end)
}

/// Successors of a call-terminated block that are callee entries (not the
/// structural fall-through). Types must not flow into a callee body.
fn call_target_successors(project: &Project, block: &BasicBlock) -> Vec<Address> {
    let last_ofs = project.segments[block.seg_idx]
        .addr_attributes
        .prev(block.end)
        .filter(|&p| p >= block.start);
    let Some(last_ofs) = last_ofs else {
        return Vec::new();
    };
    let is_call = decode_in_project(project, block.seg_idx, last_ofs)
        .is_some_and(|i| i.opcode == Opcode::Call);
    if !is_call {
        return Vec::new();
    }
    project
        .branches
        .targets((block.seg_idx, last_ofs))
        .collect()
}

// ── Fixed-point iteration ─────────────────────────────────────────────────────

pub fn compute(project: &Project) -> TypeProp {
    let mut tp = TypeProp::new();
    let mut worklist: VecDeque<Address> = VecDeque::new();

    // Seed every entry block (no CFG predecessors) with an all-unknown state.
    // Per-address seeds are applied inside the transfer function.
    for block in project.blocks.blocks() {
        if block.predecessors.is_empty() {
            tp.block_entry
                .entry((block.seg_idx, block.start))
                .or_insert_with(TypeState::all_unknown);
            worklist.push_back((block.seg_idx, block.start));
        }
    }

    while let Some((seg_idx, start)) = worklist.pop_front() {
        let Some(block) = project.blocks.block_at(seg_idx, start) else {
            continue;
        };
        let entry = tp
            .block_entry
            .get(&(seg_idx, start))
            .cloned()
            .unwrap_or_else(TypeState::all_unknown);

        let exit = transfer_block(project, block, &entry);
        let call_targets = call_target_successors(project, block);

        for succ in block.successors.iter().copied() {
            // Callee entries start fresh; their own `fn` seeds type the entry.
            let state_for_succ = if call_targets.contains(&succ) {
                TypeState::all_unknown()
            } else {
                exit.clone()
            };

            let changed = match tp.block_entry.get_mut(&succ) {
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
                    tp.block_entry.insert(succ, state_for_succ);
                    true
                }
            };
            if changed {
                worklist.push_back(succ);
            }
        }
    }

    tp
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address_attributes::AddressAttributes;
    use crate::project::BinImage;

    fn make_project(chani_extra: &str, code: &[u8]) -> Project {
        let mut chani = String::from("project[t]:\narch = 8086\n");
        chani.push_str("segment[seg000]:\n    type = code\n    start = 0x0\n");
        chani.push_str(&format!("    end = 0x{:x}\nend\n", code.len().max(1)));
        chani.push_str(
            "struct[Troop]: _pad0 = [u8; 3]; occupation = u8; _pad1 = [u8; 19]; armyskill = u8\n",
        );
        chani.push_str(chani_extra);
        chani.push_str("\nend\n");
        let mut p = Project::from_str(&chani).unwrap();
        p.segments[SegmentIdx::from(0)].addr_attributes = AddressAttributes::new(code.len().max(1));
        p.images.push(BinImage {
            seg_idx: SegmentIdx::from(0),
            load_offset: 0,
            data: code.to_vec(),
        });
        p.analyze();
        p
    }

    /// Encode a `call rel16` at `at_ofs` targeting `target_ofs`.
    fn call_rel16(at_ofs: u32, target_ofs: u32) -> [u8; 3] {
        let disp = (target_ofs as i32 - at_ofs as i32 - 3) as i16 as u16;
        [0xE8, disp as u8, (disp >> 8) as u8]
    }

    #[test]
    fn preserved_register_keeps_type_across_call() {
        // F @ 0x00: ret             — preserves every register.
        // caller @ 0x10 (fn: si = *Troop): call F ; cmp byte ptr [si+3], 0 ; ret.
        // Because F preserves si, the type survives the call and `[si+3]`
        // still resolves at 0x13.
        let mut code = vec![0xC3]; // F: ret
        code.resize(0x10, 0x90);
        code.extend_from_slice(&call_rel16(0x10, 0x00)); // 0x10: call F
        code.extend_from_slice(&[0x80, 0x7C, 0x03, 0x00]); // 0x13: cmp byte ptr [si+3], 0
        code.push(0xC3); // 0x17: ret

        let extra = "attr[seg000:0]: type = code\n\
                     attr[seg000:10]: type = code; fn = in troop: *Troop @si\n";
        let p = make_project(extra, &code);
        let seg = SegmentIdx::from(0);

        // si is still *Troop at the post-call `cmp` (0x13).
        let v = p
            .type_prop
            .type_at(&p, seg, 0x13, Location::Gp16(GpReg16::SI));
        assert!(
            matches!(v, Some(TypedVal::Typed { ref ty, .. }) if ty.as_ptr().is_some()),
            "si should stay typed across a preserving call, got {v:?}"
        );
    }

    #[test]
    fn fn_seed_propagates_through_straight_line() {
        // 0: cmp byte ptr [si+3], 4   (does not write si)
        // 4: mov ax, si               (copies the pointer type to ax)
        // 6: ret
        let code = [0x80, 0x7C, 0x03, 0x04, 0x89, 0xF0, 0xC3];
        let extra = "attr[seg000:0]: type = code; fn = in troop: *Troop @si\n";
        let p = make_project(extra, &code);
        let seg = SegmentIdx::from(0);

        // si is *Troop at entry and remains so at the `mov`.
        let v = p
            .type_prop
            .type_at(&p, seg, 0x4, Location::Gp16(GpReg16::SI));
        assert!(
            matches!(v, Some(TypedVal::Typed { ref name, .. }) if name.as_deref() == Some("troop"))
        );

        // After `mov ax, si`, ax carries the same type at the `ret`.
        let v = p
            .type_prop
            .type_at(&p, seg, 0x6, Location::Gp16(GpReg16::AX));
        assert!(matches!(v, Some(TypedVal::Typed { ref ty, .. }) if ty.as_ptr().is_some()));
    }

    /// Render one instruction through a type-aware `ProjectLookup`.
    fn render(p: &Project, seg: SegmentIdx, ofs: u32) -> String {
        let inst = decode_in_project(p, seg, ofs).unwrap();
        let arg_fmts = p.attr_at(seg, ofs).map(|a| a.arg_fmts).unwrap_or([None; 2]);
        let lookup = crate::project::ProjectLookup {
            project: p,
            sreg_map: crate::SRegMap::default(),
            register_file: None,
            default_seg: None,
            addr: Some((seg, ofs)),
        };
        let ctx = crate::DisplayContext {
            lookup: &lookup,
            arg_fmts,
        };
        inst.to_string_opts(ctx)
    }

    #[test]
    fn renders_section_5_3_accumulator_callback() {
        // The §5.3 worked example:
        //   cmp byte ptr [si+3], 4   →  cmp byte ptr troop->occupation, 4
        //   inc cx                   →  inc count
        //   add dl, [si+17h]         →  add skill_sum.lo, troop->armyskill
        //   adc dh, 0                →  adc skill_sum.hi, 0
        let code = [
            0x80, 0x7C, 0x03, 0x04, // 0x00: cmp byte ptr [si+3], 4
            0x75, 0x07, // 0x04: jnz 0x0d
            0x41, // 0x06: inc cx
            0x02, 0x54, 0x17, // 0x07: add dl, [si+17h]
            0x80, 0xD6, 0x00, // 0x0a: adc dh, 0
            0xC3, // 0x0d: ret
        ];
        let extra = "attr[seg000:0]: type = code; fn = [[[\n\
                         in troop: *Troop @si,\n\
                         inout count: u16 @cx,\n\
                         inout skill_sum: u16 @dx,\n\
                     ]]]\n";
        let p = make_project(extra, &code);
        let seg = SegmentIdx::from(0);

        let cmp = render(&p, seg, 0x00);
        assert!(cmp.contains("troop->occupation"), "{cmp}");

        let inc = render(&p, seg, 0x06);
        assert!(inc.contains("count"), "{inc}");

        let add = render(&p, seg, 0x07);
        assert!(add.contains("skill_sum.lo"), "{add}");
        assert!(add.contains("troop->armyskill"), "{add}");

        let adc = render(&p, seg, 0x0a);
        assert!(adc.contains("skill_sum.hi"), "{adc}");
    }

    #[test]
    fn reload_drops_the_type() {
        // 0: mov si, 0   (reloads si with an immediate — type is dropped)
        // 3: ret
        let code = [0xBE, 0x00, 0x00, 0xC3];
        let extra = "attr[seg000:0]: type = code; fn = in troop: *Troop @si\n";
        let p = make_project(extra, &code);
        let seg = SegmentIdx::from(0);

        // Seeded at entry...
        assert!(
            p.type_prop
                .type_at(&p, seg, 0x0, Location::Gp16(GpReg16::SI))
                .is_some()
        );
        // ...but dropped after the reload (state at the `ret`).
        assert!(
            p.type_prop
                .type_at(&p, seg, 0x3, Location::Gp16(GpReg16::SI))
                .is_none()
        );
    }
}

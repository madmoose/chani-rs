use bytes_ext::U16Ext;
use chani_disasm::{BaseReg, DataWidth, DisplayContext, IndexReg, MemRef, SymbolLookup};

use super::bitops::BitOps;
use super::register_file::RegisterFile;
use super::{
    AluOp, Callback, Dir, FLAG_AF, FLAG_CF, FLAG_PF, FLAG_SF, FLAG_ZF, RepMode, flags::*, sext16,
};
use super::{
    ModRM, RegMem,
    Register::*,
    SReg,
    Width::{self, *},
    read_hi, read_lo, sext,
};
use super::{State, StrOp};
use crate::address::{Address, addr};
use crate::clock::Clock;
use crate::cpu::CpuContext;
use crate::memory::Memory;
use crate::{DUNE_DNADL, DUNE_DNVGA, DUNE_SEG001};
use std::collections::HashMap;
use std::fmt::LowerHex;
use std::mem::swap;

struct NameMapLookup<'a>(&'a HashMap<(u16, u16), String>);

impl SymbolLookup for NameMapLookup<'_> {
    fn lookup_direct(&self, seg: u16, ofs: u16, _width: DataWidth) -> Option<&str> {
        self.0.get(&(seg, ofs)).map(String::as_str)
    }

    fn lookup_indirect(
        &self,
        _seg: chani_disasm::SReg,
        _base: Option<BaseReg>,
        _index: Option<IndexReg>,
        _disp: u16,
        _width: DataWidth,
    ) -> Option<&str> {
        None
    }

    fn lookup_offset(&self, _ofs: u16) -> Option<&str> {
        None
    }
}

#[derive(Debug)]
pub struct Cpu {
    clock: Clock,
    state: State,
    instruction_address: Address,
    register_file: RegisterFile,
    sreg_ovr: Option<SReg>,
    int_delay: bool,
    int_nmi: bool,
    int_intr: bool,
    int_number: u8,

    rep_mode: RepMode,
    cycles: u64,
    instruction_count: u64,

    pub callback_base_address: Address,
    pub callbacks: HashMap<Address, Callback>,

    logging: bool,
    names: HashMap<(u16, u16), String>,
    // pub pal: Pal888,
    globdata_log: bool,

    ppm_cnt: usize,
    ppm_cnt2: usize,
    ppm_index: usize,
    ppm_index2: usize,

    band_index: i16,
    backup_register_file: Option<RegisterFile>,
}

impl SReg {
    fn from_u8(i: u8) -> SReg {
        match i {
            0b00 => SReg::ES,
            0b01 => SReg::CS,
            0b10 => SReg::SS,
            0b11 => SReg::DS,
            _ => unreachable!(),
        }
    }
}

enum Dd {
    Byte(u8),
    Word(u16),
    Dword(u32),
}

impl Dd {
    pub fn as_u32(&self) -> u32 {
        match self {
            Dd::Byte(v) => *v as u32,
            Dd::Word(v) => *v as u32,
            Dd::Dword(v) => *v,
        }
    }
}

impl LowerHex for Dd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dd::Byte(v) => {
                write!(f, "{:02x}", v)
            }
            Dd::Word(v) => {
                write!(f, "{:04x}", v)
            }
            Dd::Dword(v) => {
                write!(f, "{:08x}", v)
            }
        }
    }
}

impl Cpu {
    pub fn new(clock: Clock) -> Self {
        let mut names = HashMap::new();

        names.insert((DUNE_DNVGA, 0x01a5), String::from("GLOBDATA_PTR"));
        names.insert((DUNE_DNVGA, 0x01a7), String::from("MAP_PTR"));
        names.insert((DUNE_DNVGA, 0x01bb), String::from("TABLAT_PTR"));

        Self {
            clock,
            callback_base_address: addr(0xfe00, 0),
            state: Default::default(),
            instruction_address: Default::default(),
            register_file: Default::default(),
            sreg_ovr: Default::default(),
            int_delay: Default::default(),
            int_nmi: Default::default(),
            int_intr: Default::default(),
            int_number: Default::default(),
            rep_mode: Default::default(),
            cycles: Default::default(),
            instruction_count: Default::default(),
            callbacks: Default::default(),
            logging: false,
            names,
            globdata_log: false,
            ppm_cnt: 0,
            ppm_cnt2: 1,
            ppm_index: 1,
            ppm_index2: 1,

            band_index: -93,
            backup_register_file: None,
        }
    }

    pub fn frequency(&self) -> f64 {
        self.clock.0
    }

    pub fn run_cycles<Context: CpuContext>(
        &mut self,
        ctx: &mut Context,
        cycles: u64,
    ) -> (u64, Option<(Callback, Address)>) {
        self.state = State::Running;
        let start_cycles = self.cycles;

        while self.cycles - start_cycles < cycles {
            self.step(ctx);
            self.instruction_count += 1;

            if !matches!(self.state, State::Running) {
                break;
            }
        }

        let callback = match self.state {
            State::Running => None,
            State::Callback(callback) => Some(callback),
        };

        (self.cycles - start_cycles, callback)
    }

    fn resolve_memory_reference<Context: CpuContext>(
        &self,
        ctx: &mut Context,
        mem_ref: MemRef,
    ) -> (Address, Dd) {
        let (addr, width) = match mem_ref {
            MemRef::Direct { seg, ofs, width } => (addr(seg, ofs), width),
            MemRef::Indirect {
                seg,
                base,
                index,
                disp,
                width,
            } => {
                let seg = self.register_file.get(seg.into());
                let base = base.map(|b| self.register_file.get(b.into())).unwrap_or(0);
                let index = index.map(|i| self.register_file.get(i.into())).unwrap_or(0);

                let ofs = base.wrapping_add(index).wrapping_add(disp);

                (addr(seg, ofs), width)
            }
        };

        let v = match width {
            DataWidth::Byte => Dd::Byte(ctx.memory().read_u8(addr)),
            DataWidth::Word => Dd::Word(ctx.memory().read_u16(addr)),
            DataWidth::Dword => {
                let lo = ctx.memory().read_u16(addr);
                let hi = ctx.memory().read_u16(addr + 2);
                Dd::Dword(((hi as u32) << 16) + (lo as u32))
            }
        };

        (addr, v)
    }

    pub fn step<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let mut op: u8;

        if !self.int_delay && self.register_file.get_if() {
            if self.int_nmi {
                self.int_nmi = false;
                self.call_int(ctx, 2);
                return;
            }
            if self.int_intr {
                self.int_intr = false;
                self.call_int(ctx, self.int_number);
                return;
            }
        }
        self.int_delay = false;

        let csip = self.register_file.get_csip();
        self.instruction_address = csip;

        // if csip == (DUNE_SEG000, 0xb713) {
        //     self.logging = false;
        // }
        // if csip == (DUNE_SEG000, 0xb7d1) {
        //     self.logging = false;
        // }

        // self.logging = csip.seg == DUNE_DNVGA && csip.ofs >= 0x1f4c && csip.ofs < 0x2024;

        // if csip == (DUNE_DNVGA, 0x230a) {
        //     println!("{}", csip);
        //     if let Some(backup_register_file) = &self.backup_register_file {
        //         const PAL: &[u8; 768] = include_bytes!("../../../../PAL.BIN");
        //         let pal = PAL.into();

        //         const W: usize = 312;
        //         const H: usize = 4;
        //         let mut data = [0u8; H * W];
        //         for y in 0..H as u16 {
        //             for x in 0..W as u16 {
        //                 let b = ctx
        //                     .memory()
        //                     .read_u8(addr(self.get_ds(), 0x4c60 + 160 + y * 400 + x));
        //                 let b = (b >> 4) + 0x10;
        //                 data[(y * (W as u16) + x) as usize] = b;
        //             }
        //         }
        //         // println!("data = {data:?}");
        //         write_ppm(
        //             &pal,
        //             &data,
        //             W,
        //             H,
        //             &format!("globdata-{:03}.ppm", self.band_index + 93),
        //         );

        //         if self.band_index < 93 {
        //             self.register_file = backup_register_file.clone();

        //             self.register_file.set(Register::IP, 0x1f4c);
        //             csip = self.register_file.get_csip();
        //             self.instruction_address = csip;
        //         } else {
        //             std::process::exit(0);
        //         }
        //     }
        // }
        // if csip == (DUNE_DNVGA, 0x1f4c) {
        //     println!("{}", csip);
        //     if self.backup_register_file.is_none() {
        //         self.backup_register_file = Some(self.register_file.clone());
        //     }

        //     println!("Clearing globdata");
        //     for i in 0..2500 {
        //         ctx.memory().write_u8(addr(DUNE_DNVGA, 0x4c60 + i), 0);
        //     }

        //     self.set_ax(self.band_index as u16);
        //     self.band_index += 1;
        //     println!("AX = {}", self.get_ax() as i16);
        //     // self.set_ax((57i16) as u16);
        //     self.set_dx(0);
        // }

        // if csip == (DUNE_DNVGA, 0x2025) {
        //     println!(
        //         "LOG\nLOG sub_27855_interpolate_horizontally ({:#x})\n",
        //         self.globdata_running_sum
        //     );
        // }
        // if csip == (DUNE_DNVGA, 0x2123) {
        //     println!(
        //         "LOG\nLOG interpolate_vertically_1 ({:#x})\n",
        //         self.globdata_running_sum
        //     );
        // }
        // if csip == (DUNE_DNVGA, 0x2153) {
        //     println!(
        //         "LOG\nLOG interpolate_vertically_2 center-end ({:#x})\n",
        //         self.globdata_running_sum
        //     );
        // }
        // if csip == (DUNE_DNVGA, 0x2221) {
        //     println!(
        //         "LOG\nLOG interpolate_vertically center-start ({:#x})\n",
        //         self.globdata_running_sum
        //     );
        // }
        // if csip == (DUNE_DNVGA, 0x22a0) {
        //     println!("LOG\nLOG post-process ({:#x})\n", self.globdata_running_sum);
        // }

        // if csip == (DUNE_DNVGA, 0x204D) {
        //     println!("\n# branch_small_bx\n");
        // }
        // if csip == (DUNE_DNVGA, 0x2066) {
        //     println!("\n# setup_partial_loop\n");
        // }
        // if csip == (DUNE_DNVGA, 0x2079) {
        //     println!("\n# branch_large_bx\n");
        // }
        // if csip == (DUNE_DNVGA, 0x208B) {
        //     println!("\n# setup_partial_loop_2\n");
        // }
        // if csip == (DUNE_DNVGA, 0x209E) {
        //     println!("\n# main_interpolation_check\n");
        // }
        // if csip == (DUNE_DNVGA, 0x20B3) {
        //     println!("\n# interpolation_loop\n");
        // }
        // if csip == (DUNE_DNVGA, 0x20CB) {
        //     println!("\n# gradient_interpolation\n");
        // }
        // if csip == (DUNE_DNVGA, 0x20E0) {
        //     println!("\n# flat_color\n");
        // }
        // if csip == (DUNE_DNVGA, 0x20EB) {
        //     println!("\n# final_loop\n");
        // }
        // if csip == (DUNE_DNVGA, 0x20ED) {
        //     println!("\n# final_loop_body\n");
        // }
        // if csip == (DUNE_DNVGA, 0x2115) {
        //     println!("\n# exit\n");
        // }

        // if csip == (DUNE_SEG000, 0xb70f) {
        //     println!("{}", self.register_file);
        // }

        // if csip == (DUNE_DNADL, 0x0b34) {
        //     println!("out 0x388 {:02x} = {:02x}", self.get_al(), self.get_ah());
        // }

        for i in 0..7 {
            // if i == 5 {
            //     continue;
            // }
            if csip == (DUNE_DNADL, 0x0100 + 3 * i) {
                println!(
                    "midi fn {:4x} al={:02x} ah={:02x} bl={:02x} bh={:02x}",
                    csip.ofs,
                    self.get_al(),
                    self.get_ah(),
                    self.get_bx().lo(),
                    self.get_bx().hi()
                );
            }
        }

        if self.logging {
            let bytes = ctx.memory().iter_from(csip.ea());
            if let Some(inst) = chani_disasm::decode(csip.seg, csip.ofs, bytes) {
                let inst_str = inst.to_string_opts(DisplayContext {
                    lookup: &NameMapLookup(&self.names),
                });
                let mem_value = if inst.reads_from_mem()
                    && let Some(mem_ref) = inst.mem_ref()
                {
                    let (_address, value) = self.resolve_memory_reference(ctx, mem_ref);
                    Some(value)
                } else {
                    None
                };
                let mem_value = mem_value.map(|v| format!("{v:x}")).unwrap_or_default();
                println!("{csip} {inst_str:-34}{mem_value:4} {}", self.register_file);
            }
        }

        self.sreg_ovr = None;
        self.rep_mode = RepMode::None;

        loop {
            op = self.fetch8(ctx);

            match op {
                0x26 => self.sreg_ovr = Some(SReg::ES),
                0x2e => self.sreg_ovr = Some(SReg::CS),
                0x36 => self.sreg_ovr = Some(SReg::SS),
                0x3e => self.sreg_ovr = Some(SReg::DS),
                0xf2 => self.rep_mode = RepMode::Repne,
                0xf3 => self.rep_mode = RepMode::Rep,
                _ => break,
            }
        }

        match op {
            0x00..=0x03 => self.op_alu_r_rm(ctx, op),
            0x04..=0x05 => self.op_alu_a_imm(ctx, op),
            0x06..=0x06 => self.op_push_sreg(ctx, op),
            0x07..=0x07 => self.op_pop_sreg(ctx, op),
            0x08..=0x0b => self.op_alu_r_rm(ctx, op),
            0x0c..=0x0d => self.op_alu_a_imm(ctx, op),
            0x0e..=0x0e => self.op_push_sreg(ctx, op),
            0x0f..=0x0f => self.op_pop_sreg(ctx, op),
            0x10..=0x13 => self.op_alu_r_rm(ctx, op),
            0x14..=0x15 => self.op_alu_a_imm(ctx, op),
            0x16..=0x16 => self.op_push_sreg(ctx, op),
            0x17..=0x17 => self.op_pop_sreg(ctx, op),
            0x18..=0x1b => self.op_alu_r_rm(ctx, op),
            0x1c..=0x1d => self.op_alu_a_imm(ctx, op),
            0x1e..=0x1e => self.op_push_sreg(ctx, op),
            0x1f..=0x1f => self.op_pop_sreg(ctx, op),
            0x20..=0x23 => self.op_alu_r_rm(ctx, op),
            0x24..=0x25 => self.op_alu_a_imm(ctx, op),
            0x26..=0x26 => unreachable!(),
            0x27..=0x27 => self.op_daa(),
            0x28..=0x2b => self.op_alu_r_rm(ctx, op),
            0x2c..=0x2d => self.op_alu_a_imm(ctx, op),
            0x2e..=0x2e => unreachable!(),
            0x2f..=0x2f => self.op_das(),
            0x30..=0x33 => self.op_alu_r_rm(ctx, op),
            0x34..=0x35 => self.op_alu_a_imm(ctx, op),
            0x36..=0x36 => unreachable!(),
            0x37..=0x37 => self.op_aaa(),
            0x38..=0x3b => self.op_alu_r_rm(ctx, op),
            0x3c..=0x3d => self.op_alu_a_imm(ctx, op),
            0x3e..=0x3e => unreachable!(),
            0x3f..=0x3f => self.op_aas(),
            0x40..=0x47 => self.op_inc_reg(op),
            0x48..=0x4f => self.op_dec_reg(op),
            0x50..=0x57 => self.op_push_reg(ctx, op),
            0x58..=0x5f => self.op_pop_reg(ctx, op),
            0x60..=0x7f => self.op_jcc(ctx, op),
            0x80..=0x83 => self.op_grp1_rmw_imm(ctx, op),
            0x84..=0x85 => self.op_test_rm_r(ctx, op),
            0x86..=0x87 => self.op_xchg_rm_r(ctx, op),
            0x88..=0x8b => self.op_mov_rm_r(ctx, op),
            0x8c => self.op_mov_rm16_sreg(ctx, op),
            0x8d => self.op_lea_r16_m16(ctx),
            0x8e => self.op_mov_rm16_sreg(ctx, op),
            0x8f => self.op_pop_rm16(ctx),
            0x90..=0x97 => self.op_xchg_ax_r(op),
            0x98 => self.op_cbw(),
            0x99 => self.op_cwd(),
            0x9a => self.op_call_far(ctx),
            0x9b => self.op_wait(),
            0x9c => self.op_pushf(ctx),
            0x9d => self.op_popf(ctx),
            0x9e => self.op_sahf(),
            0x9f => self.op_lahf(),
            0xa0..=0xa3 => self.op_mov_a_m(ctx, op),
            0xa4..=0xa5 => self.op_movs(ctx, op),
            0xa6..=0xa7 => self.op_cmps(ctx, op),
            0xa8..=0xa9 => self.op_test_a_imm(ctx, op),
            0xaa..=0xab => self.op_stos(ctx, op),
            0xac..=0xad => self.op_lods(ctx, op),
            0xae..=0xaf => self.op_scas(ctx, op),
            0xb0..=0xbf => self.op_mov_reg_imm(ctx, op),
            0xc0 => self.op_ret_imm16_intraseg(ctx),
            0xc1 => self.op_ret_intraseg(ctx),
            0xc2 => self.op_ret_imm16_intraseg(ctx),
            0xc3 => self.op_ret_intraseg(ctx),
            0xc4 => self.op_les_r16_m16(ctx),
            0xc5 => self.op_lds_r16_m16(ctx),
            0xc6..=0xc7 => self.op_mov_m_imm(ctx, op),
            0xc8 => self.op_ret_imm16_interseg(ctx),
            0xc9 => self.op_ret_interseg(ctx),
            0xca => self.op_ret_imm16_interseg(ctx),
            0xcb => self.op_ret_interseg(ctx),
            0xcc => self.op_int_3(ctx),
            0xcd => self.op_int_imm8(ctx),
            0xce => self.op_into(ctx),
            0xcf => self.op_iret(ctx),
            0xd0..=0xd3 => self.op_grp2_rmw(ctx, op),
            0xd4 => self.op_aam(ctx),
            0xd5 => self.op_aad(ctx),
            0xd6 => self.op_salc(),
            0xd7 => self.op_xlat(ctx),
            0xd8 => self.op_esc(ctx),
            0xd9 => self.op_esc(ctx),
            0xda => self.op_esc(ctx),
            0xdb => self.op_esc(ctx),
            0xdc => self.op_esc(ctx),
            0xdd => self.op_esc(ctx),
            0xde => self.op_esc(ctx),
            0xdf => self.op_esc(ctx),
            0xe0 => self.op_loopnz(ctx),
            0xe1 => self.op_loopz(ctx),
            0xe2 => self.op_loop(ctx),
            0xe3 => self.op_jcxz(ctx),
            0xe4 => self.op_in_al_imm8(ctx),
            0xe5 => self.op_in_ax_imm8(ctx),
            0xe6 => self.op_out_al_imm8(ctx),
            0xe7 => self.op_out_ax_imm8(ctx),
            0xe8 => self.op_call_near(ctx),
            0xe9 => self.op_jmp_near(ctx),
            0xea => self.op_jmp_far(ctx),
            0xeb => self.op_jmp_short(ctx),
            0xec => self.op_in_al_dx(ctx),
            0xed => self.op_in_ax_dx(ctx),
            0xee => self.op_out_al_dx(ctx),
            0xef => self.op_out_ax_dx(ctx),
            0xf0 => self.op_lock_prefix(),
            0xf1 => self.op_unused(),
            0xf2 => self.op_repne(),
            0xf3 => self.op_rep(),
            0xf4 => self.op_hlt(),
            0xf5 => self.op_cmc(),
            0xf6..=0xf7 => self.op_grp3_rmw(ctx, op),
            0xf8 => self.op_clc(),
            0xf9 => self.op_stc(),
            0xfa => self.op_cli(),
            0xfb => self.op_sti(),
            0xfc => self.op_cld(),
            0xfd => self.op_std(),
            0xfe => self.op_grp4_rm8(ctx),
            0xff => self.op_grp5(ctx),
        };
    }

    pub fn dump_single_line(&self) {
        println!("{}", self.register_file);
    }

    pub fn dump(&self) {
        println!();
        println!(
            "\tax={:04x} es={:04x} sp={:04x} ip={:04x}",
            self.get_ax(),
            self.get_es(),
            self.get_sp(),
            self.get_ip()
        );
        println!(
            "\tcx={:04x} cs={:04x} bp={:04x} flags={:04x}",
            self.get_cx(),
            self.get_cs(),
            self.get_bp(),
            self.get_flags()
        );
        println!(
            "\tdx={:04x} ss={:04x} di={:04x}\tO{} D{} I{} T{} S{} Z{} A{} P{} C{}",
            self.get_dx(),
            self.get_ss(),
            self.get_di(),
            self.register_file.get_of() as u8,
            self.register_file.get_df() as u8,
            self.register_file.get_if() as u8,
            self.register_file.get_tf() as u8,
            self.register_file.get_sf() as u8,
            self.register_file.get_zf() as u8,
            self.register_file.get_af() as u8,
            self.register_file.get_pf() as u8,
            self.register_file.get_cf() as u8,
        );
        println!(
            "\tbx={:04x} ds={:04x} si={:04x}",
            self.get_bx(),
            self.get_ds(),
            self.get_si(),
        );

        println!()
    }

    pub fn register_callback(&mut self, memory: &mut Memory, callback: Callback) -> Address {
        let address = self.callback_base_address;
        self.callback_base_address += 2;

        self.callbacks.insert(address, callback);
        memory.write_u16(address, 0x38fe);

        address
    }

    pub fn get_instruction_address(&self) -> Address {
        self.instruction_address
    }

    pub fn get_state(&self) -> State {
        self.state
    }

    pub fn set_ax(&mut self, v: u16) {
        self.register_file.set(AX, v);
    }

    pub fn set_al(&mut self, v: u8) {
        self.register_file.set_lo(AX, v);
    }

    pub fn set_ah(&mut self, v: u8) {
        self.register_file.set_hi(AX, v);
    }

    pub fn set_cx(&mut self, v: u16) {
        self.register_file.set(CX, v);
    }

    pub fn set_dx(&mut self, v: u16) {
        self.register_file.set(DX, v);
    }

    pub fn set_bx(&mut self, v: u16) {
        self.register_file.set(BX, v);
    }

    pub fn set_bl(&mut self, v: u8) {
        self.register_file.set_lo(BX, v);
    }

    pub fn set_bh(&mut self, v: u8) {
        self.register_file.set_hi(BX, v);
    }

    pub fn set_sp(&mut self, v: u16) {
        self.register_file.set(SP, v);
    }

    pub fn set_bp(&mut self, v: u16) {
        self.register_file.set(BP, v);
    }

    pub fn set_di(&mut self, v: u16) {
        self.register_file.set(DI, v);
    }

    pub fn set_si(&mut self, v: u16) {
        self.register_file.set(SI, v);
    }

    pub fn set_es(&mut self, v: u16) {
        self.register_file.set(ES, v);
    }

    pub fn set_cs(&mut self, v: u16) {
        self.register_file.set(CS, v);
    }

    pub fn set_ss(&mut self, v: u16) {
        self.register_file.set(SS, v);
    }

    pub fn set_ds(&mut self, v: u16) {
        self.register_file.set(DS, v);
    }

    pub fn set_ip(&mut self, v: u16) {
        self.register_file.set(IP, v);
    }

    pub fn set_flags(&mut self, flags: u16) {
        let int_was_set = self.register_file.get_if();

        self.register_file.set_flags(flags);

        let int_is_set = self.register_file.get_if();

        if !int_was_set && int_is_set {
            self.int_delay = true;
        }
    }

    pub fn clc(&mut self) {
        self.set_cf(false);
    }

    pub fn stc(&mut self) {
        self.set_cf(true);
    }

    pub fn cli(&mut self) {
        self.set_if(false);
    }

    pub fn sti(&mut self) {
        self.set_if(true);
    }

    pub fn iret(&mut self, memory: &Memory) {
        let ip = self.pop(memory);
        let cs = self.pop(memory);
        let flags = self.pop(memory);

        self.set_ip(ip);
        self.set_cs(cs);
        self.set_flags(flags);
    }

    pub fn get_ax(&self) -> u16 {
        self.register_file.get(AX)
    }

    pub fn get_al(&self) -> u8 {
        self.register_file.get_lo(AX)
    }

    pub fn get_ah(&self) -> u8 {
        self.register_file.get_hi(AX)
    }

    pub fn get_cx(&self) -> u16 {
        self.register_file.get(CX)
    }

    pub fn get_dx(&self) -> u16 {
        self.register_file.get(DX)
    }

    pub fn get_dl(&self) -> u8 {
        self.register_file.get_lo(DX)
    }

    pub fn get_dh(&self) -> u8 {
        self.register_file.get_hi(DX)
    }

    pub fn get_bx(&self) -> u16 {
        self.register_file.get(BX)
    }

    pub fn get_cxdx(&self) -> u32 {
        let cx = self.get_cx() as u32;
        let dx = self.get_dx() as u32;
        (cx << 16) + dx
    }

    pub fn get_sp(&self) -> u16 {
        self.register_file.get(SP)
    }

    pub fn get_bp(&self) -> u16 {
        self.register_file.get(BP)
    }

    pub fn get_di(&self) -> u16 {
        self.register_file.get(DI)
    }

    pub fn get_si(&self) -> u16 {
        self.register_file.get(SI)
    }

    pub fn get_es(&self) -> u16 {
        self.register_file.get(ES)
    }

    pub fn get_cs(&self) -> u16 {
        self.register_file.get(CS)
    }

    pub fn get_ss(&self) -> u16 {
        self.register_file.get(SS)
    }

    pub fn get_ds(&self) -> u16 {
        self.register_file.get(DS)
    }

    pub fn get_dssi(&self) -> Address {
        let ds = self.get_ds();
        let si = self.get_si();
        addr(ds, si)
    }

    pub fn get_ip(&self) -> u16 {
        self.register_file.get(IP)
    }

    pub fn get_flags(&self) -> u16 {
        self.register_file.get_flags()
    }

    fn get_cf(&self) -> bool {
        self.register_file.get_cf()
    }

    fn get_pf(&self) -> bool {
        self.register_file.get_pf()
    }

    fn get_af(&self) -> bool {
        self.register_file.get_af()
    }

    fn get_zf(&self) -> bool {
        self.register_file.get_zf()
    }

    fn get_sf(&self) -> bool {
        self.register_file.get_sf()
    }

    fn get_df(&self) -> bool {
        self.register_file.get_df()
    }

    fn get_of(&self) -> bool {
        self.register_file.get_of()
    }

    fn set_cf(&mut self, cond: bool) {
        self.register_file.set_cf(cond);
    }

    pub fn set_pf(&mut self, cond: bool) {
        self.register_file.set_pf(cond);
    }

    fn set_af(&mut self, cond: bool) {
        self.register_file.set_af(cond);
    }

    fn set_zf(&mut self, cond: bool) {
        self.register_file.set_zf(cond);
    }

    fn set_sf(&mut self, cond: bool) {
        self.register_file.set_sf(cond);
    }

    pub fn set_if(&mut self, cond: bool) {
        self.register_file.set_if(cond);
    }

    fn set_tf(&mut self, cond: bool) {
        self.register_file.set_tf(cond);
    }

    fn set_df(&mut self, cond: bool) {
        self.register_file.set_df(cond);
    }

    fn set_of(&mut self, cond: bool) {
        self.register_file.set_of(cond);
    }

    fn mem_read8(&self, memory: &Memory, seg: u16, ofs: u16) -> u8 {
        let v = memory.read_u8(addr(seg, ofs));

        if self.logging {
            // let csip = self.get_instruction_address();
            //     if seg == DUNE_SEG001 && (0x4948..0x4948 + 792).contains(&ofs) {
            //         println!("\nTABLAT[{}] = {:04x}\n", ofs - 0x4948, v);
            //     }
            if self.globdata_log && seg == DUNE_SEG001 && (0x4c60..0x8b3b).contains(&ofs) {
                // println!(
                //     "LOG {csip} READ BYTE GLOBDATA[{}] = {:02x}",
                //     ofs - 0x4c60,
                //     v
                // );
                println!("{},{:02x}", ofs - 0x4c60, v);
                // self.dump_globdata_buffer_as_ppm(memory);
            }
        }

        v
    }

    fn mem_read16(&self, memory: &Memory, seg: u16, ofs: u16) -> u16 {
        memory.read_u16(addr(seg, ofs))
        // let lo = memory.read_u8(addr(seg, ofs)) as u16;
        // let hi = memory.read_u8(addr(seg, ofs.wrapping_add(1))) as u16;
        // (hi << 8) + lo

        // v
    }

    fn mem_readw(&self, memory: &Memory, seg: u16, ofs: u16, w: Width) -> u16 {
        match w {
            W8 => self.mem_read8(memory, seg, ofs) as u16,
            W16 => self.mem_read16(memory, seg, ofs),
        }
    }

    fn mem_write8(&mut self, memory: &mut Memory, seg: u16, ofs: u16, v: u8) {
        memory.write_u8(addr(seg, ofs), v);
        if self.logging {
            let csip = self.get_instruction_address();

            if seg == DUNE_SEG001 && (0x4948..0x4c60).contains(&ofs) {
                // println!("LOG {csip} WRITE BYTE TABLAT[{}] = {:02x}", ofs - 0x4948, v);
            } else if self.globdata_log && seg == DUNE_SEG001 && (0x4c60..0x8b3b).contains(&ofs) {
                // if self.globdata_first {
                //     self.globdata_first = false;
                //     for ofs in 0x4c60..0x4c60 + 3290 {
                //         memory.write_u8(addr(DUNE_SEG001, ofs), 0);
                //     }
                // }

                println!("{},{:02x}", ofs - 0x4c60, v);
                println!(
                    "LOG {csip} WRITE BYTE GLOBDATA[{}] = {:02x}",
                    ofs - 0x4c60,
                    v
                );
                // self.globdata_running_sum = self.globdata_running_sum.wrapping_add(v as u64);
                // self.dump_globdata_buffer_as_ppm(memory);
            } else if seg == 0x3823 {
                // println!(
                //     "LOG {csip} WRITE BYTE FRAMEBUFFER[{:04x}:{:04x}] = {:02x}",
                //     seg, ofs, v
                // );
                // self.dump_output_as_ppm(memory);
            }
        }
    }

    fn mem_write16(&mut self, memory: &mut Memory, seg: u16, ofs: u16, v: u16) {
        memory.write_u16(addr(seg, ofs), v);
        if self.logging {
            let csip = self.get_instruction_address();

            if seg == DUNE_SEG001 && (0x4948..0x4c60).contains(&ofs) {
                // println!("LOG {csip} WRITE WORD TABLAT[{}] = {:04x}", ofs - 0x4948, v);
            } else if self.globdata_log && seg == DUNE_SEG001 && (0x4c60..0x8b3b).contains(&ofs) {
                // if self.globdata_first {
                //     self.globdata_first = false;
                //     for ofs in 0x4c60..0x4c60 + 3290 {
                //         memory.write_u8(addr(DUNE_SEG001, ofs), 0);
                //     }
                // }

                println!("{},{:02x}", ofs - 0x4c60, v & 0xff);
                println!("{},{:02x}", ofs - 0x4c60 + 1, (v >> 8));

                println!(
                    "LOG {csip} WRITE BYTE GLOBDATA[{}] = {:02x}",
                    ofs - 0x4c60,
                    v & 0xff
                );
                // self.globdata_running_sum =
                //     self.globdata_running_sum.wrapping_add((v & 0xff) as u64);

                println!(
                    "LOG {csip} WRITE BYTE GLOBDATA[{}] = {:02x}",
                    ofs - 0x4c60 + 1,
                    v >> 8
                );
                // self.dump_globdata_buffer_as_ppm(memory);
            } else if seg == 0x3823 {
                // println!(
                //     "LOG {csip} WRITE WORD FRAMEBUFFER[{:04x}:{:04x}] = {:04x}",
                //     seg, ofs, v
                // );
                // self.dump_output_as_ppm(memory);
            }
        }
    }

    // fn dump_globdata_buffer_as_ppm(&mut self, memory: &mut Memory) {
    //     const PPM_CNT: usize = 1;

    //     const PAL: &[u8; 768] = include_bytes!("../../../../PAL-1760651713.BIN");
    //     let pal = PAL.into();

    //     self.ppm_cnt2 += 1;
    //     // if (self.ppm_cnt2 % PPM_CNT) == PPM_CNT - 1 {
    //     let mut data = [0u8; 16091];
    //     memory.read_bytes(addr(DUNE_SEG001, 0x4c60), &mut data);
    //     let filename = format!("log-globdata/log-globdata-{:06}.ppm", self.ppm_index2);
    //     write_ppm(&pal, &data, 329, 10, &filename);
    //     self.ppm_index2 += 1;
    //     // if self.ppm_index2 > 3000 {
    //     //     exit(0);
    //     // }
    //     // }
    // }

    // fn dump_output_as_ppm(&mut self, memory: &mut Memory) {
    //     const PPM_CNT: usize = 100;

    //     const PAL: &[u8; 768] = include_bytes!("../../../../PAL-1760651713.BIN");
    //     let pal = PAL.into();

    //     self.ppm_cnt = (self.ppm_cnt + 1) % PPM_CNT;
    //     if self.ppm_cnt == PPM_CNT - 1 {
    //         let mut data = [0u8; 64000];
    //         memory.read_bytes(addr(0x3823, 0x0000), &mut data);
    //         let filename = format!("log-map/log-map-{:06}.ppm", self.ppm_index);
    //         write_ppm(&pal, &data, 320, 200, &filename);
    //         self.ppm_index += 1;
    //     }
    // }

    fn mem_writew(&mut self, memory: &mut Memory, seg: u16, ofs: u16, v: u16, w: Width) {
        match w {
            W8 => self.mem_write8(memory, seg, ofs, read_lo(v)),
            W16 => self.mem_write16(memory, seg, ofs, v),
        }
    }

    fn reg_readw_ax(&self, w: Width) -> u16 {
        match w {
            W8 => self.register_file.get_lo(AX) as u16,
            W16 => self.get_ax(),
        }
    }

    fn reg_writew_ax(&mut self, w: Width, v: u16) {
        match w {
            W8 => self.register_file.set_lo(AX, v as u8),
            W16 => self.register_file.set(AX, v),
        }
    }

    fn reg_read8(&self, reg_idx: u8) -> u8 {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.get_lo(AX),
            0b001 => self.register_file.get_lo(CX),
            0b010 => self.register_file.get_lo(DX),
            0b011 => self.register_file.get_lo(BX),
            0b100 => self.register_file.get_hi(AX),
            0b101 => self.register_file.get_hi(CX),
            0b110 => self.register_file.get_hi(DX),
            0b111 => self.register_file.get_hi(BX),
            _ => panic!(),
        }
    }

    fn reg_read16(&self, reg_idx: u8) -> u16 {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.get_ax(),
            0b001 => self.register_file.get(CX),
            0b010 => self.register_file.get(DX),
            0b011 => self.register_file.get(BX),
            0b100 => self.register_file.get(SP),
            0b101 => self.register_file.get(BP),
            0b110 => self.register_file.get(SI),
            0b111 => self.register_file.get(DI),
            _ => panic!(),
        }
    }

    fn reg_readw(&self, reg_idx: u8, w: Width) -> u16 {
        assert!(reg_idx < 0b1000);
        match w {
            W8 => self.reg_read8(reg_idx) as u16,
            W16 => self.reg_read16(reg_idx),
        }
    }

    fn reg_write8(&mut self, reg_idx: u8, v: u8) {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.set_lo(AX, v),
            0b001 => self.register_file.set_lo(CX, v),
            0b010 => self.register_file.set_lo(DX, v),
            0b011 => self.register_file.set_lo(BX, v),
            0b100 => self.register_file.set_hi(AX, v),
            0b101 => self.register_file.set_hi(CX, v),
            0b110 => self.register_file.set_hi(DX, v),
            0b111 => self.register_file.set_hi(BX, v),
            _ => panic!(),
        }
    }

    fn reg_write16(&mut self, reg_idx: u8, v: u16) {
        assert!(reg_idx < 0b1000);
        match reg_idx {
            0b000 => self.register_file.set(AX, v),
            0b001 => self.register_file.set(CX, v),
            0b010 => self.register_file.set(DX, v),
            0b011 => self.register_file.set(BX, v),
            0b100 => self.register_file.set(SP, v),
            0b101 => self.register_file.set(BP, v),
            0b110 => self.register_file.set(SI, v),
            0b111 => self.register_file.set(DI, v),
            _ => panic!(),
        }
    }

    fn reg_writew(&mut self, reg_idx: u8, v: u16, w: Width) {
        match w {
            W8 => self.reg_write8(reg_idx, v as u8),
            W16 => self.reg_write16(reg_idx, v),
        }
    }

    fn get_sreg_ovr(&self, sreg_def: SReg) -> SReg {
        self.sreg_ovr.unwrap_or(sreg_def)
    }

    fn read_sreg(&self, sreg: SReg) -> u16 {
        match sreg {
            SReg::ES => self.get_es(),
            SReg::CS => self.get_cs(),
            SReg::SS => self.get_ss(),
            SReg::DS => self.get_ds(),
        }
    }

    fn write_sreg(&mut self, sreg: SReg, v: u16) {
        match sreg {
            SReg::ES => self.set_es(v),
            SReg::CS => self.set_cs(v),
            SReg::SS => self.set_ss(v),
            SReg::DS => self.set_ds(v),
        };
    }

    fn read_sreg_ovr(&self, sreg_def: SReg) -> u16 {
        let sreg = self.get_sreg_ovr(sreg_def);
        self.read_sreg(sreg)
    }

    pub fn raise_nmi(&mut self) {
        self.int_nmi = true;
    }

    pub fn raise_intr(&mut self, num: u8) {
        self.int_intr = true;
        self.int_number = num;
    }

    pub fn perform_iret(&mut self, memory: &mut Memory) {
        let ip = self.pop(memory);
        let cs = self.pop(memory);
        let flags = self.pop(memory);

        self.set_ip(ip);
        self.set_cs(cs);
        self.set_flags(flags);
    }

    fn fetch8<Context: CpuContext>(&mut self, ctx: &mut Context) -> u8 {
        ctx.memory().disable_logging();

        let csip = self.register_file.get_csip();
        let v = self.mem_read8(ctx.memory(), csip.seg, csip.ofs);

        ctx.memory().enable_logging();

        self.set_ip(csip.ofs.wrapping_add(1));
        v
    }

    fn fetch16<Context: CpuContext>(&mut self, ctx: &mut Context) -> u16 {
        ctx.memory().disable_logging();

        let csip = self.register_file.get_csip();
        let v = self.mem_read16(ctx.memory(), csip.seg, csip.ofs);

        ctx.memory().enable_logging();

        self.set_ip(csip.ofs.wrapping_add(2));
        v
    }

    fn fetchw<Context: CpuContext>(&mut self, ctx: &mut Context, w: Width) -> u16 {
        match w {
            W8 => self.fetch8(ctx) as u16,
            W16 => self.fetch16(ctx),
        }
    }

    fn modrm_mem_sw<Context: CpuContext>(
        &mut self,
        ctx: &mut Context,
        modrm: u8,
        w: Width,
    ) -> ModRM {
        let mod_bits = modrm >> 6;
        let rm = modrm & 0b111;

        if mod_bits == 0b11 {
            ModRM {
                w,
                rm: RegMem::Reg(rm),
            }
        } else {
            let sreg = match rm {
                0b000 => SReg::DS,
                0b001 => SReg::DS,
                0b010 => SReg::SS,
                0b011 => SReg::SS,
                0b100 => SReg::DS,
                0b101 => SReg::DS,
                0b110 if mod_bits == 0 => SReg::DS,
                0b110 => SReg::SS,
                0b111 => SReg::DS,
                _ => unreachable!(),
            };

            let mut ofs = match rm {
                0b000 => self.get_bx().wrapping_add(self.get_si()),
                0b001 => self.get_bx().wrapping_add(self.get_di()),
                0b010 => self.get_bp().wrapping_add(self.get_si()),
                0b011 => self.get_bp().wrapping_add(self.get_di()),
                0b100 => self.get_si(),
                0b101 => self.get_di(),
                0b110 if mod_bits == 0 => self.fetch16(ctx),
                0b110 => self.get_bp(),
                0b111 => self.get_bx(),
                _ => unreachable!(),
            };
            ofs = ofs.wrapping_add(match mod_bits {
                0b01 => sext(self.fetch8(ctx)),
                0b10 => self.fetch16(ctx),
                _ => 0,
            });

            ModRM {
                w,
                rm: RegMem::Mem { sreg, ofs },
            }
        }
    }

    fn modrm_reg_sw(&self, modrm: u8, w: Width) -> ModRM {
        let reg = (modrm >> 3) & 0b111;

        ModRM {
            w,
            rm: RegMem::Reg(reg),
        }
    }

    fn read_modrm<Context: CpuContext>(&self, ctx: &mut Context, src: ModRM) -> u16 {
        match src.rm {
            RegMem::Reg(reg) => self.reg_readw(reg, src.w),
            RegMem::Mem { sreg, ofs } => {
                let seg = self.read_sreg_ovr(sreg);
                self.mem_readw(ctx.memory(), seg, ofs, src.w)
            }
        }
    }

    fn write_modrm<Context: CpuContext>(&mut self, ctx: &mut Context, dst: ModRM, v: u16) {
        match dst.rm {
            RegMem::Reg(reg) => {
                self.reg_writew(reg, v, dst.w);
            }
            RegMem::Mem { sreg, ofs } => {
                self.mem_writew(ctx.memory(), self.read_sreg_ovr(sreg), ofs, v, dst.w);
            }
        }
    }

    fn push(&mut self, memory: &mut Memory, v: u16) {
        let ss = self.get_ss();
        let sp = self.get_sp().wrapping_sub(2);

        self.set_sp(sp);
        memory.write_u16(addr(ss, sp), v);
        // self.mem_write16(memory, ss, sp, v);
    }

    fn pop(&mut self, memory: &Memory) -> u16 {
        let ss = self.get_ss();
        let sp = self.get_sp();
        let r = memory.read_u16(addr(ss, sp));
        // let r = self.mem_read16(memory, ss, sp);
        self.set_sp(sp.wrapping_add(2));
        r
    }

    fn update_flags_add8(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf8_add(res, dst, src, cf));
        self.set_pf(pf8(res));
        self.set_af(af8(res, dst, src));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(of8_add(res, dst, src));
    }

    fn update_flags_sub8(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf8_sub(res, dst, src, cf));
        self.set_pf(pf8(res));
        self.set_af(af8(res, dst, src));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(of8_sub(res, dst, src));
    }

    fn update_flags_bin8(&mut self, res: u16, _dst: u16, _src: u16) {
        self.set_cf(false);
        self.set_pf(pf8(res));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
        self.set_of(false);
    }

    fn update_flags_add16(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf16_add(res, dst, src, cf));
        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn update_flags_sub16(&mut self, res: u16, dst: u16, src: u16, cf: bool) {
        self.set_cf(cf16_sub(res, dst, src, cf));
        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src));
    }

    fn update_flags_bin16(&mut self, res: u16, _dst: u16, _src: u16) {
        self.set_cf(false);
        self.set_pf(pf16(res));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(false);
    }

    fn alu_w(&mut self, func: AluOp, a: u16, b: u16, w: Width) -> u16 {
        let c = self.get_cf();
        let mut res = match func {
            AluOp::Add => a.wrapping_add(b),
            AluOp::Or => a | b,
            AluOp::Adc => a.wrapping_add(b).wrapping_add(c as u16),
            AluOp::Sbb => a.wrapping_sub(b).wrapping_sub(c as u16),
            AluOp::And => a & b,
            AluOp::Sub => a.wrapping_sub(b),
            AluOp::Xor => a ^ b,
            AluOp::Cmp => a.wrapping_sub(b),
        };

        if w == W8 {
            res &= 0x00ff;
        }

        match w {
            W8 => {
                match func {
                    AluOp::Add => self.update_flags_add8(res, a, b, false),
                    AluOp::Or => self.update_flags_bin8(res, a, b),
                    AluOp::Adc => self.update_flags_add8(res, a, b, c),
                    AluOp::Sbb => self.update_flags_sub8(res, a, b, c),
                    AluOp::And => self.update_flags_bin8(res, a, b),
                    AluOp::Sub => self.update_flags_sub8(res, a, b, false),
                    AluOp::Xor => self.update_flags_bin8(res, a, b),
                    AluOp::Cmp => self.update_flags_sub8(res, a, b, false),
                };
            }
            W16 => {
                match func {
                    AluOp::Add => self.update_flags_add16(res, a, b, false),
                    AluOp::Or => self.update_flags_bin16(res, a, b),
                    AluOp::Adc => self.update_flags_add16(res, a, b, c),
                    AluOp::Sbb => self.update_flags_sub16(res, a, b, c),
                    AluOp::And => self.update_flags_bin16(res, a, b),
                    AluOp::Sub => self.update_flags_sub16(res, a, b, false),
                    AluOp::Xor => self.update_flags_bin16(res, a, b),
                    AluOp::Cmp => self.update_flags_sub16(res, a, b, false),
                };
            }
        }

        res
    }

    fn op_alu_r_rm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let modrm = self.fetch8(ctx);
        let w = Width::from_op(op);
        let d = Dir::from_op(op);
        let func = AluOp::from((op >> 3) & 0b111);

        let mem = self.modrm_mem_sw(ctx, modrm, w);
        let reg: ModRM = self.modrm_reg_sw(modrm, w);

        let mut a = self.read_modrm(ctx, reg);
        let mut b = self.read_modrm(ctx, mem);
        if matches!(d, Dir::RegToRM) {
            swap(&mut a, &mut b);
        }
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            match d {
                Dir::RegToRM => self.write_modrm(ctx, mem, r),
                Dir::RMToReg => self.write_modrm(ctx, reg, r),
            }
        }

        self.cycles += 3;
        if mem.is_mem() {
            match d {
                // If writing to mem
                Dir::RegToRM => {
                    if func != AluOp::Cmp {
                        self.cycles += 6
                    }
                }
                // If reading from mem
                Dir::RMToReg => self.cycles += 6,
            }
            self.cycles += 6;
        }
    }

    fn op_alu_a_imm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let func = AluOp::from((op >> 3) & 0b111);

        let a = self.reg_readw_ax(w);
        let b = self.fetchw(ctx, w);
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            self.reg_writew_ax(w, r);
        }

        self.cycles += 4;
    }

    fn op_push_sreg<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let sreg = (op >> 3) & 0b111;
        let v = self.read_sreg(SReg::from_u8(sreg));
        self.push(ctx.memory(), v);

        self.cycles += 10;
    }

    fn op_pop_sreg<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let sreg = SReg::from_u8((op >> 3) & 0b111);
        let v = self.pop(ctx.memory());
        self.write_sreg(sreg, v);

        if sreg == SReg::SS {
            self.int_delay = true;
        }

        self.cycles += 8;
    }

    fn op_daa(&mut self) {
        let mut al = self.get_ax() as u8;
        let mut af = self.get_af();
        let mut cf = self.get_cf();
        let old_al = al;

        let al_check = match af {
            true => 0x9F,
            false => 0x99,
        };

        if (old_al & 0x0f) > 9 || af {
            al = al.wrapping_add(0x06);
            af = true;
        } else {
            af = false;
        }

        if (old_al > al_check) || cf {
            al = al.wrapping_add(0x60);
            cf = true;
        } else {
            cf = false;
        }

        self.register_file.set_lo(AX, al);
        self.set_cf(cf);
        self.set_pf(pf8(al as u16));
        self.set_af(af);
        self.set_zf(zf8(al as u16));
        self.set_sf(sf8(al as u16));
    }

    fn op_das(&mut self) {
        let old_al = self.get_ax() as u8;
        let old_af = self.get_af();
        let old_cf = self.get_cf();

        let mut new_al = old_al;
        let mut new_cf = false;
        let mut new_af = false;

        let al_check = match old_af {
            true => 0x9F,
            false => 0x99,
        };

        if (old_al & 0x0f) > 9 || old_af {
            new_al = new_al.wrapping_sub(0x06);
            new_af = true;
        }

        if (old_al > al_check) || old_cf {
            new_al = new_al.wrapping_sub(0x60);
            new_cf = true;
        }

        self.register_file.set_lo(AX, new_al);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_al as u16));
        self.set_af(new_af);
        self.set_zf(zf8(new_al as u16));
        self.set_sf(sf8(new_al as u16));

        self.cycles += 4;
    }

    fn op_aaa(&mut self) {
        let old_ax = self.get_ax();
        let old_af = self.get_af();

        let mut new_ax = old_ax;
        let mut new_cf = false;
        let mut new_af = false;

        if (old_ax & 0x0f) > 0x09 || old_af {
            let ah = read_hi(old_ax).wrapping_add(1) as u16;
            let al = read_lo(old_ax).wrapping_add(6) as u16;
            new_ax = (ah << 8) + al;
            new_af = true;
            new_cf = true;
        }
        new_ax &= 0xff0f;

        self.register_file.set(AX, new_ax);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_ax));
        self.set_af(new_af);
        self.set_zf(zf8(new_ax));
        self.set_sf(sf8(new_ax));

        self.cycles += 4;
    }

    fn op_aas(&mut self) {
        let old_ax = self.get_ax();
        let old_af = self.register_file.get_af();

        let mut new_ax = old_ax;
        let mut new_cf = false;
        let mut new_af = false;

        if (old_ax & 0x0f) > 0x09 || old_af {
            let al = read_lo(old_ax).wrapping_sub(6) as u16;
            let ah = read_hi(old_ax).wrapping_sub(1) as u16;
            new_ax = (ah << 8) + al;
            new_af = true;
            new_cf = true;
        }
        new_ax &= 0xff0f;

        self.register_file.set(AX, new_ax);
        self.set_cf(new_cf);
        self.set_pf(pf8(new_ax));
        self.set_af(new_af);
        self.set_zf(zf8(new_ax));
        self.set_sf(sf8(new_ax));

        self.cycles += 4;
    }

    fn op_inc_reg(&mut self, op: u8) {
        let reg = op & 0b111;
        let dst = self.reg_read16(reg);
        let src = 1;
        let res = dst.wrapping_add(src);

        self.reg_write16(reg, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn op_dec_reg(&mut self, op: u8) {
        let reg = op & 0b111;
        let dst = self.reg_read16(reg);
        let src = 1;
        let res = dst.wrapping_sub(src);

        self.reg_write16(reg, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src))
    }

    fn op_push_reg<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let reg_idx = op & 0b111;

        // On 8086 'push ss' pushes the updated value of ss
        let ss = self.register_file.get(SS);
        let sp = self.register_file.get(SP).wrapping_sub(2);
        self.register_file.set(SP, sp);

        let v = self.reg_read16(reg_idx);

        self.mem_write16(ctx.memory(), ss, sp, v);

        self.cycles += 11;
    }

    fn op_pop_reg<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let reg_idx = op & 0b111;
        let v = self.pop(ctx.memory());

        self.reg_write16(reg_idx, v);

        self.cycles += 8;
    }

    fn op_jcc<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let cond = (op >> 1) & 0b111;
        let neg = (op & 1) != 0;

        let inc = self.fetch8(ctx) as i8;

        let mut r = match cond {
            0b000 => self.get_of(),
            0b001 => self.get_cf(),
            0b010 => self.get_zf(),
            0b011 => self.get_cf() | self.get_zf(),
            0b100 => self.get_sf(),
            0b101 => self.get_pf(),
            0b110 => self.get_sf() ^ self.get_of(),
            0b111 => self.get_sf() ^ self.get_of() | self.get_zf(),
            _ => unreachable!(),
        };

        if neg {
            r = !r;
        }

        if r {
            let ip = self.register_file.get(IP);
            let ip = ip.wrapping_add_signed(inc as i16);
            self.register_file.set(IP, ip);
        }

        self.cycles += 4;
        if r {
            self.cycles += 12;
        }
    }

    fn op_grp1_rmw_imm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);
        let func = AluOp::from((modrm >> 3) & 0b111);
        let mem = self.modrm_mem_sw(ctx, modrm, w);

        let imm = match op & 0b11 {
            0b00 => self.fetch8(ctx) as u16,
            0b01 => self.fetch16(ctx),
            _ => sext(self.fetch8(ctx)),
        };

        let a = self.read_modrm(ctx, mem);
        let b = imm;
        let r = self.alu_w(func, a, b, w);

        if func != AluOp::Cmp {
            self.write_modrm(ctx, mem, r);
        }

        self.cycles += 4;
        // If reading from mem
        if mem.is_mem() {
            self.cycles += 6;
        }
        // If writing to mem
        if func != AluOp::Cmp {
            self.cycles += 7;
        }
    }

    fn op_test_rm_r<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);

        let mem = self.modrm_mem_sw(ctx, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        let a = self.read_modrm(ctx, mem);
        let b = self.read_modrm(ctx, reg);

        self.alu_w(AluOp::And, a, b, w);
    }

    fn op_xchg_rm_r<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);

        let mem = self.modrm_mem_sw(ctx, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        let a = self.read_modrm(ctx, mem);
        let b = self.read_modrm(ctx, reg);

        self.write_modrm(ctx, reg, a);
        self.write_modrm(ctx, mem, b);
    }

    fn op_mov_rm_r<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let d = Dir::from_op(op);
        let modrm = self.fetch8(ctx);

        let mem = self.modrm_mem_sw(ctx, modrm, w);
        let reg = self.modrm_reg_sw(modrm, w);

        match d {
            Dir::RegToRM => {
                let v = self.read_modrm(ctx, reg);
                self.write_modrm(ctx, mem, v);
            }
            Dir::RMToReg => {
                let v = self.read_modrm(ctx, mem);
                self.write_modrm(ctx, reg, v);
            }
        }

        self.cycles += 2;
        if mem.is_mem() {
            // If one argument is mem
            self.cycles += 6;
            if d == Dir::RegToRM {
                // If destination is mem
                self.cycles += 1;
            }
        }
    }

    fn op_mov_rm16_sreg<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let d = Dir::from_op(op);
        let modrm = self.fetch8(ctx);
        let sreg_idx = (modrm >> 3) & 0b11;
        let sreg = SReg::from_u8(sreg_idx);

        match d {
            Dir::RegToRM => {
                let v = self.read_sreg(sreg);
                let dst = self.modrm_mem_sw(ctx, modrm, W16);
                self.write_modrm(ctx, dst, v);
            }
            Dir::RMToReg => {
                let src = self.modrm_mem_sw(ctx, modrm, W16);
                let v = self.read_modrm(ctx, src);
                self.write_sreg(sreg, v);
            }
        }
    }

    fn op_lea_r16_m16<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let src = self.modrm_mem_sw(ctx, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);
        self.write_modrm(ctx, dst, src.ofs());

        self.cycles += 2;
    }

    fn op_pop_rm16<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let dst = self.modrm_mem_sw(ctx, modrm, W16);
        let v = self.pop(ctx.memory());
        self.write_modrm(ctx, dst, v);

        self.cycles += 8;
        if dst.is_mem() {
            self.cycles += 11;
        }
    }

    fn op_xchg_ax_r(&mut self, op: u8) {
        let reg_idx = op & 0b111;
        let rv = self.reg_read16(reg_idx);
        let ax = self.reg_read16(0);

        self.reg_write16(0, rv);
        self.reg_write16(reg_idx, ax);
    }

    fn op_cbw(&mut self) {
        let ax = self.get_ax() as u8;
        self.set_ax(sext(ax));
    }

    fn op_cwd(&mut self) {
        let ax = self.get_ax();
        let dx = if ax & 0x8000 != 0 { 0xffff } else { 0x0000 };
        self.set_dx(dx);
    }

    fn op_call_far<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let ofs = self.fetch16(ctx);
        let seg = self.fetch16(ctx);

        self.push(ctx.memory(), self.get_cs());
        self.push(ctx.memory(), self.get_ip());

        self.set_cs(seg);
        self.set_ip(ofs);

        self.cycles = 28;
    }

    fn op_wait(&self) {
        unimplemented!();
    }

    fn op_pushf<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let flags = self.get_flags();
        self.push(ctx.memory(), flags);
    }

    fn op_popf<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let flags = self.pop(ctx.memory());

        self.set_flags(flags);
    }

    fn op_sahf(&mut self) {
        let ah = read_hi(self.get_ax()) as u16;
        self.set_sf((ah & FLAG_SF) != 0);
        self.set_zf((ah & FLAG_ZF) != 0);
        self.set_af((ah & FLAG_AF) != 0);
        self.set_pf((ah & FLAG_PF) != 0);
        self.set_cf((ah & FLAG_CF) != 0);
    }

    fn op_lahf(&mut self) {
        let flags = read_lo(self.get_flags()) as u16;
        let ax = (flags << 8) | (self.get_al() as u16);
        self.set_ax(ax);
    }

    fn op_mov_a_m<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);

        let seg = self.read_sreg_ovr(SReg::DS);
        let ofs = self.fetch16(ctx);

        if op & 2 == 0 {
            let v = self.mem_readw(ctx.memory(), seg, ofs, w);
            self.reg_writew_ax(w, v);
        } else {
            let v = self.reg_readw_ax(w);
            self.mem_writew(ctx.memory(), seg, ofs, v, w);
        }

        self.cycles += 10;
    }

    #[inline]
    fn strop_delta(&mut self, w: Width) -> i16 {
        let delta = if !self.get_df() { 1 } else { -1 };

        match w {
            W8 => delta,
            W16 => 2 * delta,
        }
    }

    #[inline]
    fn op_strop<Context: CpuContext>(&mut self, ctx: &mut Context, strop: StrOp, op: u8) {
        let w = Width::from_op(op);
        let delta = self.strop_delta(w);
        let src_sreg = self.get_sreg_ovr(SReg::DS);

        let src_seg = self.read_sreg(src_sreg);
        let dst_seg = self.get_es();

        loop {
            if self.rep_mode != RepMode::None {
                let cx = self.get_cx();

                if cx == 0 {
                    break;
                }

                // TODO: Check interrupt pending

                self.set_cx(cx.wrapping_sub(1));
            }

            let dst_ofs = self.get_di();
            let src_ofs = self.get_si();

            match strop {
                StrOp::Movs => {
                    let v = self.mem_readw(ctx.memory(), src_seg, src_ofs, w);
                    self.mem_writew(ctx.memory(), dst_seg, dst_ofs, v, w);

                    self.set_si(src_ofs.wrapping_add_signed(delta));
                    self.set_di(dst_ofs.wrapping_add_signed(delta));
                }
                StrOp::Cmps => {
                    let a = self.mem_readw(ctx.memory(), dst_seg, dst_ofs, w);
                    let b = self.mem_readw(ctx.memory(), src_seg, src_ofs, w);

                    self.alu_w(AluOp::Cmp, b, a, w);

                    self.set_si(src_ofs.wrapping_add_signed(delta));
                    self.set_di(dst_ofs.wrapping_add_signed(delta));
                }
                StrOp::Scas => {
                    let a = self.reg_readw_ax(w);
                    let b = self.mem_readw(ctx.memory(), dst_seg, dst_ofs, w);

                    self.alu_w(AluOp::Cmp, a, b, w);

                    self.set_di(dst_ofs.wrapping_add_signed(delta));
                }
                StrOp::Lods => {
                    let v = self.mem_readw(ctx.memory(), src_seg, src_ofs, w);
                    self.reg_writew_ax(w, v);

                    self.set_si(src_ofs.wrapping_add_signed(delta));
                }
                StrOp::Stos => {
                    let v = self.reg_readw_ax(w);
                    self.mem_writew(ctx.memory(), dst_seg, dst_ofs, v, w);

                    self.set_di(dst_ofs.wrapping_add_signed(delta));
                }
            }

            if strop == StrOp::Cmps || strop == StrOp::Scas {
                if self.rep_mode == RepMode::Rep && !self.get_zf() {
                    break;
                }
                if self.rep_mode == RepMode::Repne && self.get_zf() {
                    break;
                }
            }
            if self.rep_mode == RepMode::None {
                break;
            }
        }
    }

    fn op_movs<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        self.op_strop(ctx, StrOp::Movs, op);
    }

    fn op_cmps<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        self.op_strop(ctx, StrOp::Cmps, op);
    }

    fn op_test_a_imm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let imm = self.fetchw(ctx, w);
        let a = self.reg_readw_ax(w);

        self.alu_w(AluOp::And, a, imm, w);
    }

    fn op_stos<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        self.op_strop(ctx, StrOp::Stos, op);
    }

    fn op_lods<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        self.op_strop(ctx, StrOp::Lods, op);
    }

    fn op_scas<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        self.op_strop(ctx, StrOp::Scas, op);
    }

    fn op_mov_reg_imm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let reg_idx = op & 0b111;
        let w = if (op & 0b1000) == 0 { W8 } else { W16 };
        let imm = self.fetchw(ctx, w);
        self.reg_writew(reg_idx, imm, w);
    }

    fn op_ret_imm16_intraseg<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let imm = self.fetch16(ctx);

        let ip = self.pop(ctx.memory());
        self.set_ip(ip);

        let sp = self.get_sp().wrapping_add(imm);
        self.set_sp(sp);
    }

    fn op_ret_intraseg<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let ip = self.pop(ctx.memory());
        self.set_ip(ip);
    }

    fn op_les_r16_m16<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let src = self.modrm_mem_sw(ctx, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);

        assert!(src.is_mem());

        let ofs = self.read_modrm(ctx, src);
        let seg = self.read_modrm(ctx, src.wrapping_add(2));

        self.write_modrm(ctx, dst, ofs);
        self.set_es(seg);

        self.cycles += 16;
    }

    fn op_lds_r16_m16<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let src = self.modrm_mem_sw(ctx, modrm, W16);
        let dst = self.modrm_reg_sw(modrm, W16);

        assert!(src.is_mem());

        let ofs = self.read_modrm(ctx, src);
        let seg = self.read_modrm(ctx, src.wrapping_add(2));

        self.write_modrm(ctx, dst, ofs);
        self.set_ds(seg);

        self.cycles += 16;
    }

    fn op_mov_m_imm<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);
        let dst = self.modrm_mem_sw(ctx, modrm, w);
        let imm = self.fetchw(ctx, w);

        self.write_modrm(ctx, dst, imm);

        self.cycles += 10;
    }

    fn op_ret_imm16_interseg<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let ip = self.pop(ctx.memory());
        let cs = self.pop(ctx.memory());
        let sp_delta = self.fetch16(ctx);
        let sp = self.get_sp();

        self.set_cs(cs);
        self.set_ip(ip);
        self.set_sp(sp.wrapping_add(sp_delta));
    }

    fn op_ret_interseg<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let ip = self.pop(ctx.memory());
        let cs = self.pop(ctx.memory());

        self.set_cs(cs);
        self.set_ip(ip);
    }

    fn call_int<Context: CpuContext>(&mut self, ctx: &mut Context, num: u8) {
        let num = num as u16;
        let int_ip = self.mem_read16(ctx.memory(), 0, 4 * num);
        let int_cs = self.mem_read16(ctx.memory(), 0, 4 * num + 2);

        // println!("call_int {num} {}", addr(int_cs, int_ip));

        if addr(int_cs, int_ip).ea() == 0 {
            println!("No handler for interrupt {num}");
            todo!();
        }

        let flags = self.get_flags();
        let cs = self.get_cs();
        let ip = self.get_ip();

        self.push(ctx.memory(), flags);
        self.push(ctx.memory(), cs);
        self.push(ctx.memory(), ip);

        self.set_if(false);
        self.set_tf(false);

        self.set_cs(int_cs);
        self.set_ip(int_ip);
    }

    fn op_int_3<Context: CpuContext>(&mut self, ctx: &mut Context) {
        self.call_int(ctx, 3);

        self.cycles += 52;
    }

    fn op_int_imm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let imm = self.fetch8(ctx);
        self.call_int(ctx, imm);

        self.cycles += 51;
    }

    fn op_into<Context: CpuContext>(&mut self, ctx: &mut Context) {
        self.cycles += 4;
        if self.get_of() {
            self.cycles += 49;
            self.call_int(ctx, 4);
        }
    }

    fn op_iret<Context: CpuContext>(&mut self, ctx: &mut Context) {
        self.perform_iret(ctx.memory());
    }

    fn op_grp2_rmw<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);
        let func = (modrm >> 3) & 0b111;
        let dst = self.modrm_mem_sw(ctx, modrm, w);

        let count = if op & 2 == 0 {
            1
        } else {
            read_lo(self.get_cx())
        };

        if count == 0 {
            return;
        }

        let src = self.read_modrm(ctx, dst);
        let mut cf = self.get_cf();

        match w {
            W8 => {
                let res;
                let src = src as u8;
                match func {
                    0b000 => (res, cf) = src.rol(count, cf),
                    0b001 => (res, cf) = src.ror(count, cf),
                    0b010 => (res, cf) = src.rcl(count, cf),
                    0b011 => (res, cf) = src.rcr(count, cf),
                    0b100 => (res, cf) = src.shl(count, cf),
                    0b101 => (res, cf) = src.shr(count, cf),
                    0b110 => (res, cf) = src.setmo(count, cf),
                    0b111 => (res, cf) = src.sar(count, cf),
                    _ => unreachable!(),
                }

                let of = if func == 0b110 {
                    false
                } else {
                    res.msb() != src.msb()
                };
                let res = res as u16;

                self.write_modrm(ctx, dst, res);
                self.set_of(of);
                self.set_cf(cf);
                if func >= 0b100 {
                    self.set_zf(zf8(res));
                    self.set_sf(sf8(res));
                    self.set_pf(pf8(res));
                }
            }
            W16 => {
                let res;
                match func {
                    0b000 => (res, cf) = src.rol(count, cf),
                    0b001 => (res, cf) = src.ror(count, cf),
                    0b010 => (res, cf) = src.rcl(count, cf),
                    0b011 => (res, cf) = src.rcr(count, cf),
                    0b100 => (res, cf) = src.shl(count, cf),
                    0b101 => (res, cf) = src.shr(count, cf),
                    0b110 => (res, cf) = src.setmo(count, cf),
                    0b111 => (res, cf) = src.sar(count, cf),
                    _ => unreachable!(),
                }

                let of = if func == 0b110 {
                    false
                } else {
                    res.msb() != src.msb()
                };

                self.write_modrm(ctx, dst, res);
                self.set_of(of);
                self.set_cf(cf);
                if func >= 0b100 {
                    self.set_zf(zf16(res));
                    self.set_sf(sf16(res));
                    self.set_pf(pf16(res));
                }
            }
        }
    }

    fn op_aam<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let imm = self.fetch8(ctx);

        if imm == 0 {
            self.call_int(ctx, 0);
            self.set_pf(pf8(0));
            self.set_zf(zf8(0));
            self.set_sf(sf8(0));
            return;
        }

        let tmp_al = self.get_al();
        let ah = (tmp_al / imm) as u16;
        let al = (tmp_al % imm) as u16;

        self.set_ax((ah << 8) + al);
        self.set_pf(pf8(al));
        self.set_zf(zf8(al));
        self.set_sf(sf8(al));

        self.cycles += 83;
    }

    fn op_aad<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let imm = self.fetch8(ctx);

        let al = self.get_al();
        let ah = self.get_ah();
        let res = ah.wrapping_mul(imm).wrapping_add(al) as u16;

        self.set_ax(res);
        self.set_af(false);
        self.set_cf(false);
        self.set_of(false);
        self.set_pf(pf8(res));
        self.set_zf(zf8(res));
        self.set_sf(sf8(res));
    }

    fn op_salc(&mut self) {
        if self.get_cf() {
            self.set_al(0xff);
        } else {
            self.set_al(0);
        }
    }

    fn op_xlat<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let seg = self.read_sreg_ovr(SReg::DS);
        let ofs = self.get_bx().wrapping_add(self.get_al() as u16);
        let al = self.mem_read8(ctx.memory(), seg, ofs);
        self.set_al(al);
    }

    fn op_esc<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        _ = self.read_modrm(ctx, mem);
    }

    fn op_loopnz<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch8(ctx) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 && !self.get_zf() {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_loopz<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch8(ctx) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 && self.get_zf() {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_loop<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch8(ctx) as i8;
        let cx = self.get_cx().wrapping_sub(1);
        self.set_cx(cx);

        if cx != 0 {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_jcxz<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch8(ctx) as i8;
        let cx = self.get_cx();

        if cx == 0 {
            let ip = self.get_ip().wrapping_add_signed(inc as i16);
            self.set_ip(ip);
        }
    }

    fn op_in_al_imm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.fetch8(ctx) as u16;
        let v = ctx.io_read_u8(port);
        self.set_al(v);
    }

    fn op_in_ax_imm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.fetch8(ctx) as u16;
        let v = ctx.io_read_u16(port);
        self.set_ax(v);
    }

    fn op_out_al_imm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.fetch8(ctx) as u16;
        let v = self.get_al();
        ctx.io_write_u8(port, v);
    }

    fn op_out_ax_imm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.fetch8(ctx) as u16;
        let v = self.get_ax();
        ctx.io_write_u16(port, v);
    }

    fn op_call_near<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch16(ctx);
        let ip = self.get_ip();
        self.push(ctx.memory(), ip);

        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_jmp_near<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = self.fetch16(ctx);
        let ip = self.get_ip();
        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_jmp_far<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let ofs = self.fetch16(ctx);
        let seg = self.fetch16(ctx);

        self.set_cs(seg);
        self.set_ip(ofs);
    }

    fn op_jmp_short<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let inc = sext(self.fetch8(ctx));
        let ip = self.get_ip();
        let ip = ip.wrapping_add(inc);
        self.set_ip(ip);
    }

    fn op_in_al_dx<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.get_dx();
        let v = ctx.io_read_u8(port);
        self.set_al(v);
    }

    fn op_in_ax_dx<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.get_dx();
        let v = ctx.io_read_u16(port);
        self.set_ax(v);
    }

    fn op_out_al_dx<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.get_dx();
        let v = self.get_al();
        ctx.io_write_u8(port, v);
    }

    fn op_out_ax_dx<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let port = self.get_dx();
        let v = self.get_ax();
        ctx.io_write_u16(port, v);
    }

    fn op_lock_prefix(&self) {
        unimplemented!();
    }

    fn op_unused(&self) {
        unimplemented!();
    }

    fn op_repne(&mut self) {
        self.rep_mode = RepMode::Repne;
        self.int_delay = true;
    }

    fn op_rep(&mut self) {
        self.rep_mode = RepMode::Rep;
        self.int_delay = true;
    }

    fn op_hlt(&self) {
        unimplemented!();
    }

    fn op_cmc(&mut self) {
        let cf = self.get_cf();
        self.set_cf(!cf);
    }

    fn op_grp3_rmw<Context: CpuContext>(&mut self, ctx: &mut Context, op: u8) {
        let w = Width::from_op(op);
        let modrm = self.fetch8(ctx);
        let func = (modrm >> 3) & 0b111;
        let mem = self.modrm_mem_sw(ctx, modrm, w);

        match func {
            0b000 | 0b001 => {
                // test
                let a = self.read_modrm(ctx, mem);
                let b = self.fetchw(ctx, w);
                self.alu_w(AluOp::And, a, b, w);
            }
            0b010 => {
                // not
                let v = self.read_modrm(ctx, mem);
                self.write_modrm(ctx, mem, !v);
            }
            0b011 => {
                // neg
                let v = self.read_modrm(ctx, mem);
                let res = self.alu_w(AluOp::Sub, 0, v, w);
                self.write_modrm(ctx, mem, res);
            }
            0b100 => {
                // mul
                let b = self.read_modrm(ctx, mem);
                match w {
                    W8 => {
                        let ax = (self.get_al() as u16) * b;
                        self.set_ax(ax);
                        let hi_set = read_hi(ax) != 0;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                    W16 => {
                        let dxax = (self.get_ax() as u32) * (b as u32);
                        let dx = (dxax >> 16) as u16;
                        let ax = dxax as u16;
                        self.set_dx(dx);
                        self.set_ax(ax);
                        let hi_set = dx != 0;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                }
            }
            0b101 => {
                // imul
                let b = self.read_modrm(ctx, mem);
                match w {
                    W8 => {
                        let a = (self.get_al() as i8) as i16;
                        let b = ((b as u8) as i8) as i16;
                        let xp = a.wrapping_mul(b);
                        let ax = xp as u16;

                        self.set_ax(ax);

                        let hi_set = sext(read_lo(ax)) != ax;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                    W16 => {
                        let a = (self.get_ax() as i16) as i32;
                        let b = b as i16;
                        let dxax = a.wrapping_mul(b as i32) as u32;

                        let dx = (dxax >> 16) as u16;
                        let ax = dxax as u16;
                        self.set_dx(dx);
                        self.set_ax(ax);

                        let hi_set = sext16(ax) != dxax;
                        self.set_of(hi_set);
                        self.set_cf(hi_set);
                    }
                }
            }
            0b110 => {
                // div
                let src = self.read_modrm(ctx, mem);
                match w {
                    W8 => {
                        let ax = self.get_ax();
                        let ah = read_hi(ax);

                        // Check for overflow or divide-by-zero
                        if ah as u16 >= src {
                            self.set_sf(sf8(ah as u16));
                            self.set_zf(zf8(ah as u16));
                            self.set_pf(pf8(ah as u16));
                            self.set_cf(false);
                            self.set_af(false);
                            self.set_of(false);
                            self.call_int(ctx, 0);
                            return;
                        }

                        let tmp = ax / src;
                        self.set_al(tmp as u8);
                        self.set_ah((ax % src) as u8);
                    }
                    W16 => {
                        let dx = self.get_dx();
                        let ax = self.get_ax();
                        let dxax = ((dx as u32) << 16) + (ax as u32);

                        // Check for overflow or divide-by-zero
                        if dx >= src {
                            self.set_sf(sf8(ax));
                            self.set_zf(zf8(ax));
                            self.set_pf(pf8(ax));
                            self.set_cf(false);
                            self.set_af(false);
                            self.set_of(false);
                            self.call_int(ctx, 0);
                            return;
                        }

                        let tmp = dxax / (src as u32);
                        self.set_ax(tmp as u16);
                        self.set_dx((dxax % (src as u32)) as u16);
                    }
                }
            }
            0b111 => {
                // idiv
                match w {
                    W8 => {
                        let tmp_ac = self.get_ax() as i16;
                        let tmp_b = self.read_modrm(ctx, mem) as i8;

                        if tmp_b == 0 {
                            self.call_int(ctx, 0);
                            return;
                        }

                        let mut quot = tmp_ac / (tmp_b as i16);
                        let rem = tmp_ac % (tmp_b as i16);

                        if self.rep_mode != RepMode::None {
                            quot *= -1;
                        }

                        if quot <= (i8::MIN as i16) || quot > (i8::MAX as i16) {
                            self.call_int(ctx, 0);
                            return;
                        }

                        self.set_al(quot as u8);
                        self.set_ah(rem as u8);
                    }
                    W16 => {
                        let dx = self.get_dx();
                        let ax = self.get_ax();
                        let dxax = ((dx as u32) << 16) + (ax as u32);

                        let tmp_ac = dxax as i32;
                        let tmp_b = self.read_modrm(ctx, mem) as i16 as i32;

                        if tmp_b == 0 {
                            self.call_int(ctx, 0);
                            return;
                        }

                        let mut quot = tmp_ac / tmp_b;
                        let rem = tmp_ac % tmp_b;

                        if self.rep_mode != RepMode::None {
                            quot *= -1;
                        }

                        if quot <= (i16::MIN as i32) || quot > (i16::MAX as i32) {
                            self.call_int(ctx, 0);
                            return;
                        }

                        self.set_ax(quot as u16);
                        self.set_dx(rem as u16);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn op_clc(&mut self) {
        self.set_cf(false);
        self.cycles += 2;
    }

    fn op_stc(&mut self) {
        self.set_cf(true);
        self.cycles += 2;
    }

    fn op_cli(&mut self) {
        self.set_if(false);
        self.cycles += 2;
    }

    fn op_sti(&mut self) {
        self.set_if(true);
        self.int_delay = true;
        self.cycles += 2;
    }

    fn op_cld(&mut self) {
        self.set_df(false);
        self.cycles += 2;
    }

    fn op_std(&mut self) {
        self.set_df(true);
        self.cycles += 2;
    }

    fn op_grp4_rm8<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let subop = (modrm >> 3) & 0b111;

        if modrm == 0x38 {
            if let Some(&callback) = self.callbacks.get(&self.instruction_address) {
                self.state = State::Callback((callback, self.instruction_address));
                return;
            }
        }

        match subop {
            0b000 => {
                let mem = self.modrm_mem_sw(ctx, modrm, W8);
                let dst = self.read_modrm(ctx, mem);
                let src = 1;
                let res = dst.wrapping_add(src);

                self.write_modrm(ctx, mem, res);
                self.set_pf(pf8(res));
                self.set_af(af8(res, dst, src));
                self.set_zf(zf8(res));
                self.set_sf(sf8(res));
                self.set_of(of8_add(res, dst, src));

                self.cycles += 3;
                if mem.is_mem() {
                    self.cycles += 12;
                }
            }
            0b001 => {
                let mem = self.modrm_mem_sw(ctx, modrm, W8);
                let dst = self.read_modrm(ctx, mem);
                let src = 1;
                let res = dst.wrapping_sub(src);

                self.write_modrm(ctx, mem, res);
                self.set_pf(pf8(res));
                self.set_af(af8(res, dst, src));
                self.set_zf(zf8(res));
                self.set_sf(sf8(res));
                self.set_of(of8_sub(res, dst, src));

                self.cycles += 3;
                if mem.is_mem() {
                    self.cycles += 12;
                }
            }
            _ => unreachable!(),
        }
    }

    fn op_grp5<Context: CpuContext>(&mut self, ctx: &mut Context) {
        let modrm = self.fetch8(ctx);
        let subop = (modrm >> 3) & 0b111;
        match subop {
            0b000 => self.op_grp5_inc_rm16(ctx, modrm),
            0b001 => self.op_grp5_dec_rm16(ctx, modrm),
            0b010 => self.op_grp5_call_rm16(ctx, modrm),
            0b011 => self.op_grp5_call_far(ctx, modrm),
            0b100 => self.op_grp5_jmp_rm16(ctx, modrm),
            0b101 => self.op_grp5_jmp_far(ctx, modrm),
            0b110 | 0b111 => self.op_grp5_push_rm16(ctx, modrm),
            _ => unreachable!(),
        }
    }

    fn op_grp5_inc_rm16<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let dst = self.read_modrm(ctx, mem);
        let src = 1;
        let res = dst.wrapping_add(src);

        self.write_modrm(ctx, mem, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_add(res, dst, src));
    }

    fn op_grp5_dec_rm16<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let dst = self.read_modrm(ctx, mem);
        let src = 1;
        let res = dst.wrapping_sub(src);

        self.write_modrm(ctx, mem, res);

        self.set_pf(pf16(res));
        self.set_af(af16(res, dst, src));
        self.set_zf(zf16(res));
        self.set_sf(sf16(res));
        self.set_of(of16_sub(res, dst, src));
    }

    fn op_grp5_call_rm16<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let ofs = self.read_modrm(ctx, mem);

        self.push(ctx.memory(), self.get_ip());
        self.set_ip(ofs);
        self.cycles += 16;
        if mem.is_mem() {
            self.cycles += 5;
        }
    }

    fn op_grp5_call_far<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let ofs = self.read_modrm(ctx, mem);
        let seg = self.read_modrm(ctx, mem.wrapping_add(2));

        self.push(ctx.memory(), self.get_cs());
        self.push(ctx.memory(), self.get_ip());

        self.set_cs(seg);
        self.set_ip(ofs);

        self.cycles += 37;
    }

    fn op_grp5_jmp_rm16<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let ofs = self.read_modrm(ctx, mem);

        self.set_ip(ofs);
    }

    fn op_grp5_jmp_far<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let ofs = self.read_modrm(ctx, mem);
        let seg = self.read_modrm(ctx, mem.wrapping_add(2));

        self.set_cs(seg);
        self.set_ip(ofs);
    }

    fn op_grp5_push_rm16<Context: CpuContext>(&mut self, ctx: &mut Context, modrm: u8) {
        let ss = self.register_file.get(SS);
        let sp = self.register_file.get(SP).wrapping_sub(2);
        self.register_file.set(SP, sp);

        let mem = self.modrm_mem_sw(ctx, modrm, W16);
        let v = self.read_modrm(ctx, mem);

        self.mem_write16(ctx.memory(), ss, sp, v);

        self.cycles += 11;
        if mem.is_mem() {
            self.cycles += 7;
        }
    }
}

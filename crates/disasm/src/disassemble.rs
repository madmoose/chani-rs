use std::cell::RefCell;

use smallvec::SmallVec;

use crate::{SReg, opcode_table::ArgDir};

use super::{
    decoded_instruction::DecodedInstruction,
    opcode_table::{ArgType, GROUP_1, GROUP_2, GROUP_3, GROUP_4, GROUP_5, OPCODE_TABLE},
};

pub fn decode<I>(seg: u16, ofs: u16, iter: I) -> Option<DecodedInstruction>
where
    I: Iterator<Item = u8>,
{
    let iter = RefCell::new(iter);
    let bytes: RefCell<SmallVec<[u8; 16]>> = RefCell::new(SmallVec::new());

    let mut op;
    let mut modrm = 0;
    let mut seg_ovr: Option<SReg> = None;
    let mut flag_lock = false;
    let mut flag_f2 = false;
    let mut flag_f3 = false;

    let fetch = || -> u8 {
        let b = iter.borrow_mut().next().unwrap();
        bytes.borrow_mut().push(b);
        b
    };

    let fetch16 = || -> u16 {
        let b0 = fetch();
        let b1 = fetch();
        u16::from_le_bytes([b0, b1])
    };
    let fetch32 = || -> u32 {
        let b0 = fetch();
        let b1 = fetch();
        let b2 = fetch();
        let b3 = fetch();
        u32::from_le_bytes([b0, b1, b2, b3])
    };

    op = fetch();

    loop {
        match op {
            0x26 => seg_ovr = Some(SReg::ES),
            0x2e => seg_ovr = Some(SReg::CS),
            0x36 => seg_ovr = Some(SReg::SS),
            0x3e => seg_ovr = Some(SReg::CS),
            0xf0 => flag_lock = !flag_lock,
            0xf2 => flag_f2 = true,
            0xf3 => flag_f3 = true,
            _ => break,
        }
        op = fetch();
    }

    let mut inst = OPCODE_TABLE[op as usize].clone()?;

    if inst.needs_modrm() {
        modrm = fetch();
    }
    let reg = (modrm >> 3) & 0b111;

    let group_opt = match op {
        0x80..=0x83 => Some(&GROUP_1),
        0xc0..=0xc1 => Some(&GROUP_2),
        0xd0..=0xd3 => Some(&GROUP_2),
        0xf6..=0xf7 => Some(&GROUP_3),
        0xfe => Some(&GROUP_4),
        0xff => Some(&GROUP_5),
        _ => None,
    };

    if let Some(group) = group_opt {
        let mut group_inst = group[reg as usize].to_owned();
        if let Some(group_inst) = group_inst.as_mut() {
            for i in 0..2 {
                if group_inst.arg_type[i] == ArgType::Inherit {
                    group_inst.arg_type[i] = inst.arg_type[i];
                }
                if group_inst.arg_dir[i] == ArgDir::Inherit {
                    group_inst.arg_dir[i] = inst.arg_dir[i];
                }
            }
            inst = group_inst.to_owned();
        }
    }

    let mut imm = [0u32; 2];
    #[allow(clippy::needless_range_loop)]
    for i in 0..2 {
        if inst.arg_type[i] == ArgType::None {
            break;
        }
        match inst.arg_type[i] {
            ArgType::Imm8 | ArgType::Rel8 => {
                imm[i] = fetch() as u32;
            }
            ArgType::IMem8 | ArgType::IMem16 | ArgType::Imm16 | ArgType::Rel16 => {
                imm[i] = fetch16() as u32;
            }
            ArgType::IMem32 => {
                imm[i] = fetch32();
            }
            ArgType::Mem16 | ArgType::Mem32 | ArgType::RM8 | ArgType::RM16 => {
                let r#mod = modrm >> 6;
                let rm = modrm & 0b111;
                match r#mod {
                    0b00 => {
                        if rm == 0b110 {
                            imm[i] = fetch16() as u32;
                        }
                    }
                    0b01 => {
                        imm[i] = fetch() as u32;
                    }
                    0b10 => {
                        imm[i] = fetch16() as u32;
                    }
                    _ => {}
                }
            }
            _ => {}
        };
    }

    let has_mem_arg = inst.arg_type.iter().any(|&t| is_mem_arg(t, modrm));

    Some(DecodedInstruction {
        mnemonic: inst.mnemonic,
        op_seg: seg,
        op_ofs: ofs,
        bytes: bytes.take(),
        modrm,
        arg_type: inst.arg_type,
        arg_dir: inst.arg_dir,
        seg_ovr,
        flag_lock,
        flag_f2,
        flag_f3,
        imm,
        has_mem_arg,
    })
}

pub(crate) fn is_mem_arg(arg_type: ArgType, modrm: u8) -> bool {
    match arg_type {
        ArgType::IMem8 | ArgType::IMem16 | ArgType::IMem32 | ArgType::Mem16 | ArgType::Mem32 => {
            true
        }
        ArgType::RM8 | ArgType::RM16 => modrm < 0xc0,
        _ => false,
    }
}

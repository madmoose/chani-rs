use crate::{address::addr, memory::Memory};

const PDB_OFFSET_EXIT_CALL: u16 = 0x00;
const PDB_OFFSET_BLOCK_LEN: u16 = 0x02;
const PDB_OFFSET_CPM_CALL: u16 = 0x05;
const PDB_OFFSET_EXIT: u16 = 0x0a;
const PDB_OFFSET_CTRL_C: u16 = 0x0e;
const PDB_OFFSET_FATAL_ABORT: u16 = 0x12;
const PDB_OFFSET_PARENT_PID: u16 = 0x16;
const PDB_OFFSET_JFN_TABLE: u16 = 0x18;
const PDB_OFFSET_ENVIRON: u16 = 0x2c;
const PDB_OFFSET_USER_STACK: u16 = 0x2e;
const PDB_OFFSET_JFN_LENGTH: u16 = 0x32;
const PDB_OFFSET_JFN_POINTER: u16 = 0x34;
const PDB_OFFSET_NEXT_PDB: u16 = 0x38;
const PDB_OFFSET_DOS_VER: u16 = 0x40;
const PDB_OFFSET_CALL_SYSTEM: u16 = 0x50;

pub const PDB_SIZE: u32 = 256;

struct ProgramDataBlock {
    seg: u16,
}

impl ProgramDataBlock {
    pub fn copy_from(&mut self, memory: &mut Memory, other_seg: u16) {
        memory.copy((self.seg, 0), (other_seg, 0), PDB_SIZE);
    }

    pub fn parent_pid(&self, memory: &Memory) -> u16 {
        memory.read_u16(addr(self.seg, PDB_OFFSET_PARENT_PID))
    }

    pub fn set_parent_pid(&mut self, memory: &mut Memory, parent_pid: u16) {
        memory.write_u16(addr(self.seg, PDB_OFFSET_PARENT_PID), parent_pid);
    }

    pub fn environ(&self, memory: &Memory) -> u16 {
        memory.read_u16(addr(self.seg, PDB_OFFSET_ENVIRON))
    }

    pub fn set_environ(&mut self, memory: &mut Memory, environ: u16) {
        memory.write_u16(addr(self.seg, PDB_OFFSET_ENVIRON), environ);
    }

    pub fn next_pdb(&self, memory: &Memory) -> u16 {
        memory.read_u16(addr(self.seg, PDB_OFFSET_NEXT_PDB))
    }

    pub fn set_next_pdb(&mut self, memory: &mut Memory, next_pdb: u16) {
        memory.write_u16(addr(self.seg, PDB_OFFSET_NEXT_PDB), next_pdb);
    }

    pub fn dos_ver(&self, memory: &Memory) -> u16 {
        memory.read_u16(addr(self.seg, PDB_OFFSET_DOS_VER))
    }

    pub fn set_dos_ver(&mut self, memory: &mut Memory, dos_ver: u16) {
        memory.write_u16(addr(self.seg, PDB_OFFSET_DOS_VER), dos_ver);
    }
}

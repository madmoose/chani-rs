use crate::memory::Memory;

// Returns the length of the environment segment.
// The environment segment is a series of null-terminated strings
// with a second null byte marking the end of the environment.
pub fn get_environment_length(memory: &Memory, seg: u16) -> Option<u16> {
    let mut ofs = 0;
    loop {
        let b = memory[(seg, ofs)];
        println!("b0 = {} {:#x}, ofs = {}", b as char, b, ofs);
        ofs += 1;
        if ofs == 0x8000 {
            return None;
        }
        if b != 0 {
            continue;
        }

        // Found the first zero, look for a second zero
        let b = memory[(seg, ofs)];
        println!("b1 = {} {:#x}, ofs = {}", b as char, b, ofs);
        if b == 0 {
            return Some(ofs + 1);
        }
    }
}

mod test {

    #[test]
    fn test_get_environment_length() {
        use super::*;
        use crate::address::addr;

        let mut memory = Memory::new();
        memory.write_bytes(addr(0x1000, 0), b"TEST\0ANOTHER\0\0");
        assert_eq!(get_environment_length(&memory, 0x1000), Some(14));

        // Fill the segment with non-zero values
        for ofs in 0..=0xffff {
            memory.write_u8(addr(0x2000, ofs), 0xcd);
        }
        memory.write_bytes(addr(0x2000, 0), b"TEST\0ANOTHER\0");
        assert_eq!(get_environment_length(&memory, 0x2000), None);

        // Fill the segment with non-zero values
        for ofs in 0..=0xffff {
            memory.write_u8(addr(0x3000, ofs), 0xcd);
        }
        memory.write_bytes(addr(0x3000, 0), b"\0\0");
        assert_eq!(get_environment_length(&memory, 0x3000), Some(2));
    }
}

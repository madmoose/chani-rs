use std::{collections::BTreeMap, fs::read_dir, io::Read, sync::mpsc, thread};

use chani::{
    bus::{test_bus::TestBus, Bus},
    i8088::{
        self, ea, FLAG_AF, FLAG_CF, FLAG_DF, FLAG_IF, FLAG_OF, FLAG_PF, FLAG_SF, FLAG_TF, FLAG_ZF,
    },
};
use serde::Deserialize;

// https://github.com/SingleStepTests/8088

#[derive(Debug, Deserialize)]
struct Metadata {
    opcodes: BTreeMap<String, TestOrTestGroupMetadata>,
}

#[allow(dead_code)]
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct TestMetadata {
    status: String,
    flags_mask: Option<u16>,
}

#[derive(Debug, Deserialize)]
struct TestGroupMetadata {
    reg: BTreeMap<String, TestMetadata>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum TestOrTestGroupMetadata {
    Test(TestMetadata),
    TestGroup(TestGroupMetadata),
}

#[derive(Debug)]
struct TestFile {
    name: String,
    op: u8,
    reg: Option<u8>,
    tests: Vec<Test>,
}

#[derive(Debug, Deserialize)]
struct Test {
    name: String,
    bytes: Vec<u8>,

    #[serde(rename = "initial")]
    initial_state: InitialState,

    #[serde(rename = "final")]
    expected_state: ExpectedState,
}

#[derive(Debug, Deserialize)]
struct InitialState {
    regs: InitialRegisterState,
    ram: Vec<(u32, u8)>,
}

#[derive(Debug, Deserialize)]
struct ExpectedState {
    regs: ExpectedRegisterState,
    ram: Vec<(u32, u8)>,
}

#[derive(Debug, Deserialize)]
struct InitialRegisterState {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    cs: u16,
    ss: u16,
    ds: u16,
    es: u16,
    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
    ip: u16,
    flags: u16,
}

#[derive(Debug, Deserialize)]
struct ExpectedRegisterState {
    ax: Option<u16>,
    bx: Option<u16>,
    cx: Option<u16>,
    dx: Option<u16>,
    cs: Option<u16>,
    ss: Option<u16>,
    ds: Option<u16>,
    es: Option<u16>,
    sp: Option<u16>,
    bp: Option<u16>,
    si: Option<u16>,
    di: Option<u16>,
    ip: Option<u16>,
    flags: Option<u16>,
}

const FLAG_NAMES: &str = "----ODITSZ-A-P-C";
const REG_NAMES: [&str; 13] = [
    "AX", "BX", "CX", "DX", "CS", "SS", "DS", "ES", "SP", "BP", "SI", "DI", "IP",
];

impl InitialState {
    fn dump(&self) {
        println!("Initial state:");
        print!("\tRegs:   ");
        self.dump_registers();
        println!();
        print!("\tFlags:  ");
        self.dump_flags();
        println!();
        print!("\tMemory: ");
        self.dump_memory();
        println!("\n");
    }

    fn dump_registers(&self) {
        let regs = [
            self.regs.ax,
            self.regs.bx,
            self.regs.cx,
            self.regs.dx,
            self.regs.cs,
            self.regs.ss,
            self.regs.ds,
            self.regs.es,
            self.regs.sp,
            self.regs.bp,
            self.regs.si,
            self.regs.di,
            self.regs.ip,
        ];

        REG_NAMES
            .iter()
            .zip(regs.iter())
            .enumerate()
            .for_each(|(i, (&name, &v))| {
                if i > 0 {
                    print!(" ");
                }
                print!("{}={:04X}", name, v);
            });
    }

    fn dump_flags(&self) {
        let flags = self.regs.flags;

        print!("FLAGS={:04x} ", flags);

        for n in (0..16).rev() {
            let b = ((flags >> n) & 1) != 0;
            print!(
                "{}",
                if b {
                    FLAG_NAMES.chars().nth(15 - n).unwrap()
                } else {
                    '-'
                }
            );
        }
    }

    fn dump_memory(&self) {
        self.ram.iter().for_each(|(addr, v)| {
            print!("[{:06x}]={:02X} ", addr, v);
        });
    }
}

impl ExpectedState {
    fn dump(&self) {
        println!("Expected state:");
        print!("\tRegs:     ");
        self.dump_registers();
        println!();
        print!("\tFlags:  ");
        self.dump_flags();
        println!();
        print!("\tMemory: ");
        self.dump_memory();
        println!("\n");
    }

    fn dump_registers(&self) {
        let regs = [
            self.regs.ax,
            self.regs.bx,
            self.regs.cx,
            self.regs.dx,
            self.regs.cs,
            self.regs.ss,
            self.regs.ds,
            self.regs.es,
            self.regs.sp,
            self.regs.bp,
            self.regs.si,
            self.regs.di,
            self.regs.ip,
        ];

        REG_NAMES
            .iter()
            .zip(regs.iter())
            .enumerate()
            .for_each(|(i, (&name, &v_opt))| {
                if let Some(v) = v_opt {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}={:04x}", name, v);
                }
            });
    }

    fn dump_flags(&self) {
        let Some(flags) = self.regs.flags else {
            return;
        };

        print!("FLAGS={:04x} ", flags);

        for n in (0..16).rev() {
            let b = ((flags >> n) & 1) != 0;
            print!(
                "{}",
                if b {
                    FLAG_NAMES.chars().nth(15 - n).unwrap()
                } else {
                    '-'
                }
            );
        }
    }

    fn dump_memory(&self) {
        self.ram.iter().for_each(|(addr, v)| {
            print!("[{:06x}]={:02X} ", addr, v);
        });
    }
}

fn execute_test(test: &Test, flags_mask: u16) -> bool {
    let mut ram = vec![0; 0x100000];
    let mut cpu = crate::i8088::Cpu::default();
    let mut bus = TestBus { ram: &mut ram };

    // Apply initial register state
    cpu.set_ax(test.initial_state.regs.ax);
    cpu.set_bx(test.initial_state.regs.bx);
    cpu.set_cx(test.initial_state.regs.cx);
    cpu.set_dx(test.initial_state.regs.dx);
    cpu.set_cs(test.initial_state.regs.cs);
    cpu.set_ss(test.initial_state.regs.ss);
    cpu.set_ds(test.initial_state.regs.ds);
    cpu.set_es(test.initial_state.regs.es);
    cpu.set_sp(test.initial_state.regs.sp);
    cpu.set_bp(test.initial_state.regs.bp);
    cpu.set_si(test.initial_state.regs.si);
    cpu.set_di(test.initial_state.regs.di);
    cpu.set_ip(test.initial_state.regs.ip);
    cpu.set_flags(test.initial_state.regs.flags);

    // Apply initial memory state
    for (addr, v) in test.initial_state.ram.iter().copied() {
        bus.mem_write_u8(addr, v);
    }

    // Step CPU once
    cpu.step(&mut bus);

    let mut validation_errors = Vec::new();

    #[derive(Debug)]
    enum ValidationError {
        Register {
            reg: u8,
            expected_value: u16,
            value: u16,
        },
        Flags {
            expected_value: u16,
            value: u16,
            flags_mask: u16,
        },
        Memory {
            addr: u32,
            expected_value: u8,
            value: u8,
        },
    }

    // Validate registers
    let regs = [
        (test.expected_state.regs.ax, cpu.get_ax()),
        (test.expected_state.regs.bx, cpu.get_bx()),
        (test.expected_state.regs.cx, cpu.get_cx()),
        (test.expected_state.regs.dx, cpu.get_dx()),
        (test.expected_state.regs.cs, cpu.get_cs()),
        (test.expected_state.regs.ss, cpu.get_ss()),
        (test.expected_state.regs.ds, cpu.get_ds()),
        (test.expected_state.regs.es, cpu.get_es()),
        (test.expected_state.regs.sp, cpu.get_sp()),
        (test.expected_state.regs.bp, cpu.get_bp()),
        (test.expected_state.regs.si, cpu.get_si()),
        (test.expected_state.regs.di, cpu.get_di()),
        (test.expected_state.regs.ip, cpu.get_ip()),
    ];

    for (n, (expected_value, value)) in regs.into_iter().enumerate() {
        if let Some(expected_value) = expected_value {
            if expected_value != value {
                validation_errors.push(ValidationError::Register {
                    reg: n as u8,
                    expected_value,
                    value,
                });
            }
        }
    }

    // Validate flags
    if let Some(expected_flags) = test.expected_state.regs.flags {
        let cpu_flags = cpu.get_flags();
        if (expected_flags & flags_mask) != (cpu_flags & flags_mask) {
            validation_errors.push(ValidationError::Flags {
                expected_value: expected_flags,
                value: cpu_flags,
                flags_mask,
            });
        }
    }

    // Validate memory
    let (op, reg) = extract_opcode_reg(&test.bytes);

    let test_might_have_flags_on_stack = match (op, reg) {
        (0xd4, _) => true,                                // aam
        (0xf6..=0xf7, Some(reg)) => reg == 6 || reg == 7, // div or idiv
        _ => false,
    };

    for (addr, expected_value) in test.expected_state.ram.iter().copied() {
        let value = ram[addr as usize];
        if value != expected_value {
            if test_might_have_flags_on_stack {
                let flags_addr_lo = ea(cpu.get_ss(), cpu.get_sp().wrapping_add(4));
                let flags_addr_hi = ea(cpu.get_ss(), cpu.get_sp().wrapping_add(5));

                if addr == flags_addr_lo || addr == flags_addr_hi {
                    continue;
                }
            }
            validation_errors.push(ValidationError::Memory {
                addr,
                expected_value,
                value,
            });
        }
    }

    if validation_errors.is_empty() {
        return true;
    }

    println!("Test {}:", test.name);
    println!(
        "\tBytes: {}",
        test.bytes
            .iter()
            .map(|b| format!("{b:02X}"))
            .collect::<Vec<_>>()
            .join(" ")
    );
    test.initial_state.dump();
    test.expected_state.dump();
    for e in validation_errors {
        match e {
            ValidationError::Register {
                reg,
                expected_value,
                value,
            } => {
                println!(
                    "\t{} has value {:02x}, expected {:02x}",
                    REG_NAMES[reg as usize], value, expected_value
                );
            }
            ValidationError::Flags {
                expected_value,
                value,
                flags_mask,
            } => {
                println!(
                    "\tFlags has value {:04x}, expected {:04x} (mask is {:016b})",
                    value, expected_value, flags_mask
                );
                println!();
                println!("          {}", FLAG_NAMES);
                println!("Expected: {:016b}", expected_value);
                println!("Result:   {:016b}", value);
                println!("Mask:     {:016b}", flags_mask);
                println!();
                let test_flags = expected_value & flags_mask;
                let diff_flags = test_flags ^ (value & flags_mask);

                if (diff_flags & FLAG_CF) != 0 {
                    println!("\t\tCARRY flag differs");
                }
                if (diff_flags & FLAG_PF) != 0 {
                    println!("\t\tPARITY flag differs");
                }
                if (diff_flags & FLAG_AF) != 0 {
                    println!("\t\tAUX CARRY flag differs");
                }
                if (diff_flags & FLAG_ZF) != 0 {
                    println!("\t\tZERO flag differs");
                }
                if (diff_flags & FLAG_SF) != 0 {
                    println!("\t\tSIGNED flag differs");
                }
                if (diff_flags & FLAG_TF) != 0 {
                    println!("\t\tTRAP flag differs");
                }
                if (diff_flags & FLAG_IF) != 0 {
                    println!("\t\tINTERRUPT flag differs");
                }
                if (diff_flags & FLAG_DF) != 0 {
                    println!("\t\tDIRECTION flag differs");
                }
                if (diff_flags & FLAG_OF) != 0 {
                    println!("\t\tOVERFLOW flag differs");
                }
            }
            ValidationError::Memory {
                addr,
                expected_value,
                value,
            } => {
                println!(
                    "\t[{:06x}] has value {:02x}, expected {:02x}",
                    addr, value, expected_value
                );
            }
        }
    }
    println!();

    false
}

fn extract_opcode_reg(bytes: &[u8]) -> (u8, Option<u8>) {
    const PREFIX_BYTES: [u8; 6] = [0x26, 0x2e, 0x36, 0x3e, 0xf2, 0xf3];

    let mut iter = bytes
        .iter()
        .copied()
        .skip_while(|b| PREFIX_BYTES.contains(b));

    let op = iter.next().unwrap();
    let reg = iter.next().map(|b| (b >> 3) & 0b111);

    (op, reg)
}

fn main() -> Result<(), std::io::Error> {
    let mut f = std::fs::File::open("8088-tests-v2/metadata.json").unwrap();
    let mut buf = String::new();
    f.read_to_string(&mut buf).expect("Failed to read metadata");

    let metadata: Metadata = serde_json::from_str(&buf).unwrap();

    let (sender, receiver) = mpsc::sync_channel(1);
    thread::spawn(move || {
        let mut test_files_entries: Vec<std::fs::DirEntry> = read_dir("8088-tests-v2")
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name().to_string_lossy().ends_with(".json.gz"))
            .collect();

        test_files_entries.sort_by_key(|e| e.file_name());

        for test_file_entry in test_files_entries {
            let path = test_file_entry.path();
            let Some(filename) = path.file_name().map(|osstr| osstr.to_string_lossy()) else {
                continue;
            };

            // Parse filename of the form "{op:02X}.json.gz" or "{op:02X}.{reg}.json.gz"
            let basename = filename.strip_suffix(".json.gz").unwrap();
            let (op, reg) = match basename.split_once('.') {
                Some((opcode, reg)) => (opcode, Some(reg)),
                None => (basename, None),
            };
            let op = u8::from_str_radix(op, 16).expect("Expected opcode to be a hex number");
            let reg = reg.map(|reg| reg.parse::<u8>().expect("Expected reg to be a digit"));

            let test_file = std::fs::File::open(test_file_entry.path()).unwrap();
            let mut gz_reader = flate2::read::GzDecoder::new(test_file);
            let mut buf = String::new();

            gz_reader
                .read_to_string(&mut buf)
                .expect("Failed to read test");

            let tests: Vec<Test> = serde_json::from_str(&buf).unwrap();

            sender
                .send(TestFile {
                    name: filename.to_string(),
                    op,
                    reg,
                    tests,
                })
                .unwrap();
        }
    });

    let mut tests_executed = 0;
    let mut tests_passed = 0;

    for test_file in receiver {
        println!("Executing tests {}", test_file.name);
        for test in test_file.tests {
            let op_key = format!("{:02X}", test_file.op);
            let reg_key = test_file.reg.map(|r| format!("{}", r));

            let test_metadata = match metadata.opcodes.get(&op_key).unwrap() {
                TestOrTestGroupMetadata::Test(test) => test,
                TestOrTestGroupMetadata::TestGroup(group) => {
                    group.reg.get(&reg_key.unwrap()).unwrap()
                }
            };
            let flags_mask = test_metadata.flags_mask.unwrap_or(0) as u16;

            tests_executed += 1;
            if execute_test(&test, flags_mask) {
                tests_passed += 1;
            }
        }
    }

    let tests_failed = tests_executed - tests_passed;

    println!("Passed {}/{} tests", tests_passed, tests_executed);
    if tests_failed == 0 {
        println!("No failed tests");
    } else {
        println!("Failed {} tests", tests_failed);
    }

    Ok(())
}

use std::process::exit;

use clap::Parser;

use chani_core::{address::addr, machine::Machine};

const DRIVER: &str = "DNADG";

#[derive(Parser)]
struct Args {
    song: String,
    #[arg(long)]
    dump_mem_writes: bool,
    #[arg(long)]
    dump_port_writes: bool,
    /// Log each I/O write hitting the OPL3 device in the form
    /// `OPL3: I/O write at port 0xXXX <- VV` (matches the format the
    /// reverse-engineered dune-rs play_herad driver emits).
    #[arg(long)]
    dump_opl3_writes: bool,
    #[arg(long)]
    dump_func_names: bool,
    #[arg(long)]
    dump_channels: bool,
}

fn code_label(ip: u16) {
    let labels: [(u16, &str); _] = [
        (0x0100, "ADGInit_entry"),
        (0x0103, "ADGOpen_entry"),
        (0x0106, "ADGReset_entry"),
        (0x0109, "ADGSetTickEnabled_entry"),
        (0x010c, "ADGSetDynamics_entry"),
        (0x010f, "ADGTick_entry"),
        (0x0112, "ADGSetVolume_entry"),
        (0x04ff, "ADGInit"),
        (0x0561, "ADGReset"),
        (0x05ab, "ADGSetVolume"),
        (0x05be, "ADGSetDynamics"),
        (0x0610, "ADGSetTickEnabled"),
        (0x0626, "ADGOpen"),
        (0x06f6, "ADGTick"),
        (0x1109, "ADGOplSecondaryWrite"),
        (0x1112, "ADGOplPrimaryWrite"),
        // (0x1119, "ADGOplWriteData"),
    ];

    if let Some((_, name)) = labels.iter().find(|e| e.0 == ip) {
        println!("{}", name);
    }
}

fn main() {
    let args = Args::parse();

    let driver_file = format!("./assets/{DRIVER}.BIN");
    let Ok(driver_data) = std::fs::read(&driver_file) else {
        println!("`{}` not found", driver_file);
        exit(1);
    };

    let Ok(song_data) = std::fs::read(&args.song) else {
        println!("`{}` not found", args.song);
        exit(1);
    };

    let mut machine = Machine::new();

    // Load the driver at 1000:0100
    machine
        .memory
        .write_bytes(addr(0x1000, 0x0100), &driver_data);

    // Load the song data at 2000:0000
    machine.memory.write_bytes(addr(0x2000, 0x0000), &song_data);

    // Patch out loc_004dc (HSQ file list patcher) with a `ret`.
    machine.memory.write_u8(addr(0x1000, 0x04dc), 0xc3);

    // Patch out OPL busy-wait polling loops to avoid hanging in emulation.
    // loc_01149 and loc_01158 poll the OPL status port until the chip is
    // ready; in emulation the port never clears, so we skip them entirely.
    machine.memory.write_u8(addr(0x1000, 0x1149), 0xc3);
    machine.memory.write_u8(addr(0x1000, 0x1158), 0xc3);

    machine.devices.opl3_mut().dump_writes = args.dump_opl3_writes;

    let (cpu, mut ctx) = machine.get_cpu_and_context();

    cpu.logging = false;
    cpu.dump_mem_writes = args.dump_mem_writes;
    cpu.dump_port_writes = args.dump_port_writes;

    // Init (AX = OPL port base; 0x388 = standard AdLib Gold address)
    {
        cpu.set_cs(0x1000);
        cpu.set_ip(0x0100);

        cpu.set_ax(0x0388);

        loop {
            if args.dump_func_names {
                code_label(cpu.get_ip());
            }
            cpu.step(&mut ctx);

            if cpu.get_ip() == 0x054f {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x076b {
                dump_channels(&ctx);
            }
        }
    }

    // Open (AL = play count, ES:SI = song data pointer)
    {
        cpu.set_cs(0x1000);
        cpu.set_ip(0x0103);

        cpu.set_es(0x2000);
        cpu.set_si(0x0000);

        // Play song once
        cpu.set_ax(1);

        loop {
            if args.dump_func_names {
                code_label(cpu.get_ip());
            }
            cpu.step(&mut ctx);
            if cpu.get_ip() == 0x0689 {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x076b {
                dump_channels(&ctx);
            }
        }
    }

    // Tick
    loop {
        cpu.set_cs(0x1000);
        cpu.set_ip(0x010f);

        cpu.set_es(0x2000);
        cpu.set_si(0x0000);

        loop {
            if args.dump_func_names {
                code_label(cpu.get_ip());
            }
            cpu.step(&mut ctx);
            if cpu.get_ip() == 0x072f {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x076b {
                dump_channels(&ctx);
            }
        }

        if cpu.get_al() & 0x80 == 0 {
            break;
        }
    }
}

/*
   Per-channel state layout (all arrays are [u16; 18], indexed by channel):
   - T   — ch_timer        (0x01e2 + 2*i)
   - RP  — ch_read_ptr     (0x0206 + 2*i)
   - N   — ch_note         (0x024f + 2*i, high byte of ch_program_note)
   - GC  — glide_countdown (0x0296 + 2*i, low byte of ch_glide_count_steps)
   - GS  — glide_steps     (0x0297 + 2*i, high byte of ch_glide_count_steps)
   - GA  — glide_accum     (0x02ba + 2*i, low byte of ch_glide_state)
*/
fn dump_channels(ctx: &chani_core::machine::CpuMachineContext<'_>) {
    let m = ctx.memory.read_u16(addr(0x1000, 0x128));
    let t = ctx.memory.read_u16(addr(0x1000, 0x12a));
    print!("STATE m={m:04} t={t:02}");
    for i in 0..18 {
        let timer = ctx.memory.read_u16(addr(0x1000, 0x01e2 + 2 * i));
        let n = ctx.memory.read_u8(addr(0x1000, 0x024f + 2 * i));
        let gc = ctx.memory.read_u8(addr(0x1000, 0x0296 + 2 * i));
        let gs = ctx.memory.read_u8(addr(0x1000, 0x0297 + 2 * i));
        let ga = ctx.memory.read_u8(addr(0x1000, 0x02ba + 2 * i));
        let rp = ctx.memory.read_u16(addr(0x1000, 0x0206 + 2 * i));

        print!(" | ch{i}: T={timer:04X} N={n:02X} GC={gc:02X} GS={gs:02X} GA={ga:02X} RP={rp:04X}");
    }
    println!();
}

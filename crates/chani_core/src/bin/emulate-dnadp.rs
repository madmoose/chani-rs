use std::process::exit;

use clap::Parser;

use chani_core::{address::addr, machine::Machine};

const DRIVER: &str = "DNADP";

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
        (0x0112, "ADPSetVolume_entry"),
        (0x02b5, "ADPPatchHSQFileList"),
        (0x02d8, "ADPInit"),
        (0x02fe, "ADPReset"),
        (0x030b, "ADPVolumeToAttenuation"),
        (0x0348, "ADPSetVolume"),
        (0x035b, "ADPSetDynamicsCurve"),
        (0x039c, "ADPEnablePlayback"),
        (0x03b2, "ADPOpen"),
        (0x0413, "ADPBuildChannelTable"),
        (0x0444, "ADPRewindAllChannels"),
        (0x0473, "ADPTickHandler"),
        (0x04ad, "ADPCheckSongBuffer"),
        (0x04d3, "ADPProcessTick"),
        (0x0553, "ADPLoopPointCheck"),
        (0x05aa, "ADP_OP_ProgramChange"),
        (0x062c, "ADP_OP_NoteOn"),
        (0x065b, "ADP_OP_NoteOff"),
        (0x066f, "ADP_OP_EndOfTrack"),
        (0x06a8, "ADP_OP_VolumeModulation"),
        (0x0740, "ADPApplyVelocity"),
        (0x07ea, "ADP_OP_PitchBend"),
        (0x07ef, "ADPPitchBend"),
        (0x08e1, "ADPReadWaitValue"),
        (0x091b, "ADPSilenceAllChannels"),
        (0x092d, "ADPFadeStep"),
        (0x0982, "ADPWriteMasterVolume"),
        (0x099a, "ADPMuteAllOperators"),
        (0x09ab, "ADPInstrumentWrite"),
        // (0x09c3, "ADPInstrumentWriteLoop"),
        (0x0a58, "ADPOplNoteOn"),
        (0x0a87, "ADPOplNoteOff"),
        (0x0a8f, "ADPOplFrequencyWrite"),
        (0x0aa2, "ADPOplRegisterWrite"),
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

    // Patch out ADPPatchHSQFileList with a `ret`.
    machine.memory.write_u8(addr(0x1000, 0x02b5), 0xc3);

    machine.devices.opl3_mut().dump_writes = args.dump_opl3_writes;

    let (cpu, mut ctx) = machine.get_cpu_and_context();

    cpu.logging = false;
    cpu.dump_mem_writes = args.dump_mem_writes;
    cpu.dump_port_writes = args.dump_port_writes;

    // Init
    {
        cpu.set_cs(0x1000);
        cpu.set_ip(0x0100);

        cpu.set_ax(202);
        cpu.set_cx(0);

        loop {
            if args.dump_func_names {
                code_label(cpu.get_ip());
            }
            cpu.step(&mut ctx);

            if cpu.get_ip() == 0x2fd {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x529 {
                dump_channels(&ctx);
            }
        }
    }

    // Open
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
            if cpu.get_ip() == 0x412 {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x529 {
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
            if cpu.get_ip() == 0x4ac {
                break;
            }
            if args.dump_channels && cpu.get_ip() == 0x529 {
                dump_channels(&ctx);
            }
        }

        if cpu.get_al() & 0x80 == 0 {
            break;
        }
    }
}

/*
   - T — ch_timer (word)
   - N — ch_note (byte, at [di+37h])
   - GC — ch_glide_countdown (byte, at [di+5ah])
   - GS — ch_glide_steps (byte, at [di+5bh])
   - GA — ch_glide_accum (byte, at [di+6ch])
   - RP — ch_read_ptr (word)

   STATE m=MMMM t=TT | ch0: T=TTTT N=NN GC=GG GS=SS GA=AA RP=RRRR
*/
fn dump_channels(ctx: &chani_core::machine::CpuMachineContext<'_>) {
    let m = ctx.memory.read_u16(addr(0x1000, 0x11f));
    let t = ctx.memory.read_u16(addr(0x1000, 0x121));
    print!("STATE m={m:04} t={t:02}");
    for i in 0..9 {
        let di = 0x01a2;
        let t = ctx.memory.read_u16(addr(0x1000, di + 2 * i));
        let n = ctx.memory.read_u8(addr(0x1000, di + 0x37 + 2 * i));
        let gc = ctx.memory.read_u8(addr(0x1000, di + 0x5a + 2 * i));
        let gs = ctx.memory.read_u8(addr(0x1000, di + 0x5b + 2 * i));
        let ga = ctx.memory.read_u8(addr(0x1000, di + 0x6c + 2 * i));
        let rp = ctx.memory.read_u16(addr(0x1000, di + 0x12 + 2 * i));

        print!(" | ch{i}: T={t:04X} N={n:02X} GC={gc:02X} GS={gs:02X} GA={ga:02X} RP={rp:04X}");
    }
    println!();
}

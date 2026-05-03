use std::{
    io::{self, Write},
    path::Path,
};

use chani_disasm::{
    layout::{generate_widgets, render_widgets},
    project::Project,
    seg_dataflow::SegVal,
};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let show_dataflow = args.iter().any(|a| a == "--dataflow");
    let path = args.into_iter().skip(1).find(|a| !a.starts_with('-'));

    let path = match path {
        Some(p) => p,
        None => {
            eprintln!("Usage: disasm [--dataflow] <file>");
            std::process::exit(1);
        }
    };

    let project = if Path::new(&path).extension().is_some_and(|e| e == "chani") {
        Project::from_project_file(&path)
    } else {
        Project::from_exe_file(&path)
    };

    let mut project = match project {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{path}: {e}");
            std::process::exit(1);
        }
    };

    if false {
        let mut stdout = io::BufWriter::new(io::stdout().lock());
        if let Err(e) = project.write_to(&mut stdout) {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    }

    project.analyze();

    let mut stdout = io::BufWriter::new(io::stdout().lock());

    if show_dataflow {
        if let Err(e) = print_dataflow(&project, &mut stdout)
            && e.kind() != io::ErrorKind::BrokenPipe
        {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
        return;
    }

    if let Err(e) = print_listing(&project, &mut stdout)
        && e.kind() != io::ErrorKind::BrokenPipe
    {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

fn print_listing<W: Write>(project: &Project, w: &mut W) -> io::Result<()> {
    let t0 = std::time::Instant::now();
    let (widgets, total_rows) = generate_widgets(project);
    let t1 = std::time::Instant::now();

    let mut buf = String::with_capacity(120);
    for y in 0..total_rows {
        buf.clear();
        render_widgets(&widgets, &mut buf, y);
        writeln!(w, "{buf}")?;
    }
    let t2 = std::time::Instant::now();

    eprintln!(
        "generate: {:?}  render: {:?}  total: {:?}",
        t1 - t0,
        t2 - t1,
        t2 - t0,
    );
    Ok(())
}

// ── Dataflow summary ──────────────────────────────────────────────────────────

fn fmt_seg_val<'a>(v: &SegVal, project: &'a Project) -> &'a str {
    match v {
        SegVal::Known(idx) => project.segments[*idx].name.as_str(),
        SegVal::Unknown => "?",
    }
}

fn print_dataflow<W: Write>(project: &Project, w: &mut W) -> io::Result<()> {
    let gp_names = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];

    let total_blocks = project.seg_dataflow.block_entry.len();
    let interesting_blocks = project.seg_dataflow.block_entry.values().count();

    writeln!(
        w,
        "Blocks analyzed: {total_blocks}  Blocks with resolved DS/ES: {interesting_blocks}"
    )?;
    writeln!(w)?;

    if interesting_blocks == 0 {
        writeln!(w, "(no blocks with resolved DS or ES)")?;
        return Ok(());
    }

    let addr_width = project
        .segments
        .iter()
        .map(|s| s.name.len() + 5)
        .max()
        .unwrap_or(12);

    writeln!(
        w,
        "{:<addr_width$}  {:8} {:8} {:8} {:8}  GP (known only)",
        "block", "CS", "DS", "ES", "SS"
    )?;
    writeln!(w, "{}", "-".repeat(addr_width + 2 + 9 * 4 + 20))?;

    for (&(seg_idx, start), state) in &project.seg_dataflow.block_entry {
        let seg_name = &project.segments[seg_idx].name;
        let addr = format!("{seg_name}:{start:04x}");

        let cs = fmt_seg_val(&state.sregs[1], project);
        let ds = fmt_seg_val(&state.sregs[3], project);
        let es = fmt_seg_val(&state.sregs[0], project);
        let ss = fmt_seg_val(&state.sregs[2], project);

        let mut gp_parts: Vec<String> = Vec::new();
        for (i, val) in state.gpregs.iter().enumerate() {
            if let SegVal::Known(idx) = val {
                gp_parts.push(format!("{}={}", gp_names[i], project.segments[*idx].name));
            }
        }
        let gp = gp_parts.join("  ");

        writeln!(
            w,
            "{addr:<addr_width$}  {cs:<8} {ds:<8} {es:<8} {ss:<8}  {gp}"
        )?;
    }

    Ok(())
}

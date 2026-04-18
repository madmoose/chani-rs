use std::{
    collections::BTreeMap,
    io::{self, Write},
    path::Path,
};

use chani_disasm::{
    DisplayContext,
    layout::{LayoutBuilder, Widgets, render_widgets},
    project::Project,
};

fn main() {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("Usage: disasm <file>");
            std::process::exit(1);
        }
    };

    // If given a .chani project file, load it along with its executable.
    // Otherwise, treat the argument directly as an executable and derive a project from it.
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

    project.mark_data_attributes();
    project.disassemble();
    project.generate_auto_labels();

    let mut stdout = io::BufWriter::new(io::stdout().lock());
    if let Err(e) = print_listing(&project, &mut stdout)
        && e.kind() != io::ErrorKind::BrokenPipe
    {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

fn print_listing(project: &Project, w: &mut impl Write) -> io::Result<()> {
    let mut listing = BTreeMap::<(usize, u32), Widgets>::new();

    for seg_idx in 0..project.segments.len() {
        let seg = &project.segments[seg_idx];
        let seg_start = seg.start.unwrap_or(0);
        let seg_end = seg.end.unwrap_or(0);

        let ctx = DisplayContext {
            lookup: &|sv, mem_ofs| {
                let si = project.segment_index_for(sv)?;
                project.name_at(si, mem_ofs as u32)
            },
            register_file: None,
            ofs_seg: None,
        };

        let mut ofs = 0;
        let seg_len = seg_end - seg_start;
        while ofs < seg_len {
            let mut layout_builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);

            layout_builder.layout();
            listing.insert((seg_idx, ofs), layout_builder.widgets());

            let Some(next_ofs) = project.segments[seg_idx].addr_attributes.next(ofs) else {
                break;
            };
            ofs = next_ofs;
        }
    }

    let mut buf = String::with_capacity(120);
    for widgets in listing.values() {
        let lines = widgets.iter().map(|w| w.y).max().unwrap_or_default() + 1;
        for y in 0..lines {
            buf.clear();
            render_widgets(widgets, &mut buf, y);
            writeln!(w, "{buf}")?;
        }
    }

    Ok(())
}

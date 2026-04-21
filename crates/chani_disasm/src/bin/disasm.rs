use std::{
    collections::BTreeMap,
    io::{self, Write, stdout},
    path::Path,
};

use chani_disasm::{
    DisplayContext, SRegMap,
    layout::{LayoutBuilder, Widget, Widgets, render_widgets},
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

    project.analyze();

    let listing = build_listing(&project);

    // let mut stdout = io::BufWriter::new(io::stdout().lock());
    // if let Err(e) = print_listing(&listing, &mut stdout)
    //     && e.kind() != io::ErrorKind::BrokenPipe
    // {
    //     eprintln!("error: {e}");
    //     std::process::exit(1);
    // }
}

type Listing = BTreeMap<(usize, u32), Vec<Widget>>;

fn build_listing(project: &Project) -> Listing {
    let mut listing: Listing = BTreeMap::<(usize, u32), Widgets>::new();

    for seg_idx in 0..project.segments.len() {
        let seg = &project.segments[seg_idx];
        let seg_start = seg.start.unwrap_or(0);
        let seg_end = seg.end.unwrap_or(0);

        let lookup = chani_disasm::project::ProjectLookup {
            project,
            sreg_map: SRegMap {
                cs: Some(seg_idx),
                ..Default::default()
            },
            register_file: None,
            default_seg: None,
        };
        let ctx = DisplayContext { lookup: &lookup };

        let mut ofs = seg_start;
        while ofs < seg_end {
            println!("\n===================\n");
            let mut layout_builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);

            layout_builder.layout();
            let widgets = layout_builder.widgets();

            {
                let mut stdout = stdout().lock();
                let mut buf = String::with_capacity(120);
                let lines = widgets.iter().map(|w| w.y).max().unwrap_or_default() + 1;
                for y in 0..lines {
                    buf.clear();
                    render_widgets(&widgets, &mut buf, y);
                    let _ = writeln!(stdout, "{buf}");
                }
            }

            listing.insert((seg_idx, ofs), widgets);

            let Some(next_ofs) = project.segments[seg_idx].addr_attributes.next(ofs) else {
                break;
            };
            ofs = next_ofs;
        }
    }

    listing
}

fn print_listing(listing: &Listing, w: &mut impl Write) -> io::Result<()> {
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

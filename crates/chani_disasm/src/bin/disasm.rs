use std::{
    collections::BTreeMap,
    fmt, fs,
    io::{self, Write},
    path::Path,
};

use chani_datafile::{ast, parser};
use chani_disasm::{
    attributes::Attributes,
    branch_map::BranchMap,
    decode,
    exe_mz::ExeMz,
    project::{Project, Segment},
    work_queue::WorkQueue,
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
        load_project(&path)
    } else {
        init_project(&path)
    };

    let mut project = match project {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{path}: {e}");
            std::process::exit(1);
        }
    };

    // {
    //     let mut w = std::io::stdout().lock();
    //     let _ = project.write_to(&mut w);
    // }

    let branches = disassemble(&mut project);

    generate_auto_labels(&mut project, &branches);

    let mut stdout = io::BufWriter::new(io::stdout().lock());
    if let Err(e) = print_listing(&project, &branches, &mut stdout) {
        if e.kind() != io::ErrorKind::BrokenPipe {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    }
}

fn init_project(exe_path: &str) -> Result<Project, String> {
    let data = fs::read(exe_path).map_err(|e| e.to_string())?;
    let exe = ExeMz::load(&data, exe_path.to_owned()).map_err(|e| e.to_string())?;
    let segments = make_segments(&exe);
    Ok(Project {
        project: String::new(),
        file: exe_path.to_owned(),
        hash: None,
        arch: String::new(),
        segments,
        attrs: BTreeMap::new(),
        exe,
    })
}

fn load_project(chani_path: &str) -> Result<Project, String> {
    let content = fs::read_to_string(chani_path).map_err(|e| e.to_string())?;
    let tokens = parser::parse(&content)?;
    let doc = ast::Document::from_tokens(tokens)?;
    let mut project = Project::from_document(doc)?;

    let base = Path::new(chani_path).parent().unwrap_or(Path::new("."));
    let exe_path = base.join(&project.file).to_string_lossy().into_owned();
    let data = fs::read(&exe_path).map_err(|e| format!("{exe_path}: {e}"))?;
    let exe = ExeMz::load(&data, exe_path).map_err(|e| e.to_string())?;

    project.exe = exe;

    Ok(project)
}

fn make_segments(exe: &ExeMz) -> Vec<Segment> {
    use std::collections::BTreeSet;

    let mut seg_set: BTreeSet<u16> = BTreeSet::new();

    seg_set.insert(exe.head.cs);

    for reloc in &exe.relocations {
        let ea = reloc.seg as usize * 16 + reloc.ofs as usize;
        if let Some(bytes) = exe.image.get(ea..ea + 2) {
            let seg = u16::from_le_bytes([bytes[0], bytes[1]]);
            seg_set.insert(seg);
        }
    }

    seg_set.insert(exe.head.ss);

    let segs: Vec<u16> = seg_set.into_iter().collect();
    let image_size = exe.image.len() as u32;

    segs.iter()
        .enumerate()
        .map(|(i, &seg)| {
            let start = seg as u32 * 16;
            let end = segs.get(i + 1).map_or(image_size, |&s| s as u32 * 16);
            Segment {
                name: format!("seg{i:03}"),
                r#type: None,
                start,
                end,
                attrs: Attributes::new(end.saturating_sub(start) as usize),
            }
        })
        .collect()
}

fn disassemble(project: &mut Project) -> BranchMap {
    use chani_disasm::project::{AddrSegment, AttrType};

    let mut queue: WorkQueue<(usize, u16)> = WorkQueue::new();
    let mut branches = BranchMap::new();

    if let Some(seg_idx) = project.segment_index_for(project.exe.head.cs) {
        queue.push((seg_idx, project.exe.head.ip));
    }

    // Seed the queue with all names explicitly marked as code.
    for attr in project.attrs.values() {
        if attr.r#type != Some(AttrType::Code) {
            continue;
        }
        let seg_idx = match attr.addr.0 {
            AddrSegment::Idx(i) => Some(i),
            AddrSegment::Value(v) => project.segment_index_for(v),
        };
        if let Some(seg_idx) = seg_idx {
            queue.push((seg_idx, attr.addr.1));
        }
    }

    while let Some((seg_idx, ofs)) = queue.pop() {
        // Skip if already disassembled.
        if project.segments[seg_idx].attrs.is_op(ofs) {
            continue;
        }

        let seg = (project.segments[seg_idx].start / 16) as u16;
        let mut cur_ofs = ofs;
        loop {
            let pos = seg as usize * 16 + cur_ofs as usize;
            let Some(inst) = decode(seg, cur_ofs, project.exe.image[pos..].iter().copied()) else {
                break;
            };

            let len = inst.bytes.len();

            project.segments[seg_idx].attrs.mark_as_code(cur_ofs, len);

            // Enqueue branch target if statically known.
            if inst.branches() {
                if let Some((dst_seg, dst_ofs)) = inst.branch_destination() {
                    if let Some(dst_idx) = project.segment_index_for(dst_seg) {
                        branches.add((seg_idx, cur_ofs), (dst_idx, dst_ofs));
                        queue.push((dst_idx, dst_ofs));
                    }
                }
            }

            cur_ofs = cur_ofs.wrapping_add(len as u16);

            if inst.stops_control_flow() {
                break;
            }
        }
    }

    branches
}

fn generate_auto_labels(project: &mut Project, branches: &BranchMap) {
    fn ea(seg: u16, ofs: u16) -> u32 {
        (seg as u32) * 16 + (ofs as u32)
    }

    // All branch targets.
    for (seg_idx, ofs) in branches.all_targets() {
        let seg = (project.segments[seg_idx].start / 16) as u16;
        let label = format!("loc_{:05x}", ea(seg, ofs));
        project.ensure_auto_label(seg_idx, ofs, label);
    }

    // All attrs explicitly typed as code without a name.
    use chani_disasm::project::{AddrSegment, AttrType};
    let code_addrs: Vec<(usize, u16)> = project
        .attrs
        .values()
        .filter(|a| a.r#type == Some(AttrType::Code) && a.name.is_none())
        .filter_map(|a| {
            let seg_idx = match a.addr.0 {
                AddrSegment::Idx(i) => Some(i),
                AddrSegment::Value(v) => project.segment_index_for(v),
            }?;
            Some((seg_idx, a.addr.1))
        })
        .collect();

    for (seg_idx, ofs) in code_addrs {
        let seg = (project.segments[seg_idx].start / 16) as u16;
        let label = format!("loc_{:05x}", ea(seg, ofs));
        project.ensure_auto_label(seg_idx, ofs, label);
    }
}

#[derive(Default)]
struct ListingStream {
    lf: bool,
    seg_name: String,
    ofs: u16,
}

const DEFAULT_BODY_COL: usize = 28;
const DEFAULT_COMMENT_COL: usize = 58;

impl ListingStream {
    pub fn code(
        &self,
        w: &mut impl fmt::Write,
        label: Option<&str>,
        body: &str,
        comments: &[&str],
    ) -> fmt::Result {
        let mut comments = comments.iter();

        if self.lf {
            writeln!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
        }

        if let Some(label) = label {
            write!(w, "{}:{:04x}  {label}:", self.seg_name, self.ofs)?;
            if let Some(&cmt) = comments.next() {
                let written = self.addr_width() + 2 + label.len() + 1;
                Self::pad_to_comment(w, written)?;
                write!(w, "; {cmt}")?;
            }
            writeln!(w)?;
        }

        write!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
        self.pad_to_body(w, self.addr_width())?;
        write!(w, "{body}")?;
        if let Some(&cmt) = comments.next() {
            let written = DEFAULT_BODY_COL + body.len();
            Self::pad_to_comment(w, written)?;
            write!(w, "; {cmt}")?;
        }
        writeln!(w)?;

        while let Some(&cmt) = comments.next() {
            write!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
            Self::pad_to_comment(w, self.addr_width())?;
            writeln!(w, "; {cmt}")?;
        }

        Ok(())
    }

    pub fn data(
        &self,
        w: &mut impl fmt::Write,
        label: Option<&str>,
        body: &str,
        comments: &[&str],
    ) -> fmt::Result {
        let mut comments = comments.iter();

        if self.lf {
            writeln!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
        }

        write!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
        if let Some(label) = label {
            write!(w, "  {label} ")?;
            let written = self.addr_width() + 2 + label.len() + 1;
            self.pad_to_body(w, written)?;
        } else {
            self.pad_to_body(w, self.addr_width())?;
        }
        write!(w, "{body}")?;
        if let Some(&cmt) = comments.next() {
            let label_width = label.map_or(0, |l| 2 + l.len() + 1);
            let written = DEFAULT_BODY_COL.max(self.addr_width() + label_width) + body.len();
            Self::pad_to_comment(w, written)?;
            write!(w, "; {cmt}")?;
        }
        writeln!(w)?;

        while let Some(&cmt) = comments.next() {
            write!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
            Self::pad_to_comment(w, self.addr_width())?;
            writeln!(w, "; {cmt}")?;
        }

        Ok(())
    }

    fn addr_width(&self) -> usize {
        self.seg_name.len() + 1 + 4 // "seg000:0000"
    }

    fn pad_to(w: &mut impl fmt::Write, written: usize, col: usize) -> fmt::Result {
        let pad = if written < col { col - written } else { 1 };
        for _ in 0..pad {
            write!(w, " ")?;
        }
        Ok(())
    }

    fn pad_to_body(&self, w: &mut impl fmt::Write, written: usize) -> fmt::Result {
        Self::pad_to(w, written, DEFAULT_BODY_COL)
    }

    fn pad_to_comment(w: &mut impl fmt::Write, written: usize) -> fmt::Result {
        Self::pad_to(w, written, DEFAULT_COMMENT_COL)
    }
}

fn print_listing(project: &Project, branches: &BranchMap, w: &mut impl Write) -> io::Result<()> {
    use chani_disasm::project::AttrType;

    // Format (seg_idx, ofs) as "seg_name:ofs".
    let fmt_addr = |addr: (usize, u16)| -> String {
        format!("{}:{:04x}", project.segments[addr.0].name, addr.1)
    };

    // File header
    writeln!(w, "; {}", project.file)?;
    if !project.project.is_empty() {
        writeln!(w, "; Project: {}", project.project)?;
    }
    if let Some(hash) = &project.hash {
        let s = hash
            .bytes
            .iter()
            .map(|v| format!("{v:02x}"))
            .collect::<Vec<_>>()
            .join("");
        writeln!(w, "; SHA1: {s}")?;
    }
    if !project.arch.is_empty() {
        writeln!(w, "; Arch: {}", project.arch)?;
    }
    writeln!(w)?;

    let mut first_line = true;
    let mut last_was_code = false;
    let mut listing = ListingStream::default();
    let mut buf = String::new();

    for seg_idx in 0..project.segments.len() {
        let seg = (project.segments[seg_idx].start / 16) as u16;
        let seg_name = project.segments[seg_idx].name.clone();
        let seg_type = project.segments[seg_idx]
            .r#type
            .as_deref()
            .unwrap_or("unknown");
        let seg_start = project.segments[seg_idx].start;
        let seg_end = project.segments[seg_idx].end;

        writeln!(
            w,
            "; Segment: {} ({})  {:05x}..{:05x}",
            seg_name, seg_type, seg_start, seg_end
        )?;
        writeln!(w)?;

        listing.seg_name = seg_name;

        let mut ofs: u16 = 0;
        let size = project.segments[seg_idx].size();
        while (ofs as usize) < size {
            let pos = seg as usize * 16 + ofs as usize;
            listing.ofs = ofs;

            let is_op = project.segments[seg_idx].attrs.is_op(ofs);

            if is_op {
                let attr = project.attr_at(seg_idx, ofs);
                let label = attr.and_then(|attr| attr.name.as_deref());
                let xref: Vec<String> = branches.sources((seg_idx, ofs)).map(&fmt_addr).collect();

                listing.lf = !first_line && (last_was_code != is_op || label.is_some());

                let body = if let Some(inst) =
                    decode(seg, ofs, project.exe.image[pos..].iter().copied())
                {
                    use chani_disasm::{DisplayContext, project::AddrSegment};
                    let imm_seg = attr
                        .and_then(|a| a.imm_seg.as_ref())
                        .and_then(|addr_seg| match addr_seg {
                            AddrSegment::Idx(i) => Some(project.segments[*i].start / 16),
                            AddrSegment::Value(v) => Some(*v as u32),
                        })
                        .map(|v| v as u16);
                    inst.to_string_opts(DisplayContext {
                        lookup: &|seg_val, mem_ofs| {
                            let seg_idx = project.segment_index_for(seg_val)?;
                            project.name_at(seg_idx, mem_ofs)
                        },
                        register_file: None,
                        imm_seg,
                    })
                } else {
                    String::new()
                };

                let xref_comment;
                let mut comments: Vec<&str> = Vec::new();
                if !xref.is_empty() {
                    xref_comment = format!("CODE XREF: {}", xref.join(", "));
                    comments.push(&xref_comment);
                };

                buf.clear();
                let _ = listing.code(&mut buf, label, &body, &comments);
                w.write_all(buf.as_bytes())?;

                let len = project.segments[seg_idx].attrs.op_len(ofs);
                ofs = ofs.wrapping_add(len as u16);
            } else {
                listing.lf = !first_line && last_was_code != is_op;

                let attr = project.attr_at(seg_idx, ofs);
                let label = attr.and_then(|a| a.name.as_deref());
                let data_type = attr.and_then(|a| a.r#type.as_ref());

                use chani_disasm::project::AddrSegment;
                let imm_seg = attr
                    .and_then(|a| a.imm_seg.as_ref())
                    .and_then(|addr_seg| match addr_seg {
                        AddrSegment::Idx(i) => Some(project.segments[*i].start / 16),
                        AddrSegment::Value(v) => Some(*v as u32),
                    })
                    .map(|v| v as u16);

                // Flatten array types into (elem_type, count); scalar types are count=1.
                let (elem_type, count) = match data_type {
                    Some(AttrType::Array { elem, count }) => (Some(elem.as_ref()), *count),
                    other => (other.map(|t| t), 1),
                };

                let mut cur_pos = pos;
                for i in 0..count {
                    let cur_ofs = ofs.wrapping_add((cur_pos - pos) as u16);
                    listing.ofs = cur_ofs;
                    let entry_label = if i == 0 { label } else { None };

                    let (body, elem_size) = match elem_type {
                        Some(AttrType::U16) => {
                            let b0 = project.exe.image[cur_pos];
                            let b1 = project.exe.image.get(cur_pos + 1).copied().unwrap_or(0);
                            let val = u16::from_le_bytes([b0, b1]);
                            let body = if let Some(iseg) = imm_seg
                                && let Some(name) = project
                                    .segment_index_for(iseg)
                                    .and_then(|si| project.name_at(si, val))
                            {
                                format!("dw {name}")
                            } else {
                                format!("dw {:04x}h", val)
                            };
                            (body, 2usize)
                        }
                        Some(AttrType::U32) => {
                            let b0 = project.exe.image[cur_pos];
                            let b1 = project.exe.image.get(cur_pos + 1).copied().unwrap_or(0);
                            let b2 = project.exe.image.get(cur_pos + 2).copied().unwrap_or(0);
                            let b3 = project.exe.image.get(cur_pos + 3).copied().unwrap_or(0);
                            let val = u32::from_le_bytes([b0, b1, b2, b3]);
                            let body = if let Some(iseg) = imm_seg
                                && let Some(name) = project
                                    .segment_index_for(iseg)
                                    .and_then(|si| project.name_at(si, val as u16))
                            {
                                format!("dd {name}")
                            } else {
                                format!("dd {:08x}h", val)
                            };
                            (body, 4usize)
                        }
                        _ => (format!("db {:02x}h", project.exe.image[cur_pos]), 1usize),
                    };

                    buf.clear();
                    let _ = listing.data(&mut buf, entry_label, &body, &[]);
                    w.write_all(buf.as_bytes())?;

                    cur_pos += elem_size;
                    listing.lf = false;
                }

                ofs = ofs.wrapping_add((cur_pos - pos) as u16);
            }

            last_was_code = is_op;
            first_line = false;
        }
    }
    Ok(())
}

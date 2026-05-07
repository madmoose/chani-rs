use std::{
    collections::{HashMap, HashSet},
    io::{self, Write},
    path::Path,
};

use chani_disasm::{
    Address,
    layout::{WidgetKind, generate_widgets},
    project::Project,
    seg_dataflow::SegVal,
};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let show_dataflow = args.iter().any(|a| a == "--dataflow");
    let show_html = args.iter().any(|a| a == "--html");
    let path = args.into_iter().skip(1).find(|a| !a.starts_with('-'));

    let path = match path {
        Some(p) => p,
        None => {
            eprintln!("Usage: disasm [--dataflow] [--html] <file>");
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

    if show_html {
        if let Err(e) = print_listing_html(&project, &path, &mut stdout)
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
    let mut widget_iter = widgets.iter().peekable();
    for y in 0..total_rows {
        buf.clear();
        let mut cursor = 0u32;
        while let Some(widget) = widget_iter.peek() {
            if widget.y != y {
                break;
            }
            let widget = widget_iter.next().unwrap();
            while cursor < widget.x {
                buf.push(' ');
                cursor += 1;
            }
            buf.push_str(&widget.text);
            cursor += widget.text.len() as u32;
        }
        writeln!(w, "{buf}")?;
    }
    let t2 = std::time::Instant::now();

    _ = w.flush();

    eprintln!(
        "generate: {:?}  render: {:?}  total: {:?}",
        t1 - t0,
        t2 - t1,
        t2 - t0,
    );
    Ok(())
}

// ── HTML listing ─────────────────────────────────────────────────────────────

fn widget_class(kind: &WidgetKind) -> &'static str {
    match kind {
        WidgetKind::Address => "addr",
        WidgetKind::Label => "label",
        WidgetKind::Separator => "sep",
        WidgetKind::Opcode => "op",
        WidgetKind::Operand { .. } => "operand",
        WidgetKind::Punctuation => "punct",
        WidgetKind::Data => "data",
        WidgetKind::ArrayIndex { .. } => "array-idx",
        WidgetKind::StructField { .. } => "struct-field",
        WidgetKind::FileHeader => "file-hdr",
        WidgetKind::SegmentHeader => "seg-hdr",
        WidgetKind::SegmentDecl => "seg-decl",
        WidgetKind::AssumeDir => "assume",
        WidgetKind::Comment => "comment",
        WidgetKind::XrefIn => "xref-in",
    }
}

fn html_escape(s: &str, out: &mut String) {
    for ch in s.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(ch),
        }
    }
}

fn print_listing_html<W: Write>(project: &Project, path: &str, w: &mut W) -> io::Result<()> {
    let title = Path::new(path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("Disassembly");

    write!(
        w,
        r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>{title}</title>
<style>
body {{ background: #1e1e1e; color: #d4d4d4; font-family: "Consolas","Cascadia Code","Fira Mono",monospace; font-size: 14px; margin: 1rem 2rem; }}
h1 {{ color: #d4d4d4; font-size: 1rem; margin-bottom: 0.5rem; }}
pre.listing {{ margin: 0; line-height: 1.4; }}
.addr {{ color: #569cd6; }}
.label {{ color: #dcdcaa; }}
.sep {{ color: #6a9955; }}
.op {{ color: #c586c0; }}
.operand {{ color: #9cdcfe; }}
.punct {{ color: #d4d4d4; }}
.data {{ color: #b5cea8; }}
.array-idx {{ color: #ce9178; }}
.struct-field {{ color: #9cdcfe; }}
.file-hdr {{ color: #6a9955; }}
.seg-hdr {{ color: #6a9955; }}
.seg-decl {{ color: #dcdcaa; }}
.assume {{ color: #c586c0; }}
.comment {{ color: #6a9955; font-style: italic; }}
.xref-in {{ color: #4ec9b0; }}
a {{ color: inherit; text-decoration: none; }}
a:hover {{ text-decoration: underline; }}
</style>
</head>
<body>
<h1>{title}</h1>
<pre class="listing"><code>
"#
    )?;

    let mut label_to_anchor: HashMap<&str, String> = HashMap::new();
    for (&(seg_idx, ofs), attr) in &project.attrs {
        if let Some(name) = attr.name.as_deref() {
            let seg_name = &project.segments[seg_idx].name;
            label_to_anchor.insert(name, format!("{}-{:04x}", seg_name, ofs));
        }
    }

    let t0 = std::time::Instant::now();
    let (widgets, total_rows) = generate_widgets(project);
    let t1 = std::time::Instant::now();

    let block_break_rows: HashSet<u32> = widgets
        .iter()
        .filter(|w| matches!(w.kind, WidgetKind::Separator | WidgetKind::SegmentDecl))
        .map(|w| w.y)
        .collect();

    let mut emitted_anchors: HashSet<String> = HashSet::new();
    let mut buf = String::with_capacity(256);
    let mut widget_iter = widgets.iter().peekable();
    for y in 0..total_rows {
        if y > 0 && block_break_rows.contains(&y) {
            write!(w, "</code></pre>\n<pre class=\"listing\"><code>")?;
        }
        buf.clear();
        let mut cursor = 0u32;
        while let Some(widget) = widget_iter.peek() {
            if widget.y != y {
                break;
            }
            let widget = widget_iter.next().unwrap();
            while cursor < widget.x {
                buf.push(' ');
                cursor += 1;
            }
            match &widget.kind {
                WidgetKind::Address => {
                    let anchor = addr_to_anchor(project, (widget.seg_idx, widget.ofs));
                    if emitted_anchors.insert(anchor.clone()) {
                        buf.push_str("<span id=\"");
                        buf.push_str(&anchor);
                        buf.push_str("\" class=\"addr\">");
                    } else {
                        buf.push_str("<span class=\"addr\">");
                    }
                    html_escape(&widget.text, &mut buf);
                    buf.push_str("</span>");
                }
                WidgetKind::Operand { .. } => {
                    let anchor = widget.link_addr
                        .map(|a| addr_to_anchor(project, a))
                        .or_else(|| label_to_anchor.get(widget.text.as_str()).cloned());
                    if let Some(anchor) = anchor {
                        buf.push_str("<a href=\"#");
                        buf.push_str(&anchor);
                        buf.push_str("\" class=\"operand\">");
                        html_escape(&widget.text, &mut buf);
                        buf.push_str("</a>");
                    } else {
                        buf.push_str("<span class=\"operand\">");
                        html_escape(&widget.text, &mut buf);
                        buf.push_str("</span>");
                    }
                }
                WidgetKind::XrefIn => {
                    buf.push_str("<span class=\"xref-in\">");
                    render_xref_html(&widget.text, widget.link_addr, project, &label_to_anchor, &mut buf);
                    buf.push_str("</span>");
                }
                WidgetKind::Data => {
                    buf.push_str("<span class=\"data\">");
                    render_data_html(&widget.text, widget.link_addr, project, &label_to_anchor, &mut buf);
                    buf.push_str("</span>");
                }
                _ => {
                    let cls = widget_class(&widget.kind);
                    buf.push_str("<span class=\"");
                    buf.push_str(cls);
                    buf.push_str("\">");
                    html_escape(&widget.text, &mut buf);
                    buf.push_str("</span>");
                }
            }
            cursor += widget.text.len() as u32;
        }
        writeln!(w, "{buf}")?;
    }
    let t2 = std::time::Instant::now();

    write!(w, "</code></pre>\n</body>\n</html>\n")?;
    _ = w.flush();

    eprintln!(
        "generate: {:?}  render: {:?}  total: {:?}",
        t1 - t0,
        t2 - t1,
        t2 - t0,
    );
    Ok(())
}

fn addr_to_anchor(project: &Project, (seg_idx, ofs): Address) -> String {
    format!("{}-{:04x}", project.segments[seg_idx].name, ofs)
}

fn render_xref_html(
    text: &str,
    link_addr: Option<Address>,
    project: &Project,
    label_map: &HashMap<&str, String>,
    buf: &mut String,
) {
    // text format: "; ← src (kind)"
    let prefix = "; \u{2190} ";
    let Some(rest) = text.strip_prefix(prefix) else {
        html_escape(text, buf);
        return;
    };
    let Some(paren_pos) = rest.rfind(" (") else {
        html_escape(text, buf);
        return;
    };
    let src = &rest[..paren_pos];
    let suffix = &rest[paren_pos..]; // " (call)" / " (jmp)" / " (data)"

    let anchor = if let Some(addr) = link_addr {
        addr_to_anchor(project, addr)
    } else if let Some(a) = label_map.get(src) {
        a.clone()
    } else if src.contains(':') {
        src.replace(':', "-")
    } else {
        html_escape(text, buf);
        return;
    };

    buf.push_str(prefix);
    buf.push_str("<a href=\"#");
    buf.push_str(&anchor);
    buf.push_str("\">");
    html_escape(src, buf);
    buf.push_str("</a>");
    html_escape(suffix, buf);
}

fn render_data_html(
    text: &str,
    link_addr: Option<Address>,
    project: &Project,
    label_map: &HashMap<&str, String>,
    buf: &mut String,
) {
    // link_addr is set by Ofs16 when a label was resolved.
    if let Some(addr) = link_addr {
        for prefix in ["dw ", "dd "] {
            if let Some(label) = text.strip_prefix(prefix) {
                let anchor = addr_to_anchor(project, addr);
                buf.push_str(prefix);
                buf.push_str("<a href=\"#");
                buf.push_str(&anchor);
                buf.push_str("\">");
                html_escape(label, buf);
                buf.push_str("</a>");
                return;
            }
        }
    }
    // Fallback: label-map lookup for unique labels (covers edge cases).
    for prefix in ["dw ", "dd "] {
        if let Some(label) = text.strip_prefix(prefix)
            && let Some(anchor) = label_map.get(label)
        {
            buf.push_str(prefix);
            buf.push_str("<a href=\"#");
            buf.push_str(anchor);
            buf.push_str("\">");
            html_escape(label, buf);
            buf.push_str("</a>");
            return;
        }
    }
    html_escape(text, buf);
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

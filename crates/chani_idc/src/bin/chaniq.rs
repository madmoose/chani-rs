use std::collections::BTreeSet;
use std::io;
use std::path::Path;
use std::fs;

use anyhow::{Context, Result, bail};
use chani_disasm::data_type::{CompositeDataType, DataType, DisplayFmt, ScalarDataType};
use chani_disasm::layout::LayoutBuilder;
use chani_disasm::project::{Attr, AttrType, Assumes, Project, ProjectLookup, SegmentIdx};
use chani_disasm::{Address, DisplayContext, Opcode, SRegMap, decode};
use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(name = "chaniq", about = "chani project query and annotation tool")]
struct Cli {
    /// Path to the .chani project file
    project: std::path::PathBuf,
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Set annotations at an address (does not require binary)
    Set(SetArgs),
    /// Print all annotations at an address (does not require binary)
    Get { addr: String },
    /// Find annotations by name or comment substring (does not require binary)
    Find { pattern: String },
    /// List annotations in a segment or address range (does not require binary)
    List {
        #[arg(value_name = "RANGE", help = "seg001  |  seg001:1000  |  seg001:1000-2000  (hex offsets)")]
        range: String,
    },
    /// Search the rendered listing for a text pattern (requires binary)
    Search(SearchArgs),
    /// Show cross-references to an address (requires binary)
    Xref { addr: String },
    /// Show disassembly of the function body at an address (requires binary)
    Func { addr: String },
    /// Show callers of the function at an address (requires binary)
    Callers { addr: String },
    /// Show callees of the function at an address (requires binary)
    Callees { addr: String },
    /// Run project consistency checks (does not require binary)
    Check,
}

#[derive(Args)]
struct SetArgs {
    /// Address in the form seg:ofs (hex offset), e.g. seg001:22cb
    addr: String,
    /// Set the label name (empty string clears it)
    #[arg(long)]
    name: Option<String>,
    /// Set the type, e.g. "code", "u16", "dec(u16)", "[u8; 16]"
    #[arg(long = "type", value_name = "TYPE")]
    r#type: Option<String>,
    /// Set the comment (empty string clears it)
    #[arg(long)]
    comment: Option<String>,
    /// Set the ofs_seg field (segment name used to resolve ofs16 immediates)
    #[arg(long = "ofs-seg", value_name = "SEG")]
    ofs_seg: Option<String>,
    /// Set segment-register assumption, e.g. "ds:seg001"; repeatable; replaces all existing
    #[arg(long, value_name = "SR:SEG")]
    assume: Vec<String>,
    /// Set operand display format, e.g. "0:hex" or "1:dec"
    #[arg(long = "arg", value_name = "N:FMT")]
    arg_fmt: Vec<String>,
    /// Remove the attr entirely
    #[arg(long)]
    delete: bool,
    /// Write output to stdout instead of modifying the file in place
    #[arg(long)]
    stdout: bool,
}

#[derive(Args)]
struct SearchArgs {
    pattern: String,
    /// Print N lines of context after each match
    #[arg(short = 'A', long = "after-context", value_name = "N")]
    after_context: Option<usize>,
    /// Print N lines of context before each match
    #[arg(short = 'B', long = "before-context", value_name = "N")]
    before_context: Option<usize>,
    /// Print N lines of context before and after each match
    #[arg(short = 'C', long = "context", value_name = "N")]
    context: Option<usize>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let path = cli.project.as_path();
    match cli.cmd {
        Cmd::Set(args) => cmd_set(path, args),
        Cmd::Get { addr } => cmd_get(path, &addr),
        Cmd::Find { pattern } => cmd_find(path, &pattern),
        Cmd::List { range } => cmd_list(path, &range),
        Cmd::Search(args) => cmd_search(path, &args),
        Cmd::Xref { addr } => cmd_xref(path, &addr),
        Cmd::Func { addr } => cmd_func(path, &addr),
        Cmd::Callers { addr } => cmd_callers(path, &addr),
        Cmd::Callees { addr } => cmd_callees(path, &addr),
        Cmd::Check => cmd_check(path),
    }
}

// ── Load helpers ──────────────────────────────────────────────────────────────

fn load_str(path: &Path) -> Result<Project> {
    let content = fs::read_to_string(path)
        .with_context(|| format!("cannot read {}", path.display()))?;
    Project::from_str(&content).map_err(|e| anyhow::anyhow!("{e}"))
}

fn load_analyzed(path: &Path) -> Result<Project> {
    let path_str = path.to_str().unwrap_or("");
    let mut project = Project::from_project_file(path_str)
        .map_err(|e| anyhow::anyhow!("{e}"))?;
    project.analyze();
    Ok(project)
}

// ── Address helpers ───────────────────────────────────────────────────────────

fn parse_addr(project: &Project, s: &str) -> Result<(SegmentIdx, u32)> {
    let (seg_name, ofs_str) = s
        .split_once(':')
        .ok_or_else(|| anyhow::anyhow!("address must be 'seg:ofs', got '{s}'"))?;
    let seg_idx = project
        .segment_by_name(seg_name)
        .ok_or_else(|| anyhow::anyhow!("unknown segment '{seg_name}'"))?;
    let ofs = u32::from_str_radix(ofs_str.trim_start_matches("0x"), 16)
        .map_err(|_| anyhow::anyhow!("invalid hex offset '{ofs_str}'"))?;
    Ok((seg_idx, ofs))
}

fn fmt_addr(project: &Project, (seg_idx, ofs): Address) -> String {
    format!("{}:{:04x}", project.segments[seg_idx].name, ofs)
}

// ── Disassembly helpers ───────────────────────────────────────────────────────

fn decode_at(
    project: &Project,
    seg_idx: SegmentIdx,
    ofs: u32,
) -> Option<chani_disasm::DecodedInstruction> {
    let seg = &project.segments[seg_idx];
    let seg_val = (seg.start.unwrap_or(0) / 16) as u16;
    decode(
        seg_val,
        ofs as u16,
        project.bytes_at_seg(seg_idx, ofs).iter().copied(),
    )
}

fn render_addr(project: &Project, seg_idx: SegmentIdx, ofs: u32) -> String {
    let sreg_map = project
        .seg_dataflow
        .state_at(project, seg_idx, ofs)
        .map(|s| s.to_sreg_map())
        .unwrap_or(SRegMap { cs: Some(seg_idx), ..Default::default() });
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
    };
    let ctx = DisplayContext { lookup: &lookup, arg_fmts: [None; 2] };
    let mut builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);
    builder.layout();
    let n = builder.lines();
    // The instruction/data line is always the last line; comments are above it.
    let mut buf = String::new();
    builder.render(&mut buf, n.saturating_sub(1));
    buf
}

fn print_addr_block(project: &Project, seg_idx: SegmentIdx, ofs: u32) {
    let sreg_map = project
        .seg_dataflow
        .state_at(project, seg_idx, ofs)
        .map(|s| s.to_sreg_map())
        .unwrap_or(SRegMap { cs: Some(seg_idx), ..Default::default() });
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
    };
    let ctx = DisplayContext { lookup: &lookup, arg_fmts: [None; 2] };
    let mut builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);
    builder.layout();
    let n = builder.lines();
    let mut buf = String::new();
    for y in 0..n {
        buf.clear();
        builder.render(&mut buf, y);
        println!("{buf}");
    }
}

// ── Parse helpers ─────────────────────────────────────────────────────────────

fn parse_display_fmt(s: &str) -> Result<DisplayFmt> {
    match s {
        "hex" => Ok(DisplayFmt::Hex),
        "dec" => Ok(DisplayFmt::Dec),
        "signed" => Ok(DisplayFmt::SignedDec),
        "bin" => Ok(DisplayFmt::Bin),
        "char" => Ok(DisplayFmt::Char),
        _ => bail!("unknown display format '{s}'; expected hex, dec, signed, bin, or char"),
    }
}

// ── cmd_set ───────────────────────────────────────────────────────────────────

fn cmd_set(path: &Path, args: SetArgs) -> Result<()> {
    let content = fs::read_to_string(path)
        .with_context(|| format!("cannot read {}", path.display()))?;
    let mut project = Project::from_str(&content).map_err(|e| anyhow::anyhow!("{e}"))?;

    let (seg_idx, ofs) = parse_addr(&project, &args.addr)?;

    if args.delete {
        project.attrs.remove(&(seg_idx, ofs));
    } else {
        if args.name.is_none()
            && args.r#type.is_none()
            && args.comment.is_none()
            && args.ofs_seg.is_none()
            && args.assume.is_empty()
            && args.arg_fmt.is_empty()
        {
            bail!("specify at least one of --name, --type, --comment, --ofs-seg, --assume, --arg (or --delete)");
        }

        let parsed_type = args
            .r#type
            .as_deref()
            .map(|s| project.parse_type_str(s).map_err(|e| anyhow::anyhow!("{e}")))
            .transpose()?;

        let parsed_ofs_seg = args
            .ofs_seg
            .as_deref()
            .map(|s| {
                project
                    .segment_by_name(s)
                    .ok_or_else(|| anyhow::anyhow!("unknown segment '{s}'"))
            })
            .transpose()?;

        let parsed_assumes: Assumes = args
            .assume
            .iter()
            .map(|s| {
                s.split_once(':')
                    .map(|(r, seg)| (r.to_string(), seg.to_string()))
                    .ok_or_else(|| anyhow::anyhow!("assume must be 'sr:seg', got '{s}'"))
            })
            .collect::<Result<_>>()?;

        let mut parsed_arg_fmts: Vec<(usize, DisplayFmt)> = Vec::new();
        for s in &args.arg_fmt {
            let (idx_str, fmt_str) = s
                .split_once(':')
                .ok_or_else(|| anyhow::anyhow!("--arg must be 'N:fmt', got '{s}'"))?;
            let idx: usize = idx_str
                .parse()
                .map_err(|_| anyhow::anyhow!("operand index must be 0 or 1, got '{idx_str}'"))?;
            if idx > 1 {
                bail!("operand index must be 0 or 1, got {idx}");
            }
            parsed_arg_fmts.push((idx, parse_display_fmt(fmt_str)?));
        }

        let attr = project.attrs.entry((seg_idx, ofs)).or_insert_with(|| Attr {
            addr: (seg_idx, ofs),
            r#type: None,
            name: None,
            is_auto_label: false,
            ofs_seg: None,
            comment: None,
            assume: Assumes::default(),
            arg_fmts: [None; 2],
        });

        if let Some(name) = args.name {
            attr.name = if name.is_empty() { None } else { Some(name) };
            attr.is_auto_label = false;
        }
        if let Some(t) = parsed_type {
            attr.r#type = Some(t);
        }
        if let Some(comment) = args.comment {
            attr.comment = if comment.is_empty() { None } else { Some(comment) };
        }
        if parsed_ofs_seg.is_some() {
            attr.ofs_seg = parsed_ofs_seg;
        }
        if !parsed_assumes.is_empty() {
            attr.assume = parsed_assumes;
        }
        for (idx, fmt) in parsed_arg_fmts {
            attr.arg_fmts[idx] = Some(fmt);
        }
    }

    if args.stdout {
        project.write_to(&mut io::stdout().lock())?;
    } else {
        let mut buf = Vec::new();
        project.write_to(&mut buf)?;
        fs::write(path, &buf)
            .with_context(|| format!("cannot write {}", path.display()))?;
    }

    Ok(())
}

// ── cmd_get ───────────────────────────────────────────────────────────────────

fn cmd_get(path: &Path, addr_str: &str) -> Result<()> {
    let project = load_str(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;

    println!("{}", fmt_addr(&project, (seg_idx, ofs)));

    let Some(attr) = project.attr_at(seg_idx, ofs) else {
        println!("  (no annotations)");
        return Ok(());
    };

    if let Some(t) = &attr.r#type {
        println!("  type:    {}", t.type_str(&project.segments, &project.structs));
    }
    if let Some(name) = &attr.name {
        let auto = if attr.is_auto_label { " (auto)" } else { "" };
        println!("  name:    {name}{auto}");
    }
    if let Some(seg_idx) = attr.ofs_seg {
        println!("  ofs_seg: {}", project.segments[seg_idx].name);
    }
    if !attr.assume.is_empty() {
        let s: Vec<String> = attr.assume.iter().map(|(r, seg)| format!("{r}:{seg}")).collect();
        println!("  assume:  {}", s.join(" "));
    }
    for (i, fmt) in attr.arg_fmts.iter().enumerate() {
        if let Some(f) = fmt {
            println!("  arg[{i}]:  {}", f.as_str());
        }
    }
    if let Some(comment) = &attr.comment {
        let first = comment.lines().next().unwrap_or("");
        if comment.lines().count() > 1 {
            println!("  comment: {first}");
            for line in comment.lines().skip(1) {
                println!("           {line}");
            }
        } else {
            println!("  comment: {first}");
        }
    }

    Ok(())
}

// ── cmd_find ──────────────────────────────────────────────────────────────────

fn cmd_find(path: &Path, pattern: &str) -> Result<()> {
    let project = load_str(path)?;
    let pat = pattern.to_lowercase();

    let mut found = false;
    for (&addr, attr) in &project.attrs {
        let name_match = attr.name.as_deref()
            .is_some_and(|n| n.to_lowercase().contains(&pat));
        let comment_match = attr.comment.as_deref()
            .is_some_and(|c| c.to_lowercase().contains(&pat));
        if !name_match && !comment_match {
            continue;
        }
        found = true;
        let name = attr.name.as_deref().unwrap_or("");
        let auto = if attr.is_auto_label { " (auto)" } else { "" };
        let type_str = attr.r#type.as_ref()
            .map(|t| format!("[{}]", t.type_str(&project.segments, &project.structs)))
            .unwrap_or_default();
        let comment_first = attr.comment.as_deref()
            .and_then(|c| c.lines().next())
            .unwrap_or("");
        println!("{}  {:<24}  {:<18}  {}",
            fmt_addr(&project, addr),
            format!("{name}{auto}"),
            type_str,
            comment_first,
        );
    }

    if !found {
        println!("(no matches for '{pattern}')");
    }
    Ok(())
}

// ── cmd_list ──────────────────────────────────────────────────────────────────

fn parse_list_range(project: &Project, s: &str) -> Result<(SegmentIdx, u32, u32)> {
    if let Some((seg_name, rest)) = s.split_once(':') {
        let seg_idx = project
            .segment_by_name(seg_name)
            .ok_or_else(|| anyhow::anyhow!("unknown segment '{seg_name}'"))?;
        let seg_end = project.segments[seg_idx].end.unwrap_or(u32::MAX);
        if let Some((start_str, end_str)) = rest.split_once('-') {
            let start = parse_hex_ofs(start_str)?;
            let end = parse_hex_ofs(end_str)?;
            Ok((seg_idx, start, end))
        } else {
            let start = parse_hex_ofs(rest)?;
            Ok((seg_idx, start, seg_end))
        }
    } else {
        let seg_idx = project
            .segment_by_name(s)
            .ok_or_else(|| anyhow::anyhow!("unknown segment '{s}'"))?;
        let seg_start = project.segments[seg_idx].start.unwrap_or(0);
        let seg_end = project.segments[seg_idx].end.unwrap_or(u32::MAX);
        Ok((seg_idx, seg_start, seg_end))
    }
}

fn parse_hex_ofs(s: &str) -> Result<u32> {
    u32::from_str_radix(s.trim_start_matches("0x"), 16)
        .map_err(|_| anyhow::anyhow!("invalid hex offset '{s}'"))
}

fn cmd_list(path: &Path, range_str: &str) -> Result<()> {
    let project = load_str(path)?;
    let (seg_idx, start, end) = parse_list_range(&project, range_str)?;

    let mut any = false;
    for (&addr, attr) in &project.attrs {
        if addr.0 != seg_idx || addr.1 < start || addr.1 >= end {
            continue;
        }
        any = true;
        let name = attr.name.as_deref().unwrap_or("");
        let auto = if attr.is_auto_label { " (auto)" } else { "" };
        let type_str = attr.r#type.as_ref()
            .map(|t| t.type_str(&project.segments, &project.structs))
            .unwrap_or_default();
        let comment_first = attr.comment.as_deref()
            .and_then(|c| c.lines().next())
            .unwrap_or("");
        println!("{}  {:<24}  {:<18}  {}",
            fmt_addr(&project, addr),
            format!("{name}{auto}"),
            type_str,
            comment_first,
        );
    }

    if !any {
        println!("(no annotations in range)");
    }
    Ok(())
}

// ── cmd_search ────────────────────────────────────────────────────────────────

fn collect_rendered_lines(project: &Project, seg_idx: SegmentIdx, ofs: u32) -> Vec<String> {
    let sreg_map = project
        .seg_dataflow
        .state_at(project, seg_idx, ofs)
        .map(|s| s.to_sreg_map())
        .unwrap_or(SRegMap { cs: Some(seg_idx), ..Default::default() });
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
    };
    let ctx = DisplayContext { lookup: &lookup, arg_fmts: [None; 2] };
    let mut builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);
    builder.layout();
    let n = builder.lines();
    let mut lines = Vec::with_capacity(n as usize);
    let mut buf = String::new();
    for y in 0..n {
        buf.clear();
        builder.render(&mut buf, y);
        lines.push(buf.clone());
    }
    lines
}

fn collect_all_lines(project: &Project) -> Vec<String> {
    let mut all_lines = Vec::new();
    for (seg_idx, seg) in project.segments.indexed_iter() {
        let seg_start = seg.start.unwrap_or(0);
        let seg_end = seg.end.unwrap_or(0);
        let mut ofs = seg_start;
        while ofs < seg_end {
            if project.attr_at(seg_idx, ofs).is_some() || seg.addr_attributes.is_op(ofs) {
                for line in collect_rendered_lines(project, seg_idx, ofs) {
                    all_lines.push(line.trim_end().to_string());
                }
            }
            match seg.addr_attributes.next(ofs) {
                Some(next) => ofs = next,
                None => break,
            }
        }
    }
    all_lines
}

fn cmd_search(path: &Path, args: &SearchArgs) -> Result<()> {
    let project = load_analyzed(path)?;
    let pat = args.pattern.to_lowercase();
    let before = args.before_context.or(args.context).unwrap_or(0);
    let after = args.after_context.or(args.context).unwrap_or(0);

    let all_lines = collect_all_lines(&project);

    let matches: Vec<usize> = all_lines
        .iter()
        .enumerate()
        .filter(|(_, line)| line.to_lowercase().contains(&pat))
        .map(|(i, _)| i)
        .collect();

    if before == 0 && after == 0 {
        for i in matches {
            println!("{}", all_lines[i]);
        }
        return Ok(());
    }

    // Merge overlapping or adjacent context windows into groups.
    let mut groups: Vec<(usize, usize)> = Vec::new();
    for &m in &matches {
        let start = m.saturating_sub(before);
        let end = (m + after + 1).min(all_lines.len());
        if let Some(last) = groups.last_mut() {
            if start <= last.1 {
                last.1 = last.1.max(end);
                continue;
            }
        }
        groups.push((start, end));
    }

    for (gi, (start, end)) in groups.iter().enumerate() {
        if gi > 0 {
            println!("--");
        }
        for line in &all_lines[*start..*end] {
            println!("{line}");
        }
    }

    Ok(())
}

// ── cmd_xref ──────────────────────────────────────────────────────────────────

fn cmd_xref(path: &Path, addr_str: &str) -> Result<()> {
    let project = load_analyzed(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;
    let target = (seg_idx, ofs);

    // Code xrefs from BranchMap
    let mut code_srcs: Vec<Address> = project.branches.sources(target).collect();
    code_srcs.sort();

    // Data xrefs: attrs whose ofs16 value points to target
    let mut data_srcs: Vec<Address> = Vec::new();
    for (&src_addr, attr) in &project.attrs {
        if let Some(AttrType::Data(dt)) = &attr.r#type {
            let ofs_seg = attr.ofs_seg;
            if ofs16_points_to(project.bytes_at_seg(src_addr.0, src_addr.1), dt, &project, ofs_seg, target) {
                data_srcs.push(src_addr);
            }
        }
    }
    data_srcs.sort();

    let label = project.name_at(seg_idx, ofs).unwrap_or("");
    println!("xrefs to {}  {}", fmt_addr(&project, target), label);

    if code_srcs.is_empty() && data_srcs.is_empty() {
        println!("  (none)");
        return Ok(());
    }

    if !code_srcs.is_empty() {
        println!("  code:");
        for src in code_srcs {
            let kind = decode_at(&project, src.0, src.1)
                .map(|i| if i.opcode == Opcode::Call { "call" } else { "jmp " })
                .unwrap_or("?   ");
            let line = render_addr(&project, src.0, src.1);
            println!("    [{kind}]  {line}", );
        }
    }

    if !data_srcs.is_empty() {
        println!("  data:");
        for src in data_srcs {
            println!("    {}", fmt_addr(&project, src));
        }
    }

    Ok(())
}

fn ofs16_points_to(
    bytes: &[u8],
    dt: &DataType,
    project: &Project,
    fallback_seg: Option<SegmentIdx>,
    target: Address,
) -> bool {
    match dt {
        DataType::Scalar(ScalarDataType::Ofs16(seg_opt)) => {
            let seg = seg_opt.or(fallback_seg);
            if seg != Some(target.0) { return false; }
            let lo = bytes.first().copied().unwrap_or(0) as u32;
            let hi = bytes.get(1).copied().unwrap_or(0) as u32;
            (lo | (hi << 8)) == target.1
        }
        DataType::Scalar(ScalarDataType::U16) => {
            let seg = fallback_seg;
            if seg != Some(target.0) { return false; }
            let lo = bytes.first().copied().unwrap_or(0) as u32;
            let hi = bytes.get(1).copied().unwrap_or(0) as u32;
            (lo | (hi << 8)) == target.1
        }
        DataType::Composite(CompositeDataType::Array { elem, count }) => {
            let elem_size = if bytes.is_empty() {
                return false;
            } else {
                elem.byte_size(bytes, &project.structs).max(1)
            };
            for i in 0..*count {
                let start = i * elem_size;
                let slice = bytes.get(start..).unwrap_or(&[]);
                if ofs16_points_to(slice, elem, project, fallback_seg, target) {
                    return true;
                }
            }
            false
        }
        DataType::Formatted(_, inner) => {
            ofs16_points_to(bytes, inner, project, fallback_seg, target)
        }
        _ => false,
    }
}

// ── cmd_func ──────────────────────────────────────────────────────────────────

fn cmd_func(path: &Path, addr_str: &str) -> Result<()> {
    let project = load_analyzed(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;

    // Find entry block — accept if addr is inside a block (snap to block start).
    let entry_block_start = project
        .blocks
        .block_containing(seg_idx, ofs)
        .map(|b| b.start)
        .ok_or_else(|| anyhow::anyhow!("{} is not inside a decoded basic block", addr_str))?;

    let block_addrs = function_blocks(&project, (seg_idx, entry_block_start));

    if block_addrs.is_empty() {
        bail!("no basic blocks found at {addr_str}");
    }

    let label = project.name_at(seg_idx, entry_block_start).unwrap_or("(unnamed)");
    println!("; function: {} at {}", label, fmt_addr(&project, (seg_idx, entry_block_start)));
    println!();

    let mut prev_end: Option<u32> = None;
    for &(bseg, bstart) in &block_addrs {
        let Some(block) = project.blocks.block_at(bseg, bstart) else { continue };

        // Blank separator between non-contiguous blocks
        if prev_end.is_some_and(|e| e != bstart) {
            println!();
        }
        prev_end = Some(block.end);

        let seg = &project.segments[bseg];
        let mut ofs = bstart;
        while ofs < block.end {
            print_addr_block(&project, bseg, ofs);
            let Some(next) = seg.addr_attributes.next(ofs) else { break };
            if next >= block.end { break }
            ofs = next;
        }
        // Always render the last instruction (the one at or just before block.end)
        // addr_attributes.next() may skip past it; render block.end - op_len separately
        // But the loop above already handles it via the `next < block.end` condition.
    }

    Ok(())
}

fn function_blocks(project: &Project, entry: Address) -> Vec<Address> {
    let mut visited: BTreeSet<Address> = BTreeSet::new();
    let mut queue: Vec<Address> = vec![entry];

    while let Some(addr) = queue.pop() {
        if !visited.insert(addr) { continue; }
        let Some(block) = project.blocks.block_at(addr.0, addr.1) else { continue };

        // Find last instruction offset by stepping backwards from block.end
        let last_ofs = project.segments[block.seg_idx]
            .addr_attributes
            .prev(block.end)
            .filter(|&p| p >= block.start)
            .unwrap_or(block.start);

        let last_is_call = decode_at(project, block.seg_idx, last_ofs)
            .is_some_and(|i| i.opcode == Opcode::Call);

        for &succ in &block.successors {
            if last_is_call {
                // Follow only the fall-through (return address), not the callee entry.
                if succ == (block.seg_idx, block.end) {
                    queue.push(succ);
                }
            } else {
                // Follow all successors (jmp targets, conditional branch targets).
                queue.push(succ);
            }
        }
    }

    let mut addrs: Vec<Address> = visited.into_iter().collect();
    addrs.sort();
    addrs
}

// ── cmd_callers ───────────────────────────────────────────────────────────────

fn cmd_callers(path: &Path, addr_str: &str) -> Result<()> {
    let project = load_analyzed(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;
    let target = (seg_idx, ofs);

    let label = project.name_at(seg_idx, ofs).unwrap_or("(unnamed)");
    println!("callers of {}  {}", fmt_addr(&project, target), label);

    let mut callers: Vec<Address> = project
        .branches
        .sources(target)
        .filter(|&src| {
            decode_at(&project, src.0, src.1)
                .is_some_and(|i| i.opcode == Opcode::Call)
        })
        .collect();
    callers.sort();

    if callers.is_empty() {
        println!("  (none)");
    } else {
        for src in callers {
            let line = render_addr(&project, src.0, src.1);
            println!("  {line}");
        }
    }

    Ok(())
}

// ── cmd_callees ───────────────────────────────────────────────────────────────

fn cmd_callees(path: &Path, addr_str: &str) -> Result<()> {
    let project = load_analyzed(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;

    let entry_block_start = project
        .blocks
        .block_containing(seg_idx, ofs)
        .map(|b| b.start)
        .ok_or_else(|| anyhow::anyhow!("{} is not inside a decoded basic block", addr_str))?;

    let label = project.name_at(seg_idx, entry_block_start).unwrap_or("(unnamed)");
    println!("callees of {}  {}", fmt_addr(&project, (seg_idx, entry_block_start)), label);

    let block_addrs = function_blocks(&project, (seg_idx, entry_block_start));

    let mut callees: Vec<Address> = Vec::new();
    let mut seen: BTreeSet<Address> = BTreeSet::new();

    for &(bseg, bstart) in &block_addrs {
        let Some(block) = project.blocks.block_at(bseg, bstart) else { continue };

        let last_ofs = project.segments[block.seg_idx]
            .addr_attributes
            .prev(block.end)
            .filter(|&p| p >= block.start)
            .unwrap_or(block.start);

        let last_is_call = decode_at(&project, bseg, last_ofs)
            .is_some_and(|i| i.opcode == Opcode::Call);

        if last_is_call {
            for &succ in &block.successors {
                // Call target is any successor that is NOT the fall-through.
                if succ != (bseg, block.end) && seen.insert(succ) {
                    callees.push(succ);
                }
            }
        }
    }

    callees.sort();

    if callees.is_empty() {
        println!("  (none)");
    } else {
        for callee in callees {
            let lbl = project.name_at(callee.0, callee.1).unwrap_or("(unnamed)");
            println!("  {}  {lbl}", fmt_addr(&project, callee));
        }
    }

    Ok(())
}

// ── cmd_check ─────────────────────────────────────────────────────────────────

fn cmd_check(path: &Path) -> Result<()> {
    let project = load_str(path)?;
    let mut issues = 0usize;

    // 1. ofs16 attrs with no resolvable segment
    for (&addr, attr) in &project.attrs {
        if let Some(AttrType::Data(dt)) = &attr.r#type {
            if contains_unresolved_ofs16(dt) && attr.ofs_seg.is_none() {
                println!("warn: {}  ofs16 without ofs_seg (segment unknown)",
                    fmt_addr(&project, addr));
                issues += 1;
            }
        }
    }

    // 2. code attrs overlapping data-typed attrs
    let data_addrs: Vec<(Address, usize)> = project.attrs.iter()
        .filter_map(|(&addr, attr)| {
            let dt = attr.r#type.as_ref()?.as_data()?;
            Some((addr, dt.byte_size(&[], &project.structs).max(1)))
        })
        .collect();

    'outer: for (&code_addr, attr) in &project.attrs {
        if attr.r#type != Some(AttrType::Code) { continue; }
        for &(data_addr, data_size) in &data_addrs {
            if data_addr.0 != code_addr.0 { continue; }
            if code_addr.1 >= data_addr.1 && code_addr.1 < data_addr.1 + data_size as u32 {
                println!("warn: {}  code attr overlaps data attr at {}",
                    fmt_addr(&project, code_addr),
                    fmt_addr(&project, data_addr));
                issues += 1;
                continue 'outer;
            }
        }
    }

    // 3. code attrs inside data regions (same as above but for any code byte inside data span)
    // Already covered above.

    // 4. assume entries referencing unknown segment names
    for (&addr, attr) in &project.attrs {
        for (_, seg_name) in &attr.assume {
            if project.segment_by_name(seg_name).is_none() {
                println!("warn: {}  assume references unknown segment '{seg_name}'",
                    fmt_addr(&project, addr));
                issues += 1;
            }
        }
    }

    if issues == 0 {
        println!("ok: no issues found ({} attrs checked)", project.attrs.len());
    } else {
        println!("{issues} issue(s) found");
    }

    Ok(())
}

fn contains_unresolved_ofs16(dt: &DataType) -> bool {
    match dt {
        DataType::Scalar(ScalarDataType::Ofs16(None)) => true,
        DataType::Scalar(_) => false,
        DataType::Composite(CompositeDataType::Array { elem, .. }) => {
            contains_unresolved_ofs16(elem)
        }
        DataType::Composite(CompositeDataType::Struct(_)) => false,
        DataType::Formatted(_, inner) => contains_unresolved_ofs16(inner),
    }
}

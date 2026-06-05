use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::Path;

use anyhow::{Context, Result, bail};
use chani_disasm::data_type::{CompositeDataType, DataType, DisplayFmt, ScalarDataType};
use chani_disasm::layout::LayoutBuilder;
use chani_disasm::project::{Assumes, Attr, AttrType, Project, ProjectLookup, SegmentIdx};
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
        #[arg(
            value_name = "RANGE",
            help = "seg001  |  seg001:1000  |  seg001:1000-2000  (hex offsets)"
        )]
        range: String,
    },
    /// Search the rendered listing for a text pattern (requires binary)
    Search(SearchArgs),
    /// Show cross-references to an address (requires binary)
    Xref { addr: String },
    /// Show disassembly of the function body at an address (requires binary)
    Func(FuncArgs),
    /// Print the seg_dataflow abstract register state immediately before
    /// the instruction at <addr>. Use the address of the instruction
    /// following a `call` to see the post-call state. (requires binary)
    State { addr: String },
    /// Show callers of the function at an address (requires binary)
    Callers { addr: String },
    /// Show callees of the function at an address (requires binary)
    Callees { addr: String },
    /// Show instructions that clobber DS/ES/SS preservation. With <addr>:
    /// trace one function. Without: report every analyzed function. (requires binary)
    Clobbers { addr: Option<String> },
    /// List indirect call/jmp instructions with no manually-specified targets (requires binary)
    Unresolved,
    /// List addresses with manual branch targets, optionally filtered to one address (requires binary)
    Targets { addr: Option<String> },
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
    /// Set the comment (empty string clears it). The escape sequences
    /// `\n`, `\t`, and `\\` are interpreted as newline, tab, and backslash.
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
    /// Set a manual branch destination for an indirect or self-modifying
    /// branch instruction; repeatable; replaces all existing targets.
    /// Use "" once to clear.
    #[arg(long = "target", value_name = "SEG:OFS")]
    target: Vec<String>,
    /// Set direction-less `let` type assertions, a comma-separated binding list,
    /// e.g. "troop: *Troop @si, count: u16 @ -2". Replaces all existing; use ""
    /// to clear.
    #[arg(long = "let", value_name = "BINDINGS")]
    r#let: Option<String>,
    /// Set the `fn` signature, a comma-separated binding list with directions,
    /// e.g. "in troop: *Troop @si, inout count: u16 @cx". Replaces any existing;
    /// use "" to clear.
    #[arg(long = "fn", value_name = "BINDINGS")]
    r#fn: Option<String>,
    /// Remove the attr entirely
    #[arg(long)]
    delete: bool,
    /// Write output to stdout instead of modifying the file in place
    #[arg(long)]
    stdout: bool,
}

#[derive(Args)]
struct FuncArgs {
    /// Address in the form seg:ofs (hex offset), e.g. seg001:22cb
    addr: String,
    /// Inline-annotate each direct call with the DS/ES/SS values its
    /// callee summary establishes (when they differ from the pre-call state).
    #[arg(long = "show-call-state")]
    show_call_state: bool,
    /// Carry each binding-rewritten operand's storage inline: `al` → `id@al`,
    /// `[si+3]` → `troop@si->occupation`. Raw labels are left untouched.
    #[arg(long = "annotate-storage")]
    annotate_storage: bool,
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
        Cmd::Func(args) => cmd_func(path, &args),
        Cmd::State { addr } => cmd_state(path, &addr),
        Cmd::Callers { addr } => cmd_callers(path, &addr),
        Cmd::Callees { addr } => cmd_callees(path, &addr),
        Cmd::Clobbers { addr } => cmd_clobbers(path, addr.as_deref()),
        Cmd::Unresolved => cmd_unresolved(path),
        Cmd::Targets { addr } => cmd_targets(path, addr.as_deref()),
        Cmd::Check => cmd_check(path),
    }
}

// ── Load helpers ──────────────────────────────────────────────────────────────

fn load_str(path: &Path) -> Result<Project> {
    let content =
        fs::read_to_string(path).with_context(|| format!("cannot read {}", path.display()))?;
    Project::from_str(&content).map_err(|e| anyhow::anyhow!("{e}"))
}

fn load_analyzed(path: &Path) -> Result<Project> {
    let path_str = path.to_str().unwrap_or("");
    let mut project = Project::from_project_file(path_str).map_err(|e| anyhow::anyhow!("{e}"))?;
    project.analyze();
    Ok(project)
}

// ── Address helpers ───────────────────────────────────────────────────────────

fn parse_addr(project: &Project, s: &str) -> Result<(SegmentIdx, u32)> {
    if let Some((seg_name, ofs_str)) = s.split_once(':') {
        let seg_idx = project
            .segment_by_name(seg_name)
            .ok_or_else(|| anyhow::anyhow!("unknown segment '{seg_name}'"))?;
        let ofs = u32::from_str_radix(ofs_str.trim_start_matches("0x"), 16)
            .map_err(|_| anyhow::anyhow!("invalid hex offset '{ofs_str}'"))?;
        return Ok((seg_idx, ofs));
    }

    let matches: Vec<Address> = project
        .attrs
        .iter()
        .filter_map(|(&addr, attr)| attr.name.as_deref().filter(|name| *name == s).map(|_| addr))
        .collect();

    match matches.len() {
        0 => bail!("no label or 'seg:ofs' address matches '{s}'"),
        1 => Ok(matches[0]),
        _ => bail!(
            "label '{s}' is ambiguous ({} matches); use seg:ofs form",
            matches.len()
        ),
    }
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

// Build the segment-register map for the instruction at `(seg_idx, ofs)`,
// matching the disassembly listing (see layout::generate_widgets_with_options):
// start from the dataflow state, then fall back to the segment's `assume ds:<seg>`
// directive when DS is still unknown so `ofs16` data references resolve to their
// labels (e.g. `[data_03810]` instead of `[3810h]`).
fn sreg_map_at(project: &Project, seg_idx: SegmentIdx, ofs: u32) -> SRegMap {
    let mut sreg_map = project
        .seg_dataflow
        .state_at(project, seg_idx, ofs)
        .map(|s| s.to_sreg_map())
        .unwrap_or(SRegMap {
            cs: Some(seg_idx),
            ..Default::default()
        });
    if sreg_map.ds.is_none() {
        for (reg, assume_seg) in &project.segments[seg_idx].assume {
            if reg.as_str() == "ds" {
                sreg_map.ds = project.segment_by_name(assume_seg);
                break;
            }
        }
    }
    sreg_map
}

fn render_addr(project: &Project, seg_idx: SegmentIdx, ofs: u32) -> String {
    let sreg_map = sreg_map_at(project, seg_idx, ofs);
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
        addr: Some((seg_idx, ofs)),
        annotate_storage: false,
    };
    let ctx = DisplayContext {
        lookup: &lookup,
        arg_fmts: [None; 2],
    };
    let mut builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);
    builder.layout();
    let n = builder.lines();
    // The instruction/data line is always the last line; comments are above it.
    let mut buf = String::new();
    builder.render(&mut buf, n.saturating_sub(1));
    buf
}

fn print_addr_block(
    project: &Project,
    seg_idx: SegmentIdx,
    ofs: u32,
    options: chani_disasm::layout::LayoutOptions,
) {
    let sreg_map = sreg_map_at(project, seg_idx, ofs);
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
        addr: Some((seg_idx, ofs)),
        annotate_storage: options.annotate_storage,
    };
    let ctx = DisplayContext {
        lookup: &lookup,
        arg_fmts: [None; 2],
    };
    let mut builder = LayoutBuilder::new_with_options(project, seg_idx, ofs, &ctx, options);
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

fn unescape_comment(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('\\') => out.push('\\'),
            Some(other) => {
                out.push('\\');
                out.push(other);
            }
            None => out.push('\\'),
        }
    }
    out
}

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
    let content =
        fs::read_to_string(path).with_context(|| format!("cannot read {}", path.display()))?;
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
            && args.target.is_empty()
            && args.r#let.is_none()
            && args.r#fn.is_none()
        {
            bail!(
                "specify at least one of --name, --type, --comment, --ofs-seg, --assume, --arg, --target, --let, --fn (or --delete)"
            );
        }

        let parsed_type = args
            .r#type
            .as_deref()
            .map(|s| {
                project
                    .parse_type_str(s)
                    .map_err(|e| anyhow::anyhow!("{e}"))
            })
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

        let mut parsed_targets: Vec<Address> = Vec::new();
        for s in &args.target {
            let s = s.trim();
            if s.is_empty() {
                continue;
            }
            parsed_targets.push(parse_addr(&project, s)?);
        }

        let struct_names: Vec<String> = project.structs.iter().map(|s| s.name.clone()).collect();
        let parsed_lets = args
            .r#let
            .as_deref()
            .map(|s| {
                let bindings =
                    chani_disasm::binding::parse_binding_list(s, &project.segments, &struct_names)
                        .map_err(|e| anyhow::anyhow!("{e}"))?;
                for b in &bindings {
                    if b.dir.is_some() {
                        bail!("--let bindings must not carry a direction (in/out/inout)");
                    }
                }
                Ok::<_, anyhow::Error>(bindings)
            })
            .transpose()?;
        let parsed_signature = args
            .r#fn
            .as_deref()
            .map(|s| {
                chani_disasm::binding::parse_binding_list(s, &project.segments, &struct_names)
                    .map_err(|e| anyhow::anyhow!("{e}"))
            })
            .transpose()?;

        let attr = project
            .attrs
            .entry((seg_idx, ofs))
            .or_insert_with(|| Attr::new((seg_idx, ofs)));

        if let Some(name) = args.name {
            attr.set_name(Some(name));
        }
        if let Some(t) = parsed_type {
            attr.r#type = Some(t);
        }
        if let Some(comment) = args.comment {
            let value = (!comment.is_empty()).then(|| unescape_comment(&comment));
            attr.set_comment(value);
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
        if !args.target.is_empty() {
            attr.targets = parsed_targets;
        }
        // An empty --let string clears; a non-empty one replaces.
        if let Some(lets) = parsed_lets {
            attr.lets = lets;
        }
        if let Some(signature) = parsed_signature {
            attr.signature = if signature.is_empty() {
                None
            } else {
                Some(signature)
            };
        }
    }

    if args.stdout {
        project.write_to(&mut io::stdout().lock())?;
    } else {
        let mut buf = Vec::new();
        project.write_to(&mut buf)?;
        fs::write(path, &buf).with_context(|| format!("cannot write {}", path.display()))?;
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
        println!(
            "  type:    {}",
            t.type_str(&project.segments, &project.structs)
        );
    }
    if let Some(name) = &attr.name {
        let auto = if attr.is_auto_label { " (auto)" } else { "" };
        println!("  name:    {name}{auto}");
    }
    if let Some(seg_idx) = attr.ofs_seg {
        println!("  ofs_seg: {}", project.segments[seg_idx].name);
    }
    if !attr.assume.is_empty() {
        let s: Vec<String> = attr
            .assume
            .iter()
            .map(|(r, seg)| format!("{r}:{seg}"))
            .collect();
        println!("  assume:  {}", s.join(" "));
    }
    if !attr.lets.is_empty() {
        println!(
            "  let:     {}",
            chani_disasm::binding::binding_list_to_string(
                &attr.lets,
                &project.segments,
                &project.structs,
            )
        );
    }
    if let Some(signature) = &attr.signature {
        println!(
            "  fn:      {}",
            chani_disasm::binding::binding_list_to_string(
                signature,
                &project.segments,
                &project.structs,
            )
        );
    }
    for (i, fmt) in attr.arg_fmts.iter().enumerate() {
        if let Some(f) = fmt {
            println!("  arg[{i}]:  {}", f.as_str());
        }
    }
    if !attr.targets.is_empty() {
        let s: Vec<String> = attr
            .targets
            .iter()
            .map(|&addr| fmt_addr(&project, addr))
            .collect();
        println!("  targets: {}", s.join(", "));
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
        let name_match = attr
            .name
            .as_deref()
            .is_some_and(|n| n.to_lowercase().contains(&pat));
        let comment_match = attr
            .comment
            .as_deref()
            .is_some_and(|c| c.to_lowercase().contains(&pat));
        if !name_match && !comment_match {
            continue;
        }
        found = true;
        let name = attr.name.as_deref().unwrap_or("");
        let auto = if attr.is_auto_label { " (auto)" } else { "" };
        let type_str = attr
            .r#type
            .as_ref()
            .map(|t| t.type_str(&project.segments, &project.structs))
            .unwrap_or_default();
        let comment_first = attr
            .comment
            .as_deref()
            .and_then(|c| c.lines().next())
            .unwrap_or("");
        println!(
            "{}  {:<24}  {:<18}  {}",
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
        let type_str = attr
            .r#type
            .as_ref()
            .map(|t| t.type_str(&project.segments, &project.structs))
            .unwrap_or_default();
        let comment_first = attr
            .comment
            .as_deref()
            .and_then(|c| c.lines().next())
            .unwrap_or("");
        println!(
            "{}  {:<24}  {:<18}  {}",
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
    let sreg_map = sreg_map_at(project, seg_idx, ofs);
    let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|a| a.ofs_seg);
    let lookup = ProjectLookup {
        project,
        sreg_map,
        register_file: None,
        default_seg: ofs_seg,
        addr: Some((seg_idx, ofs)),
        annotate_storage: false,
    };
    let ctx = DisplayContext {
        lookup: &lookup,
        arg_fmts: [None; 2],
    };
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
        if let Some(last) = groups.last_mut()
            && start <= last.1
        {
            last.1 = last.1.max(end);
            continue;
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
            if ofs16_points_to(
                project.bytes_at_seg(src_addr.0, src_addr.1),
                dt,
                &project,
                ofs_seg,
                target,
            ) {
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
                .map(|i| {
                    if i.opcode == Opcode::Call {
                        "call"
                    } else {
                        "jmp "
                    }
                })
                .unwrap_or("?   ");
            let line = render_addr(&project, src.0, src.1);
            println!("    [{kind}]  {line}",);
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
            if seg != Some(target.0) {
                return false;
            }
            let lo = bytes.first().copied().unwrap_or(0) as u32;
            let hi = bytes.get(1).copied().unwrap_or(0) as u32;
            (lo | (hi << 8)) == target.1
        }
        DataType::Scalar(ScalarDataType::U16) => {
            let seg = fallback_seg;
            if seg != Some(target.0) {
                return false;
            }
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

fn cmd_func(path: &Path, args: &FuncArgs) -> Result<()> {
    let project = load_analyzed(path)?;
    let addr_str = args.addr.as_str();
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;
    let options = chani_disasm::layout::LayoutOptions {
        show_call_state: args.show_call_state,
        annotate_storage: args.annotate_storage,
    };

    // Snap to the basic block containing the address.
    let block_start_addr: Address = project
        .blocks
        .block_containing(seg_idx, ofs)
        .map(|b| (b.seg_idx, b.start))
        .ok_or_else(|| anyhow::anyhow!("{} is not inside a decoded basic block", addr_str))?;

    // Resolve the function via Project::functions:
    //   1. block_start_addr itself is a function entry → use that function;
    //   2. otherwise pick a function whose block set contains this block.
    //      If several match (shared block), prefer the one whose entry has
    //      the largest address ≤ block_start_addr; emit a comment listing the
    //      other candidates so the user can rerun against an exact entry.
    //   3. if no function claims the block, fall back to the on-the-fly CFG
    //      walk so orphan-but-reachable blocks still render.
    let (entry_addr, block_addrs, alt_entries) =
        if let Some(f) = project.functions.function_at(block_start_addr) {
            (f.entry, f.blocks.clone(), Vec::new())
        } else {
            let mut candidates: Vec<Address> = project
                .functions
                .functions_with_block(block_start_addr)
                .map(|f| f.entry)
                .collect();
            if let Some(&best) = candidates
                .iter()
                .filter(|&&e| e <= block_start_addr)
                .max()
                .or_else(|| candidates.iter().min())
            {
                candidates.retain(|&e| e != best);
                let f = project.functions.function_at(best).unwrap();
                (f.entry, f.blocks.clone(), candidates)
            } else {
                // No function owns this block — fall back to the legacy walk.
                let blocks = project.blocks.function_blocks(&project, block_start_addr);
                if blocks.is_empty() {
                    bail!("no basic blocks found at {addr_str}");
                }
                (block_start_addr, blocks, Vec::new())
            }
        };

    let label = project
        .name_at(entry_addr.0, entry_addr.1)
        .unwrap_or("(unnamed)");
    println!(
        "; function: {} at {}",
        label,
        fmt_addr(&project, entry_addr)
    );
    if !alt_entries.is_empty() {
        let alt_str = alt_entries
            .iter()
            .map(|&a| {
                let n = project.name_at(a.0, a.1).unwrap_or("(unnamed)");
                format!("{} {n}", fmt_addr(&project, a))
            })
            .collect::<Vec<_>>()
            .join(", ");
        println!(
            "; (block also belongs to: {alt_str}; rerun with an explicit entry to view those)"
        );
    }
    println!();

    let mut prev_end: Option<u32> = None;
    for &(bseg, bstart) in &block_addrs {
        let Some(block) = project.blocks.block_at(bseg, bstart) else {
            continue;
        };

        // Blank separator between non-contiguous blocks
        if prev_end.is_some_and(|e| e != bstart) {
            println!();
        }
        prev_end = Some(block.end);

        let seg = &project.segments[bseg];
        let mut ofs = bstart;
        while ofs < block.end {
            print_addr_block(&project, bseg, ofs, options);
            let Some(next) = seg.addr_attributes.next(ofs) else {
                break;
            };
            if next >= block.end {
                break;
            }
            ofs = next;
        }
        // Always render the last instruction (the one at or just before block.end)
        // addr_attributes.next() may skip past it; render block.end - op_len separately
        // But the loop above already handles it via the `next < block.end` condition.
        // A fall-through into the next function is rendered by the layout pass
        // (LayoutBuilder::layout_fall_through_out), so it appears here too.
    }

    Ok(())
}

fn function_blocks(project: &Project, entry: Address) -> Vec<Address> {
    project.blocks.function_blocks(project, entry)
}

// ── cmd_state ─────────────────────────────────────────────────────────────────

fn cmd_state(path: &Path, addr_str: &str) -> Result<()> {
    use chani_disasm::seg_dataflow::SegVal;

    let project = load_analyzed(path)?;
    let (seg_idx, ofs) = parse_addr(&project, addr_str)?;
    let addr = (seg_idx, ofs);

    let Some(state) = project.seg_dataflow.state_at(&project, seg_idx, ofs) else {
        println!(
            "{}  <no dataflow state at this address>",
            fmt_addr(&project, addr)
        );
        return Ok(());
    };

    let fmt = |v: &SegVal| -> String {
        match v {
            SegVal::Known(idx) => project.segments[*idx].name.clone(),
            SegVal::Unknown => "?".to_string(),
        }
    };

    println!(
        "{}  (state immediately before this instruction)",
        fmt_addr(&project, addr)
    );
    for (i, name) in [(0u8, "es"), (1, "cs"), (2, "ss"), (3, "ds")] {
        println!("  {name} = {}", fmt(&state.sregs[i as usize]));
    }
    let gp_names = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
    for (i, name) in gp_names.iter().enumerate() {
        if let SegVal::Known(idx) = &state.gpregs[i] {
            println!("  {name} = {}", project.segments[*idx].name);
        }
    }

    Ok(())
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
        .filter(|&src| decode_at(&project, src.0, src.1).is_some_and(|i| i.opcode == Opcode::Call))
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

    let label = project
        .name_at(seg_idx, entry_block_start)
        .unwrap_or("(unnamed)");
    println!(
        "callees of {}  {}",
        fmt_addr(&project, (seg_idx, entry_block_start)),
        label
    );

    let block_addrs = function_blocks(&project, (seg_idx, entry_block_start));

    let mut callees: Vec<Address> = Vec::new();
    let mut seen: BTreeSet<Address> = BTreeSet::new();

    for &(bseg, bstart) in &block_addrs {
        let Some(block) = project.blocks.block_at(bseg, bstart) else {
            continue;
        };

        let last_ofs = project.segments[block.seg_idx]
            .addr_attributes
            .prev(block.end)
            .filter(|&p| p >= block.start)
            .unwrap_or(block.start);

        let last_is_call =
            decode_at(&project, bseg, last_ofs).is_some_and(|i| i.opcode == Opcode::Call);

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

// ── cmd_clobbers ──────────────────────────────────────────────────────────────

fn cmd_clobbers(path: &Path, addr_str: Option<&str>) -> Result<()> {
    let project = load_analyzed(path)?;

    match addr_str {
        Some(s) => {
            let (seg_idx, ofs) = parse_addr(&project, s)?;
            let entry = (seg_idx, ofs);
            if !project.function_summary.contains_key(&entry) {
                bail!("{s} is not a known function entry");
            }
            print_clobbers_for(&project, entry);
        }
        None => {
            let entries: Vec<Address> = project.function_summary.keys().copied().collect();
            let mut first = true;
            for entry in entries {
                let records =
                    chani_disasm::function_summary::trace_function_clobbers(&project, entry);
                if records.is_empty() {
                    continue;
                }
                if !first {
                    println!();
                }
                first = false;
                print_clobber_header(&project, entry);
                for rec in &records {
                    print_clobber_line(&project, rec);
                }
            }
        }
    }

    Ok(())
}

fn print_clobbers_for(project: &Project, entry: Address) {
    print_clobber_header(project, entry);
    let records = chani_disasm::function_summary::trace_function_clobbers(project, entry);
    if records.is_empty() {
        println!("  (none)");
    } else {
        for rec in &records {
            print_clobber_line(project, rec);
        }
    }
}

fn print_clobber_header(project: &Project, entry: Address) {
    let label = project.name_at(entry.0, entry.1).unwrap_or("(unnamed)");
    let summary_lines = project
        .function_summary
        .get(&entry)
        .map(|s| chani_disasm::function_summary::render_summary_comment_lines(project, s))
        .unwrap_or_else(|| vec!["preserves: -".to_string()]);
    let head = summary_lines.first().cloned().unwrap_or_default();
    println!(
        "clobbers in {}  {label}  ({head})",
        fmt_addr(project, entry)
    );
    for line in summary_lines.iter().skip(1) {
        println!("  ({line})");
    }
}

fn print_clobber_line(project: &Project, rec: &chani_disasm::function_summary::ClobberRecord) {
    let mut tags = Vec::new();
    if rec.clobbers_ds {
        tags.push("DS");
    }
    if rec.clobbers_es {
        tags.push("ES");
    }
    if rec.clobbers_ss {
        tags.push("SS");
    }
    if rec.stack_invalidated {
        tags.push("stack");
    }
    let tag_str = tags.join(",");
    let line = render_addr(project, rec.addr.0, rec.addr.1);
    println!("  [{tag_str}]  {line}    ; {}", rec.reason);
}

// ── cmd_unresolved ────────────────────────────────────────────────────────────

fn cmd_unresolved(path: &Path) -> Result<()> {
    let project = load_analyzed(path)?;
    let mut found = 0usize;
    for (seg_idx, seg) in project.segments.indexed_iter() {
        let attrs = &seg.addr_attributes;
        let base = attrs.base();
        let mut ofs_opt = if attrs.is_op(base) {
            Some(base)
        } else {
            attrs.next(base)
        };
        while let Some(ofs) = ofs_opt {
            if !attrs.is_op(ofs) {
                ofs_opt = attrs.next(ofs);
                continue;
            }
            if let Some(inst) = decode_at(&project, seg_idx, ofs) {
                let is_indirect = matches!(inst.opcode, Opcode::Call | Opcode::Jmp)
                    && inst.branch_destination().is_none();
                if is_indirect {
                    let has_manual = project
                        .attr_at(seg_idx, ofs)
                        .is_some_and(|a| !a.targets.is_empty());
                    if !has_manual {
                        let line = render_addr(&project, seg_idx, ofs);
                        println!("{line}");
                        found += 1;
                    }
                }
            }
            ofs_opt = attrs.next(ofs);
        }
    }
    if found == 0 {
        println!("(none)");
    }
    Ok(())
}

// ── cmd_targets ───────────────────────────────────────────────────────────────

fn cmd_targets(path: &Path, addr_opt: Option<&str>) -> Result<()> {
    let project = load_analyzed(path)?;

    let entries: Vec<(Address, Vec<Address>)> = if let Some(addr_str) = addr_opt {
        let addr = parse_addr(&project, addr_str)?;
        match project.attr_at(addr.0, addr.1) {
            Some(a) if !a.targets.is_empty() => vec![(addr, a.targets.clone())],
            _ => Vec::new(),
        }
    } else {
        project
            .attrs
            .iter()
            .filter(|(_, a)| !a.targets.is_empty())
            .map(|(&addr, a)| (addr, a.targets.clone()))
            .collect()
    };

    if entries.is_empty() {
        println!("(none)");
        return Ok(());
    }

    for (i, (addr, mut targets)) in entries.into_iter().enumerate() {
        if i > 0 {
            println!();
        }
        targets.sort_by(|a, b| {
            project.segments[a.0]
                .name
                .cmp(&project.segments[b.0].name)
                .then(a.1.cmp(&b.1))
        });
        let line = render_addr(&project, addr.0, addr.1);
        println!("{line}");
        for target in targets {
            let label = project
                .resolve_label(target.0, target.1)
                .unwrap_or_default();
            println!("    -> {}  {label}", fmt_addr(&project, target));
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
        if let Some(AttrType::Data(dt)) = &attr.r#type
            && contains_unresolved_ofs16(dt)
            && attr.ofs_seg.is_none()
        {
            println!(
                "warn: {}  ofs16 without ofs_seg (segment unknown)",
                fmt_addr(&project, addr)
            );
            issues += 1;
        }
    }

    // 2. code attrs overlapping data-typed attrs
    let data_addrs: Vec<(Address, usize)> = project
        .attrs
        .iter()
        .filter_map(|(&addr, attr)| {
            let dt = attr.r#type.as_ref()?.as_data()?;
            Some((addr, dt.byte_size(&[], &project.structs).max(1)))
        })
        .collect();

    'outer: for (&code_addr, attr) in &project.attrs {
        if attr.r#type != Some(AttrType::Code) {
            continue;
        }
        for &(data_addr, data_size) in &data_addrs {
            if data_addr.0 != code_addr.0 {
                continue;
            }
            if code_addr.1 >= data_addr.1 && code_addr.1 < data_addr.1 + data_size as u32 {
                println!(
                    "warn: {}  code attr overlaps data attr at {}",
                    fmt_addr(&project, code_addr),
                    fmt_addr(&project, data_addr)
                );
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
                println!(
                    "warn: {}  assume references unknown segment '{seg_name}'",
                    fmt_addr(&project, addr)
                );
                issues += 1;
            }
        }
    }

    if issues == 0 {
        println!(
            "ok: no issues found ({} attrs checked)",
            project.attrs.len()
        );
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
        DataType::Ptr(inner) => contains_unresolved_ofs16(inner),
        DataType::Tuple(members) => members.iter().any(contains_unresolved_ofs16),
    }
}

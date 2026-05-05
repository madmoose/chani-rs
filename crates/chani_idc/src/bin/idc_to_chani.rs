use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

use chani_idc::{IdcAttr, IdcDatabase, IdcStruct, parse_idc};

#[derive(Parser)]
struct Args {
    /// IDC file exported by IDA Pro
    input: PathBuf,
    /// Only output attr lines (skip struct definitions)
    #[arg(long)]
    attrs_only: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let bytes = fs::read(&args.input)?;
    let (content, _, _) = encoding_rs::WINDOWS_1252.decode(&bytes);
    let db = parse_idc(&content)?;

    let stdout = io::stdout();
    let mut out = stdout.lock();

    if !args.attrs_only {
        write_structs(&mut out, &db)?;
    }
    write_attrs(&mut out, &db)?;

    Ok(())
}

// ── Struct output ─────────────────────────────────────────────────────────────

fn write_structs(w: &mut impl Write, db: &IdcDatabase) -> io::Result<()> {
    let mut structs: Vec<&IdcStruct> = db.structs.values().collect();
    structs.sort_by_key(|s| s.name.as_str());

    for st in structs {
        writeln!(w, "struct[{}]:", st.name)?;
        for m in &st.members {
            let ty = struct_member_type_str(m.flag, m.size);
            writeln!(w, "    {} = {}", m.name, ty)?;
        }
        writeln!(w, "end")?;
        writeln!(w)?;
    }
    Ok(())
}

/// Derive a `.chani` type string for an IDC struct member from its IDA flag and byte size.
///
/// IDA flag high-nibble encoding:
///   0x0....... = byte (u8)
///   0x1....... = word (u16)
///   0x2....... = dword (u32)
///   0x5....... = ASCII char buffer (u8)
///   0x6....... = struct embedding
/// Sizes larger than the primitive width fall back to `u8[N]`.
fn struct_member_type_str(flag: u32, size: u32) -> String {
    match (flag & 0x70000000, size) {
        (0x10000000, 2) => "u16".into(),
        (0x20000000, 4) => "u32".into(),
        (_, 1) => "u8".into(),
        (_, n) => format!("u8[{n}]"),
    }
}

// ── Attr output ───────────────────────────────────────────────────────────────

fn write_attrs(w: &mut impl Write, db: &IdcDatabase) -> io::Result<()> {
    let mut addrs: Vec<u32> = db.attrs.keys().copied().collect();
    addrs.sort_unstable();

    for addr in addrs {
        let attr = &db.attrs[&addr];
        if let Some(seg) = db.segment_for_addr(addr) {
            let ofs = addr - seg.start;
            let key = format!("{}:{:04x}", seg.name, ofs);
            let line = build_attr_line(attr, db);
            if !line.is_empty() {
                writeln!(w, "attr[{key}]: {line}")?;
            }
        }
    }
    Ok(())
}

/// Build the inline property string for a single attr line.
/// Returns an empty string if there is nothing worth emitting.
fn build_attr_line(attr: &IdcAttr, db: &IdcDatabase) -> String {
    let mut props: Vec<String> = Vec::new();

    // ── type ─────────────────────────────────────────────────────────────────
    let (type_str, ofs_seg_name) = synthesize_type(attr, db);
    if let Some(t) = type_str {
        props.push(format!("type = {t}"));
    }

    // ── name ─────────────────────────────────────────────────────────────────
    if let Some(name) = &attr.name {
        props.push(format!("name = {}", encode_value(name)));
    }

    // ── ofs_seg ───────────────────────────────────────────────────────────────
    if let Some(seg_name) = ofs_seg_name {
        props.push(format!("ofs_seg = {seg_name}"));
    }

    // ── comment ───────────────────────────────────────────────────────────────
    // Prefer the regular (non-repeatable) comment; fall back to the repeat comment.
    let comment = attr.comment.as_deref().or(attr.repeat_comment.as_deref());
    if let Some(c) = comment {
        props.push(format!("comment = {}", encode_value(c)));
    }

    props.join("; ")
}

/// Derive the chani `type` string and optional `ofs_seg` segment name from an IDC attr.
fn synthesize_type(attr: &IdcAttr, db: &IdcDatabase) -> (Option<String>, Option<String>) {
    if attr.is_code {
        let ofs_seg = ofs_target_seg(attr.ofs_target, db);
        return (Some("code".into()), ofs_seg);
    }

    // Struct-typed annotation (from OpStroffEx)
    if let Some(struc_id) = attr.struc_id
        && let Some(st) = db.structs.get(&struc_id)
    {
        let t = match attr.array_size {
            Some(n) => format!("[{}; {n}]", st.name),
            None => st.name.clone(),
        };
        return (Some(t), None);
    }

    // Scalar / array data type (from MakeByte / MakeWord / MakeDword + MakeArray)
    if let Some(base_type) = &attr.data_type {
        // u16 with an OpOff base → ofs16; the base tells us which segment
        let (elem, ofs_seg) = if base_type == "u16" {
            match ofs_target_seg(attr.ofs_target, db) {
                Some(seg_name) => ("ofs16".to_owned(), Some(seg_name)),
                None => (base_type.clone(), None),
            }
        } else {
            (base_type.clone(), None)
        };

        let t = match attr.array_size {
            Some(n) => format!("[{elem}; {n}]"),
            None => elem,
        };
        return (Some(t), ofs_seg);
    }

    (None, None)
}

/// Resolve an IDC `OpOff` base address to the name of the segment that contains it.
fn ofs_target_seg(ofs_target: Option<u32>, db: &IdcDatabase) -> Option<String> {
    let target = ofs_target?;
    db.segment_for_addr(target).map(|s| s.name.clone())
}

// ── Value encoding (matches chani_datafile::writer::encode_value) ─────────────

fn encode_value(v: &str) -> String {
    if v.contains('\n') {
        let mut s = String::from("[[[");
        for line in v.lines() {
            s.push('\n');
            s.push_str("    ");
            s.push_str(line);
        }
        s.push_str("\n]]]");
        s
    } else if v.contains(';') {
        format!("[[[{v}]]]")
    } else {
        v.to_owned()
    }
}

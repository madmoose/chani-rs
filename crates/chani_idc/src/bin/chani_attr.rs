use std::fs;
use std::io;
use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use chani_disasm::project::{Assumes, Attr, Project, SegmentIdx};
use clap::Parser;

#[derive(Parser)]
#[command(about = "Set annotations on an address in a .chani project file")]
struct Args {
    /// Path to the .chani project file
    project: PathBuf,

    /// Address in the form seg:ofs (hex offset), e.g. seg001:22cb
    addr: String,

    /// Set the label name (empty string clears it)
    #[arg(long)]
    name: Option<String>,

    /// Set the type (e.g. "code", "u16", "dec(u16)", "[u8; 16]")
    #[arg(long = "type", value_name = "TYPE")]
    r#type: Option<String>,

    /// Set the comment (empty string clears it)
    #[arg(long)]
    comment: Option<String>,

    /// Remove the attr entirely (ignores --name, --type, --comment)
    #[arg(long)]
    delete: bool,

    /// Write output to stdout instead of modifying the file in place
    #[arg(long)]
    stdout: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let content = fs::read_to_string(&args.project)
        .with_context(|| format!("cannot read {}", args.project.display()))?;

    let mut project = Project::from_str(&content).map_err(|e| anyhow::anyhow!("{e}"))?;

    let (seg_idx, ofs) = parse_addr(&project, &args.addr).map_err(|e| anyhow::anyhow!("{e}"))?;

    if args.delete {
        project.attrs.remove(&(seg_idx, ofs));
    } else {
        if args.name.is_none() && args.r#type.is_none() && args.comment.is_none() {
            bail!("specify at least one of --name, --type, --comment (or --delete)");
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

        let attr = project.attrs.entry((seg_idx, ofs)).or_insert_with(|| Attr {
            addr: (seg_idx, ofs),
            r#type: None,
            name: None,
            is_auto_label: false,
            ofs_seg: None,
            comment: None,
            assume: Assumes::default(),
            arg_fmts: [None; 2],
            targets: Vec::new(),
        });

        if let Some(name) = args.name {
            attr.name = if name.is_empty() {
                None
            } else {
                Some(name.into())
            };
            attr.is_auto_label = false;
        }

        if let Some(attr_type) = parsed_type {
            attr.r#type = Some(attr_type);
        }

        if let Some(comment) = args.comment {
            attr.comment = if comment.is_empty() {
                None
            } else {
                Some(comment)
            };
        }
    }

    if args.stdout {
        project.write_to(&mut io::stdout().lock())?;
    } else {
        let mut buf = Vec::new();
        project.write_to(&mut buf)?;
        fs::write(&args.project, &buf)
            .with_context(|| format!("cannot write {}", args.project.display()))?;
    }

    Ok(())
}

fn parse_addr(project: &Project, s: &str) -> Result<(SegmentIdx, u32), String> {
    let (seg_name, ofs_str) = s
        .split_once(':')
        .ok_or_else(|| format!("address must be 'seg:ofs', got '{s}'"))?;

    let seg_idx = project
        .segment_by_name(seg_name)
        .ok_or_else(|| format!("unknown segment '{seg_name}'"))?;

    let ofs = u32::from_str_radix(ofs_str.trim_start_matches("0x"), 16)
        .map_err(|_| format!("invalid hex offset '{ofs_str}'"))?;

    Ok((seg_idx, ofs))
}

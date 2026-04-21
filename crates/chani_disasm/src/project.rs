use std::io;
use std::path::Path;
use std::{collections::BTreeMap, fs};

use chani_datafile::ast::{self, Dict, Document, Item};
use chani_datafile::parser;

use crate::basic_block::{BasicBlock, BasicBlockMap};
use crate::branch_map::BranchMap;
use crate::data_type::{CompositeDataType, DataType, ScalarDataType, StructDef, StructField};
use crate::work_queue::WorkQueue;
use crate::{SmallString, SymbolLookup, decode};
use crate::{address_attributes::AddressAttributes, exe_mz::ExeMz};

#[derive(Debug, Clone)]
pub struct Hash {
    pub bytes: Vec<u8>,
}

pub type Segments = Vec<Segment>;
pub type Structs = Vec<StructDef>;

// ── Binary loading ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryFormat {
    Exe,
    Com,
    Bin,
}

impl BinaryFormat {
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "exe" => Ok(BinaryFormat::Exe),
            "com" => Ok(BinaryFormat::Com),
            "bin" => Ok(BinaryFormat::Bin),
            _ => Err(format!("unknown binary format '{}'", s)),
        }
    }

    fn from_extension(path: &str) -> Self {
        match Path::new(path)
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.to_ascii_lowercase())
            .as_deref()
        {
            Some("exe") => BinaryFormat::Exe,
            Some("com") => BinaryFormat::Com,
            _ => BinaryFormat::Bin,
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            BinaryFormat::Exe => "exe",
            BinaryFormat::Com => "com",
            BinaryFormat::Bin => "bin",
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryDef {
    pub name: SmallString,
    pub format: BinaryFormat,
    pub path: String,
    pub hash: Option<Hash>,
    pub load: Option<(SegmentIdx, u32)>,
}

#[derive(Debug, Clone)]
pub struct BinImage {
    pub seg_idx: SegmentIdx,
    /// Within-segment offset where byte 0 of the file is placed.
    pub load_offset: u32,
    pub data: Vec<u8>,
}

// ── Core types ────────────────────────────────────────────────────────────────

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Project {
    pub project: SmallString,
    pub binaries: Vec<BinaryDef>,
    pub arch: SmallString,
    pub segments: Segments,
    pub structs: Structs,
    pub attrs: BTreeMap<(SegmentIdx, u32), Attr>,
    pub exe: Option<ExeMz>,
    pub images: Vec<BinImage>,
    pub branches: BranchMap,
    pub blocks: BasicBlockMap,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Segment {
    pub name: SmallString,
    pub r#type: Option<SmallString>,
    pub start: Option<u32>,
    pub end: Option<u32>,
    pub addr_attributes: AddressAttributes,
}

impl Segment {
    pub fn size(&self) -> usize {
        self.end
            .unwrap_or(0)
            .saturating_sub(self.start.unwrap_or(0)) as usize
    }
}

// ── Attribute / type system ───────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UnresolvedAttrType {
    Code,
    U8,
    U16,
    U32,
    Char(usize),
    Ofs16(Option<String>),
    Struct(String),
    Array {
        elem: Box<UnresolvedAttrType>,
        count: usize,
    },
    CStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct UnresolvedStructDef {
    pub fields: Vec<UnresolvedStructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct UnresolvedStructField {
    pub name: SmallString,
    pub r#type: UnresolvedAttrType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrType {
    Code,
    Data(DataType),
}

#[derive(Debug)]
pub struct FlatField {
    pub name: String,
    pub r#type: ScalarDataType,
    pub offset: usize,
}

impl AttrType {
    pub fn type_str(&self, segments: &[Segment], structs: &Structs) -> String {
        match self {
            AttrType::Code => "code".to_owned(),
            AttrType::Data(d) => d.type_str(segments, structs),
        }
    }

    pub fn byte_size(&self, bytes: &[u8], structs: &Structs) -> usize {
        match self {
            AttrType::Code => unreachable!(),
            AttrType::Data(d) => d.byte_size(bytes, structs),
        }
    }

    pub fn as_data(&self) -> Option<&DataType> {
        match self {
            AttrType::Code => None,
            AttrType::Data(data_type) => Some(data_type),
        }
    }
}

type SegmentIdx = usize;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attr {
    pub addr: (SegmentIdx, u32),
    pub r#type: Option<AttrType>,
    pub name: Option<SmallString>,
    pub ofs_seg: Option<SegmentIdx>,
    pub comment: Option<String>,
    /// True if this attr was auto-generated and should not be saved.
    pub auto_label: bool,
}

// ── Project implementation ────────────────────────────────────────────────────

impl Project {
    /// Return a slice of bytes starting at `(seg_idx, ofs)`.
    ///
    /// For bin/com-backed segments this reads from the loaded `BinImage`.
    /// For EXE-backed segments this reads from the flat `exe.image`.
    pub fn bytes_at_seg(&self, seg_idx: usize, ofs: u32) -> &[u8] {
        for img in &self.images {
            if img.seg_idx == seg_idx {
                return ofs
                    .checked_sub(img.load_offset)
                    .and_then(|o| img.data.get(o as usize..))
                    .unwrap_or(&[]);
            }
        }
        let start = self.segments[seg_idx].start.unwrap_or(0) as usize;
        self.exe
            .as_ref()
            .and_then(|e| e.image.get(start + ofs as usize..))
            .unwrap_or(&[])
    }

    pub fn from_project_file(path: &str) -> std::result::Result<Self, String> {
        let content = fs::read_to_string(path).map_err(|e| e.to_string())?;
        let tokens = parser::parse(&content)?;
        let doc = ast::Document::from_tokens(tokens)?;
        let mut project = Project::from_document(doc)?;

        let base = Path::new(path).parent().unwrap_or(Path::new("."));

        for i in 0..project.binaries.len() {
            let file_path = base
                .join(&project.binaries[i].path)
                .to_string_lossy()
                .into_owned();
            let data = fs::read(&file_path).map_err(|e| format!("{file_path}: {e}"))?;

            // TODO: verify hash when present

            match project.binaries[i].format {
                BinaryFormat::Exe => {
                    let exe = ExeMz::load(&data, file_path).map_err(|e| e.to_string())?;
                    project.exe = Some(exe);
                }
                BinaryFormat::Bin | BinaryFormat::Com => {
                    let (seg_idx, load_offset) = project.binaries[i].load.unwrap();
                    let seg = &mut project.segments[seg_idx];
                    seg.start.get_or_insert(load_offset);
                    seg.end
                        .get_or_insert(load_offset + data.len() as u32);
                    let size = seg.end.unwrap() - seg.start.unwrap();
                    seg.addr_attributes =
                        AddressAttributes::new_with_base(load_offset, size as usize);
                    project.images.push(BinImage {
                        seg_idx,
                        load_offset,
                        data,
                    });
                }
            }
        }

        Ok(project)
    }

    pub fn from_exe_file(exe_path: &str) -> std::result::Result<Self, String> {
        let data = fs::read(exe_path).map_err(|e| e.to_string())?;
        let exe = ExeMz::load(&data, exe_path.to_owned()).map_err(|e| e.to_string())?;
        let segments = make_segments(&exe);

        let binary = BinaryDef {
            name: "exe".into(),
            format: BinaryFormat::Exe,
            path: exe_path.to_owned(),
            hash: None,
            load: None,
        };

        Ok(Project {
            project: SmallString::new(),
            binaries: vec![binary],
            arch: SmallString::new(),
            segments,
            structs: Vec::new(),
            attrs: BTreeMap::new(),
            exe: Some(exe),
            images: Vec::new(),
            branches: BranchMap::new(),
            blocks: BasicBlockMap::new(),
        })
    }

    fn from_document(doc: Document) -> Result<Self, String> {
        if doc.dicts.len() != 1 {
            return Err(format!(
                "expected exactly one top-level dict, found {}",
                doc.dicts.len()
            ));
        }
        let dict = doc.dicts.into_iter().next().unwrap();
        if dict.name != "project" {
            return Err(format!(
                "expected top-level 'project' dict, found '{}'",
                dict.name
            ));
        }
        Self::from_dict(dict)
    }

    fn from_dict(dict: Dict) -> Result<Self, String> {
        let project = dict.key.clone();
        let mut arch = SmallString::new();

        let mut binary_dicts: Vec<&Dict> = Vec::new();
        let mut segment_dicts: Vec<&Dict> = Vec::new();
        let mut struct_dicts: Vec<&Dict> = Vec::new();
        let mut attr_dicts: Vec<&Dict> = Vec::new();
        let mut unresolved_structs: BTreeMap<SmallString, UnresolvedStructDef> = BTreeMap::new();
        let mut attrs = BTreeMap::new();

        // Pass 1: collect dicts, parse simple properties.
        for item in &dict.items {
            match item {
                Item::Property { key, value } => match key.as_str() {
                    "arch" => arch = value.clone(),
                    _ => return Err(format!("unknown key '{}' in project '{}'", key, project)),
                },
                Item::Dict(nested) => match nested.name.as_str() {
                    "binary" => binary_dicts.push(nested),
                    "segment" => segment_dicts.push(nested),
                    "struct" => struct_dicts.push(nested),
                    "attr" => attr_dicts.push(nested),
                    _ => return Err(format!("invalid dict '{}'", nested.name)),
                },
            }
        }

        // Pass 1.5: build Segment vec.
        let mut segments: Vec<Segment> = Vec::new();
        for d in &segment_dicts {
            segments.push(parse_segment(d)?);
        }

        // Resolve binary dicts (needs segments for `load` references).
        let mut binaries: Vec<BinaryDef> = Vec::new();
        for d in binary_dicts {
            let unresolved = parse_binary_def(d)?;
            let resolved = resolve_binary_def(unresolved, &segments)?;
            binaries.push(resolved);
        }

        // Pass 2: parse struct dicts.
        for d in struct_dicts {
            let (name, def) = parse_struct(d)?;
            unresolved_structs.insert(name, def);
        }
        validate_no_struct_cycles(&unresolved_structs)?;

        // Pass 3: resolve struct/segment names to indices.
        let structs = resolve_structs(unresolved_structs, &segments)?;
        let struct_names: Vec<SmallString> = structs.iter().map(|s| s.name.clone()).collect();

        // Pass 4: parse attr dicts.
        for d in attr_dicts {
            let attr = parse_attr(d, &segments, &struct_names)?;
            attrs.insert(attr.addr, attr);
        }

        Ok(Project {
            project,
            binaries,
            arch,
            segments,
            structs,
            attrs,
            exe: Default::default(),
            images: Vec::new(),
            branches: BranchMap::new(),
            blocks: BasicBlockMap::new(),
        })
    }

    pub fn write_to(&self, w: &mut impl io::Write) -> io::Result<()> {
        writeln!(w, "project[{}]:", self.project)?;
        writeln!(w)?;
        writeln!(w, "arch = {}", self.arch)?;
        writeln!(w)?;

        for bin in &self.binaries {
            writeln!(w, "binary[{}]:", bin.name)?;
            writeln!(w, "    format = {}", bin.format.as_str())?;
            writeln!(w, "    path   = {}", bin.path)?;
            if let Some(hash) = &bin.hash {
                let hex: String = hash.bytes.iter().map(|b| format!("{b:02x}")).collect();
                writeln!(w, "    hash   = sha1:{hex}")?;
            }
            if let Some((seg_idx, ofs)) = bin.load {
                writeln!(w, "    load   = {}:{ofs:04x}", self.segments[seg_idx].name)?;
            }
            writeln!(w, "end")?;
            writeln!(w)?;
        }

        if !self.segments.is_empty() {
            for (seg_idx, seg) in self.segments.iter().enumerate() {
                let bin_backed = self.images.iter().any(|img| img.seg_idx == seg_idx);
                let mut props: Vec<String> = Vec::new();
                if let Some(t) = &seg.r#type {
                    props.push(format!("type = {t}"));
                }
                if !bin_backed {
                    if let Some(start) = seg.start {
                        props.push(format!("start = {}", fmt_u32(start)));
                    }
                    if let Some(end) = seg.end {
                        props.push(format!("end = {}", fmt_u32(end)));
                    }
                }
                write!(w, "segment[{}]:", seg.name)?;
                if !props.is_empty() {
                    write!(w, " {}", props.join("; "))?;
                }
                writeln!(w)?;
            }
            writeln!(w)?;
        }

        if !self.structs.is_empty() {
            for def in &self.structs {
                writeln!(w, "struct[{}]:", def.name)?;
                let field_width = def.fields.iter().map(|f| f.name.len()).max().unwrap_or(0);
                for field in &def.fields {
                    writeln!(
                        w,
                        "    {:<field_width$} = {}",
                        field.name,
                        field.r#type.type_str(&self.segments, &self.structs)
                    )?;
                }
                writeln!(w, "end")?;
                writeln!(w)?;
            }
        }

        if !self.attrs.is_empty() {
            for attr in self.attrs.values() {
                if attr.auto_label
                    && attr.r#type.is_none()
                    && attr.ofs_seg.is_none()
                    && attr.comment.is_none()
                {
                    continue;
                }
                let mut props: Vec<String> = Vec::new();
                if let Some(t) = &attr.r#type {
                    props.push(format!(
                        "type = {}",
                        t.type_str(&self.segments, &self.structs)
                    ));
                }
                if let Some(ofs_seg) = attr.ofs_seg {
                    props.push(format!("ofs_seg = {}", self.segments[ofs_seg].name));
                }
                if !attr.auto_label {
                    if let Some(name) = &attr.name {
                        props.push(format!("name = {name}"));
                    }
                }
                if let Some(comment) = &attr.comment {
                    props.push(format!("comment = {comment}"));
                }
                if !props.is_empty() {
                    write!(
                        w,
                        "attr[{}:{:04x}]:",
                        self.segments[attr.addr.0].name, attr.addr.1
                    )?;
                    write!(w, " {}", props.join("; "))?;
                    writeln!(w)?;
                }
            }
            writeln!(w)?;
        }

        writeln!(w, "end")
    }

    pub fn analyze(&mut self) {
        self.mark_data_attributes();
        self.disassemble();
        self.build_basic_blocks();
        self.generate_auto_labels();
    }

    /// Mark all data-typed attributes in `addr_attributes` with their byte extents.
    pub fn mark_data_attributes(&mut self) {
        // Phase 1: collect (seg_idx, ofs, DataType) — clone to release the attrs borrow.
        let attrs: Vec<(usize, u32, DataType)> = self
            .attrs
            .values()
            .filter_map(|attr| {
                if let Some(AttrType::Data(ref d)) = attr.r#type {
                    let (seg_idx, ofs) = attr.addr;
                    Some((seg_idx, ofs, d.clone()))
                } else {
                    None
                }
            })
            .collect();

        // Phase 2: compute actual byte sizes from real binary data (all immutable borrows).
        let spans: Vec<(usize, u32, usize)> = attrs
            .into_iter()
            .map(|(seg_idx, ofs, data_type)| {
                let bytes = self.bytes_at_seg(seg_idx, ofs);
                let size = data_type.byte_size(bytes, &self.structs).max(1);
                (seg_idx, ofs, size)
            })
            .collect();

        // Phase 3: mark (mutable borrow).
        for (seg_idx, ofs, size) in spans {
            self.segments[seg_idx]
                .addr_attributes
                .mark_as_data(ofs, size as u32);
        }
    }

    /// Run the recursive disassembly pass.
    pub fn disassemble(&mut self) {
        let mut queue: WorkQueue<(usize, u32)> = WorkQueue::new();
        let mut branches = BranchMap::new();

        // Collect all entry seeds before touching addr_attributes (avoids borrow conflicts).
        let mut seeds: Vec<(usize, u32)> = Vec::new();

        if let Some(exe) = &self.exe {
            if let Some(seg_idx) = self.segment_index_for(exe.head.cs) {
                seeds.push((seg_idx, exe.head.ip as u32));
            }
        }

        for img in &self.images {
            let bin_def = self
                .binaries
                .iter()
                .find(|b| b.load.is_some_and(|(si, _)| si == img.seg_idx));
            if matches!(bin_def.map(|b| &b.format), Some(BinaryFormat::Com)) {
                seeds.push((img.seg_idx, img.load_offset + 0x100));
            }
        }

        let code_seeds: Vec<(usize, u32)> = self
            .attrs
            .values()
            .filter(|a| a.r#type == Some(AttrType::Code))
            .map(|a| a.addr)
            .collect();
        seeds.extend(code_seeds);

        for addr in seeds {
            queue.push(addr);
            self.segments[addr.0]
                .addr_attributes
                .mark_as_block_start(addr.1);
        }

        while let Some((seg_idx, ofs)) = queue.pop() {
            if !self.segments[seg_idx].addr_attributes.is_unmarked(ofs, 1) {
                continue;
            }

            let seg_val = (self.segments[seg_idx].start.unwrap_or(0) / 16) as u16;
            let mut cur_ofs = ofs;

            loop {
                let Some(inst) = decode(
                    seg_val,
                    cur_ofs as u16,
                    self.bytes_at_seg(seg_idx, cur_ofs).iter().copied(),
                ) else {
                    break;
                };

                let len = inst.bytes.len() as u32;
                self.segments[seg_idx]
                    .addr_attributes
                    .mark_as_code(cur_ofs, len);

                if inst.branches() {
                    if let Some((dst_seg, dst_ofs)) = inst.branch_destination() {
                        if let Some(dst_idx) = self.segment_index_for(dst_seg) {
                            branches.add((seg_idx, cur_ofs), (dst_idx, dst_ofs as u32));
                            queue.push((dst_idx, dst_ofs as u32));
                            self.segments[dst_idx]
                                .addr_attributes
                                .mark_as_block_start(dst_ofs as u32);
                        }
                    }
                    if !inst.stops_control_flow() {
                        // Fall-through of a conditional branch / call is a new block start.
                        let fall_through = cur_ofs.wrapping_add(len);
                        self.segments[seg_idx]
                            .addr_attributes
                            .mark_as_block_start(fall_through);
                    }
                }

                if inst.stops_control_flow() {
                    self.segments[seg_idx]
                        .addr_attributes
                        .mark_as_stops_flow(cur_ofs);
                    break;
                }

                cur_ofs = cur_ofs.wrapping_add(len);
            }
        }

        self.branches = branches;
    }

    /// Build the basic block map from the results of `disassemble()`.
    /// Must be called after `disassemble()`.
    pub fn build_basic_blocks(&mut self) {
        struct PendingBlock {
            seg_idx: usize,
            start: u32,
            end: u32,
            last_instr: u32,
            has_fall_through: bool,
        }

        let mut pending: Vec<PendingBlock> = Vec::new();

        for seg_idx in 0..self.segments.len() {
            let attrs = &self.segments[seg_idx].addr_attributes;
            let base = attrs.base();

            let mut block_start: Option<u32> = None;
            let mut last_instr: u32 = base;

            let mut ofs_opt = if attrs.is_op(base) {
                Some(base)
            } else {
                attrs.next(base)
            };

            while let Some(ofs) = ofs_opt {
                let attrs = &self.segments[seg_idx].addr_attributes;

                if !attrs.is_op(ofs) {
                    // Hit data or unmarked — close any open block without fall-through.
                    if let Some(start) = block_start.take() {
                        let end = last_instr + attrs.op_len(last_instr);
                        pending.push(PendingBlock {
                            seg_idx,
                            start,
                            end,
                            last_instr,
                            has_fall_through: false,
                        });
                    }
                    ofs_opt = attrs.next(ofs);
                    continue;
                }

                if attrs.is_block_start(ofs) {
                    if let Some(start) = block_start.take() {
                        // Close previous block; fall-through continues to ofs.
                        pending.push(PendingBlock {
                            seg_idx,
                            start,
                            end: ofs,
                            last_instr,
                            has_fall_through: true,
                        });
                    }
                    block_start = Some(ofs);
                }

                last_instr = ofs;

                if attrs.stops_flow(ofs) {
                    if let Some(start) = block_start.take() {
                        let end = ofs + attrs.op_len(ofs);
                        pending.push(PendingBlock {
                            seg_idx,
                            start,
                            end,
                            last_instr: ofs,
                            has_fall_through: false,
                        });
                    }
                }

                ofs_opt = attrs.next(ofs);
            }

            // Close any block still open at segment end.
            if let Some(start) = block_start {
                let attrs = &self.segments[seg_idx].addr_attributes;
                let end = last_instr + attrs.op_len(last_instr);
                pending.push(PendingBlock {
                    seg_idx,
                    start,
                    end,
                    last_instr,
                    has_fall_through: false,
                });
            }
        }

        // Build block entries with successors.
        let mut map = BasicBlockMap::new();
        for pb in &pending {
            let last_addr = (pb.seg_idx, pb.last_instr);
            let mut successors: smallvec::SmallVec<[(usize, u32); 2]> =
                self.branches.targets(last_addr).collect();
            if pb.has_fall_through {
                successors.push((pb.seg_idx, pb.end));
            }
            map.insert(BasicBlock {
                seg_idx: pb.seg_idx,
                start: pb.start,
                end: pb.end,
                successors,
                predecessors: Vec::new(),
            });
        }

        // Invert successor edges to populate predecessors.
        let edges: Vec<((usize, u32), (usize, u32))> = map
            .blocks()
            .flat_map(|b| {
                let from = (b.seg_idx, b.start);
                b.successors.iter().map(move |&to| (to, from))
            })
            .collect();

        for (to, from) in edges {
            if let Some(block) = map.block_at_mut(to.0, to.1) {
                block.predecessors.push(from);
            }
        }

        self.blocks = map;
    }

    /// Generate automatic labels for branch targets and typed-but-unnamed addresses.
    pub fn generate_auto_labels(&mut self) {
        // For EXE-backed segments the canonical address is the flat linear address
        // (seg_paragraph * 16 + ofs).  For bin/com-backed segments there is no
        // paragraph base — the offset itself is the meaningful address.
        let label_addr = |seg_idx: usize, ofs: u32| -> u32 {
            if self.images.iter().any(|img| img.seg_idx == seg_idx) {
                ofs
            } else {
                let seg = (self.segments[seg_idx].start.unwrap_or(0) / 16);
                seg * 16 + ofs
            }
        };

        for (seg_idx, ofs) in self.branches.all_targets() {
            let label = format!("loc_{:05x}", label_addr(seg_idx, ofs)).into();
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }

        let code_addrs: Vec<(usize, u32)> = self
            .attrs
            .values()
            .filter(|a| a.r#type == Some(AttrType::Code) && a.name.is_none())
            .map(|a| a.addr)
            .collect();
        for (seg_idx, ofs) in code_addrs {
            let label = format!("loc_{:05x}", label_addr(seg_idx, ofs)).into();
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }

        let data_addrs: Vec<(usize, u32)> = self
            .attrs
            .values()
            .filter(|a| a.name.is_none() && matches!(a.r#type, Some(AttrType::Data(_))))
            .map(|a| a.addr)
            .collect();
        for (seg_idx, ofs) in data_addrs {
            let label = format!("data_{:05x}", label_addr(seg_idx, ofs)).into();
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }

        // Collect all ofs16 targets (direct or inside composites) and label them.
        let ofs16_info: Vec<(usize, u32, DataType, Option<usize>)> = self
            .attrs
            .values()
            .filter_map(|attr| {
                if let Some(AttrType::Data(ref d)) = attr.r#type {
                    Some((attr.addr.0, attr.addr.1, d.clone(), attr.ofs_seg))
                } else {
                    None
                }
            })
            .collect();
        let mut ofs16_targets: Vec<(usize, u32)> = Vec::new();
        for (seg_idx, ofs, data_type, fallback_seg) in &ofs16_info {
            let bytes = self.bytes_at_seg(*seg_idx, *ofs);
            collect_ofs16_targets(
                data_type,
                bytes,
                &self.structs,
                *fallback_seg,
                &mut ofs16_targets,
            );
        }
        for (seg_idx, ofs) in ofs16_targets {
            let label = format!("data_{:05x}", label_addr(seg_idx, ofs)).into();
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }
    }

    pub fn attr_at(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<&Attr> {
        self.attrs.get(&(seg_idx, ofs))
    }

    pub fn name_at(&self, seg_idx: usize, ofs: u32) -> Option<&str> {
        self.attr_at(seg_idx, ofs)?.name.as_deref()
    }

    fn ensure_auto_label(
        attrs: &mut BTreeMap<(SegmentIdx, u32), Attr>,
        seg_idx: SegmentIdx,
        ofs: u32,
        label: SmallString,
    ) {
        let key = (seg_idx, ofs);
        let existing = attrs.get(&key);
        if existing.is_some_and(|a| a.name.is_some()) {
            return;
        }
        let attr = attrs.entry(key).or_insert_with(|| Attr {
            addr: key,
            r#type: None,
            name: None,
            ofs_seg: None,
            comment: None,
            auto_label: true,
        });
        attr.name = Some(label);
        attr.auto_label = true;
    }

    /// Look up an EXE-style segment register value → segment index.
    pub fn segment_index_for(&self, seg: u16) -> Option<usize> {
        let target = seg as u32 * 16;
        self.segments.iter().position(|s| s.start == Some(target))
    }
}

// ── Symbol lookup ─────────────────────────────────────────────────────────────

pub struct ProjectLookup<'a> {
    pub project: &'a Project,
    pub sreg_map: crate::SRegMap,
    pub register_file: Option<crate::RegisterFile>,
    pub default_seg: Option<usize>,
}

impl SymbolLookup for ProjectLookup<'_> {
    fn lookup_direct(&self, seg: u16, ofs: u16, _width: crate::DataWidth) -> Option<&str> {
        let idx = self.project.segment_index_for(seg)?;
        self.project.name_at(idx, ofs as u32)
    }

    fn lookup_indirect(
        &self,
        seg: crate::SReg,
        base: Option<crate::BaseReg>,
        index: Option<crate::IndexReg>,
        disp: u16,
        width: crate::DataWidth,
    ) -> Option<&str> {
        let rf = self.register_file.as_ref();
        let base_val = base
            .and_then(|b| rf.map(|rf| rf.get_base_reg(b)))
            .unwrap_or(0);
        let idx_val = index
            .and_then(|i| rf.map(|rf| rf.get_index_reg(i)))
            .unwrap_or(0);
        let ofs = base_val.wrapping_add(idx_val).wrapping_add(disp);

        if let Some(seg_idx) = self.sreg_map.get(seg) {
            return self.project.name_at(seg_idx, ofs as u32);
        }

        let seg_val = rf?.get_sreg(seg);
        self.lookup_direct(seg_val, ofs, width)
    }

    fn lookup_offset(&self, ofs: u16) -> Option<&str> {
        self.project.name_at(self.default_seg?, ofs as u32)
    }
}

// ── Segment auto-detection from EXE ──────────────────────────────────────────

fn make_segments(exe: &ExeMz) -> Vec<Segment> {
    use std::collections::BTreeSet;

    let mut seg_set: BTreeSet<u16> = BTreeSet::new();
    seg_set.insert(exe.head.cs);
    seg_set.insert(exe.head.ss);

    for reloc in &exe.relocations {
        let ea = reloc.seg as usize * 16 + reloc.ofs as usize;
        if let Some(bytes) = exe.image.get(ea..ea + 2) {
            let seg = u16::from_le_bytes([bytes[0], bytes[1]]);
            seg_set.insert(seg);
        }
    }

    let segs: Vec<u16> = seg_set.into_iter().collect();
    let image_size = exe.image.len() as u32;

    segs.iter()
        .enumerate()
        .map(|(i, &seg)| {
            let start = seg as u32 * 16;
            let end = segs.get(i + 1).map_or(image_size, |&s| s as u32 * 16);
            Segment {
                name: format!("seg{i:03}").into(),
                r#type: None,
                start: Some(start),
                end: Some(end),
                addr_attributes: AddressAttributes::new((end - start) as usize),
            }
        })
        .collect()
}

// ── Formatting helpers ────────────────────────────────────────────────────────

fn fmt_u32(v: u32) -> String {
    if v == 0 {
        "0".to_string()
    } else {
        format!("0x{:x}", v)
    }
}

// ── Ofs16 target collection ───────────────────────────────────────────────────

fn collect_ofs16_targets(
    data_type: &DataType,
    bytes: &[u8],
    structs: &Structs,
    fallback_seg: Option<usize>,
    targets: &mut Vec<(usize, u32)>,
) {
    match data_type {
        DataType::Scalar(scalar) => {
            let seg = match scalar {
                ScalarDataType::Ofs16(seg_opt) => seg_opt.or(fallback_seg),
                ScalarDataType::U16 => fallback_seg,
                _ => None,
            };
            if let Some(seg_idx) = seg {
                let lo = bytes.first().copied().unwrap_or(0) as u32;
                let hi = bytes.get(1).copied().unwrap_or(0) as u32;
                targets.push((seg_idx, lo | (hi << 8)));
            }
        }
        DataType::Composite(CompositeDataType::Struct(idx)) => {
            let mut cursor = 0usize;
            for f in &structs[*idx].fields {
                let field_bytes = bytes.get(cursor..).unwrap_or(&[]);
                collect_ofs16_targets(&f.r#type, field_bytes, structs, None, targets);
                cursor += f.r#type.byte_size(field_bytes, structs);
            }
        }
        DataType::Composite(CompositeDataType::Array { elem, count }) => {
            let mut cursor = 0usize;
            for _ in 0..*count {
                let elem_bytes = bytes.get(cursor..).unwrap_or(&[]);
                collect_ofs16_targets(elem, elem_bytes, structs, None, targets);
                cursor += elem.byte_size(elem_bytes, structs);
            }
        }
    }
}

// ── Parsing helpers ───────────────────────────────────────────────────────────

fn parse_hash(s: &str) -> Result<Hash, String> {
    let hex = s
        .strip_prefix("sha1:")
        .ok_or_else(|| format!("unsupported hash format '{}': expected 'sha1:<hex>'", s))?;
    let bytes = parse_hex_bytes(hex)?;
    if bytes.len() != 20 {
        return Err(format!("sha1 hash must be 20 bytes, got {}", bytes.len()));
    }
    Ok(Hash { bytes })
}

fn parse_hex_bytes(s: &str) -> Result<Vec<u8>, String> {
    let s = s.trim();
    if s.len() % 2 != 0 {
        return Err(format!("invalid hex string length: '{}'", s));
    }
    (0..s.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&s[i..i + 2], 16).map_err(|_| format!("invalid hex byte in '{}'", s))
        })
        .collect()
}

fn parse_u32(s: &str) -> Result<u32, String> {
    let s = s.trim();
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        u32::from_str_radix(hex, 16).map_err(|_| format!("invalid hex number: '{}'", s))
    } else {
        s.parse::<u32>()
            .map_err(|_| format!("invalid number: '{}'", s))
    }
}

// ── Binary dict parsing ───────────────────────────────────────────────────────

struct UnresolvedBinaryDef {
    name: SmallString,
    format: Option<BinaryFormat>,
    path: String,
    hash: Option<Hash>,
    load_str: Option<SmallString>,
}

fn parse_binary_def(dict: &Dict) -> Result<UnresolvedBinaryDef, String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err("binary name cannot be empty".to_string());
    }
    let mut format = None;
    let mut path = String::new();
    let mut hash = None;
    let mut load_str = None;

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            match key.as_str() {
                "format" => format = Some(BinaryFormat::from_str(value.trim())?),
                "path" => path = value.trim().to_owned(),
                "hash" => hash = Some(parse_hash(value.trim())?),
                "load" => load_str = Some(value.trim().into()),
                _ => return Err(format!("unknown key '{}' in binary '{}'", key, name)),
            }
        }
    }

    if path.is_empty() {
        return Err(format!("binary '{}' is missing 'path'", name));
    }

    Ok(UnresolvedBinaryDef {
        name,
        format,
        path,
        hash,
        load_str,
    })
}

fn resolve_binary_def(
    unresolved: UnresolvedBinaryDef,
    segments: &[Segment],
) -> Result<BinaryDef, String> {
    let format = unresolved
        .format
        .unwrap_or_else(|| BinaryFormat::from_extension(&unresolved.path));

    let load = match &unresolved.load_str {
        None => None,
        Some(s) => {
            let (seg_name, ofs_str) = s
                .split_once(':')
                .ok_or_else(|| format!("invalid load '{}': missing ':'", s))?;
            let ofs = u32::from_str_radix(ofs_str.trim(), 16)
                .map_err(|_| format!("invalid offset in load '{}'", s))?;
            let seg_idx = segments
                .iter()
                .position(|seg| seg.name == seg_name.trim())
                .ok_or_else(|| {
                    format!(
                        "unknown segment '{}' in load '{}' of binary '{}'",
                        seg_name, s, unresolved.name
                    )
                })?;
            Some((seg_idx, ofs))
        }
    };

    match format {
        BinaryFormat::Exe if load.is_some() => {
            return Err(format!(
                "binary '{}' (exe) must not have 'load'",
                unresolved.name
            ));
        }
        BinaryFormat::Bin | BinaryFormat::Com if load.is_none() => {
            return Err(format!(
                "binary '{}' ({}) requires 'load'",
                unresolved.name,
                format.as_str()
            ));
        }
        _ => {}
    }

    Ok(BinaryDef {
        name: unresolved.name,
        format,
        path: unresolved.path,
        hash: unresolved.hash,
        load,
    })
}

// ── Segment dict parsing ──────────────────────────────────────────────────────

fn parse_segment(dict: &Dict) -> Result<Segment, String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err("segment name cannot be empty".to_string());
    }
    let mut seg_type = None;
    let mut start: Option<u32> = None;
    let mut end: Option<u32> = None;

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            match key.as_str() {
                "type" => seg_type = Some(value.clone()),
                "start" => start = Some(parse_u32(value)?),
                "end" => end = Some(parse_u32(value)?),
                _ => return Err(format!("unknown key '{}' in segment '{}'", key, name)),
            }
        }
    }

    let size = end.unwrap_or(0).saturating_sub(start.unwrap_or(0)) as usize;
    Ok(Segment {
        name,
        r#type: seg_type,
        start,
        end,
        addr_attributes: AddressAttributes::new(size),
    })
}

// ── Struct dict parsing ───────────────────────────────────────────────────────

fn parse_struct(dict: &Dict) -> Result<(SmallString, UnresolvedStructDef), String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err("struct name cannot be empty".to_string());
    }
    let mut fields = Vec::new();

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            let field_type = parse_attr_type(value).map_err(|_| {
                format!(
                    "invalid type '{}' for field '{}' in struct '{}'",
                    value, key, name
                )
            })?;
            if field_type == UnresolvedAttrType::Code {
                return Err(format!(
                    "'code' is not valid as a struct field type (field '{}' in struct '{}')",
                    key, name
                ));
            }
            fields.push(UnresolvedStructField {
                name: key.clone(),
                r#type: field_type,
            });
        }
    }

    Ok((name, UnresolvedStructDef { fields }))
}

fn parse_attr_type(s: &str) -> Result<UnresolvedAttrType, ()> {
    let s = s.trim();
    if let Some(inner) = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        let (elem_str, count_str) = inner.split_once(';').ok_or(())?;
        let elem = parse_attr_type(elem_str)?;
        let count = count_str.trim().parse::<usize>().map_err(|_| ())?;
        return Ok(UnresolvedAttrType::Array {
            elem: Box::new(elem),
            count,
        });
    }
    if let Some(inner) = s.strip_prefix("char(").and_then(|s| s.strip_suffix(')')) {
        let n = inner.trim().parse::<usize>().map_err(|_| ())?;
        return Ok(UnresolvedAttrType::Char(n));
    }
    if s == "ofs16" {
        return Ok(UnresolvedAttrType::Ofs16(None));
    }
    if let Some(inner) = s.strip_prefix("ofs16(").and_then(|s| s.strip_suffix(')')) {
        return Ok(UnresolvedAttrType::Ofs16(Some(inner.trim().to_owned())));
    }
    match s {
        "code" => Ok(UnresolvedAttrType::Code),
        "u8" => Ok(UnresolvedAttrType::U8),
        "u16" => Ok(UnresolvedAttrType::U16),
        "u32" => Ok(UnresolvedAttrType::U32),
        "cstr" => Ok(UnresolvedAttrType::CStr),
        _ => {
            if !s.is_empty() && s.chars().all(|c| c.is_alphanumeric() || c == '_') {
                Ok(UnresolvedAttrType::Struct(s.to_owned()))
            } else {
                Err(())
            }
        }
    }
}

fn validate_no_struct_cycles(
    structs: &BTreeMap<SmallString, UnresolvedStructDef>,
) -> Result<(), String> {
    for name in structs.keys() {
        check_struct_for_cycle(name, structs, &mut Vec::new())?;
    }
    Ok(())
}

fn check_struct_for_cycle(
    name: &str,
    structs: &BTreeMap<SmallString, UnresolvedStructDef>,
    path: &mut Vec<String>,
) -> Result<(), String> {
    if let Some(pos) = path.iter().position(|n| n == name) {
        let mut cycle = path[pos..].to_vec();
        cycle.push(name.to_owned());
        return Err(format!("struct cycle detected: {}", cycle.join(" -> ")));
    }
    let Some(def) = structs.get(name) else {
        return Ok(());
    };
    path.push(name.to_owned());
    for field in &def.fields {
        check_attr_type_for_cycle(&field.r#type, structs, path)?;
    }
    path.pop();
    Ok(())
}

fn check_attr_type_for_cycle(
    t: &UnresolvedAttrType,
    structs: &BTreeMap<SmallString, UnresolvedStructDef>,
    path: &mut Vec<String>,
) -> Result<(), String> {
    match t {
        UnresolvedAttrType::Struct(name) => check_struct_for_cycle(name, structs, path),
        UnresolvedAttrType::Array { elem, .. } => check_attr_type_for_cycle(elem, structs, path),
        _ => Ok(()),
    }
}

fn resolve_data_type(
    t: &UnresolvedAttrType,
    segments: &[Segment],
    struct_names: &[SmallString],
) -> Result<DataType, String> {
    match t {
        UnresolvedAttrType::Code => Err("'code' is not valid as an embedded type".to_owned()),
        UnresolvedAttrType::U8 => Ok(DataType::Scalar(ScalarDataType::U8)),
        UnresolvedAttrType::U16 => Ok(DataType::Scalar(ScalarDataType::U16)),
        UnresolvedAttrType::U32 => Ok(DataType::Scalar(ScalarDataType::U32)),
        UnresolvedAttrType::Char(n) => Ok(DataType::Scalar(ScalarDataType::Char(*n))),
        UnresolvedAttrType::CStr => Ok(DataType::Scalar(ScalarDataType::CStr)),
        UnresolvedAttrType::Ofs16(None) => Ok(DataType::Scalar(ScalarDataType::Ofs16(None))),
        UnresolvedAttrType::Ofs16(Some(seg_name)) => {
            let idx = segments
                .iter()
                .position(|s| &s.name == seg_name)
                .ok_or_else(|| format!("unknown segment '{}' in ofs16", seg_name))?;
            Ok(DataType::Scalar(ScalarDataType::Ofs16(Some(idx))))
        }
        UnresolvedAttrType::Struct(name) => {
            let idx = struct_names
                .iter()
                .position(|n| n == name)
                .ok_or_else(|| format!("unknown struct '{}'", name))?;
            Ok(DataType::Composite(CompositeDataType::Struct(idx)))
        }
        UnresolvedAttrType::Array { elem, count } => {
            let elem = resolve_data_type(elem, segments, struct_names)?;
            Ok(DataType::Composite(CompositeDataType::Array {
                elem: Box::new(elem),
                count: *count,
            }))
        }
    }
}

fn resolve_attr_type(
    t: &UnresolvedAttrType,
    segments: &[Segment],
    struct_names: &[SmallString],
) -> Result<AttrType, String> {
    match t {
        UnresolvedAttrType::Code => Ok(AttrType::Code),
        other => Ok(AttrType::Data(resolve_data_type(
            other,
            segments,
            struct_names,
        )?)),
    }
}

fn resolve_structs(
    unresolved: BTreeMap<SmallString, UnresolvedStructDef>,
    segments: &[Segment],
) -> Result<Vec<StructDef>, String> {
    let struct_names: Vec<SmallString> = unresolved.keys().cloned().collect();

    unresolved
        .into_values()
        .zip(struct_names.iter())
        .map(|(udef, name)| {
            let fields = udef
                .fields
                .iter()
                .map(|f| {
                    Ok(StructField {
                        name: f.name.as_str().into(),
                        r#type: resolve_data_type(&f.r#type, segments, &struct_names)?,
                    })
                })
                .collect::<Result<Vec<_>, String>>()?;
            Ok(StructDef {
                name: name.as_str().into(),
                fields,
            })
        })
        .collect()
}

// ── Attr dict parsing ─────────────────────────────────────────────────────────

fn parse_attr(
    dict: &Dict,
    segments: &[Segment],
    struct_names: &[SmallString],
) -> Result<Attr, String> {
    let addr = parse_addr(&dict.key, segments)?;
    let mut r#type: Option<AttrType> = None;
    let mut name: Option<SmallString> = None;
    let mut ofs_seg: Option<SegmentIdx> = None;
    let mut comment: Option<String> = None;

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            match key.as_str() {
                "name" => name = Some(value.clone()),
                "type" => {
                    let unresolved = parse_attr_type(value.trim())
                        .map_err(|_| format!("invalid type '{}'", value))?;
                    r#type = Some(resolve_attr_type(&unresolved, segments, struct_names)?);
                }
                "ofs_seg" => {
                    let seg_name = value.trim();
                    let idx = segments
                        .iter()
                        .position(|s| s.name == seg_name)
                        .ok_or_else(|| {
                            format!(
                                "unknown segment '{}' in ofs_seg of attr '{}'",
                                seg_name, dict.key
                            )
                        })?;
                    ofs_seg = Some(idx);
                }
                "comment" => comment = Some(value.to_string()),
                _ => return Err(format!("unknown key '{}' in attr '{}'", key, dict.key)),
            }
        }
    }

    Ok(Attr {
        addr,
        r#type,
        name,
        ofs_seg,
        comment,
        auto_label: false,
    })
}

fn parse_addr(s: &str, segments: &[Segment]) -> Result<(SegmentIdx, u32), String> {
    let (seg, ofs) = s
        .split_once(':')
        .ok_or_else(|| format!("invalid address '{}': missing ':'", s))?;
    let ofs = u32::from_str_radix(ofs.trim(), 16)
        .map_err(|_| format!("invalid address offset in '{}'", s))?;
    let seg = seg.trim();
    let addr_seg = segments
        .iter()
        .position(|s| s.name == seg)
        .ok_or_else(|| format!("unknown segment '{}' in address '{}'", seg, s))?;
    Ok((addr_seg, ofs))
}

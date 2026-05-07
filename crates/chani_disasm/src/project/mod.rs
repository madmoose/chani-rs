mod architecure;
mod loadexpr;
mod parse;
mod segments;

use std::io;
use std::path::Path;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
};

use sha1::{Digest, Sha1};

use chani_datafile::ast::{self, Dict, Document, Item};
use chani_datafile::{SmallString, parser};

use crate::basic_block::{BasicBlock, BasicBlockMap};
use crate::branch_map::BranchMap;
use crate::data_type::{CompositeDataType, DataType, DisplayFmt, ScalarDataType, StructDef};
use crate::project::architecure::Architecture;
use crate::project::loadexpr::LoadExpr;
use crate::seg_dataflow::SegDataflow;
use crate::work_queue::WorkQueue;
use crate::{Address, MemRef, SymbolLookup, decode};
use crate::{address_attributes::AddressAttributes, exe_mz::ExeMz};

use parse::{
    UnresolvedStructDef, fmt_load_expr, fmt_u32, parse_attr, parse_attr_type_str, parse_file_def,
    parse_load_expr, parse_segment, parse_struct, resolve_structs, validate_no_struct_cycles,
};

pub use segments::{Segment, SegmentIdx, Segments};

#[derive(Debug, Clone)]
pub struct Hash {
    pub bytes: Vec<u8>,
}

pub type Assume = (SmallString, SmallString);
pub type Assumes = Vec<Assume>;
pub type Structs = Vec<StructDef>;

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Project {
    pub name: SmallString,
    pub files: Vec<FileDef>,
    pub arch: Architecture,
    pub segments: Segments,
    pub structs: Structs,
    pub attrs: BTreeMap<Address, Attr>,
    pub exe: Option<ExeMz>,
    pub images: Vec<BinImage>,
    pub branches: BranchMap,
    pub blocks: BasicBlockMap,
    pub seg_dataflow: SegDataflow,
    pub data_xrefs: BTreeMap<Address, BTreeSet<Address>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FileFormat {
    Exe,
    Com,
    Bin,
}

impl FileFormat {
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "exe" => Ok(FileFormat::Exe),
            "com" => Ok(FileFormat::Com),
            "bin" => Ok(FileFormat::Bin),
            _ => Err(format!("unknown file format '{}'", s)),
        }
    }

    fn from_extension(path: &str) -> Self {
        match Path::new(path)
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.to_ascii_lowercase())
            .as_deref()
        {
            Some("exe") => FileFormat::Exe,
            Some("com") => FileFormat::Com,
            _ => FileFormat::Bin,
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            FileFormat::Exe => "exe",
            FileFormat::Com => "com",
            FileFormat::Bin => "bin",
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileDef {
    pub name: SmallString,
    pub format: FileFormat,
    pub path: String,
    pub hash: Option<Hash>,
}

#[derive(Debug, Clone)]
pub struct BinImage {
    pub seg_idx: SegmentIdx,
    /// Within-segment offset where byte 0 of the file is placed.
    pub load_offset: u32,
    pub data: Vec<u8>,
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
    pub fn type_str(&self, segments: &Segments, structs: &Structs) -> String {
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

#[allow(unused)]
#[derive(Debug, Clone, Hash)]
pub struct Attr {
    pub addr: Address,
    pub r#type: Option<AttrType>,
    pub name: Option<SmallString>,
    pub is_auto_label: bool,
    pub ofs_seg: Option<SegmentIdx>,
    pub comment: Option<String>,
    /// Assumed segment register values at this address.
    pub assume: Vec<(SmallString, SmallString)>,
    /// Per-operand display format for code instructions (operand 0 and 1).
    pub arg_fmts: [Option<DisplayFmt>; 2],
}

// ── Project implementation ────────────────────────────────────────────────────

#[allow(unused)]
fn hexdump(buf: &[u8]) {
    for (i, chunk) in buf.chunks(16).enumerate() {
        // Print offset
        print!("{:08x}  ", i * 16);

        // Print hex bytes
        for (j, byte) in chunk.iter().enumerate() {
            print!("{:02x} ", byte);
            if j == 7 {
                print!(" ");
            }
        }

        // Pad if less than 16 bytes
        if chunk.len() < 16 {
            for j in chunk.len()..16 {
                print!("   ");
                if j == 7 {
                    print!(" ");
                }
            }
        }

        // Print ASCII representation
        print!(" |");
        for byte in chunk {
            let ch = if byte.is_ascii_graphic() || *byte == b' ' {
                *byte as char
            } else {
                '.'
            };
            print!("{}", ch);
        }
        println!("|");
    }
}

impl Project {
    /// Return a slice of bytes starting at `(seg_idx, ofs)`.
    ///
    /// For bin/com-backed segments this reads from the loaded `BinImage`.
    /// For EXE-backed segments this reads from the flat `exe.image`.
    pub fn bytes_at_seg(&self, seg_idx: SegmentIdx, ofs: u32) -> &[u8] {
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

    /// Parse a project from a string without loading any binary files.
    /// Segments and structs are fully resolved; `images`, `exe`, and binary bytes are empty.
    pub fn from_str(content: &str) -> std::result::Result<Self, String> {
        let tokens = parser::parse(content)?;
        let doc = ast::Document::from_tokens(tokens)?;
        Project::from_document(doc)
    }

    pub fn from_project_file(path: &str) -> std::result::Result<Self, String> {
        let content = fs::read_to_string(path).map_err(|e| e.to_string())?;
        let tokens = parser::parse(&content)?;
        let doc = ast::Document::from_tokens(tokens)?;
        let mut project = Project::from_document(doc)?;

        let base = Path::new(path).parent().unwrap_or(Path::new("."));

        // Load each binary file, validate or compute its SHA1 hash.
        let mut binary_data: Vec<Option<Vec<u8>>> = vec![None; project.files.len()];
        let mut computed_hashes: Vec<Vec<u8>> = Vec::with_capacity(project.files.len());
        for (i, file) in project.files.iter().enumerate() {
            let file_path = base.join(&file.path).to_string_lossy().into_owned();
            let data = fs::read(&file_path).map_err(|e| format!("{file_path}: {e}"))?;

            let computed = sha1_of(&data);
            if let Some(expected) = &file.hash {
                if expected.bytes != computed {
                    let exp: String = expected.bytes.iter().map(|b| format!("{b:02x}")).collect();
                    let got: String = computed.iter().map(|b| format!("{b:02x}")).collect();
                    return Err(format!(
                        "{}: hash mismatch\n  expected sha1:{exp}\n  actual   sha1:{got}",
                        file.path
                    ));
                }
            }
            computed_hashes.push(computed);

            match file.format {
                FileFormat::Exe => {
                    let exe = ExeMz::load(&data, file_path).map_err(|e| e.to_string())?;
                    project.exe = Some(exe);
                }
                FileFormat::Com | FileFormat::Bin => {
                    binary_data[i] = Some(data);
                }
            }
        }
        for (file, computed) in project.files.iter_mut().zip(computed_hashes) {
            if file.hash.is_none() {
                file.hash = Some(Hash { bytes: computed });
            }
        }

        // Apply segment load expressions.
        for (seg_idx, seg) in project.segments.indexed_iter_mut() {
            let load = match seg.load.clone() {
                Some(l) => l,
                None => continue,
            };

            let data = match project.files[load.file_idx].format {
                FileFormat::Exe => &project.exe.as_ref().unwrap().image,
                FileFormat::Com | FileFormat::Bin => binary_data[load.file_idx].as_ref().unwrap(),
            };

            let (seg_start, seg_end, file_start, file_end) = load
                .resolve(data.len() as u32)
                .map_err(|e| format!("load resolution error for segment '{}': {e}", seg.name))?;

            let image_data = data[file_start as usize..file_end as usize].to_vec();

            seg.start.get_or_insert(seg_start);
            seg.end.get_or_insert(seg_end);
            let size = seg.end.unwrap() - seg.start.unwrap();
            seg.addr_attributes = AddressAttributes::new_with_base(seg_start, size as usize);

            project.images.push(BinImage {
                seg_idx,
                load_offset: seg_start,
                data: image_data,
            });
        }

        Ok(project)
    }

    pub fn from_exe_file(exe_path: &str) -> std::result::Result<Self, String> {
        let data = fs::read(exe_path).map_err(|e| e.to_string())?;
        let exe = ExeMz::load(&data, exe_path.to_owned()).map_err(|e| e.to_string())?;
        let mut segments = make_segments(&exe);

        let file = FileDef {
            name: "exe".into(),
            format: FileFormat::Exe,
            path: exe_path.to_owned(),
            hash: None,
        };

        let mut images = Vec::new();

        for (seg_idx, seg) in segments.indexed_iter_mut() {
            let load = match seg.load.clone() {
                Some(l) => l,
                None => continue,
            };

            let data = &exe.image;

            let (seg_start, seg_end, file_start, file_end) = load
                .resolve(data.len() as u32)
                .map_err(|e| format!("load resolution error for segment '{}': {e}", seg.name))?;

            dbg!(seg_start, seg_end, file_start, file_end);

            let image_data = data[file_start as usize..file_end as usize].to_vec();

            seg.start.get_or_insert(seg_start);
            seg.end.get_or_insert(seg_end);
            let size = seg.end.unwrap() - seg.start.unwrap();
            seg.addr_attributes = AddressAttributes::new_with_base(seg_start, size as usize);

            images.push(BinImage {
                seg_idx,
                load_offset: seg_start,
                data: image_data,
            });
        }

        Ok(Project {
            name: SmallString::new(),
            files: vec![file],
            arch: Architecture::_8086,
            segments,
            structs: Vec::new(),
            attrs: BTreeMap::new(),
            exe: Some(exe),
            images,
            branches: BranchMap::new(),
            blocks: BasicBlockMap::new(),
            seg_dataflow: SegDataflow::new(),
            data_xrefs: BTreeMap::new(),
        })
    }

    pub fn from_document(doc: Document) -> Result<Self, String> {
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
        let name = dict.key.clone();
        let mut arch = Architecture::_8086;

        let mut file_dicts: Vec<&Dict> = Vec::new();
        let mut segment_dicts: Vec<&Dict> = Vec::new();
        let mut struct_dicts: Vec<&Dict> = Vec::new();
        let mut attr_dicts: Vec<&Dict> = Vec::new();
        let mut unresolved_structs: BTreeMap<SmallString, UnresolvedStructDef> = BTreeMap::new();
        let mut attrs = BTreeMap::new();

        // Pass 1: collect dicts, parse simple properties.
        for item in &dict.items {
            match item {
                Item::Property { key, value, line } => match key.as_str() {
                    "arch" => arch = value.parse().map_err(|_| "Invalid architecture")?,
                    _ => {
                        return Err(format!(
                            "line {line}: unknown key '{}' in project '{}'",
                            key, name
                        ));
                    }
                },
                Item::Dict(nested) => match nested.name.as_str() {
                    "file" => file_dicts.push(nested),
                    "segment" => segment_dicts.push(nested),
                    "struct" => struct_dicts.push(nested),
                    "attr" => attr_dicts.push(nested),
                    _ => {
                        return Err(format!(
                            "line {}: invalid dict '{}'",
                            nested.line, nested.name
                        ));
                    }
                },
            }
        }

        // Pass 1.5: build Segment vec.
        let mut segments = Segments::default();
        let mut unresolved_loads: Vec<(SegmentIdx, SmallString, u32)> = Vec::new();
        for d in &segment_dicts {
            let (seg, load_str) = parse_segment(d)?;
            if let Some((ls, line)) = load_str {
                unresolved_loads.push((SegmentIdx::from(segments.len()), ls, line));
            }
            segments.push(seg);
        }

        // Resolve file dicts.
        let mut files: Vec<FileDef> = Vec::new();
        for d in file_dicts {
            files.push(parse_file_def(d)?);
        }

        // Resolve segment load expressions (needs binaries to be known).
        for (seg_idx, load_str, line) in unresolved_loads {
            let load = parse_load_expr(&load_str, &files).map_err(|e| {
                format!(
                    "line {line}: invalid load '{}' in segment '{}': {e}",
                    load_str, segments[seg_idx].name
                )
            })?;
            segments[seg_idx].load = Some(load);
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
            name,
            files,
            arch,
            segments,
            structs,
            attrs,
            exe: Default::default(),
            images: Vec::new(),
            branches: BranchMap::new(),
            blocks: BasicBlockMap::new(),
            seg_dataflow: SegDataflow::new(),
            data_xrefs: BTreeMap::new(),
        })
    }

    pub fn write_to(&self, w: &mut impl io::Write) -> io::Result<()> {
        use chani_datafile::{BlockDict, InlineDict};

        let mut project = BlockDict::root("project", self.name.as_str());
        project.blank();
        project.prop("arch", &self.arch.to_string());
        project.blank();

        for bin in &self.files {
            let mut bin_dict = BlockDict::new("file", bin.name.as_str());
            bin_dict.prop("format", bin.format.as_str());
            bin_dict.prop("path", &bin.path);
            if let Some(hash) = &bin.hash {
                let hex: String = hash.bytes.iter().map(|b| format!("{b:02x}")).collect();
                bin_dict.prop("hash", &format!("sha1:{hex}"));
            }
            project.add_block(bin_dict);
            project.blank();
        }

        if !self.segments.is_empty() {
            for (seg_idx, seg) in self.segments.indexed_iter() {
                let bin_backed = self.images.iter().any(|img| img.seg_idx == seg_idx);
                let mut seg_dict = BlockDict::new("segment", seg.name.as_str());
                if let Some(t) = &seg.r#type {
                    seg_dict.prop("type", t.as_str());
                }
                if !bin_backed {
                    if let Some(start) = seg.start {
                        seg_dict.prop("start", &fmt_u32(start));
                    }
                    if let Some(end) = seg.end {
                        seg_dict.prop("end", &fmt_u32(end));
                    }
                } else {
                    // For binary-backed segments, only write `end` when it differs from the
                    // value the load expression would produce (e.g. BSS space beyond the file).
                    if let (Some(end), Some(img)) = (
                        seg.end,
                        self.images.iter().find(|img| img.seg_idx == seg_idx),
                    ) {
                        let derived_end = img.load_offset + img.data.len() as u32;
                        if end != derived_end {
                            seg_dict.prop("end", &fmt_u32(end));
                        }
                    }
                }
                if let Some(load) = &seg.load {
                    seg_dict.prop("load", &fmt_load_expr(load, &self.files));
                }
                if !seg.assume.is_empty() {
                    let assume_str = seg
                        .assume
                        .iter()
                        .map(|(sreg, seg)| format!("{sreg}:{seg}"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    seg_dict.prop("assume", &assume_str);
                }
                project.add_block(seg_dict);
                project.blank();
            }
        }

        if !self.structs.is_empty() {
            for def in &self.structs {
                let mut struct_dict = BlockDict::new("struct", def.name.as_str());
                for field in &def.fields {
                    struct_dict.prop(
                        field.name.as_str(),
                        &field.r#type.type_str(&self.segments, &self.structs),
                    );
                }
                project.add_block(struct_dict);
                project.blank();
            }
        }

        if !self.attrs.is_empty() {
            for attr in self.attrs.values() {
                let key = format!("{}:{:04x}", self.segments[attr.addr.0].name, attr.addr.1);
                let mut attr_dict = InlineDict::new("attr", &key);
                if let Some(t) = &attr.r#type {
                    attr_dict.prop("type", &t.type_str(&self.segments, &self.structs));
                }
                if let Some(name) = &attr.name
                    && !attr.is_auto_label
                {
                    attr_dict.prop("name", name);
                }
                if let Some(ofs_seg) = attr.ofs_seg {
                    attr_dict.prop("ofs_seg", self.segments[ofs_seg].name.as_str());
                }
                if !attr.assume.is_empty() {
                    let assume_str = attr
                        .assume
                        .iter()
                        .map(|(sreg, seg)| format!("{sreg}:{seg}"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    attr_dict.prop("assume", &assume_str);
                }
                for (i, fmt) in attr.arg_fmts.iter().enumerate() {
                    if let Some(fmt) = fmt {
                        attr_dict.prop(format!("arg[{i}]"), fmt.as_str());
                    }
                }
                if let Some(comment) = &attr.comment {
                    attr_dict.prop_encoded("comment", comment.as_str());
                }
                if !attr_dict.is_empty() {
                    project.add_inline(attr_dict);
                }
            }
            project.blank();
        }

        project.write_to(w)
    }

    pub fn analyze(&mut self) {
        self.mark_data_attributes();
        self.disassemble();
        self.build_basic_blocks();
        self.seg_dataflow = crate::seg_dataflow::compute(self);
        self.generate_auto_labels();
        self.build_data_xrefs();
    }

    fn build_data_xrefs(&mut self) {
        let pairs: Vec<(Address, Address)> = self
            .attrs
            .iter()
            .filter_map(|(&src, attr)| {
                let dt = attr.r#type.as_ref()?.as_data()?;
                let bytes = self.bytes_at_seg(src.0, src.1);
                let mut targets = Vec::new();
                collect_ofs16_targets(dt, bytes, &self.structs, attr.ofs_seg, &mut targets);
                Some(targets.into_iter().map(move |t| (t, src)))
            })
            .flatten()
            .collect();
        self.data_xrefs.clear();
        for (target, src) in pairs {
            self.data_xrefs.entry(target).or_default().insert(src);
        }
    }

    /// Mark all data-typed attributes in `addr_attributes` with their byte extents.
    pub fn mark_data_attributes(&mut self) {
        let addr_data_sizes: Vec<(Address, usize)> = self
            .attrs
            .iter()
            .filter_map(|(&addr, attr)| {
                let data_type = attr.r#type.as_ref().and_then(|typ| typ.as_data())?;
                Some((addr, data_type))
            })
            .map(|(addr, data_type)| {
                let bytes = self.bytes_at_seg(addr.0, addr.1);
                let size = data_type.byte_size(bytes, &self.structs).max(1);
                (addr, size)
            })
            .collect();

        for (addr, size) in addr_data_sizes {
            self.segments[addr.0]
                .addr_attributes
                .mark_as_data(addr.1, size as u32);
        }
    }

    /// Run the recursive disassembly pass.
    pub fn disassemble(&mut self) {
        let mut queue: WorkQueue<Address> = WorkQueue::new();
        let mut branches = BranchMap::new();

        let mut seeds: Vec<Address> = Vec::new();

        if let Some(exe) = &self.exe {
            if let Some(seg_idx) = self.segment_index_for(exe.head.cs) {
                seeds.push((seg_idx, exe.head.ip as u32));
            }
        }

        let code_seeds: Vec<Address> = self
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
            seg_idx: SegmentIdx,
            start: u32,
            end: u32,
            last_instr: u32,
            has_fall_through: bool,
        }

        let mut pending: Vec<PendingBlock> = Vec::new();

        for (seg_idx, seg) in self.segments.indexed_iter_mut() {
            let attrs = &seg.addr_attributes;
            let base = attrs.base();

            let mut block_start: Option<u32> = None;
            let mut last_instr: u32 = base;

            let mut ofs_opt = if attrs.is_op(base) {
                Some(base)
            } else {
                attrs.next(base)
            };

            while let Some(ofs) = ofs_opt {
                let attrs = &seg.addr_attributes;

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
                let attrs = &seg.addr_attributes;
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
            let mut successors: smallvec::SmallVec<[Address; 2]> =
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
        let edges: Vec<(Address, Address)> = map
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
        // EXE/COM segments share a flat address space; their labels use the raw offset.
        // BIN segments have independent address spaces; prefix with the segment name to
        // prevent collisions when multiple BIN segments have overlapping offsets.
        let label_suffix = |seg_idx: SegmentIdx, ofs: u32| -> String {
            let is_bin = self.segments[seg_idx]
                .load
                .as_ref()
                .is_some_and(|l| matches!(self.files[l.file_idx].format, FileFormat::Bin));
            if is_bin {
                format!("{}_{:05x}", self.segments[seg_idx].name, ofs)
            } else if self.images.iter().any(|img| img.seg_idx == seg_idx) {
                format!("{ofs:05x}")
            } else {
                let seg = self.segments[seg_idx].start.unwrap_or(0) / 16;
                format!("{:05x}", seg * 16 + ofs)
            }
        };

        for (seg_idx, ofs) in self.branches.all_targets() {
            let label = format!("loc_{}", label_suffix(seg_idx, ofs));
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }

        let code_addrs: Vec<Address> = self
            .attrs
            .values()
            .filter(|a| a.r#type == Some(AttrType::Code) && a.name.is_none())
            .map(|a| a.addr)
            .collect();
        for (seg_idx, ofs) in code_addrs {
            let label = format!("loc_{}", label_suffix(seg_idx, ofs));
            Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
        }

        {
            let data_addrs: Vec<Address> = self
                .attrs
                .values()
                .filter(|a| a.name.is_none() && matches!(a.r#type, Some(AttrType::Data(_))))
                .map(|a| a.addr)
                .collect();
            for (seg_idx, ofs) in data_addrs {
                let label = format!("data_{}", label_suffix(seg_idx, ofs));
                Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
            }
        }

        // Collect all ofs16 targets (direct or inside composites) and label them.
        {
            let ofs16_info: Vec<(SegmentIdx, u32, DataType, Option<SegmentIdx>)> = self
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
            let mut ofs16_targets: Vec<Address> = Vec::new();
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
                let label = format!("data_{}", label_suffix(seg_idx, ofs));
                Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
            }
        }

        // Collect immediate instruction arguments with a specified ofs_seg
        {
            let mut imm_targets: Vec<Address> = Vec::new();
            for (seg_idx, seg) in self.segments.indexed_iter() {
                let seg_start = seg.start.unwrap_or(0);

                let mut ofs_opt = {
                    let base = seg.addr_attributes.base();
                    if seg.addr_attributes.is_op(base) {
                        Some(base)
                    } else {
                        seg.addr_attributes.next(base)
                    }
                };

                while let Some(ofs) = ofs_opt {
                    if !seg.addr_attributes.is_op(ofs) {
                        ofs_opt = seg.addr_attributes.next(ofs);
                        continue;
                    }

                    if let Some(ofs_seg) = self.attrs.get(&(seg_idx, ofs)).and_then(|a| a.ofs_seg)
                    {
                        let seg_val = (seg_start / 16) as u16;
                        let bytes = self.bytes_at_seg(seg_idx, ofs);
                        if let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) {
                            for (i, &arg_type) in inst.arg_type.iter().enumerate() {
                                if matches!(
                                    arg_type,
                                    crate::opcode_table::ArgType::Imm8
                                        | crate::opcode_table::ArgType::Imm16
                                ) {
                                    imm_targets.push((ofs_seg, inst.imm[i] as u32));
                                }
                            }
                        }
                    }

                    ofs_opt = seg.addr_attributes.next(ofs);
                }
            }
            for (seg_idx, ofs) in imm_targets {
                let label = format!("data_{}", label_suffix(seg_idx, ofs));
                Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
            }
        }

        // Collect auto-label candidates from static memory references in code,
        // resolved via segment dataflow analysis.
        {
            let mut static_mem_targets: Vec<Address> = Vec::new();
            for (seg_idx, seg) in self.segments.indexed_iter() {
                let base = seg.addr_attributes.base();
                let seg_start = seg.start.unwrap_or(0);

                let mut ofs_opt = if seg.addr_attributes.is_op(base) {
                    Some(base)
                } else {
                    seg.addr_attributes.next(base)
                };

                while let Some(ofs) = ofs_opt {
                    if !seg.addr_attributes.is_op(ofs) {
                        ofs_opt = seg.addr_attributes.next(ofs);
                        continue;
                    }

                    let seg_val = (seg_start / 16) as u16;
                    let bytes = self.bytes_at_seg(seg_idx, ofs);
                    if let Some(inst) = decode(seg_val, ofs as u16, bytes.iter().copied()) {
                        match inst.mem_ref() {
                            Some(MemRef::Indirect {
                                seg: sreg,
                                base: None,
                                index: None,
                                disp,
                                ..
                            }) => {
                                if let Some(state) = self.seg_dataflow.state_at(self, seg_idx, ofs)
                                {
                                    if let Some(target_seg) = state.to_sreg_map().get(sreg) {
                                        static_mem_targets.push((target_seg, disp as u32));
                                    }
                                }
                            }
                            Some(MemRef::Direct {
                                seg,
                                ofs: target_ofs,
                                ..
                            }) => {
                                if let Some(target_seg) = self.segment_index_for(seg) {
                                    static_mem_targets.push((target_seg, target_ofs as u32));
                                }
                            }
                            _ => {}
                        }
                    }

                    ofs_opt = seg.addr_attributes.next(ofs);
                }
            }
            for (seg_idx, ofs) in static_mem_targets {
                let label = format!("data_{}", label_suffix(seg_idx, ofs));
                Self::ensure_auto_label(&mut self.attrs, seg_idx, ofs, label);
            }
        }
    }

    pub fn attr_at(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<&Attr> {
        self.attrs.get(&(seg_idx, ofs))
    }

    pub fn name_at(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<&str> {
        self.attr_at(seg_idx, ofs)?.name.as_deref()
    }

    /// Resolve an address to a label, preferring structured paths over flat names.
    ///
    /// Priority:
    /// 1. Exact user-supplied label at `(seg_idx, ofs)`
    /// 2. Array/struct path from a named typed attr that covers the address
    ///    (e.g. `locations[45]` or `locations[45].status`)
    /// 3. Exact auto-label at `(seg_idx, ofs)`
    pub fn resolve_label(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<String> {
        if let Some(attr) = self.attr_at(seg_idx, ofs) {
            if let Some(name) = &attr.name {
                if !attr.is_auto_label {
                    return Some(name.clone());
                }
            }
        }
        if let Some(label) = self.resolve_typed_label(seg_idx, ofs) {
            return Some(label);
        }
        self.name_at(seg_idx, ofs).map(String::from)
    }

    fn resolve_typed_label(&self, seg_idx: SegmentIdx, ofs: u32) -> Option<String> {
        for (&(_, attr_ofs), attr) in self.attrs.range((seg_idx, 0)..=(seg_idx, ofs)).rev() {
            let rel = (ofs - attr_ofs) as usize;
            if rel == 0 {
                continue; // exact match already handled by resolve_label
            }
            let Some(name) = attr.name.as_deref().filter(|_| !attr.is_auto_label) else {
                continue;
            };
            let Some(dt) = attr.r#type.as_ref().and_then(|t| t.as_data()) else {
                continue;
            };
            if let Some(label) = path_in_type(dt, rel, name, &self.structs) {
                return Some(label);
            }
        }
        None
    }

    fn ensure_auto_label(
        attrs: &mut BTreeMap<Address, Attr>,
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
            is_auto_label: false,
            ofs_seg: None,
            comment: None,
            assume: Assumes::default(),
            arg_fmts: [None; 2],
        });
        attr.name = Some(label);
        attr.is_auto_label = true;
    }

    /// Parse a type string and resolve it against this project's segments and structs.
    pub fn parse_type_str(&self, s: &str) -> Result<AttrType, String> {
        let struct_names: Vec<SmallString> = self.structs.iter().map(|s| s.name.clone()).collect();
        parse_attr_type_str(s, &self.segments, &struct_names)
    }

    /// Find a segment by name.
    pub fn segment_by_name(&self, name: &str) -> Option<SegmentIdx> {
        self.segments
            .indexed_iter()
            .find(|(_, s)| s.name == name)
            .map(|(idx, _)| idx)
    }

    /// Look up an EXE-style segment register value → segment index.
    pub fn segment_index_for(&self, seg: u16) -> Option<SegmentIdx> {
        let target = seg as u32 * 16;
        for (idx, s) in self.segments.indexed_iter() {
            if s.start == Some(target) {
                return Some(idx);
            }
        }
        None
    }
}

// ── Structured label resolution helpers ──────────────────────────────────────

/// Returns the byte size of `dt` without reading any binary data.
/// Returns `None` for variable-size types (`cstr`, `unknown`).
fn fixed_size(dt: &DataType, structs: &Structs) -> Option<usize> {
    match dt {
        DataType::Scalar(ScalarDataType::CStr | ScalarDataType::Unknown) => None,
        DataType::Scalar(s) => Some(s.byte_size(&[])),
        DataType::Composite(CompositeDataType::Array { elem, count }) => {
            Some(fixed_size(elem, structs)? * count)
        }
        DataType::Composite(CompositeDataType::Struct(idx)) => {
            let def = &structs[*idx];
            let mut total = 0usize;
            for field in &def.fields {
                total += fixed_size(&field.r#type, structs)?;
            }
            Some(total)
        }
        DataType::Formatted(_, inner) => fixed_size(inner, structs),
    }
}

/// Build a label path for byte offset `rel` into type `dt` starting at `prefix`.
/// Returns `None` if `rel` is out of range or the type is variable-size.
fn path_in_type(dt: &DataType, rel: usize, prefix: &str, structs: &Structs) -> Option<String> {
    match dt {
        DataType::Composite(CompositeDataType::Array { elem, count }) => {
            let elem_size = fixed_size(elem, structs).filter(|&s| s > 0)?;
            let index = rel / elem_size;
            if index >= *count {
                return None;
            }
            let inner_rel = rel % elem_size;
            let indexed = format!("{prefix}[{index}]");
            if inner_rel == 0 {
                Some(indexed)
            } else {
                path_in_type(elem, inner_rel, &indexed, structs)
            }
        }
        DataType::Composite(CompositeDataType::Struct(idx)) => {
            let def = &structs[*idx];
            let mut cursor = 0usize;
            for field in &def.fields {
                let field_size = fixed_size(&field.r#type, structs)?;
                if rel >= cursor && rel < cursor + field_size {
                    let inner_rel = rel - cursor;
                    let field_path = format!("{prefix}.{}", field.name);
                    return if inner_rel == 0 {
                        Some(field_path)
                    } else {
                        path_in_type(&field.r#type, inner_rel, &field_path, structs)
                    };
                }
                cursor += field_size;
            }
            None
        }
        DataType::Formatted(_, inner) => path_in_type(inner, rel, prefix, structs),
        DataType::Scalar(_) => {
            if rel == 0 { Some(prefix.to_string()) } else { None }
        }
    }
}

// ── Symbol lookup ─────────────────────────────────────────────────────────────

pub struct ProjectLookup<'a> {
    pub project: &'a Project,
    pub sreg_map: crate::SRegMap,
    pub register_file: Option<crate::RegisterFile>,
    pub default_seg: Option<SegmentIdx>,
}

impl SymbolLookup for ProjectLookup<'_> {
    fn lookup_direct(&self, seg: u16, ofs: u16, _width: crate::DataWidth) -> Option<String> {
        let idx = self.project.segment_index_for(seg)?;
        self.project.resolve_label(idx, ofs as u32)
    }

    fn lookup_indirect(
        &self,
        seg: crate::SReg,
        base: Option<crate::BaseReg>,
        index: Option<crate::IndexReg>,
        disp: u16,
        _width: crate::DataWidth,
    ) -> Option<String> {
        // println!(
        //     "lookup_indirect: seg={seg:?} base={base:?} index={index:?} disp={disp:#04x} w={_width:?}\n"
        // );

        let seg = self.sreg_map.get(seg)?;
        let mut base = base;
        let mut index = index;
        let mut disp = disp;

        if let Some(rf) = self.register_file.as_ref() {
            if let Some(base_val) = base.map(|b| rf.get_base_reg(b)) {
                disp = disp.wrapping_add(base_val);
                base = None;
            }
            if let Some(index_val) = index.map(|b| rf.get_index_reg(b)) {
                disp = disp.wrapping_add(index_val);
                index = None;
            }
            // println!("seg={seg:?} base={base:?} index={index:?} disp={disp:#04x} w={width:?}\n");
        }

        // let base = base.map(|b| rf.get_base_reg(b));
        // let idx = index.map(|i| rf.get_index_reg(i));

        let name = match (base, index) {
            (None, None) => self.project.resolve_label(seg, disp as u32),
            (None, Some(index)) => Some({
                let name = self.project.resolve_label(seg, disp as u32)?;
                format!("{name}[{index}]")
            }),
            (Some(base), None) => Some({
                let name = self.project.resolve_label(seg, disp as u32)?;
                format!("{name}[{base}]")
            }),
            (Some(base), Some(index)) => Some({
                let name = self.project.resolve_label(seg, disp as u32)?;
                format!("{name}[{base}+{index}]")
            }),
        };

        name
    }

    fn lookup_offset(&self, ofs: u16) -> Option<String> {
        self.project.resolve_label(self.default_seg?, ofs as u32)
    }
}

// ── Segment auto-detection from EXE ──────────────────────────────────────────

fn make_segments(exe: &ExeMz) -> Segments {
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
            println!("seg: start={start:x}, end={end:x}");
            Segment {
                name: format!("seg{i:03}"),
                r#type: None,
                start: Some(0),
                end: Some(end - start),
                addr_attributes: AddressAttributes::new((end - start) as usize),
                assume: Assumes::default(),
                load: Some(LoadExpr {
                    file_idx: 0,
                    seg_start: 0,
                    seg_end: None,
                    file_start: start,
                    file_end: if end == image_size { None } else { Some(end) },
                }),
            }
        })
        .collect::<Vec<_>>()
        .into()
}

// ── Ofs16 target collection ───────────────────────────────────────────────────

fn sha1_of(data: &[u8]) -> Vec<u8> {
    Sha1::digest(data).to_vec()
}

fn collect_ofs16_targets(
    data_type: &DataType,
    bytes: &[u8],
    structs: &Structs,
    fallback_seg: Option<SegmentIdx>,
    targets: &mut Vec<Address>,
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
        DataType::Formatted(_, inner) => {
            collect_ofs16_targets(inner, bytes, structs, fallback_seg, targets);
        }
    }
}

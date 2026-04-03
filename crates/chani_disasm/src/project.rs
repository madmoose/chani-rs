use std::collections::BTreeMap;
use std::io;
use std::str::FromStr;

use chani_datafile::ast::{Dict, Document, Item};

use crate::{attributes::Attributes, exe_mz::ExeMz};

#[derive(Debug, Clone)]
pub struct Hash {
    pub bytes: Vec<u8>,
}

#[allow(unused)]
#[derive(Debug, Default, Clone)]
pub struct Project {
    pub project: String,
    pub file: String,
    pub hash: Option<Hash>,
    pub arch: String,
    pub segments: Vec<Segment>,
    pub attrs: BTreeMap<(AddrSegment, u16), Attr>,
    pub exe: ExeMz,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Segment {
    pub name: String,
    pub r#type: Option<String>,
    pub start: u32,
    pub end: u32,
    pub attrs: Attributes,
}

impl Segment {
    pub fn size(&self) -> usize {
        self.end.saturating_sub(self.start) as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrType {
    Code,
    U8,
    U16,
    U32,
    Array { elem: Box<AttrType>, count: usize },
}

impl FromStr for AttrType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if let Some(inner) = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            let (elem_str, count_str) = inner.split_once(';').ok_or(())?;
            let elem = elem_str.trim().parse::<AttrType>()?;
            let count = count_str.trim().parse::<usize>().map_err(|_| ())?;
            return Ok(AttrType::Array {
                elem: Box::new(elem),
                count,
            });
        }
        match s {
            "code" => Ok(Self::Code),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            _ => Err(()),
        }
    }
}

impl AttrType {
    pub fn type_str(&self) -> String {
        match self {
            AttrType::Code => "code".to_owned(),
            AttrType::U8 => "u8".to_owned(),
            AttrType::U16 => "u16".to_owned(),
            AttrType::U32 => "u32".to_owned(),
            AttrType::Array { elem, count } => format!("[{}; {}]", elem.type_str(), count),
        }
    }

    /// Byte size of a single instance of this type.
    pub fn byte_size(&self) -> usize {
        match self {
            AttrType::Code => 1,
            AttrType::U8 => 1,
            AttrType::U16 => 2,
            AttrType::U32 => 4,
            AttrType::Array { elem, count } => elem.byte_size() * count,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddrSegment {
    Idx(usize),
    Value(u16),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attr {
    pub addr: (AddrSegment, u16),
    pub r#type: Option<AttrType>,
    pub name: Option<String>,
    pub imm_seg: Option<AddrSegment>,
    /// True if this attr was auto-generated and should not be saved to the project file.
    pub auto: bool,
}

impl Project {
    // Sample document:
    // ```
    // project[cryo-dune-3.7-cd]:

    // file = DNCDPRG.EXE
    // hash = sha1:c55e9e35c24941d8590c068c69cb6cee85e4afcb
    // arch = 8086
    // segment[seg000]: type = code; start = 0; end = 0x1f4b0
    // segment[seg001]: type = data; start = 0x1f4b0; end = 0x2d1ce
    // name[seg000:0000]: type = code; name = start
    // name[seg000:003a]: name = 1003A_exit_with_error
    // name[seg000:0098]: type = code; name = sub_10098_adjust_sub_resource_pointers
    // name[seg000:00b0]: type = code; name = sub_100B0_initialize_resources
    // name[seg000:00d1]: type = code; name = sub_100D1_initialize_resources
    // name[seg000:0169]: type = code; name = sub_10169_map2_resource_func
    // name[seg000:020c]: name = script_2
    // ```

    pub fn attr_at(&self, seg_idx: usize, ofs: u16) -> Option<&Attr> {
        self.attrs.get(&(AddrSegment::Idx(seg_idx), ofs))
    }

    pub fn name_at(&self, seg_idx: usize, ofs: u16) -> Option<&str> {
        self.attr_at(seg_idx, ofs)?.name.as_deref()
    }

    /// Insert an auto-generated label at (seg_idx, ofs) if there is no existing name there.
    /// Does nothing if an attr with a name already exists at that address.
    pub fn ensure_auto_label(&mut self, seg_idx: usize, ofs: u16, label: String) {
        let key = (AddrSegment::Idx(seg_idx), ofs);
        let existing = self.attrs.get(&key);
        if existing.is_some_and(|a| a.name.is_some()) {
            return;
        }
        let attr = self.attrs.entry(key.clone()).or_insert_with(|| Attr {
            addr: key,
            r#type: None,
            name: None,
            imm_seg: None,
            auto: true,
        });
        attr.name = Some(label);
        attr.auto = true;
    }

    pub fn segment_index_for(&self, seg: u16) -> Option<usize> {
        let target = seg as u32 * 16;
        self.segments.iter().position(|s| s.start == target)
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
        let project = dict.key.clone();
        let mut file = String::new();
        let mut hash = None;
        let mut arch = String::new();
        let mut segments = Vec::new();
        let mut attrs = BTreeMap::new();

        for item in &dict.items {
            match item {
                Item::Property { key, value } => match key.as_str() {
                    "file" => file = value.clone(),
                    "hash" => hash = Some(parse_hash(&value)?),
                    "arch" => arch = value.clone(),
                    _ => return Err(format!("unknown key '{}' in project '{}'", key, project)),
                },
                Item::Dict(nested) => match nested.name.as_str() {
                    "segment" => segments.push(parse_segment(nested)?),
                    "attr" => {}
                    _ => {
                        return Err(format!("invalid key '{}'", nested.name));
                    }
                },
            }
        }

        for item in &dict.items {
            if let Item::Dict(nested) = item {
                if nested.name == "attr" {
                    let attr = parse_attr(nested, &segments)?;
                    attrs.insert(attr.addr.clone(), attr);
                }
            }
        }

        Ok(Project {
            project,
            file,
            hash,
            arch,
            segments,
            attrs,
            exe: Default::default(),
        })
    }

    pub fn write_to(&self, w: &mut impl io::Write) -> io::Result<()> {
        writeln!(w, "project[{}]:", self.project)?;
        writeln!(w)?;
        writeln!(w, "file = {}", self.file)?;
        if let Some(hash) = &self.hash {
            write!(w, "hash = sha1:")?;
            for b in &hash.bytes {
                write!(w, "{:02x}", b)?;
            }
            writeln!(w)?;
        }
        writeln!(w, "arch = {}", self.arch)?;
        writeln!(w)?;
        if !self.segments.is_empty() {
            for seg in &self.segments {
                write!(w, "segment[{}]:", seg.name)?;
                if let Some(t) = &seg.r#type {
                    write!(w, " type = {};", t)?;
                }
                write!(w, " start = {};", fmt_u32(seg.start))?;
                write!(w, " end = {}", fmt_u32(seg.end))?;
                writeln!(w)?;
            }
            writeln!(w)?;
        }
        if !self.attrs.is_empty() {
            for attr in self.attrs.values().filter(|a| !a.auto) {
                match &attr.addr.0 {
                    AddrSegment::Idx(i) => {
                        write!(w, "attr[{}:{:04x}]:", self.segments[*i].name, attr.addr.1)?
                    }
                    AddrSegment::Value(v) => write!(w, "attr[{:04x}:{:04x}]:", v, attr.addr.1)?,
                }
                if let Some(t) = &attr.r#type {
                    write!(w, " type = {};", t.type_str())?;
                }
                if let Some(imm_seg) = &attr.imm_seg {
                    let seg_str = match imm_seg {
                        AddrSegment::Idx(i) => self.segments[*i].name.clone(),
                        AddrSegment::Value(v) => format!("{v:04x}"),
                    };
                    write!(w, " imm_seg = {};", seg_str)?;
                }
                if let Some(name) = &attr.name {
                    write!(w, " name = {}", name)?;
                }
                writeln!(w)?;
            }
            writeln!(w)?;
        }
        writeln!(w, "end")
    }
}

fn fmt_u32(v: u32) -> String {
    if v == 0 {
        "0".to_string()
    } else {
        format!("0x{:x}", v)
    }
}

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

fn parse_segment(dict: &Dict) -> Result<Segment, String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err("segment name cannot be empty".to_string());
    }
    let mut seg_type = None;
    let mut start = 0u32;
    let mut end = 0u32;

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            match key.as_str() {
                "type" => seg_type = Some(value.clone()),
                "start" => start = parse_u32(&value)?,
                "end" => end = parse_u32(&value)?,
                _ => return Err(format!("unknown key '{}' in segment '{}'", key, name)),
            }
        }
    }

    let size = end.saturating_sub(start) as usize;
    Ok(Segment {
        name,
        r#type: seg_type,
        start,
        end,
        attrs: Attributes::new(size),
    })
}

fn parse_attr(dict: &Dict, segments: &[Segment]) -> Result<Attr, String> {
    let addr = parse_addr(&dict.key, segments)?;
    let mut r#type: Option<AttrType> = None;
    let mut name: Option<String> = None;
    let mut imm_seg: Option<AddrSegment> = None;

    for item in &dict.items {
        if let Item::Property { key, value } = item {
            match key.as_str() {
                "name" => name = Some(value.clone()),
                "type" => {
                    r#type = Some(
                        value
                            .parse::<AttrType>()
                            .map_err(|_| format!("invalid type '{}'", value))?,
                    );
                }
                "imm_seg" => {
                    let (seg, _) = parse_addr(&format!("{}:0000", value.trim()), segments)?;
                    imm_seg = Some(seg);
                }
                _ => return Err(format!("unknown key '{}' in attr '{}'", key, dict.key)),
            }
        }
    }

    Ok(Attr {
        addr,
        r#type,
        name,
        imm_seg,
        auto: false,
    })
}

fn parse_addr(s: &str, segments: &[Segment]) -> Result<(AddrSegment, u16), String> {
    let (seg, ofs) = s
        .split_once(':')
        .ok_or_else(|| format!("invalid address '{}': missing ':'", s))?;
    let ofs = u16::from_str_radix(ofs.trim(), 16)
        .map_err(|_| format!("invalid address offset in '{}'", s))?;
    let seg = seg.trim();
    let addr_seg = if let Some(idx) = segments.iter().position(|s| s.name == seg) {
        AddrSegment::Idx(idx)
    } else {
        let val = u16::from_str_radix(seg, 16)
            .map_err(|_| format!("unknown segment '{}' in address '{}'", seg, s))?;
        AddrSegment::Value(val)
    };
    Ok((addr_seg, ofs))
}

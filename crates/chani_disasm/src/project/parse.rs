use std::collections::BTreeMap;

use chani_datafile::ast::{Dict, Item};

use crate::SmallString;
use crate::address_attributes::AddressAttributes;
use crate::data_type::{
    CompositeDataType, DataType, DisplayFmt, ScalarDataType, StructDef, StructField,
};
use crate::project::{Assumes, Segments};

use super::{Attr, AttrType, FileDef, FileFormat, Hash, LoadExpr, Segment, SegmentIdx};

// ── Unresolved intermediate types ─────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UnresolvedAttrType {
    Code,
    U8,
    U16,
    U32,
    Bool,
    Str(usize),
    Ofs16(Option<String>),
    Struct(String),
    Array {
        elem: Box<UnresolvedAttrType>,
        count: usize,
    },
    CStr,
    Formatted(DisplayFmt, Box<UnresolvedAttrType>),
    Ptr(Box<UnresolvedAttrType>),
    Tuple(Vec<UnresolvedAttrType>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct UnresolvedStructDef {
    comment: Option<String>,
    fields: Vec<UnresolvedStructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct UnresolvedStructField {
    pub name: SmallString,
    pub r#type: UnresolvedAttrType,
    pub comment: Option<String>,
}

// ── Formatting helpers ────────────────────────────────────────────────────────

pub(super) fn fmt_u32(v: u32) -> String {
    if v == 0 {
        "0".to_string()
    } else {
        format!("0x{:x}", v)
    }
}

pub(super) fn fmt_load_expr(expr: &LoadExpr, binaries: &[FileDef]) -> String {
    let file_name = binaries[expr.file_idx].name.as_str();
    let has_seg_range = expr.seg_start != 0 || expr.seg_end.is_some();
    let has_file_range = expr.file_start != 0 || expr.file_end.is_some();

    let mut s = String::new();
    if has_seg_range {
        s.push('[');
        if expr.seg_start != 0 {
            s.push_str(&fmt_u32(expr.seg_start));
        }
        s.push_str("..");
        if let Some(end) = expr.seg_end {
            s.push_str(&fmt_u32(end));
        }
        s.push_str("]:");
    }
    s.push_str(file_name);
    if has_file_range {
        s.push('[');
        if expr.file_start != 0 {
            s.push_str(&fmt_u32(expr.file_start));
        }
        s.push_str("..");
        if let Some(end) = expr.file_end {
            s.push_str(&fmt_u32(end));
        }
        s.push(']');
    }
    s
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

fn parse_sreg(s: &str) -> Result<usize, String> {
    match s.to_ascii_lowercase().as_str() {
        "es" => Ok(0),
        "cs" => Ok(1),
        "ss" => Ok(2),
        "ds" => Ok(3),
        r => Err(format!("unknown segment register '{r}'")),
    }
}

// ── Load expression helpers ───────────────────────────────────────────────────

pub type LoadRange = (Option<u32>, Option<u32>);

/// Parse `[<start>?..<end>?]` from the front of `s`.
/// Returns `((start, end), remainder)`.
fn parse_range(s: &str) -> Result<(LoadRange, &str), String> {
    let s = s
        .strip_prefix('[')
        .ok_or_else(|| "expected '['".to_string())?
        .trim_start();

    let (start, s) = if s.starts_with("..") {
        (None, s)
    } else {
        let dot = s
            .find("..")
            .ok_or_else(|| "expected '..' in range".to_string())?;
        let start = parse_u32(s[..dot].trim())?;
        (Some(start), &s[dot..])
    };

    let s = s
        .strip_prefix("..")
        .ok_or_else(|| "expected '..'".to_string())?
        .trim_start();

    let (end, s) = if s.starts_with(']') {
        (None, s)
    } else {
        let close = s.find(']').ok_or_else(|| "expected ']'".to_string())?;
        let end = parse_u32(s[..close].trim())?;
        (Some(end), &s[close..])
    };

    let s = s
        .strip_prefix(']')
        .ok_or_else(|| "expected ']'".to_string())?;

    Ok(((start, end), s))
}

/// Parse a load expression: `(<seg-range>:)?<file-name>(<file-range>)?`
pub(super) fn parse_load_expr(s: &str, files: &[FileDef]) -> Result<LoadExpr, String> {
    let s = s.trim();

    let (seg_start, seg_end, s) = if s.starts_with('[') {
        let ((start, end), rest) = parse_range(s).map_err(|e| format!("invalid seg range: {e}"))?;
        let rest = rest
            .strip_prefix(':')
            .ok_or_else(|| "expected ':' after seg range".to_string())?;
        (start.unwrap_or(0), end, rest)
    } else {
        (0, None, s)
    };

    let name_end = s
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(s.len());
    let file_name = &s[..name_end];
    let s = &s[name_end..];

    if file_name.is_empty() {
        return Err("expected file name".to_string());
    }

    let (file_start, file_end) = if s.starts_with('[') {
        let ((start, end), rest) =
            parse_range(s).map_err(|e| format!("invalid file range: {e}"))?;
        let rest = rest.trim();
        if !rest.is_empty() {
            return Err(format!("unexpected input after load expression: '{rest}'"));
        }
        (start.unwrap_or(0), end)
    } else {
        let rest = s.trim();
        if !rest.is_empty() {
            return Err(format!("unexpected input after file name: '{rest}'"));
        }
        (0, None)
    };

    let file_idx = files
        .iter()
        .position(|b| b.name == file_name)
        .ok_or_else(|| format!("unknown file '{file_name}'"))?;

    Ok(LoadExpr {
        file_idx,
        seg_start,
        seg_end,
        file_start,
        file_end,
    })
}

// ── File dict parsing ─────────────────────────────────────────────────────────

pub(super) fn parse_file_def(dict: &Dict) -> Result<FileDef, String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err(format!("line {}: file name cannot be empty", dict.line));
    }
    let mut format: Option<FileFormat> = None;
    let mut path = String::new();
    let mut hash = None;

    for item in &dict.items {
        if let Item::Property { key, value, line } = item {
            match key.as_str() {
                "format" => {
                    format = Some(
                        FileFormat::from_str(value.trim())
                            .map_err(|e| format!("line {line}: {e}"))?,
                    )
                }
                "path" => path = value.trim().to_owned(),
                "hash" => {
                    hash = Some(parse_hash(value.trim()).map_err(|e| format!("line {line}: {e}"))?)
                }
                _ => {
                    return Err(format!(
                        "line {line}: unknown key '{}' in file '{}'",
                        key, name
                    ));
                }
            }
        }
    }

    if path.is_empty() {
        return Err(format!(
            "line {}: file '{}' is missing 'path'",
            dict.line, name
        ));
    }

    let format = format.unwrap_or_else(|| FileFormat::from_extension(&path));
    Ok(FileDef {
        name,
        format,
        path,
        hash,
    })
}

// ── Segment dict parsing ──────────────────────────────────────────────────────

pub(super) fn parse_segment(dict: &Dict) -> Result<(Segment, Option<(SmallString, u32)>), String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err(format!("line {}: segment name cannot be empty", dict.line));
    }
    let mut seg_type = None;
    let mut start: Option<u32> = None;
    let mut end: Option<u32> = None;
    let mut load_str: Option<(SmallString, u32)> = None;
    let mut assume: Assumes = Vec::new();

    for item in &dict.items {
        if let Item::Property { key, value, line } = item {
            match key.as_str() {
                "type" => seg_type = Some(value.clone()),
                "start" => start = Some(parse_u32(value).map_err(|e| format!("line {line}: {e}"))?),
                "end" => end = Some(parse_u32(value).map_err(|e| format!("line {line}: {e}"))?),
                "load" => load_str = Some((value.trim().into(), *line)),
                "assume" => {
                    assume = value
                        .trim()
                        .split_ascii_whitespace()
                        .map(|s| {
                            s.split_once(':')
                                .map(|(reg, seg)| (SmallString::from(reg), SmallString::from(seg)))
                        })
                        .collect::<Option<Vec<_>>>()
                        .ok_or_else(|| {
                            format!("line {line}: failed to parse 'assume' field `{value}`")
                        })?;
                }
                key if key.starts_with("assume[") && key.ends_with(']') => {
                    let sreg_str = &key["assume[".len()..key.len() - 1];
                    parse_sreg(sreg_str)
                        .map_err(|e| format!("line {line}: {e} in segment '{name}'"))?;
                    assume.push((sreg_str.into(), value.trim().into()));
                }
                _ => {
                    return Err(format!(
                        "line {line}: unknown key '{}' in segment '{}'",
                        key, name
                    ));
                }
            }
        }
    }

    let size = end.unwrap_or(0).saturating_sub(start.unwrap_or(0)) as usize;
    Ok((
        Segment {
            name,
            r#type: seg_type,
            start,
            end,
            addr_attributes: AddressAttributes::new(size),
            assume,
            load: None,
        },
        load_str,
    ))
}

// ── Struct dict parsing ───────────────────────────────────────────────────────

pub(super) fn parse_struct(dict: &Dict) -> Result<(SmallString, UnresolvedStructDef), String> {
    let name = dict.key.clone();
    if name.is_empty() {
        return Err(format!("line {}: struct name cannot be empty", dict.line));
    }
    let mut comment = None;
    let mut fields = Vec::new();

    for item in &dict.items {
        match item {
            // A struct-level `comment` property; any other top-level property is a
            // legacy flat `field_name = type` declaration (no comment).
            Item::Property { key, value, line } => {
                if key.as_str() == "comment" {
                    comment = Some(value.to_string());
                    continue;
                }
                let field_type = parse_field_type(value, key, &name, *line)?;
                fields.push(UnresolvedStructField {
                    name: key.clone(),
                    r#type: field_type,
                    comment: None,
                });
            }
            // The current form: each field is a `field[name]:` sub-dict carrying a
            // `type` and an optional `comment`.
            Item::Dict(field_dict) => {
                if field_dict.name.as_str() != "field" {
                    return Err(format!(
                        "line {}: unknown nested block '{}' in struct '{}'",
                        field_dict.line, field_dict.name, name
                    ));
                }
                let field_name = field_dict.key.clone();
                let mut field_type = None;
                let mut field_comment = None;
                for prop in &field_dict.items {
                    if let Item::Property { key, value, line } = prop {
                        match key.as_str() {
                            "type" => {
                                field_type =
                                    Some(parse_field_type(value, &field_name, &name, *line)?);
                            }
                            "comment" => field_comment = Some(value.to_string()),
                            other => {
                                return Err(format!(
                                    "line {line}: unknown key '{other}' in field '{field_name}' of struct '{name}'"
                                ));
                            }
                        }
                    }
                }
                let field_type = field_type.ok_or_else(|| {
                    format!(
                        "line {}: field '{field_name}' in struct '{name}' is missing a 'type'",
                        field_dict.line
                    )
                })?;
                fields.push(UnresolvedStructField {
                    name: field_name,
                    r#type: field_type,
                    comment: field_comment,
                });
            }
        }
    }

    Ok((name, UnresolvedStructDef { comment, fields }))
}

/// Parse a struct field's type string, rejecting `code`.
fn parse_field_type(
    value: &str,
    field_name: &str,
    struct_name: &str,
    line: u32,
) -> Result<UnresolvedAttrType, String> {
    let field_type = parse_attr_type(value).map_err(|_| {
        format!(
            "line {line}: invalid type '{value}' for field '{field_name}' in struct '{struct_name}'"
        )
    })?;
    if field_type == UnresolvedAttrType::Code {
        return Err(format!(
            "line {line}: 'code' is not valid as a struct field type (field '{field_name}' in struct '{struct_name}')"
        ));
    }
    Ok(field_type)
}

fn parse_display_fmt(s: &str) -> Option<DisplayFmt> {
    match s {
        "hex" => Some(DisplayFmt::Hex),
        "dec" => Some(DisplayFmt::Dec),
        "signed" => Some(DisplayFmt::SignedDec),
        "bin" => Some(DisplayFmt::Bin),
        "char" => Some(DisplayFmt::Char),
        _ => None,
    }
}

/// Detect `keyword(inner)` wrappers for display-format keywords other than `char`.
/// Returns `(DisplayFmt, inner_str)` or `None`.
fn try_parse_fmt_wrapper(s: &str) -> Option<(DisplayFmt, &str)> {
    let paren = s.find('(')?;
    if !s.ends_with(')') {
        return None;
    }
    let kw = &s[..paren];
    let inner = s[paren + 1..s.len() - 1].trim();
    let fmt = match kw {
        "hex" => DisplayFmt::Hex,
        "dec" => DisplayFmt::Dec,
        "signed" => DisplayFmt::SignedDec,
        "bin" => DisplayFmt::Bin,
        "char" => DisplayFmt::Char,
        _ => return None,
    };
    Some((fmt, inner))
}

/// Split `s` on commas that are not nested inside `[]` or `()`. Used for tuple
/// members and binding lists.
pub(crate) fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    for (i, c) in s.char_indices() {
        match c {
            '[' | '(' => depth += 1,
            ']' | ')' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    let tail = s[start..].trim();
    if !tail.is_empty() || !parts.is_empty() {
        parts.push(tail);
    }
    parts
}

fn parse_attr_type(s: &str) -> Result<UnresolvedAttrType, ()> {
    let s = s.trim();

    // *T — near pointer
    if let Some(inner) = s.strip_prefix('*') {
        return Ok(UnresolvedAttrType::Ptr(Box::new(parse_attr_type(inner)?)));
    }

    // (T, U, …) — tuple of multiple values
    if let Some(inner) = s.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
        let inner = inner.trim();
        if inner.is_empty() {
            return Ok(UnresolvedAttrType::Tuple(Vec::new()));
        }
        let members = split_top_level_commas(inner)
            .iter()
            .map(|m| parse_attr_type(m))
            .collect::<Result<Vec<_>, ()>>()?;
        return Ok(UnresolvedAttrType::Tuple(members));
    }

    // [elem; count]
    if let Some(inner) = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        let (elem_str, count_str) = inner.split_once(';').ok_or(())?;
        let elem = parse_attr_type(elem_str)?;
        let count = count_str.trim().parse::<usize>().map_err(|_| ())?;
        return Ok(UnresolvedAttrType::Array {
            elem: Box::new(elem),
            count,
        });
    }

    // str(N) — fixed-length string buffer
    if let Some(inner) = s.strip_prefix("str(").and_then(|s| s.strip_suffix(')')) {
        let n = inner.trim().parse::<usize>().map_err(|_| ())?;
        return Ok(UnresolvedAttrType::Str(n));
    }

    if s == "ofs16" {
        return Ok(UnresolvedAttrType::Ofs16(None));
    }
    if let Some(inner) = s.strip_prefix("ofs16(").and_then(|s| s.strip_suffix(')')) {
        return Ok(UnresolvedAttrType::Ofs16(Some(inner.trim().to_owned())));
    }

    // dec(type), hex(type), bin(type), signed(type)
    if let Some((fmt, inner_s)) = try_parse_fmt_wrapper(s) {
        let inner = parse_attr_type(inner_s)?;
        return Ok(UnresolvedAttrType::Formatted(fmt, Box::new(inner)));
    }

    match s {
        "code" => Ok(UnresolvedAttrType::Code),
        "u8" => Ok(UnresolvedAttrType::U8),
        "u16" => Ok(UnresolvedAttrType::U16),
        "u32" => Ok(UnresolvedAttrType::U32),
        "bool" => Ok(UnresolvedAttrType::Bool),
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

/// Parse a type string (e.g. `"dec(u16)"`, `"code"`) and resolve it against the given segments and struct names.
pub(super) fn parse_attr_type_str(
    s: &str,
    segments: &Segments,
    struct_names: &[SmallString],
) -> Result<super::AttrType, String> {
    let unresolved = parse_attr_type(s).map_err(|_| format!("invalid type string: '{s}'"))?;
    resolve_attr_type(&unresolved, segments, struct_names)
}

pub(super) fn validate_no_struct_cycles(
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
        UnresolvedAttrType::Formatted(_, inner) => check_attr_type_for_cycle(inner, structs, path),
        _ => Ok(()),
    }
}

fn resolve_data_type(
    t: &UnresolvedAttrType,
    segments: &Segments,
    struct_names: &[SmallString],
) -> Result<DataType, String> {
    match t {
        UnresolvedAttrType::Code => Err("'code' is not valid as an embedded type".to_owned()),
        UnresolvedAttrType::U8 => Ok(DataType::Scalar(ScalarDataType::U8)),
        UnresolvedAttrType::U16 => Ok(DataType::Scalar(ScalarDataType::U16)),
        UnresolvedAttrType::U32 => Ok(DataType::Scalar(ScalarDataType::U32)),
        UnresolvedAttrType::Bool => Ok(DataType::Scalar(ScalarDataType::Bool)),
        UnresolvedAttrType::Str(n) => Ok(DataType::Scalar(ScalarDataType::Str(*n))),
        UnresolvedAttrType::CStr => Ok(DataType::Scalar(ScalarDataType::CStr)),
        UnresolvedAttrType::Ofs16(None) => Ok(DataType::Scalar(ScalarDataType::Ofs16(None))),
        UnresolvedAttrType::Ofs16(Some(seg_name)) => {
            let idx = segments
                .iter()
                .position(|s| &s.name == seg_name)
                .ok_or_else(|| format!("unknown segment '{}' in ofs16", seg_name))?;
            Ok(DataType::Scalar(ScalarDataType::Ofs16(Some(
                SegmentIdx::from(idx),
            ))))
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
        UnresolvedAttrType::Formatted(fmt, inner) => {
            let inner = resolve_data_type(inner, segments, struct_names)?;
            Ok(DataType::Formatted(*fmt, Box::new(inner)))
        }
        UnresolvedAttrType::Ptr(inner) => {
            let inner = resolve_data_type(inner, segments, struct_names)?;
            Ok(DataType::Ptr(Box::new(inner)))
        }
        UnresolvedAttrType::Tuple(members) => {
            let members = members
                .iter()
                .map(|m| resolve_data_type(m, segments, struct_names))
                .collect::<Result<Vec<_>, String>>()?;
            Ok(DataType::Tuple(members))
        }
    }
}

fn resolve_attr_type(
    t: &UnresolvedAttrType,
    segments: &Segments,
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

pub(super) fn resolve_structs(
    unresolved: BTreeMap<SmallString, UnresolvedStructDef>,
    segments: &Segments,
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
                        comment: f.comment.clone(),
                    })
                })
                .collect::<Result<Vec<_>, String>>()?;
            Ok(StructDef {
                name: name.as_str().into(),
                comment: udef.comment.clone(),
                fields,
            })
        })
        .collect()
}

// ── Attr dict parsing ─────────────────────────────────────────────────────────

pub(super) fn parse_attr(
    dict: &Dict,
    segments: &Segments,
    struct_names: &[SmallString],
) -> Result<Attr, String> {
    let addr = parse_addr(&dict.key, segments).map_err(|e| format!("line {}: {e}", dict.line))?;
    let mut r#type: Option<AttrType> = None;
    let mut name: Option<SmallString> = None;
    let mut ofs_seg: Option<SegmentIdx> = None;
    let mut comment: Option<String> = None;
    let mut assume = Assumes::default();
    let mut arg_fmts: [Option<DisplayFmt>; 2] = [None; 2];
    let mut targets: Vec<(SegmentIdx, u32)> = Vec::new();
    let mut lets: Vec<crate::binding::Binding> = Vec::new();
    let mut signature: Option<Vec<crate::binding::Binding>> = None;

    for item in &dict.items {
        if let Item::Property { key, value, line } = item {
            match key.as_str() {
                "name" => name = Some(value.clone()),
                "type" => {
                    let unresolved = parse_attr_type(value.trim())
                        .map_err(|_| format!("line {line}: invalid type '{}'", value))?;
                    r#type = Some(
                        resolve_attr_type(&unresolved, segments, struct_names)
                            .map_err(|e| format!("line {line}: {e}"))?,
                    );
                }
                "ofs_seg" => {
                    let seg_name = value.trim();
                    let idx = segments
                        .iter()
                        .position(|s| s.name == seg_name)
                        .ok_or_else(|| {
                            format!(
                                "line {line}: unknown segment '{}' in ofs_seg of attr '{}'",
                                seg_name, dict.key
                            )
                        })?;
                    ofs_seg = Some(SegmentIdx::from(idx));
                }
                "comment" => comment = Some(value.to_string()),
                "assume" => {
                    assume = value
                        .trim()
                        .split_ascii_whitespace()
                        .map(|s| {
                            s.split_once(':')
                                .map(|(reg, seg)| (SmallString::from(reg), SmallString::from(seg)))
                        })
                        .collect::<Option<Vec<_>>>()
                        .ok_or_else(|| {
                            format!("line {line}: failed to parse 'assume' field `{value}`")
                        })?;
                }
                "targets" => {
                    for tok in value.split(',') {
                        let tok = tok.trim();
                        if tok.is_empty() {
                            continue;
                        }
                        let target = parse_addr(tok, segments).map_err(|e| {
                            format!(
                                "line {line}: invalid target '{tok}' in attr '{}': {e}",
                                dict.key
                            )
                        })?;
                        targets.push(target);
                    }
                }
                k if k.starts_with("arg[") && k.ends_with(']') => {
                    let idx_str = &k["arg[".len()..k.len() - 1];
                    let idx = idx_str
                        .parse::<usize>()
                        .map_err(|_| format!("line {line}: invalid arg index in '{k}'"))?;
                    if idx >= 2 {
                        return Err(format!("line {line}: arg index out of range: {idx}"));
                    }
                    let fmt = parse_display_fmt(value.trim()).ok_or_else(|| {
                        format!("line {line}: unknown display format '{}'", value.trim())
                    })?;
                    arg_fmts[idx] = Some(fmt);
                }
                "let" => {
                    let bindings =
                        crate::binding::parse_binding_list(value, segments, struct_names)
                            .map_err(|e| format!("line {line}: in 'let' of '{}': {e}", dict.key))?;
                    for b in &bindings {
                        if b.dir.is_some() {
                            return Err(format!(
                                "line {line}: 'let' binding in '{}' must not carry a direction",
                                dict.key
                            ));
                        }
                    }
                    lets.extend(bindings);
                }
                "fn" => {
                    let bindings =
                        crate::binding::parse_binding_list(value, segments, struct_names)
                            .map_err(|e| format!("line {line}: in 'fn' of '{}': {e}", dict.key))?;
                    signature = Some(bindings);
                }
                _ => {
                    return Err(format!(
                        "line {line}: unknown key '{}' in attr '{}'",
                        key, dict.key
                    ));
                }
            }
        }
    }

    Ok(Attr {
        addr,
        r#type,
        name,
        is_auto_label: false,
        ofs_seg,
        comment,
        assume,
        arg_fmts,
        targets,
        lets,
        signature,
    })
}

fn parse_addr(s: &str, segments: &Segments) -> Result<(SegmentIdx, u32), String> {
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
    Ok((SegmentIdx::from(addr_seg), ofs))
}

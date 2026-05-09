//! Parsed descriptions of software interrupts (DOS, BIOS, mouse, …).
//!
//! Source files live under `chani-rs/assets/*.dict` and use the same dict
//! syntax as `.chani` projects. They are embedded at compile time and
//! parsed once on demand.
//!
//! A description matches an `int N` site when:
//! - the dict's `int` field equals the interrupt vector, and
//! - every `condition[reg]` declared in the dict has the *same* value in
//!   the per-site abstract register state produced by
//!   [`crate::simple_const_propagation`].
//!
//! The match is exact: an `Unknown` register value never satisfies a
//! condition.
//!
//! Comments produced by [`IntDescription::format_comment`] are intended
//! for display only and must not be persisted to project files.
//!
//! See `chani-rs/assets/int-21h.dict` for the source format.
//!
//! ```text
//! int[INT 21,4C]:
//!     int = 21
//!     condition[ah] = 4c
//!     title = Terminate Process With Return Code
//!     input[al] = return code (for batch files)
//!     terminates = true
//!     description = [[[ ... ]]]
//! end
//! ```

use chani_datafile::{
    ast::{self, Item},
    parser,
};

use crate::{
    GpReg8, GpReg16,
    simple_const_propagation::{RegState, Val8, Val16},
};

/// Embedded dict files, baked into the binary at compile time.
const EMBEDDED_DICTS: &[&str] = &[
    include_str!("../../../assets/int-10h.dict"),
    include_str!("../../../assets/int-21h.dict"),
    include_str!("../../../assets/int-33h.dict"),
];

/// One register condition, e.g. `condition[ah] = 4c` → `Cond { reg: AH, value: 0x4c }`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cond {
    pub reg: CondReg,
    pub value: u16,
}

/// Register slot a `condition[...]` may name. Only registers whose value
/// the const-prop pass tracks are representable here.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CondReg {
    Gp8(GpReg8),
    Gp16(GpReg16),
}

#[derive(Debug, Clone)]
pub struct IntDescription {
    pub key: String,
    pub vector: u8,
    pub conditions: Vec<Cond>,
    pub title: Option<String>,
    /// `input[reg]` annotations, in source order.
    pub inputs: Vec<(String, String)>,
    pub returns: Option<String>,
    pub terminates: bool,
    pub description: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub struct IntDescriptions {
    pub entries: Vec<IntDescription>,
}

impl IntDescriptions {
    pub fn new() -> Self {
        Self::default()
    }

    /// Load the descriptions embedded into the crate at build time. Panics
    /// if a baked-in asset fails to parse — the assets are part of the
    /// crate and any malformed file is a programming error.
    pub fn load_embedded() -> Self {
        let mut out = Self::default();
        for src in EMBEDDED_DICTS {
            let parsed = Self::parse_str(src).expect("embedded interrupt dict failed to parse");
            out.entries.extend(parsed.entries);
        }
        out
    }

    pub fn parse_str(content: &str) -> Result<Self, String> {
        let tokens = parser::parse(content)?;
        let doc = ast::Document::from_tokens(tokens)?;

        let mut entries = Vec::new();
        for dict in &doc.dicts {
            if dict.name != "int" {
                continue;
            }
            entries.push(parse_int_dict(dict)?);
        }
        Ok(Self { entries })
    }

    /// Find the most specific description matching `(vector, state)`.
    /// Among matching entries, the one with the most conditions wins; if
    /// several tie, the first in source order is returned.
    pub fn find(&self, vector: u8, state: &RegState) -> Option<&IntDescription> {
        self.entries
            .iter()
            .filter(|e| e.vector == vector && e.matches(state))
            .max_by_key(|e| e.conditions.len())
    }
}

impl IntDescription {
    /// True iff every declared condition agrees with the given register state.
    pub fn matches(&self, state: &RegState) -> bool {
        self.conditions.iter().all(|c| match c.reg {
            CondReg::Gp8(r) => matches!(state.get_gp8(r), Val8::Const(v) if v as u16 == c.value),
            CondReg::Gp16(r) => matches!(state.get_gp16(r), Val16::Const(v) if v == c.value),
        })
    }

    /// Render a multi-line comment summarising the interrupt for use as a
    /// block comment above the `int` instruction.
    pub fn format_comment(&self) -> String {
        let mut s = String::new();

        // Header line: title with vector/condition badge.
        let header = match self.title.as_deref() {
            Some(t) => format!("{} - {t}", self.key),
            None => format!("{}", self.key),
        };
        s.push_str(&header);

        if self.terminates {
            s.push_str("\n  (terminates)");
        }

        for (slot, text) in &self.inputs {
            for (i, line) in text.lines().enumerate() {
                if i == 0 {
                    s.push_str(&format!("\n  {slot} = {line}"));
                } else {
                    s.push_str(&format!("\n      {line}"));
                }
            }
        }

        if let Some(r) = &self.returns {
            s.push_str(&format!("\n  returns: {r}"));
        }

        s
    }
}

fn parse_int_dict(dict: &ast::Dict) -> Result<IntDescription, String> {
    let mut vector: Option<u8> = None;
    let mut conditions: Vec<Cond> = Vec::new();
    let mut title: Option<String> = None;
    let mut inputs: Vec<(String, String)> = Vec::new();
    let mut returns: Option<String> = None;
    let mut terminates = false;
    let mut description: Option<String> = None;

    for item in &dict.items {
        let Item::Property { key, value, line } = item else {
            return Err(format!(
                "line {}: nested dicts not supported in int description",
                dict.line
            ));
        };

        if let Some(reg_part) = bracket_subscript(key, "condition") {
            let reg = parse_cond_reg(reg_part).ok_or_else(|| {
                format!("line {line}: unknown register '{reg_part}' in condition[…]")
            })?;
            let v = parse_hex_u16(value).ok_or_else(|| {
                format!("line {line}: condition[{reg_part}] value '{value}' is not hex")
            })?;
            conditions.push(Cond { reg, value: v });
            continue;
        }

        if let Some(slot) = bracket_subscript(key, "input") {
            inputs.push((slot.to_owned(), value.clone()));
            continue;
        }

        match key.as_str() {
            "int" => {
                vector = Some(
                    parse_hex_u16(value)
                        .and_then(|v| u8::try_from(v).ok())
                        .ok_or_else(|| format!("line {line}: int = '{value}' is not a hex byte"))?,
                );
            }
            "title" => title = Some(value.clone()),
            "returns" => returns = Some(value.clone()),
            "terminates" => terminates = matches!(value.as_str(), "true" | "1" | "yes"),
            "description" => description = Some(value.clone()),
            other => {
                return Err(format!("line {line}: unknown key '{other}' in int dict"));
            }
        }
    }

    let vector = vector
        .ok_or_else(|| format!("line {}: int dict '{}' missing 'int'", dict.line, dict.key))?;

    Ok(IntDescription {
        key: dict.key.clone(),
        vector,
        conditions,
        title,
        inputs,
        returns,
        terminates,
        description,
    })
}

/// If `key` looks like `prefix[inner]`, return `inner`.
fn bracket_subscript<'a>(key: &'a str, prefix: &str) -> Option<&'a str> {
    let rest = key.strip_prefix(prefix)?;
    let rest = rest.strip_prefix('[')?;
    rest.strip_suffix(']')
}

fn parse_hex_u16(s: &str) -> Option<u16> {
    let s = s.trim();
    let s = s.strip_prefix("0x").unwrap_or(s);
    let s = s.strip_suffix('h').unwrap_or(s);
    u16::from_str_radix(s, 16).ok()
}

fn parse_cond_reg(name: &str) -> Option<CondReg> {
    match name {
        "al" => Some(CondReg::Gp8(GpReg8::AL)),
        "cl" => Some(CondReg::Gp8(GpReg8::CL)),
        "dl" => Some(CondReg::Gp8(GpReg8::DL)),
        "bl" => Some(CondReg::Gp8(GpReg8::BL)),
        "ah" => Some(CondReg::Gp8(GpReg8::AH)),
        "ch" => Some(CondReg::Gp8(GpReg8::CH)),
        "dh" => Some(CondReg::Gp8(GpReg8::DH)),
        "bh" => Some(CondReg::Gp8(GpReg8::BH)),
        "ax" => Some(CondReg::Gp16(GpReg16::AX)),
        "cx" => Some(CondReg::Gp16(GpReg16::CX)),
        "dx" => Some(CondReg::Gp16(GpReg16::DX)),
        "bx" => Some(CondReg::Gp16(GpReg16::BX)),
        "sp" => Some(CondReg::Gp16(GpReg16::SP)),
        "bp" => Some(CondReg::Gp16(GpReg16::BP)),
        "si" => Some(CondReg::Gp16(GpReg16::SI)),
        "di" => Some(CondReg::Gp16(GpReg16::DI)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::simple_const_propagation::RegState;

    #[test]
    fn embedded_dicts_parse() {
        let db = IntDescriptions::load_embedded();
        assert!(db.entries.iter().any(|e| e.vector == 0x21));
        assert!(db.entries.iter().any(|e| e.vector == 0x33));
    }

    #[test]
    fn int_21_4c_terminates() {
        let db = IntDescriptions::load_embedded();
        let mut state = RegState::default();
        state.set_gp8(GpReg8::AH, Val8::Const(0x4c));
        let entry = db.find(0x21, &state).expect("4c should match");
        assert_eq!(
            entry.title.as_deref(),
            Some("Terminate Process With Return Code")
        );
        assert!(entry.terminates);
    }

    #[test]
    fn int_21_unknown_ah_no_match() {
        let db = IntDescriptions::load_embedded();
        let state = RegState::default();
        assert!(db.find(0x21, &state).is_none());
    }

    #[test]
    fn int_33_ah_zero_matches_mouse_reset() {
        let db = IntDescriptions::load_embedded();
        let mut state = RegState::default();
        // xor ax, ax → both halves are Const(0)
        state.set_gp16(GpReg16::AX, Val16::Const(0));
        let entry = db.find(0x33, &state).expect("ah=0 should match");
        assert_eq!(entry.vector, 0x33);
        assert_eq!(entry.conditions.len(), 1);
    }

    #[test]
    fn most_specific_match_wins() {
        // int 21h has two ah conditions in our assets (00 and 4C and 3D).
        // Pick by AH=0x00 → "Program Terminate".
        let db = IntDescriptions::load_embedded();
        let mut state = RegState::default();
        state.set_gp8(GpReg8::AH, Val8::Const(0x00));
        let entry = db.find(0x21, &state).expect("ah=00 should match");
        assert_eq!(entry.title.as_deref(), Some("Program Terminate"));
    }

    #[test]
    fn format_comment_includes_title_and_terminates() {
        let db = IntDescriptions::load_embedded();
        let mut state = RegState::default();
        state.set_gp8(GpReg8::AH, Val8::Const(0x4c));
        let entry = db.find(0x21, &state).unwrap();
        let s = entry.format_comment();
        assert!(s.contains("Terminate Process With Return Code"));
        assert!(s.contains("(terminates)"));
        assert!(s.contains("al = return code"));
    }

    #[test]
    fn analyze_populates_auto_comments_and_does_not_persist() {
        use crate::address_attributes::AddressAttributes;
        use crate::project::{BinImage, Project, SegmentIdx};

        // mov ah, 4ch ; int 21h ; ret
        let code: &[u8] = &[0xb4, 0x4c, 0xcd, 0x21, 0xc3];
        let chani = format!(
            concat!(
                "project[test]:\n\n",
                "arch = 8086\n\n",
                "segment[seg000]:\n",
                "    type = code\n",
                "    start = 0x0\n",
                "    end = 0x{len:x}\n",
                "end\n\n",
                "attr[seg000:0]: type = code\n\n",
                "end\n",
            ),
            len = code.len()
        );

        let mut p = Project::from_str(&chani).unwrap();
        p.segments[SegmentIdx::from(0)].addr_attributes = AddressAttributes::new(code.len());
        p.images.push(BinImage {
            seg_idx: SegmentIdx::from(0),
            load_offset: 0,
            data: code.to_vec(),
        });
        p.analyze();

        let int_addr = (SegmentIdx::from(0), 2u32);
        let auto = p
            .auto_comments
            .get(&int_addr)
            .expect("auto comment expected at int 21h site");
        assert!(auto.contains("Terminate Process With Return Code"));
        assert!(auto.contains("(terminates)"));

        // Round-trip through the serializer and confirm the auto comment
        // is *not* written.
        let mut buf = Vec::new();
        p.write_to(&mut buf).unwrap();
        let serialized = String::from_utf8(buf).unwrap();
        assert!(
            !serialized.contains("Terminate Process With Return Code"),
            "auto-only comment must not be serialized"
        );
    }
}

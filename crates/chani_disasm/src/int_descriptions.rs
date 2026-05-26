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
    GpReg8, GpReg16, SReg,
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

    /// Parse the `returns` annotation into the set of segment and 16-bit GP
    /// registers the interrupt may modify.
    ///
    /// Returned items recognised:
    /// - 16-bit GP regs (`ax`, `cx`, …, `di`).
    /// - 8-bit halves (`al`, `ah`, …); these promote to the parent 16-bit
    ///   register since the dataflow tracks 16-bit values only.
    /// - Segment registers (`es`, `ds`, `ss`, `cs`).
    /// - Segment:offset pairs (`ds:bx`, `es:di`, `bl:cx`) — both halves are
    ///   added.
    /// - Flag mnemonics (`cf`, `zf`, `sf`, `of`, `pf`, `af`) — ignored.
    ///
    /// If `returns` is `None`, both lists are empty (no register output).
    pub fn clobbered_regs(&self) -> (Vec<SReg>, Vec<GpReg16>) {
        let Some(returns) = self.returns.as_deref() else {
            return (Vec::new(), Vec::new());
        };

        let mut sregs: Vec<SReg> = Vec::new();
        let mut gpregs: Vec<GpReg16> = Vec::new();

        for item in returns.split(',') {
            for part in item.split(':') {
                let part = part.trim();
                if part.is_empty() {
                    continue;
                }
                if let Some(s) = parse_sreg(part) {
                    if !sregs.contains(&s) {
                        sregs.push(s);
                    }
                } else if let Some(g) = parse_gp16(part) {
                    if !gpregs.contains(&g) {
                        gpregs.push(g);
                    }
                } else if let Some(g) = parse_gp8_to_parent(part) {
                    if !gpregs.contains(&g) {
                        gpregs.push(g);
                    }
                }
                // Anything else (flags, unrecognised mnemonics) is ignored.
            }
        }

        (sregs, gpregs)
    }

    /// Render a multi-line comment summarising the interrupt for use as a
    /// block comment above the `int` instruction.
    pub fn format_comment(&self) -> String {
        let mut s = String::new();

        // Header line: title with vector/condition badge.
        let header = match self.title.as_deref() {
            Some(t) => format!("{} - {t}", self.key),
            None => self.key.clone(),
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

fn parse_sreg(name: &str) -> Option<SReg> {
    match name {
        "es" => Some(SReg::ES),
        "cs" => Some(SReg::CS),
        "ss" => Some(SReg::SS),
        "ds" => Some(SReg::DS),
        _ => None,
    }
}

fn parse_gp16(name: &str) -> Option<GpReg16> {
    match name {
        "ax" => Some(GpReg16::AX),
        "cx" => Some(GpReg16::CX),
        "dx" => Some(GpReg16::DX),
        "bx" => Some(GpReg16::BX),
        "sp" => Some(GpReg16::SP),
        "bp" => Some(GpReg16::BP),
        "si" => Some(GpReg16::SI),
        "di" => Some(GpReg16::DI),
        _ => None,
    }
}

/// Map an 8-bit register name (`al`, `ah`, …) to its 16-bit parent. The
/// dataflow tracks 16-bit values only, so writing either half is modelled
/// as clobbering the whole register.
fn parse_gp8_to_parent(name: &str) -> Option<GpReg16> {
    match name {
        "al" | "ah" => Some(GpReg16::AX),
        "cl" | "ch" => Some(GpReg16::CX),
        "dl" | "dh" => Some(GpReg16::DX),
        "bl" | "bh" => Some(GpReg16::BX),
        _ => None,
    }
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
    fn clobbered_regs_parses_returns_field() {
        let mk = |returns: Option<&str>| -> IntDescription {
            IntDescription {
                key: "test".into(),
                vector: 0x21,
                conditions: Vec::new(),
                title: None,
                inputs: Vec::new(),
                returns: returns.map(|s| s.into()),
                terminates: false,
                description: None,
            }
        };

        // Plain register list.
        let (sregs, gpregs) = mk(Some("al")).clobbered_regs();
        assert!(sregs.is_empty());
        assert_eq!(gpregs, vec![GpReg16::AX]);

        // Segment:offset pair.
        let (sregs, gpregs) = mk(Some("al, ds:bx")).clobbered_regs();
        assert_eq!(sregs, vec![SReg::DS]);
        assert_eq!(gpregs, vec![GpReg16::AX, GpReg16::BX]);

        // Flags are ignored.
        let (sregs, gpregs) = mk(Some("al, zf")).clobbered_regs();
        assert!(sregs.is_empty());
        assert_eq!(gpregs, vec![GpReg16::AX]);

        // Wide+narrow names collapse to one.
        let (sregs, gpregs) = mk(Some("al, ax, ah")).clobbered_regs();
        assert!(sregs.is_empty());
        assert_eq!(gpregs, vec![GpReg16::AX]);

        // Bl:cx — both clobbered.
        let (sregs, gpregs) = mk(Some("al, ah, bh, bl:cx")).clobbered_regs();
        assert!(sregs.is_empty());
        assert_eq!(gpregs, vec![GpReg16::AX, GpReg16::BX, GpReg16::CX]);

        // No `returns` annotation → no clobbers.
        let (sregs, gpregs) = mk(None).clobbered_regs();
        assert!(sregs.is_empty());
        assert!(gpregs.is_empty());
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

    /// Build an analysed Project from raw code bytes loaded at offset 0 with
    /// the given Code-attr seeds. Mirrors the helper in
    /// `function_preserves::tests` but kept separate so the int-description
    /// tests don't depend on test-only code in another module.
    fn make_project(code: &[u8], code_seeds: &[u32]) -> crate::project::Project {
        use crate::address_attributes::AddressAttributes;
        use crate::project::{BinImage, Project, SegmentIdx};

        let mut chani = String::new();
        chani.push_str("project[test]:\n\n");
        chani.push_str("arch = 8086\n\n");
        chani.push_str("segment[seg000]:\n");
        chani.push_str("    type = code\n");
        chani.push_str("    start = 0x0\n");
        chani.push_str(&format!("    end = 0x{:x}\n", code.len()));
        chani.push_str("end\n\n");
        for &e in code_seeds {
            chani.push_str(&format!("attr[seg000:{e:x}]: type = code\n"));
        }
        chani.push_str("\nend\n");
        let mut p = Project::from_str(&chani).unwrap();
        p.segments[SegmentIdx::from(0)].addr_attributes = AddressAttributes::new(code.len());
        p.images.push(BinImage {
            seg_idx: SegmentIdx::from(0),
            load_offset: 0,
            data: code.to_vec(),
        });
        p.analyze();
        p
    }

    #[test]
    fn terminating_int_ends_basic_block_and_wipes_orphan_bytes() {
        use crate::project::SegmentIdx;
        let seg = SegmentIdx::from(0);
        // mov ah, 4ch ; int 21h ; ret
        // The ret is reached only by linear fall-through past the terminating
        // int — it should be wiped and not appear in any block.
        let code: &[u8] = &[0xb4, 0x4c, 0xcd, 0x21, 0xc3];
        let p = make_project(code, &[0x00]);

        let entry = p.blocks.block_at(seg, 0).expect("entry block at 0");
        assert_eq!(entry.end, 4, "block should end immediately after int 21h");
        assert!(
            entry.successors.is_empty(),
            "terminating int leaves no fall-through, got {:?}",
            entry.successors
        );
        assert!(
            p.blocks.block_at(seg, 4).is_none(),
            "the orphan ret at 0x04 must not be a block"
        );
        let attrs = &p.segments[seg].addr_attributes;
        assert!(
            !attrs.is_op(4),
            "orphan byte should have its code mark wiped"
        );
        assert!(
            attrs.stops_flow(2),
            "the int instruction itself should be marked as stops_flow"
        );
    }

    #[test]
    fn non_terminating_int_keeps_fall_through() {
        use crate::project::SegmentIdx;
        let seg = SegmentIdx::from(0);
        // mov ah, 09h (Display String — does NOT terminate) ; int 21h ; ret
        let code: &[u8] = &[0xb4, 0x09, 0xcd, 0x21, 0xc3];
        let p = make_project(code, &[0x00]);

        // The block should run uninterrupted through the int to the ret at 0x04.
        let entry = p.blocks.block_at(seg, 0).expect("entry block at 0");
        assert_eq!(
            entry.end, 5,
            "non-terminating int should not split the block; expected end=5, got {}",
            entry.end
        );
        let attrs = &p.segments[seg].addr_attributes;
        assert!(
            !attrs.stops_flow(2),
            "non-terminating int must not be flow-stopping"
        );
        assert!(attrs.is_op(4), "ret at 0x04 must remain decoded");
    }

    #[test]
    fn post_terminating_int_branch_target_kept_alive() {
        use crate::project::SegmentIdx;
        let seg = SegmentIdx::from(0);
        // 0x00: mov ah, 4ch
        // 0x02: int 21h        ← terminator
        // 0x04: ret             ← branch target from below; kept as code
        // 0x05: nop * 11
        // 0x10: jmp 0x04        (3 bytes)
        let mut code = vec![0xb4, 0x4c, 0xcd, 0x21, 0xc3];
        code.resize(0x10, 0x90);
        // jmp rel16 to 0x04: disp = 0x04 - (0x10 + 3) = -0x0F = 0xfff1
        code.extend_from_slice(&[0xE9, 0xF1, 0xFF]);
        let p = make_project(&code, &[0x00, 0x10]);

        // The ret at 0x04 is reachable via the jmp at 0x10, so it must
        // survive the wipe and form its own block.
        let entry = p.blocks.block_at(seg, 0).expect("entry block at 0");
        assert_eq!(entry.end, 4, "block 0 ends at terminating int");
        assert!(entry.successors.is_empty());

        let kept = p
            .blocks
            .block_at(seg, 4)
            .expect("ret at 0x04 should still be a block (jmp targets it)");
        assert_eq!(kept.end, 5);
        let attrs = &p.segments[seg].addr_attributes;
        assert!(attrs.is_op(4), "branch-targeted ret must remain decoded");
    }
}

//! Register & stack type bindings — `name: type @ location`.
//!
//! A binding attaches a [`DataType`] to a storage location (a register, a flag,
//! or a bp-relative stack slot), optionally naming it and tagging it with a
//! data-flow [`Direction`]. Bindings are the unit shared by the `let` (type
//! assertion) and `fn` (function signature) attribute properties; see
//! `docs/type-annotations.html`.

use std::fmt::{self, Display};

use crate::data_type::DataType;
use crate::project::{Segments, parse_data_type_str};
use crate::{GpReg8, GpReg16, SReg, SmallString};

/// A processor flag usable as a binding location.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum FlagId {
    Cf,
    Zf,
    Sf,
    Of,
    Pf,
    Af,
}

impl FlagId {
    pub fn as_str(self) -> &'static str {
        match self {
            FlagId::Cf => "cf",
            FlagId::Zf => "zf",
            FlagId::Sf => "sf",
            FlagId::Of => "of",
            FlagId::Pf => "pf",
            FlagId::Af => "af",
        }
    }

    /// Dense index 0..6 for array-backed state tables.
    pub fn idx(self) -> usize {
        match self {
            FlagId::Cf => 0,
            FlagId::Zf => 1,
            FlagId::Sf => 2,
            FlagId::Of => 3,
            FlagId::Pf => 4,
            FlagId::Af => 5,
        }
    }

    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "cf" => FlagId::Cf,
            "zf" => FlagId::Zf,
            "sf" => FlagId::Sf,
            "of" => FlagId::Of,
            "pf" => FlagId::Pf,
            "af" => FlagId::Af,
            _ => return None,
        })
    }
}

/// A storage location: either a register/flag or a signed bp-relative stack
/// offset. Unifies registers and the stack — there is no second mechanism for
/// stack slots.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    Gp16(GpReg16),
    Gp8(GpReg8),
    Seg(SReg),
    Flag(FlagId),
    /// Signed offset from the frame base `bp`. Negative reaches locals
    /// (`[bp-N]`); positive reaches incoming stack arguments (`[bp+N]`).
    Stack(i32),
}

impl Location {
    fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if let Some(loc) = parse_register(s) {
            return Ok(loc);
        }
        // Stack slot: a signed bp-relative offset.
        if s.starts_with('+') || s.starts_with('-') {
            let (neg, rest) = match s.split_at(1) {
                ("-", r) => (true, r.trim()),
                ("+", r) => (false, r.trim()),
                _ => unreachable!(),
            };
            let mag =
                if let Some(hex) = rest.strip_prefix("0x").or_else(|| rest.strip_prefix("0X")) {
                    i32::from_str_radix(hex, 16)
                } else {
                    rest.parse::<i32>()
                }
                .map_err(|_| format!("invalid stack offset '{s}'"))?;
            return Ok(Location::Stack(if neg { -mag } else { mag }));
        }
        Err(format!("invalid location '@{s}'"))
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Gp16(r) => write!(f, "@{r}"),
            Location::Gp8(r) => write!(f, "@{r}"),
            Location::Seg(r) => write!(f, "@{r}"),
            Location::Flag(fl) => write!(f, "@{}", fl.as_str()),
            Location::Stack(n) => {
                if *n < 0 {
                    write!(f, "@ -{:#x}", -n)
                } else {
                    write!(f, "@ +{:#x}", n)
                }
            }
        }
    }
}

fn parse_register(s: &str) -> Option<Location> {
    Some(match s {
        "ax" => Location::Gp16(GpReg16::AX),
        "cx" => Location::Gp16(GpReg16::CX),
        "dx" => Location::Gp16(GpReg16::DX),
        "bx" => Location::Gp16(GpReg16::BX),
        "sp" => Location::Gp16(GpReg16::SP),
        "bp" => Location::Gp16(GpReg16::BP),
        "si" => Location::Gp16(GpReg16::SI),
        "di" => Location::Gp16(GpReg16::DI),
        "al" => Location::Gp8(GpReg8::AL),
        "cl" => Location::Gp8(GpReg8::CL),
        "dl" => Location::Gp8(GpReg8::DL),
        "bl" => Location::Gp8(GpReg8::BL),
        "ah" => Location::Gp8(GpReg8::AH),
        "ch" => Location::Gp8(GpReg8::CH),
        "dh" => Location::Gp8(GpReg8::DH),
        "bh" => Location::Gp8(GpReg8::BH),
        "es" => Location::Seg(SReg::ES),
        "cs" => Location::Seg(SReg::CS),
        "ss" => Location::Seg(SReg::SS),
        "ds" => Location::Seg(SReg::DS),
        _ => return FlagId::from_str(s).map(Location::Flag),
    })
}

/// The data-flow role of a binding in a function signature. A bare `let`
/// assertion carries no direction.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Direction {
    /// Caller supplies on entry; callee does not produce it.
    In,
    /// Caller supplies and reads back (accumulator / scratch).
    InOut,
    /// Callee produces; caller never supplied it (a "return value").
    Out,
}

impl Direction {
    pub fn as_str(self) -> &'static str {
        match self {
            Direction::In => "in",
            Direction::InOut => "inout",
            Direction::Out => "out",
        }
    }

    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "in" => Direction::In,
            "inout" => Direction::InOut,
            "out" => Direction::Out,
            _ => return None,
        })
    }
}

/// One `[direction] name: type @ location` binding.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Binding {
    /// `fn` bindings carry a direction; `let` bindings do not (`None`).
    pub dir: Option<Direction>,
    /// `None` for the unnamed binding `_`.
    pub name: Option<SmallString>,
    pub ty: DataType,
    pub loc: Location,
}

impl Binding {
    /// Parse one binding: `[direction] name ":" type "@" location`.
    pub fn parse(
        s: &str,
        segments: &Segments,
        struct_names: &[SmallString],
    ) -> Result<Self, String> {
        let s = s.trim();
        let (head, loc_str) = s
            .split_once('@')
            .ok_or_else(|| format!("binding '{s}' is missing '@location'"))?;
        let (name_part, type_str) = head
            .split_once(':')
            .ok_or_else(|| format!("binding '{s}' is missing ': type'"))?;

        let toks: Vec<&str> = name_part.split_whitespace().collect();
        let (dir, name) = match toks.as_slice() {
            [d, n] if Direction::from_str(d).is_some() => (Direction::from_str(d), *n),
            [n] => (None, *n),
            _ => return Err(format!("binding '{s}' has a malformed name/direction")),
        };

        if name != "_" && !name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(format!("binding '{s}' has an invalid name '{name}'"));
        }
        let name = if name == "_" {
            None
        } else {
            Some(name.to_owned())
        };

        let ty = parse_data_type_str(type_str.trim(), segments, struct_names)?;
        let loc = Location::parse(loc_str)?;

        Ok(Binding { dir, name, ty, loc })
    }

    /// Render the binding to its `.chani` text form, resolving type names
    /// against `segments`/`structs`.
    pub fn to_string(&self, segments: &Segments, structs: &crate::project::Structs) -> String {
        let mut out = String::new();
        if let Some(dir) = self.dir {
            out.push_str(dir.as_str());
            out.push(' ');
        }
        out.push_str(self.name.as_deref().unwrap_or("_"));
        out.push_str(": ");
        out.push_str(&self.ty.type_str(segments, structs));
        out.push(' ');
        out.push_str(&self.loc.to_string());
        out
    }
}

/// Parse a comma-separated binding list (the `let`/`fn` value form). Tolerates
/// interior newlines and a trailing comma (the `[[[ … ]]]` multi-line form).
pub fn parse_binding_list(
    s: &str,
    segments: &Segments,
    struct_names: &[SmallString],
) -> Result<Vec<Binding>, String> {
    crate::project::split_top_level_commas(s)
        .into_iter()
        .map(str::trim)
        .filter(|p| !p.is_empty())
        .map(|p| Binding::parse(p, segments, struct_names))
        .collect()
}

/// Serialize a binding list to its comma-separated `.chani` value form.
pub fn binding_list_to_string(
    bindings: &[Binding],
    segments: &Segments,
    structs: &crate::project::Structs,
) -> String {
    bindings
        .iter()
        .map(|b| b.to_string(segments, structs))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::project::Project;

    fn project() -> Project {
        let chani = "project[t]:\n\
                     arch = 8086\n\
                     segment[seg000]: type = code; start = 0; end = 0x10\n\
                     struct[Troop]: occupation = u8\n\
                     struct[Location]: flags = u8\n\
                     end\n";
        Project::from_str(chani).unwrap()
    }

    fn parse(p: &Project, s: &str) -> Binding {
        let names: Vec<SmallString> = p.structs.iter().map(|s| s.name.clone()).collect();
        Binding::parse(s, &p.segments, &names).unwrap()
    }

    #[test]
    fn parses_all_location_kinds() {
        let p = project();
        assert_eq!(
            parse(&p, "troop: *Troop @si").loc,
            Location::Gp16(GpReg16::SI)
        );
        assert_eq!(parse(&p, "count: u16 @ -2").loc, Location::Stack(-2));
        assert_eq!(
            parse(&p, "location: *Location @ +6").loc,
            Location::Stack(6)
        );
        assert_eq!(parse(&p, "n: u16 @ -0x10").loc, Location::Stack(-16));
        assert_eq!(parse(&p, "_: bool @cf").loc, Location::Flag(FlagId::Cf));
        assert_eq!(parse(&p, "x: u8 @dl").loc, Location::Gp8(GpReg8::DL));
        assert_eq!(parse(&p, "x: u16 @ds").loc, Location::Seg(SReg::DS));
    }

    #[test]
    fn parses_direction_and_name() {
        let p = project();
        let b = parse(&p, "in troop: *Troop @si");
        assert_eq!(b.dir, Some(Direction::In));
        assert_eq!(b.name.as_deref(), Some("troop"));

        let b = parse(&p, "inout count: u16 @cx");
        assert_eq!(b.dir, Some(Direction::InOut));

        let b = parse(&p, "_: bool @cf");
        assert_eq!(b.dir, None);
        assert_eq!(b.name, None);
    }

    #[test]
    fn round_trips_through_text() {
        let p = project();
        let names: Vec<SmallString> = p.structs.iter().map(|s| s.name.clone()).collect();
        for src in ["troop: *Troop @si", "count: u16 @cx", "_: bool @cf"] {
            let b = Binding::parse(src, &p.segments, &names).unwrap();
            let rendered = b.to_string(&p.segments, &p.structs);
            let reparsed = Binding::parse(&rendered, &p.segments, &names).unwrap();
            assert_eq!(b, reparsed);
        }
    }

    #[test]
    fn attr_let_fn_round_trip_through_project() {
        // A project carrying both a `let` and a multi-line `fn` signature.
        let chani = "project[t]:\n\
                     arch = 8086\n\
                     segment[seg000]: type = code; start = 0; end = 0x100\n\
                     struct[Troop]: occupation = u8\n\
                     struct[Location]: flags = u8\n\
                     attr[seg000:003a]: let = troop: *Troop @si, tmp: u16 @ -2\n\
                     attr[seg000:0072]: fn = [[[\n\
                         in troop: *Troop @si,\n\
                         in location: *Location @di,\n\
                         inout count: u16 @cx,\n\
                         inout skill_sum: u16 @dx,\n\
                     ]]]\n\
                     end\n";
        let p = Project::from_str(chani).unwrap();

        // Parsed as expected.
        let sig = p
            .attr_at(p.segment_by_name("seg000").unwrap(), 0x72)
            .unwrap()
            .signature
            .as_ref()
            .unwrap();
        assert_eq!(sig.len(), 4);
        assert_eq!(sig[2].name.as_deref(), Some("count"));
        assert_eq!(sig[2].dir, Some(Direction::InOut));

        let lets = &p
            .attr_at(p.segment_by_name("seg000").unwrap(), 0x3a)
            .unwrap()
            .lets;
        assert_eq!(lets.len(), 2);
        assert_eq!(lets[1].loc, Location::Stack(-2));

        // Serialize and re-parse: the bindings survive a round trip.
        let mut buf = Vec::new();
        p.write_to(&mut buf).unwrap();
        let p2 = Project::from_str(std::str::from_utf8(&buf).unwrap()).unwrap();
        let sig2 = p2
            .attr_at(p2.segment_by_name("seg000").unwrap(), 0x72)
            .unwrap()
            .signature
            .clone()
            .unwrap();
        assert_eq!(sig, &sig2);
    }

    #[test]
    fn parses_full_fn_list() {
        let p = project();
        let names: Vec<SmallString> = p.structs.iter().map(|s| s.name.clone()).collect();
        let list = "in troop: *Troop @si, in location: *Location @di, \
                    inout count: u16 @cx, inout skill_sum: u16 @dx,";
        let bindings = parse_binding_list(list, &p.segments, &names).unwrap();
        assert_eq!(bindings.len(), 4);
        assert_eq!(bindings[2].dir, Some(Direction::InOut));
        assert_eq!(bindings[3].loc, Location::Gp16(GpReg16::DX));
    }
}

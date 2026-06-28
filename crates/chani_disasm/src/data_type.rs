use crate::{
    SmallString,
    project::{SegmentIdx, Segments, Structs},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum DisplayFmt {
    #[default]
    Default,
    Hex,
    Dec,
    SignedDec,
    Bin,
    Char,
}

impl DisplayFmt {
    pub fn as_str(self) -> &'static str {
        match self {
            DisplayFmt::Default => unreachable!("Default fmt is not serialized"),
            DisplayFmt::Hex => "hex",
            DisplayFmt::Dec => "dec",
            DisplayFmt::SignedDec => "signed",
            DisplayFmt::Bin => "bin",
            DisplayFmt::Char => "char",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScalarDataType {
    Unknown,
    U8,
    U16,
    U32,
    /// A flag (CF, ZF, …) or a boolean byte. Size is contextual: 0 when the
    /// value lives in a flag, 1 when it lives in a byte. `byte_size` returns 1.
    Bool,
    /// Fixed-length string buffer of `n` bytes.
    Str(usize),
    /// Null-terminated C string; byte size is variable (0 is returned as a sentinel).
    CStr,
    Ofs16(Option<SegmentIdx>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompositeDataType {
    Struct(usize),
    Array { elem: Box<DataType>, count: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataType {
    Scalar(ScalarDataType),
    Composite(CompositeDataType),
    /// Display format wrapper: `dec(u16)`, `bin(u8)`, etc.
    Formatted(DisplayFmt, Box<DataType>),
    /// Near pointer to `T` (`*T`). The segment is supplied by the
    /// `assume`/segment-dataflow analysis; this carries only the offset's
    /// pointee type. Used in register/stack bindings, not data-layout attrs.
    Ptr(Box<DataType>),
    /// Tuple `(T, U, …)` — multiple return values in a binding signature.
    Tuple(Vec<DataType>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: SmallString,
    pub comment: Option<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: SmallString,
    pub r#type: DataType,
    pub comment: Option<String>,
}

impl DataType {
    pub fn type_str(&self, segments: &Segments, structs: &Structs) -> String {
        match self {
            DataType::Scalar(s) => s.type_str(segments),
            DataType::Composite(c) => c.type_str(segments, structs),
            DataType::Formatted(fmt, inner) => {
                format!("{}({})", fmt.as_str(), inner.type_str(segments, structs))
            }
            DataType::Ptr(inner) => format!("*{}", inner.type_str(segments, structs)),
            DataType::Tuple(members) => {
                let inner = members
                    .iter()
                    .map(|m| m.type_str(segments, structs))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({inner})")
            }
        }
    }

    pub fn byte_size(&self, bytes: &[u8], structs: &Structs) -> usize {
        match self {
            DataType::Scalar(s) => s.byte_size(bytes),
            DataType::Composite(c) => c.byte_size(bytes, structs),
            DataType::Formatted(_, inner) => inner.byte_size(bytes, structs),
            // Near pointer: 2 bytes.
            DataType::Ptr(_) => 2,
            DataType::Tuple(members) => {
                let mut cursor = 0usize;
                for m in members {
                    cursor += m.byte_size(bytes.get(cursor..).unwrap_or(&[]), structs);
                }
                cursor
            }
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self {
            DataType::Scalar(_) => true,
            DataType::Formatted(_, inner) => inner.is_scalar(),
            DataType::Composite(_) | DataType::Ptr(_) | DataType::Tuple(_) => false,
        }
    }

    pub fn is_composite(&self) -> bool {
        match self {
            DataType::Composite(_) => true,
            DataType::Formatted(_, inner) => inner.is_composite(),
            DataType::Scalar(_) | DataType::Ptr(_) | DataType::Tuple(_) => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            DataType::Composite(CompositeDataType::Array { .. }) => true,
            DataType::Formatted(_, inner) => inner.is_array(),
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            DataType::Composite(CompositeDataType::Struct(_)) => true,
            DataType::Formatted(_, inner) => inner.is_struct(),
            _ => false,
        }
    }

    pub fn as_scalar(&self) -> Option<&ScalarDataType> {
        match self {
            DataType::Scalar(s) => Some(s),
            DataType::Formatted(_, inner) => inner.as_scalar(),
            DataType::Composite(_) | DataType::Ptr(_) | DataType::Tuple(_) => None,
        }
    }

    /// The pointee type if this is a (possibly format-wrapped) `*T`.
    pub fn as_ptr(&self) -> Option<&DataType> {
        match self {
            DataType::Ptr(inner) => Some(inner),
            DataType::Formatted(_, inner) => inner.as_ptr(),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<(&DataType, usize)> {
        match self {
            DataType::Composite(CompositeDataType::Array { elem, count }) => Some((elem, *count)),
            DataType::Formatted(_, inner) => inner.as_array(),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<usize> {
        match self {
            DataType::Composite(CompositeDataType::Struct(idx)) => Some(*idx),
            DataType::Formatted(_, inner) => inner.as_struct(),
            _ => None,
        }
    }
}

impl ScalarDataType {
    pub fn type_str(&self, segments: &Segments) -> String {
        match self {
            ScalarDataType::Unknown => "unknown".to_owned(),
            ScalarDataType::U8 => "u8".to_owned(),
            ScalarDataType::U16 => "u16".to_owned(),
            ScalarDataType::U32 => "u32".to_owned(),
            ScalarDataType::Bool => "bool".to_owned(),
            ScalarDataType::Str(n) => format!("str({n})"),
            ScalarDataType::Ofs16(None) => "ofs16".to_owned(),
            ScalarDataType::Ofs16(Some(idx)) => format!("ofs16({})", segments[*idx].name),
            ScalarDataType::CStr => "cstr".to_owned(),
        }
    }

    pub fn byte_size(&self, bytes: &[u8]) -> usize {
        match self {
            ScalarDataType::Unknown => 1,
            ScalarDataType::U8 => 1,
            ScalarDataType::Bool => 1,
            ScalarDataType::U16 | ScalarDataType::Ofs16(_) => 2,
            ScalarDataType::U32 => 4,
            ScalarDataType::Str(n) => *n,
            ScalarDataType::CStr => bytes
                .iter()
                .position(|&b| b == 0)
                .map(|n| n + 1)
                .unwrap_or(bytes.len()),
        }
    }
}

impl CompositeDataType {
    pub fn type_str(&self, segments: &Segments, structs: &Structs) -> String {
        match self {
            CompositeDataType::Struct(idx) => structs[*idx].name.to_string(),
            CompositeDataType::Array { elem, count } => {
                format!("[{}; {}]", elem.type_str(segments, structs), count)
            }
        }
    }

    pub fn byte_size(&self, bytes: &[u8], structs: &Structs) -> usize {
        match self {
            CompositeDataType::Struct(idx) => {
                let mut cursor = 0usize;
                for f in &structs[*idx].fields {
                    cursor += f
                        .r#type
                        .byte_size(bytes.get(cursor..).unwrap_or(&[]), structs);
                }
                cursor
            }
            CompositeDataType::Array { elem, count } => {
                let mut cursor = 0usize;
                for _ in 0..*count {
                    cursor += elem.byte_size(bytes.get(cursor..).unwrap_or(&[]), structs);
                }
                cursor
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::project::Project;

    /// Render a type string through a project (which owns the segment/struct
    /// tables `type_str` needs).
    fn type_str(p: &Project, s: &str) -> String {
        p.parse_type_str(s)
            .unwrap()
            .type_str(&p.segments, &p.structs)
    }

    #[test]
    fn ptr_bool_tuple_round_trip() {
        let chani = "project[t]:\n\
                     arch = 8086\n\
                     segment[seg000]: type = code; start = 0; end = 0x10\n\
                     struct[Troop]: occupation = u8\n\
                     end\n";
        let p = Project::from_str(chani).unwrap();

        assert_eq!(type_str(&p, "*Troop"), "*Troop");
        assert_eq!(type_str(&p, "*[u16; 4]"), "*[u16; 4]");
        assert_eq!(type_str(&p, "**u8"), "**u8");
        assert_eq!(type_str(&p, "(u16, u16)"), "(u16, u16)");
        assert_eq!(type_str(&p, "(*Troop, bool)"), "(*Troop, bool)");
        assert_eq!(type_str(&p, "bool"), "bool");
        assert_eq!(type_str(&p, "()"), "()");
    }
}

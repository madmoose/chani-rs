use crate::{
    SmallString,
    project::{Segment, Structs},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScalarDataType {
    U8,
    U16,
    U32,
    Char(usize),
    Ofs16(Option<usize>),
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: SmallString,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: SmallString,
    pub r#type: DataType,
}

impl DataType {
    pub fn type_str(&self, segments: &[Segment], structs: &Structs) -> String {
        match self {
            DataType::Scalar(s) => s.type_str(segments),
            DataType::Composite(c) => c.type_str(segments, structs),
        }
    }

    pub fn byte_size(&self, structs: &Structs) -> usize {
        match self {
            DataType::Scalar(s) => s.byte_size(),
            DataType::Composite(c) => c.byte_size(structs),
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self {
            DataType::Scalar(_) => true,
            _ => false,
        }
    }

    pub fn is_composite(&self) -> bool {
        match self {
            DataType::Composite(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            DataType::Composite(CompositeDataType::Array { .. }) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            DataType::Composite(CompositeDataType::Struct(_)) => true,
            _ => false,
        }
    }

    pub fn as_scalar(&self) -> Option<&ScalarDataType> {
        match self {
            DataType::Scalar(s) => Some(s),
            DataType::Composite(_) => None,
        }
    }

    pub fn as_array(&self) -> Option<(&DataType, usize)> {
        match self {
            DataType::Composite(CompositeDataType::Array { elem, count }) => Some((elem, *count)),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<usize> {
        match self {
            DataType::Composite(CompositeDataType::Struct(idx)) => Some(*idx),
            _ => None,
        }
    }
}

impl ScalarDataType {
    pub fn type_str(&self, segments: &[Segment]) -> String {
        match self {
            ScalarDataType::U8 => "u8".to_owned(),
            ScalarDataType::U16 => "u16".to_owned(),
            ScalarDataType::U32 => "u32".to_owned(),
            ScalarDataType::Char(n) => format!("char[{n}]"),
            ScalarDataType::Ofs16(None) => "ofs16".to_owned(),
            ScalarDataType::Ofs16(Some(idx)) => format!("ofs16({})", segments[*idx].name),
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            ScalarDataType::U8 => 1,
            ScalarDataType::U16 | ScalarDataType::Ofs16(_) => 2,
            ScalarDataType::U32 => 4,
            ScalarDataType::Char(n) => *n,
        }
    }
}

impl CompositeDataType {
    pub fn type_str(&self, segments: &[Segment], structs: &Structs) -> String {
        match self {
            CompositeDataType::Struct(idx) => structs[*idx].name.to_string(),
            CompositeDataType::Array { elem, count } => {
                format!("[{}; {}]", elem.type_str(segments, structs), count)
            }
        }
    }

    pub fn byte_size(&self, structs: &Structs) -> usize {
        match self {
            CompositeDataType::Struct(idx) => structs[*idx]
                .fields
                .iter()
                .map(|f| f.r#type.byte_size(structs))
                .sum(),
            CompositeDataType::Array { elem, count } => elem.byte_size(structs) * count,
        }
    }
}

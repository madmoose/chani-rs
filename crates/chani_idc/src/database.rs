use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct IdcSegment {
    pub name: String,
    pub start: u32,
    pub end: u32,
    pub base: u32,
}

#[derive(Debug, Default, Clone)]
pub struct IdcEnum {
    pub name: String,
    pub constants: HashMap<u32, String>,
}

#[derive(Debug, Default, Clone)]
pub struct IdcStructMember {
    pub name: String,
    pub offset: u32,
    pub flag: u32,
    pub target_id: i64,
    pub size: u32,
}

#[derive(Debug, Default, Clone)]
pub struct IdcStruct {
    pub name: String,
    pub members: Vec<IdcStructMember>,
}

#[derive(Debug, Default, Clone)]
pub struct IdcFunction {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Default, Clone)]
pub struct IdcAttr {
    pub name: Option<String>,
    pub comment: Option<String>,
    pub repeat_comment: Option<String>,
    pub is_code: bool,
    pub data_type: Option<String>,
    pub array_size: Option<u32>,
    pub ofs_target: Option<u32>,
    pub struc_id: Option<i64>,
    pub enum_id: Option<i64>,
}

#[derive(Debug, Default)]
pub struct IdcDatabase {
    pub segments: Vec<IdcSegment>,
    pub enums: HashMap<i64, IdcEnum>,
    pub enum_name_to_id: HashMap<String, i64>,
    pub structs: HashMap<i64, IdcStruct>,
    pub struct_name_to_id: HashMap<String, i64>,
    pub functions: Vec<IdcFunction>,
    pub attrs: HashMap<u32, IdcAttr>,
}

impl IdcDatabase {
    pub fn segment_for_addr(&self, addr: u32) -> Option<&IdcSegment> {
        self.segments
            .iter()
            .find(|s| addr >= s.start && addr < s.end)
    }
}

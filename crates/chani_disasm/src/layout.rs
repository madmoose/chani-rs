use std::fmt::Write;

use crate::{
    DecodedInstruction, DisplayContext, SmallString,
    data_type::{CompositeDataType, DataType, ScalarDataType},
    disassemble,
    project::{AttrType, Project},
};

#[derive(Debug, Clone)]
pub struct Widget {
    pub x: u32,
    pub y: u32,
    pub text: SmallString,
}

pub type Widgets = Vec<Widget>;

pub struct LayoutBuilder<'a> {
    project: &'a Project,
    widgets: Widgets,
    seg_idx: usize,
    base_ofs: u32,
    ofs: u32,
    label_x0: u32,
    text_x0: u32,
    y: u32,
    line_state: LineState,
    ctx: &'a DisplayContext<'a>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum LineState {
    StartOfLine,
    InLine,
    EndOfLine,
    BlankLine,
}

impl<'a> LayoutBuilder<'a> {
    pub fn new(
        project: &'a Project,
        seg_idx: usize,
        ofs: u32,
        ctx: &'a DisplayContext<'a>,
    ) -> Self {
        let widgets = Widgets::new();
        let label_x0 = project.segments[seg_idx].name.len() as u32 + 1 + 4 + 1;
        let text_x0 = label_x0 + 16;
        let y = 0;
        Self {
            project,
            widgets,
            seg_idx,
            base_ofs: ofs,
            ofs,
            label_x0,
            text_x0,
            y,
            line_state: LineState::StartOfLine,
            ctx,
        }
    }

    pub fn lines(&self) -> u32 {
        self.y + 1
    }

    pub fn render(&self, buf: &mut String, y: u32) {
        render_widgets(&self.widgets, buf, y);
    }

    pub fn widgets(&self) -> Widgets {
        self.widgets.clone()
    }

    fn new_line(&mut self) {
        if self.line_state != LineState::StartOfLine {
            self.line_state = LineState::EndOfLine;
        }
    }

    fn blank_line(&mut self) {
        if self.line_state == LineState::BlankLine {
            return;
        }

        if self.line_state == LineState::EndOfLine {
            self.y += 1;
        }

        let text = self.make_address();
        self.widgets.push(Widget {
            x: 0,
            y: self.y,
            text,
        });
        self.line_state = LineState::BlankLine;
    }

    fn add(&mut self, x: u32, text: SmallString) {
        if self.line_state == LineState::EndOfLine || self.line_state == LineState::BlankLine {
            self.y += 1;
            self.line_state = LineState::StartOfLine;
        }
        if self.line_state == LineState::StartOfLine {
            let text = self.make_address();
            self.widgets.push(Widget {
                x: 0,
                y: self.y,
                text,
            });
        }
        self.widgets.push(Widget { x, y: self.y, text });
        self.line_state = LineState::InLine;
    }

    fn make_address(&mut self) -> SmallString {
        let mut address = SmallString::new();
        let _ = write!(
            address,
            "{}:{:04x}",
            self.project.segments[self.seg_idx].name, self.ofs
        );

        address
    }

    pub fn layout(&mut self) {
        let attr = self.project.attr_at(self.seg_idx, self.base_ofs);
        let label = attr.and_then(|attr| attr.name.as_deref());
        let seg = &self.project.segments[self.seg_idx];
        let is_code = seg.addr_attributes.is_op(self.base_ofs);

        let prev_ofs = seg.addr_attributes.prev(self.base_ofs);
        let prev_is_code = prev_ofs
            .map(|ofs| seg.addr_attributes.is_code(ofs))
            .unwrap_or_default();

        if is_code {
            let seg_val = (seg.start.unwrap_or_default() / 16) as u16;
            let bytes = self.project.bytes_at_seg(self.seg_idx, self.base_ofs);
            let inst =
                disassemble::decode(seg_val, self.base_ofs as u16, bytes.iter().copied()).unwrap();

            let prev_stops_flow = prev_ofs
                .map(|ofs| seg.addr_attributes.stops_flow(ofs))
                .unwrap_or_default();

            self.layout_instruction(label, &inst, prev_stops_flow);
        } else {
            if prev_is_code {
                self.blank_line();
            }

            let default_data_type = DataType::Scalar(ScalarDataType::U8);
            let data_type = attr
                .and_then(|attr| attr.r#type.as_ref())
                .map(|typ| match typ {
                    AttrType::Code => unreachable!(),
                    AttrType::Data(attr_data_type) => attr_data_type,
                })
                .unwrap_or(&default_data_type);

            if let Some(label) = label {
                self.blank_line();
                self.add(self.label_x0, format!("{label}:").into());

                let long_label = self.label_x0 + label.len() as u32 + 1 >= self.text_x0;

                if long_label || data_type.is_composite() {
                    self.new_line();
                }
            }

            self.layout_data(self.text_x0, data_type);
        }

        self.widgets.sort_by_key(|w| (w.y, w.x));
    }

    fn layout_instruction(
        &mut self,
        label: Option<&str>,
        inst: &DecodedInstruction,
        prev_stops_flow: bool,
    ) {
        if prev_stops_flow {
            self.add(
                self.label_x0,
                "; ---------------------------------------------------------------------------"
                    .into(),
            );
            self.new_line();
            self.blank_line();
        }

        if let Some(label) = label {
            self.blank_line();
            self.add(self.label_x0, format!("{label}:").into());
            self.new_line();
        }

        let mut opcode = SmallString::new();
        let _ = inst.format_opcode(&mut opcode);
        let w = opcode.len() as u32;
        self.add(self.text_x0, opcode);

        let mut x = self.text_x0 + u32::max(w + 1, 8);
        for i in 0..inst.arg_count() {
            if i > 0 {
                self.add(x, ", ".into());
                x += 2;
            }
            let mut s = SmallString::new();
            let _ = inst.format_arg(&mut s, i, self.ctx);
            let w = s.len() as u32;

            self.add(x, s);

            x += w;
        }

        self.new_line();
    }

    fn layout_data(&mut self, x: u32, data: &DataType) {
        match data {
            DataType::Scalar(scalar) => self.layout_scalar(x, scalar),
            DataType::Composite(CompositeDataType::Array { elem, count }) => {
                self.layout_array(x, elem, *count)
            }
            DataType::Composite(CompositeDataType::Struct(idx)) => self.layout_struct(x, *idx),
        }
    }

    fn layout_scalar(&mut self, x: u32, scalar: &ScalarDataType) {
        let bytes = self.project.bytes_at_seg(self.seg_idx, self.ofs);
        match scalar {
            ScalarDataType::U8 => {
                self.add(
                    x,
                    format!("db {}", format_numeric_value(read_u8(bytes))).into(),
                );
            }
            ScalarDataType::U16 => {
                self.add(
                    x,
                    format!("dw {}", format_numeric_value(read_u16(bytes))).into(),
                );
            }
            ScalarDataType::U32 => {
                self.add(
                    x,
                    format!("dd {}", format_numeric_value(read_u32(bytes))).into(),
                );
            }
            ScalarDataType::Char(n) => {
                self.add(x, format!("db {}", format_string_value(bytes, *n)).into());
            }
            ScalarDataType::Ofs16(seg_idx) => {
                let ofs_seg_idx = self
                    .project
                    .attr_at(self.seg_idx, self.base_ofs)
                    .and_then(|attr| attr.ofs_seg)
                    .or(*seg_idx);

                let v = read_u16(bytes);

                if let Some(ofs_seg_idx) = ofs_seg_idx
                    && let Some(name) = self.project.name_at(ofs_seg_idx, v)
                {
                    self.add(x, format!("{}", name).into());
                } else {
                    self.add(x, format!("dw {}", format_numeric_value(v)).into());
                }
            }
        }

        self.new_line();
    }

    fn layout_array(&mut self, x: u32, elem: &DataType, count: usize) {
        let index_w = count.ilog10() + 1;

        if elem.is_scalar() {
            for i in 0..count {
                self.add(x, format!("[{0:>1$}]", i, index_w as usize).into());
                self.layout_data(x + index_w + 3, elem);
                self.new_line();
                self.ofs += elem.byte_size(&self.project.structs) as u32;
            }
            return;
        }

        for i in 0..count {
            self.add(x, format!("[{0:>1$}] {{", i, index_w as usize).into());

            self.new_line();

            self.layout_data(x + 2, elem);

            self.add(x, "}".into());
            self.new_line();
        }
    }

    fn layout_struct(&mut self, x: u32, struct_idx: usize) {
        let fields = &self.project.structs[struct_idx].fields;

        let name_w = fields
            .iter()
            .map(|f| f.name.len())
            .max()
            .unwrap_or_default() as u32;
        let data_x0 = x + name_w + 1;

        for f in fields {
            self.add(x, f.name.clone());
            self.layout_data(data_x0, &f.r#type);
            self.ofs += f.r#type.byte_size(&self.project.structs) as u32;
        }
    }
}

pub fn render_widgets(widgets: &[Widget], buf: &mut String, y: u32) {
    let mut cursor = 0u32;
    for widget in widgets.iter().filter(|w| w.y == y) {
        while cursor < widget.x {
            buf.push(' ');
            cursor += 1;
        }
        buf.push_str(&widget.text);
        cursor += widget.text.len() as u32;
    }
}

fn read_u8(bytes: &[u8]) -> u32 {
    bytes.get(0).copied().unwrap_or_default() as u32
}

fn read_u16(bytes: &[u8]) -> u32 {
    let bytes = [
        bytes.get(0).copied().unwrap_or_default(),
        bytes.get(1).copied().unwrap_or_default(),
    ];
    u16::from_le_bytes(bytes) as u32
}

fn read_u32(bytes: &[u8]) -> u32 {
    let bytes = [
        bytes.get(0).copied().unwrap_or_default(),
        bytes.get(1).copied().unwrap_or_default(),
        bytes.get(2).copied().unwrap_or_default(),
        bytes.get(3).copied().unwrap_or_default(),
    ];
    u32::from_le_bytes(bytes)
}

fn most_significant_nybble(n: u32) -> u8 {
    if n == 0 {
        return 0;
    }

    let nybble_position = (31 - n.leading_zeros()) / 4;
    ((n >> (nybble_position * 4)) & 0xf) as u8
}

fn format_numeric_value(v: u32) -> SmallString {
    let mut s = SmallString::new();

    if v < 10 {
        let _ = write!(s, "{v}");
        return s;
    }

    if most_significant_nybble(v) > 9 {
        let _ = write!(s, "0");
    }
    let _ = write!(s, "{v:x}h");

    s
}

fn format_string_value(bytes: &[u8], n: usize) -> SmallString {
    let slice = &bytes[..bytes.len().min(n)];
    let mut s = SmallString::from("db '");

    for &b in slice {
        if b >= 0x20 && b < 0x7f && b != b'\'' && b != b'\\' {
            let _ = write!(s, "{}", b as char);
        } else {
            let _ = write!(s, "\\x{b:02x}");
        }
    }

    s.push('\'');

    s
}

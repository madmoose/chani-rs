use std::fmt::Write;

use std::path::Path;

use crate::{
    DataWidth, DecodedInstruction, DisplayContext, SmallString,
    data_type::{CompositeDataType, DataType, DisplayFmt, ScalarDataType},
    disassemble,
    project::{self, AttrType, FileFormat, Project, Segment, SegmentIdx},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WidgetKind {
    Address,
    Label,
    Separator,
    Opcode,
    Operand { index: usize },
    Punctuation,
    Data,
    ArrayIndex { base_ofs: u32, index: usize },
    StructField { base_ofs: u32, field_index: usize },
    FileHeader,
    SegmentHeader,
    SegmentDecl,
    AssumeDir,
    Comment,
}

#[derive(Debug, Clone)]
pub struct Widget {
    pub kind: WidgetKind,
    pub seg_idx: SegmentIdx,
    pub ofs: u32,
    pub x: u32,
    pub y: u32,
    pub text: SmallString,
}

pub type Widgets = Vec<Widget>;

pub struct LayoutBuilder<'a> {
    project: &'a Project,
    widgets: Widgets,
    seg_idx: SegmentIdx,
    base_ofs: u32,
    ofs: u32,
    label_x0: u32,
    text_x0: u32,
    comment_x0: u32,
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
        seg_idx: SegmentIdx,
        ofs: u32,
        ctx: &'a DisplayContext<'a>,
    ) -> Self {
        let widgets = Widgets::new();
        let label_x0 = project.segments[seg_idx].name.len() as u32 + 1 + 4 + 1;
        let text_x0 = label_x0 + 16;
        let comment_x0 = text_x0 + 40;
        let y = 0;
        Self {
            project,
            widgets,
            seg_idx,
            base_ofs: ofs,
            ofs,
            label_x0,
            text_x0,
            comment_x0,
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
            kind: WidgetKind::Address,
            seg_idx: self.seg_idx,
            ofs: self.ofs,
            x: 0,
            y: self.y,
            text,
        });
        self.line_state = LineState::BlankLine;
    }

    fn add(&mut self, x: u32, kind: WidgetKind, text: SmallString) {
        if self.line_state == LineState::EndOfLine || self.line_state == LineState::BlankLine {
            self.y += 1;
            self.line_state = LineState::StartOfLine;
        }
        if self.line_state == LineState::StartOfLine {
            let text = self.make_address();
            self.widgets.push(Widget {
                kind: WidgetKind::Address,
                seg_idx: self.seg_idx,
                ofs: self.ofs,
                x: 0,
                y: self.y,
                text,
            });
        }
        self.widgets.push(Widget {
            kind,
            seg_idx: self.seg_idx,
            ofs: self.ofs,
            x,
            y: self.y,
            text,
        });
        self.line_state = LineState::InLine;
    }

    fn layout_block_comment(&mut self, lines: &[&str]) {
        for line in lines {
            let text = format!("; {line}");
            self.add(self.label_x0, WidgetKind::Comment, text);
            self.new_line();
        }
    }

    fn layout_inline_comment(&mut self, comment: &str) {
        let x = self.comment_x0;
        self.widgets.push(Widget {
            kind: WidgetKind::Comment,
            seg_idx: self.seg_idx,
            ofs: self.base_ofs,
            x,
            y: self.y,
            text: format!("; {comment}"),
        });
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
        let comment = attr.and_then(|attr| attr.comment.as_deref());
        let seg = &self.project.segments[self.seg_idx];
        let is_code = seg.addr_attributes.is_op(self.base_ofs);

        let comment_lines: Vec<&str> = comment.map(|c| c.lines().collect()).unwrap_or_default();

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

            self.layout_instruction(label, &comment_lines, &inst, prev_stops_flow);
        } else {
            let prev_data_type = prev_ofs
                .and_then(|prev_ofs| self.project.attr_at(self.seg_idx, prev_ofs))
                .and_then(|attr| attr.r#type.as_ref())
                .and_then(|attr_type| attr_type.as_data());

            let prev_is_composite = prev_data_type
                .map(|data_type| data_type.is_composite())
                .unwrap_or_default();

            let default_data_type = DataType::Scalar(ScalarDataType::Unknown);
            let data_type = attr
                .and_then(|attr| attr.r#type.as_ref())
                .map(|typ| match typ {
                    AttrType::Code => unreachable!(),
                    AttrType::Data(attr_data_type) => attr_data_type,
                })
                .unwrap_or(&default_data_type);

            if prev_is_code
                || data_type.is_composite()
                || prev_is_composite
                || comment_lines.len() > 1
            {
                self.blank_line();
            }

            if comment_lines.len() > 1 {
                self.layout_block_comment(&comment_lines);
            }

            if let Some(label) = label {
                self.add(self.label_x0, WidgetKind::Label, format!("{label}:"));

                let long_label = self.label_x0 + label.len() as u32 + 1 >= self.text_x0;

                if long_label {
                    self.new_line();
                }
            }

            self.layout_data(self.text_x0, data_type);

            if let [single] = comment_lines.as_slice() {
                self.layout_inline_comment(single);
            }
        }

        self.widgets.sort_by_key(|w| (w.y, w.x));
    }

    fn layout_instruction(
        &mut self,
        label: Option<&str>,
        comment_lines: &[&str],
        inst: &DecodedInstruction,
        prev_stops_flow: bool,
    ) {
        if prev_stops_flow {
            self.add(
                self.label_x0,
                WidgetKind::Separator,
                "; ---------------------------------------------------------------------------"
                    .into(),
            );
            self.new_line();
            self.blank_line();
        }

        if label.is_some() || comment_lines.len() > 1 {
            self.blank_line();
        }

        if comment_lines.len() > 1 {
            self.layout_block_comment(comment_lines);
        }

        if let Some(label) = label {
            self.add(self.label_x0, WidgetKind::Label, format!("{label}:"));
            self.new_line();
        }

        let mut opcode = SmallString::new();
        let _ = inst.format_opcode(&mut opcode);
        let w = opcode.len() as u32;
        self.add(self.text_x0, WidgetKind::Opcode, opcode);

        let arg_fmts = self
            .project
            .attr_at(self.seg_idx, self.base_ofs)
            .map(|attr| attr.arg_fmts)
            .unwrap_or([None; 2]);
        let inst_ctx = DisplayContext {
            lookup: self.ctx.lookup,
            arg_fmts,
        };

        let mut x = self.text_x0 + u32::max(w + 1, 8);
        for i in 0..inst.arg_count() {
            if i > 0 {
                self.add(x, WidgetKind::Punctuation, ", ".into());
                x += 2;
            }
            let mut s = SmallString::new();
            let _ = inst.format_arg(&mut s, i, &inst_ctx);
            let w = s.len() as u32;

            self.add(x, WidgetKind::Operand { index: i }, s);

            x += w;
        }

        if let [single] = comment_lines {
            self.layout_inline_comment(single);
        }

        self.new_line();
    }

    fn layout_data(&mut self, x: u32, data: &DataType) {
        self.layout_data_fmt(x, data, DisplayFmt::Default);
    }

    fn layout_data_fmt(&mut self, x: u32, data: &DataType, fmt: DisplayFmt) {
        match data {
            DataType::Formatted(inner_fmt, inner) => self.layout_data_fmt(x, inner, *inner_fmt),
            DataType::Scalar(scalar) => self.layout_scalar(x, scalar, fmt),
            DataType::Composite(CompositeDataType::Array { elem, count }) => {
                self.layout_array_fmt(x, elem, *count, fmt)
            }
            DataType::Composite(CompositeDataType::Struct(idx)) => self.layout_struct(x, *idx),
        }
    }

    fn layout_scalar(&mut self, x: u32, scalar: &ScalarDataType, fmt: DisplayFmt) {
        let bytes = self.project.bytes_at_seg(self.seg_idx, self.ofs);

        match scalar {
            ScalarDataType::Unknown => {
                let b = read_u8(bytes);
                let mut s = SmallString::new();
                let _ = write!(s, "db {}", format_numeric_value(b));
                let b = b as u8;
                if b.is_ascii_graphic() {
                    if b == b'\'' {
                        let _ = write!(s, " '\''");
                    } else {
                        let _ = write!(s, " '{}'", b as char);
                    }
                }
                self.add(x, WidgetKind::Data, s);
                self.ofs += 1;
            }
            ScalarDataType::U8 => {
                let v = read_u8(bytes);
                let text = if fmt == DisplayFmt::Char {
                    let b = v as u8;
                    if b.is_ascii_graphic() && b != b'\'' {
                        format!("db '{}'", b as char)
                    } else {
                        format!("db {}", format_numeric_value(v))
                    }
                } else {
                    format!("dw {}", format_value(v, DataWidth::Byte, fmt))
                };
                self.add(x, WidgetKind::Data, text);
                self.ofs += 1;
            }
            ScalarDataType::U16 => {
                self.add(
                    x,
                    WidgetKind::Data,
                    format!("dw {}", format_value(read_u16(bytes), DataWidth::Word, fmt)),
                );
                self.ofs += 2;
            }
            ScalarDataType::U32 => {
                self.add(
                    x,
                    WidgetKind::Data,
                    format!(
                        "dd {}",
                        format_value(read_u32(bytes), DataWidth::Dword, fmt)
                    ),
                );
                self.ofs += 4;
            }
            ScalarDataType::Str(n) => {
                self.add(
                    x,
                    WidgetKind::Data,
                    format!("db {}", format_string_value(bytes, *n)),
                );
                self.ofs += *n as u32;
            }
            ScalarDataType::CStr => {
                let null_pos = bytes.iter().position(|&b| b == 0);
                let str_len = null_pos.unwrap_or(bytes.len());
                let mut text = format_string_value(bytes, str_len);
                if null_pos.is_some() {
                    let _ = write!(text, ", 0");
                }
                self.add(x, WidgetKind::Data, text);
                self.ofs += str_len as u32 + 1;
            }
            ScalarDataType::Ofs16(seg_idx) => {
                let ofs_seg_idx = self
                    .project
                    .attr_at(self.seg_idx, self.base_ofs)
                    .and_then(|attr| attr.ofs_seg)
                    .or(*seg_idx);

                let v = read_u16(bytes);

                if let Some(ofs_seg_idx) = ofs_seg_idx
                    && let Some(name) = self.project.resolve_label(ofs_seg_idx, v)
                {
                    self.add(x, WidgetKind::Data, format!("dw {}", name));
                } else {
                    self.add(
                        x,
                        WidgetKind::Data,
                        format!("dw {}", format_value(v, DataWidth::Word, fmt)),
                    );
                }
                self.ofs += 2;
            }
        }

        self.new_line();
    }

    fn layout_array_fmt(&mut self, x: u32, elem: &DataType, count: usize, fmt: DisplayFmt) {
        let base_ofs = self.ofs;
        let index_w = count.ilog10() + 1;

        if elem.is_scalar() {
            for i in 0..count {
                self.add(
                    x,
                    WidgetKind::ArrayIndex { base_ofs, index: i },
                    format!("[{0:>1$}]", i, index_w as usize),
                );
                self.layout_data_fmt(x + index_w + 3, elem, fmt);
                self.new_line();
            }
            return;
        }

        for i in 0..count {
            self.add(
                x,
                WidgetKind::ArrayIndex { base_ofs, index: i },
                format!("[{0:>1$}] {{", i, index_w as usize),
            );
            self.new_line();
            self.layout_data_fmt(x + 2, elem, fmt);
            self.add(x, WidgetKind::Punctuation, "}".into());
            self.new_line();
        }
    }

    fn layout_struct(&mut self, x: u32, struct_idx: usize) {
        let base_ofs = self.ofs;
        let fields = &self.project.structs[struct_idx].fields;

        let name_w = fields
            .iter()
            .map(|f| f.name.len())
            .max()
            .unwrap_or_default() as u32;
        let data_x0 = x + name_w + 1;

        for (field_index, f) in fields.iter().enumerate() {
            self.add(
                x,
                WidgetKind::StructField {
                    base_ofs,
                    field_index,
                },
                f.name.clone(),
            );
            self.layout_data(data_x0, &f.r#type);
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
    bytes.first().copied().unwrap_or_default() as u32
}

fn read_u16(bytes: &[u8]) -> u32 {
    let bytes = [
        bytes.first().copied().unwrap_or_default(),
        bytes.get(1).copied().unwrap_or_default(),
    ];
    u16::from_le_bytes(bytes) as u32
}

fn read_u32(bytes: &[u8]) -> u32 {
    let bytes = [
        bytes.first().copied().unwrap_or_default(),
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

fn format_value(v: u32, width: DataWidth, fmt: DisplayFmt) -> SmallString {
    let mut s = SmallString::new();
    match fmt {
        DisplayFmt::Default | DisplayFmt::Hex | DisplayFmt::Char => return format_numeric_value(v),
        DisplayFmt::Dec => {
            let _ = write!(s, "{v}");
        }
        DisplayFmt::SignedDec => {
            let _ = write!(s, "{}", width.sign_extend(v));
        }
        DisplayFmt::Bin => {
            let _ = write!(s, "0b{v:b}");
        }
    }
    s
}

fn format_string_value(bytes: &[u8], n: usize) -> SmallString {
    let slice = &bytes[..bytes.len().min(n)];
    let mut s = SmallString::new();

    for &b in slice {
        if (0x20..0x7f).contains(&b) && b != b'\'' && b != b'\\' {
            let _ = write!(s, "{}", b as char);
        } else if b == b'\\' {
            let _ = write!(s, "\\");
        } else {
            let _ = write!(s, "\\x{b:02x}");
        }
    }

    s.push('\'');

    s
}

fn push_addr_widget(
    widgets: &mut Vec<Widget>,
    seg_idx: SegmentIdx,
    seg_name: &str,
    ofs: u32,
    y: u32,
) {
    let mut text = SmallString::new();
    let _ = write!(text, "{seg_name}:{ofs:04x}");
    widgets.push(Widget {
        kind: WidgetKind::Address,
        seg_idx,
        ofs,
        x: 0,
        y,
        text,
    });
}

fn push_header_line(
    widgets: &mut Vec<Widget>,
    kind: WidgetKind,
    seg_idx: SegmentIdx,
    seg_name: &str,
    ofs: u32,
    x: u32,
    y: &mut u32,
    text: &str,
) {
    push_addr_widget(widgets, seg_idx, seg_name, ofs, *y);
    let mut s = SmallString::new();
    let _ = write!(s, "{text}");
    widgets.push(Widget {
        kind,
        seg_idx,
        ofs,
        x,
        y: *y,
        text: s,
    });
    *y += 1;
}

fn push_header_blank(
    widgets: &mut Vec<Widget>,
    seg_idx: SegmentIdx,
    seg_name: &str,
    ofs: u32,
    y: &mut u32,
) {
    push_addr_widget(widgets, seg_idx, seg_name, ofs, *y);
    *y += 1;
}

fn labeled_item_lines(items: &[(&str, &str)]) -> Vec<String> {
    let width = items.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
    items
        .iter()
        .map(|(k, v)| format!("; {k:<width$}: {v}"))
        .collect()
}

fn centered_box_lines(titles: &[&str]) -> Vec<String> {
    const BOX_INNER: usize = 73;
    let border = format!("; +{}+", "-".repeat(BOX_INNER));
    let mut lines = vec![border.clone()];
    for &title in titles {
        let pad = BOX_INNER.saturating_sub(title.len());
        let lpad = pad / 2;
        let rpad = pad - lpad;
        lines.push(format!(
            "; |{}{title}{}|",
            " ".repeat(lpad),
            " ".repeat(rpad)
        ));
    }
    lines.push(border);
    lines
}

fn generate_file_header(
    project: &Project,
    seg_idx: SegmentIdx,
    seg_name: &str,
    widgets: &mut Vec<Widget>,
    global_y: &mut u32,
) {
    let label_x0 = seg_name.len() as u32 + 6;

    let hl = |w: &mut Vec<Widget>, y: &mut u32, text: &str| {
        push_header_line(
            w,
            WidgetKind::FileHeader,
            seg_idx,
            seg_name,
            0,
            label_x0,
            y,
            text,
        );
    };

    hl(widgets, global_y, ";");
    for line in &centered_box_lines(&[
        "",
        "This file is generated by Chani Disassembler",
        // "",
        // "~ thomas.fach-pedersen.net ~",
        "",
    ]) {
        hl(widgets, global_y, line);
    }
    hl(widgets, global_y, ";");

    let mut items: Vec<(&str, String)> = Vec::new();

    if let Some(file) = project.files.first() {
        let file_name = Path::new(&file.path)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(&file.path)
            .to_owned();
        items.push(("File Name", file_name));

        let fmt = match file.format {
            FileFormat::Exe => "MS-DOS executable (EXE)",
            FileFormat::Com => "MS-DOS COM executable",
            FileFormat::Bin => "Binary",
        };
        items.push(("Format", fmt.to_owned()));

        if let Some(hash) = &file.hash {
            let hex = hash.bytes.iter().map(|b| format!("{b:02x}")).collect();
            items.push(("Input SHA1", hex));
        }
    }

    if let Some(exe) = &project.exe {
        items.push((
            "Loaded length",
            format_numeric_value(exe.image.len() as u32).to_string(),
        ));

        let entry_str = match project.segment_index_for(exe.head.cs) {
            Some(idx) => format!("{}:{:04x}", project.segments[idx].name, exe.head.ip),
            None => format!("{:04X}h:{:04X}h", exe.head.cs, exe.head.ip),
        };
        items.push(("Entry Point", entry_str));
    }

    let item_refs: Vec<(&str, &str)> = items.iter().map(|(k, v)| (*k, v.as_str())).collect();
    for line in &labeled_item_lines(&item_refs) {
        hl(widgets, global_y, line);
    }

    hl(widgets, global_y, ";");
    push_header_blank(widgets, seg_idx, seg_name, 0, global_y);
}

fn generate_segment_header(
    seg: &Segment,
    seg_idx: SegmentIdx,
    widgets: &mut Vec<Widget>,
    global_y: &mut u32,
) {
    let seg_name = seg.name.as_str();
    let label_x0 = seg_name.len() as u32 + 6;
    let text_x0 = label_x0 + 16;

    let sh = |w: &mut Vec<Widget>, y: &mut u32, text: &str| {
        push_header_line(
            w,
            WidgetKind::SegmentHeader,
            seg_idx,
            seg_name,
            0,
            label_x0,
            y,
            text,
        );
    };

    sh(widgets, global_y, &format!("; {}", "-".repeat(75)));
    sh(widgets, global_y, ";");

    let type_str = match seg.r#type.as_deref() {
        Some("code") => "Pure code",
        Some("data") => "Pure data",
        Some("stack") => "Stack",
        Some(other) => other,
        None => "Unknown",
    };
    sh(widgets, global_y, &format!("; Segment type: {type_str}"));
    sh(widgets, global_y, ";");

    let class = match seg.r#type.as_deref() {
        Some("data") => "DATA",
        Some("stack") => "STACK",
        _ => "CODE",
    };
    push_header_line(
        widgets,
        WidgetKind::SegmentDecl,
        seg_idx,
        seg_name,
        0,
        label_x0,
        global_y,
        &format!("{seg_name:<16}segment byte public '{class}' use16"),
    );

    if seg.r#type.as_deref() == Some("code") || seg.r#type.is_none() {
        push_header_line(
            widgets,
            WidgetKind::AssumeDir,
            seg_idx,
            seg_name,
            0,
            text_x0,
            global_y,
            &format!("assume cs:{seg_name}"),
        );
    }
    for (reg, assume_seg) in &seg.assume {
        push_header_line(
            widgets,
            WidgetKind::AssumeDir,
            seg_idx,
            seg_name,
            0,
            text_x0,
            global_y,
            &format!("assume {reg}:{assume_seg}"),
        );
    }

    sh(widgets, global_y, ";");
}

/// Build a globally-y-positioned flat widget list for the entire project.
/// Returns the widgets and the total number of rows.
pub fn generate_widgets(project: &Project) -> (Vec<Widget>, u32) {
    let mut all_widgets: Vec<Widget> = Vec::new();
    let mut global_y = 0u32;

    let (first_seg_idx, first_seg_name) = project
        .segments
        .indexed_iter()
        .next()
        .map(|(idx, seg)| (idx, seg.name.as_str().to_owned()))
        .unwrap_or_else(|| (SegmentIdx::from(0usize), String::new()));

    for (i, (seg_idx, seg)) in project.segments.indexed_iter().enumerate() {
        if i == 0 {
            generate_file_header(
                project,
                first_seg_idx,
                &first_seg_name,
                &mut all_widgets,
                &mut global_y,
            );
        }

        generate_segment_header(seg, seg_idx, &mut all_widgets, &mut global_y);

        let seg_start = seg.start.unwrap_or(0);
        let seg_end = seg.end.unwrap_or(0);

        let mut ofs = seg_start;
        while ofs < seg_end {
            let sreg_map = project
                .seg_dataflow
                .state_at(project, seg_idx, ofs + seg_start)
                .map(|s| s.to_sreg_map())
                .unwrap_or(crate::SRegMap {
                    cs: Some(seg_idx),
                    ..Default::default()
                });

            let ofs_seg = project.attr_at(seg_idx, ofs).and_then(|attr| attr.ofs_seg);

            let lookup = project::ProjectLookup {
                project,
                sreg_map,
                register_file: None,
                default_seg: ofs_seg,
            };
            let ctx = DisplayContext {
                lookup: &lookup,
                arg_fmts: [None; 2],
            };

            let mut builder = LayoutBuilder::new(project, seg_idx, ofs, &ctx);
            builder.layout();
            let local_widgets = builder.widgets();

            let local_max_y = local_widgets.iter().map(|w| w.y).max().unwrap_or(0);
            for mut w in local_widgets {
                w.y += global_y;
                all_widgets.push(w);
            }
            global_y += local_max_y + 1;

            let Some(next_ofs) = project.segments[seg_idx].addr_attributes.next(ofs) else {
                break;
            };
            ofs = next_ofs;
        }
    }

    (all_widgets, global_y)
}

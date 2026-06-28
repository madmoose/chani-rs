use std::io;

fn encode_value(v: &str) -> String {
    if v.contains('\n') {
        let mut result = String::from("[[[");
        for line in v.lines() {
            result.push('\n');
            result.push_str("    ");
            result.push_str(line);
        }
        result.push_str("\n]]]");
        result
    } else if v.contains(';') {
        format!("[[[{v}]]]")
    } else {
        v.to_owned()
    }
}

#[derive(Debug)]
enum BlockItem {
    Blank,
    Prop(String, String),
    Block(BlockDict),
    Inline(InlineDict),
}

/// Builder for a block-style dict (`name[key]:\n    ...\nend`).
#[derive(Debug)]
pub struct BlockDict {
    name: String,
    key: String,
    prop_indent: usize,
    items: Vec<BlockItem>,
}

impl BlockDict {
    /// Dict with 4-space prop indentation (binary, struct, …).
    pub fn new(name: impl Into<String>, key: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            key: key.into(),
            prop_indent: 4,
            items: Vec::new(),
        }
    }

    /// Dict with no prop indentation (the top-level project block).
    pub fn root(name: impl Into<String>, key: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            key: key.into(),
            prop_indent: 0,
            items: Vec::new(),
        }
    }

    pub fn blank(&mut self) -> &mut Self {
        self.items.push(BlockItem::Blank);
        self
    }

    pub fn prop_encoded(&mut self, key: impl Into<String>, value: &str) -> &mut Self {
        self.items
            .push(BlockItem::Prop(key.into(), encode_value(value)));
        self
    }

    /// Add a `key = value` property. Multiline values are auto-wrapped in `[[[ ]]]`.
    pub fn prop(&mut self, key: impl Into<String>, value: &str) -> &mut Self {
        self.items
            .push(BlockItem::Prop(key.into(), value.to_string()));
        self
    }

    pub fn add_block(&mut self, dict: BlockDict) -> &mut Self {
        self.items.push(BlockItem::Block(dict));
        self
    }

    pub fn add_inline(&mut self, dict: InlineDict) -> &mut Self {
        self.items.push(BlockItem::Inline(dict));
        self
    }

    pub fn write_to(&self, w: &mut impl io::Write) -> io::Result<()> {
        let key_width = self
            .items
            .iter()
            .filter_map(|i| {
                if let BlockItem::Prop(k, _) = i {
                    Some(k.len())
                } else {
                    None
                }
            })
            .max()
            .unwrap_or(0);
        let padding = " ".repeat(self.prop_indent);

        writeln!(w, "{}[{}]:", self.name, self.key)?;
        for item in &self.items {
            match item {
                BlockItem::Blank => writeln!(w)?,
                BlockItem::Prop(k, v) => writeln!(w, "{padding}{k:<key_width$} = {v}")?,
                BlockItem::Block(b) => b.write_to(w)?,
                BlockItem::Inline(i) => {
                    write!(w, "{padding}")?;
                    i.write_to(w)?;
                }
            }
        }
        writeln!(w, "end")
    }
}

/// Builder for an inline dict (`name[key]: k1 = v1; k2 = v2`).
#[derive(Debug)]
pub struct InlineDict {
    name: String,
    key: String,
    props: Vec<(String, String)>,
}

impl InlineDict {
    pub fn new(name: impl Into<String>, key: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            key: key.into(),
            props: Vec::new(),
        }
    }

    /// Add a `key = value` property. Multiline values are auto-wrapped in `[[[ ]]]`.
    pub fn prop(&mut self, key: impl Into<String>, value: &str) -> &mut Self {
        self.props.push((key.into(), value.to_string()));
        self
    }

    pub fn prop_encoded(&mut self, key: impl Into<String>, value: &str) -> &mut Self {
        self.props.push((key.into(), encode_value(value)));
        self
    }

    pub fn is_empty(&self) -> bool {
        self.props.is_empty()
    }

    pub fn write_to(&self, w: &mut impl io::Write) -> io::Result<()> {
        write!(w, "{}[{}]:", self.name, self.key)?;
        for (i, (k, v)) in self.props.iter().enumerate() {
            if i == 0 {
                write!(w, " ")?;
            } else {
                write!(w, "; ")?;
            }
            write!(w, "{k} = {v}")?;
        }
        writeln!(w)
    }
}

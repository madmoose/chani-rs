use std::fmt;

pub const DEFAULT_COMMENT_COL: usize = 58;

enum LineBody {
    Label { name: String },
    Code { text: String },
    Data { text: String },
}

pub struct ListingLine {
    seg_name: String,
    ofs: u16,
    body: LineBody,
    comments: Vec<String>,
    comment_col: usize,
}

impl ListingLine {
    pub fn label(seg_name: impl Into<String>, ofs: u16, name: impl Into<String>) -> Self {
        Self {
            seg_name: seg_name.into(),
            ofs,
            body: LineBody::Label { name: name.into() },
            comments: Vec::new(),
            comment_col: DEFAULT_COMMENT_COL,
        }
    }

    pub fn code(seg_name: impl Into<String>, ofs: u16, text: impl Into<String>) -> Self {
        Self {
            seg_name: seg_name.into(),
            ofs,
            body: LineBody::Code { text: text.into() },
            comments: Vec::new(),
            comment_col: DEFAULT_COMMENT_COL,
        }
    }

    pub fn data(seg_name: impl Into<String>, ofs: u16, text: impl Into<String>) -> Self {
        Self {
            seg_name: seg_name.into(),
            ofs,
            body: LineBody::Data { text: text.into() },
            comments: Vec::new(),
            comment_col: DEFAULT_COMMENT_COL,
        }
    }

    pub fn add_comment(mut self, comment: impl Into<String>) -> Self {
        self.comments.push(comment.into());
        self
    }

    pub fn with_comment_col(mut self, col: usize) -> Self {
        self.comment_col = col;
        self
    }

    pub fn write_to(&self, w: &mut impl fmt::Write) -> fmt::Result {
        match &self.body {
            LineBody::Label { name } => {
                write!(w, "{}:{:04x}", self.seg_name, self.ofs)?;
                write!(w, " {name}:")?;
                if let Some(cmt) = self.comments.first() {
                    // "seg000:0000  name:" — compute written chars
                    let written = self.seg_name.len() + 1 + 4 + 2 + name.len() + 1;
                    let pad = if written < self.comment_col {
                        self.comment_col - written
                    } else {
                        1
                    };
                    for _ in 0..pad {
                        write!(w, " ")?;
                    }
                    write!(w, "; {cmt}")?;
                }
                writeln!(w)?;
            }
            LineBody::Code { text } | LineBody::Data { text } => {
                write!(w, "    {text}")?;

                if let Some(cmt) = self.comments.first() {
                    // columns written: seg_name + ":" + "xxxx" + "    " + text
                    let written = self.seg_name.len() + 1 + 4 + 4 + text.len();
                    let pad = if written < self.comment_col {
                        self.comment_col - written
                    } else {
                        1
                    };
                    for _ in 0..pad {
                        write!(w, " ")?;
                    }
                    write!(w, "; {cmt}")?;
                }

                writeln!(w)?;
            }
        }

        for cmt in self.comments.iter().skip(1) {
            for _ in 0..self.comment_col {
                write!(w, " ")?;
            }
            write!(w, "; {cmt}")?;
        }

        Ok(())
    }
}

impl fmt::Display for ListingLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_to(f)
    }
}

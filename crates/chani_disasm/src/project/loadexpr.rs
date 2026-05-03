/// Parsed (but not yet file-size-resolved) load expression on a segment.
///
/// Grammar: `(<seg-range>:)?<file-name>(<file-range>)?`
/// where a range is `[<start>?..<end>?]`.
#[derive(Debug, Clone)]
pub struct LoadExpr {
    pub file_idx: usize,
    pub seg_start: u32,
    pub seg_end: Option<u32>,
    pub file_start: u32,
    pub file_end: Option<u32>,
}

impl LoadExpr {
    /// Resolve to `(seg_start, seg_end, file_start, file_end)` given the file size.
    pub fn resolve(&self, file_size: u32) -> Result<(u32, u32, u32, u32), String> {
        let seg_start = self.seg_start;
        let file_start = self.file_start;

        if file_start > file_size {
            return Err(format!(
                "file_start {file_start:#x} > file_size {file_size:#x}"
            ));
        }

        let length = match (self.seg_end, self.file_end) {
            (Some(se), Some(fe)) => {
                let seg_len = se
                    .checked_sub(seg_start)
                    .ok_or_else(|| format!("seg_end {se:#x} < seg_start {seg_start:#x}"))?;
                let file_len = fe
                    .checked_sub(file_start)
                    .ok_or_else(|| format!("file_end {fe:#x} < file_start {file_start:#x}"))?;
                if seg_len != file_len {
                    return Err(format!(
                        "length mismatch: seg_len {seg_len:#x} != file_len {file_len:#x}"
                    ));
                }
                seg_len
            }
            (Some(se), None) => se
                .checked_sub(seg_start)
                .ok_or_else(|| format!("seg_end {se:#x} < seg_start {seg_start:#x}"))?,
            (None, Some(fe)) => fe
                .checked_sub(file_start)
                .ok_or_else(|| format!("file_end {fe:#x} < file_start {file_start:#x}"))?,
            (None, None) => file_size - file_start,
        };

        let seg_end = seg_start + length;
        let file_end = file_start + length;

        if file_end > file_size {
            return Err(format!("file_end {file_end:#x} > file_size {file_size:#x}"));
        }

        Ok((seg_start, seg_end, file_start, file_end))
    }
}

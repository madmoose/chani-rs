use std::ops::{Index, IndexMut};

use crate::{
    SmallString,
    address_attributes::AddressAttributes,
    project::{Assumes, LoadExpr},
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SegmentIdx(usize);

#[derive(Debug, Default, Clone)]
pub struct Segments {
    inner: Vec<Segment>,
}

impl Index<SegmentIdx> for Segments {
    type Output = Segment;

    fn index(&self, index: SegmentIdx) -> &Self::Output {
        &self.inner[index.0]
    }
}

impl IndexMut<SegmentIdx> for Segments {
    fn index_mut(&mut self, index: SegmentIdx) -> &mut Self::Output {
        &mut self.inner[index.0]
    }
}

impl IntoIterator for Segments {
    type Item = Segment;
    type IntoIter = std::vec::IntoIter<Segment>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a Segments {
    type Item = &'a Segment;
    type IntoIter = std::slice::Iter<'a, Segment>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut Segments {
    type Item = &'a mut Segment;
    type IntoIter = std::slice::IterMut<'a, Segment>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl From<Vec<Segment>> for Segments {
    fn from(value: Vec<Segment>) -> Self {
        Self { inner: value }
    }
}

impl From<usize> for SegmentIdx {
    fn from(value: usize) -> Self {
        SegmentIdx(value)
    }
}

impl From<SegmentIdx> for usize {
    fn from(value: SegmentIdx) -> Self {
        value.0
    }
}

impl Segments {
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn push(&mut self, seg: Segment) {
        self.inner.push(seg);
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Segment> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Segment> {
        self.inner.iter_mut()
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item = (SegmentIdx, &Segment)> {
        self.inner
            .iter()
            .enumerate()
            .map(|(i, s)| (SegmentIdx(i), s))
    }

    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item = (SegmentIdx, &mut Segment)> {
        self.inner
            .iter_mut()
            .enumerate()
            .map(|(i, s)| (SegmentIdx(i), s))
    }
}

#[derive(Debug, Clone)]
pub struct Segment {
    pub name: SmallString,
    pub r#type: Option<SmallString>,
    pub start: Option<u32>,
    pub end: Option<u32>,
    pub addr_attributes: AddressAttributes,
    /// Assumed segment register values: indexed by SReg (ES=0, CS=1, SS=2, DS=3).
    pub assume: Assumes,
    pub load: Option<LoadExpr>,
}

impl Segment {
    pub fn size(&self) -> usize {
        self.end
            .unwrap_or(0)
            .saturating_sub(self.start.unwrap_or(0)) as usize
    }
}

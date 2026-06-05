//! Application model: the listing as a navigable grid of widgets.
//!
//! The listing is the shared widget grid from
//! [`chani_disasm::layout::generate_widgets_with_options`] — the same one the
//! HTML exporter walks. We add: a colour per [`WidgetKind`] matching the HTML
//! palette, a cursor that hops between *navigable* widgets, link-following via
//! [`chani_disasm::layout::resolve_link_at`], and an address-keyed back stack.

use std::collections::{BTreeMap, HashMap};
use std::ops::Range;
use std::path::PathBuf;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::Frame;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};

use chani_disasm::layout::{
    LayoutOptions, Widget, WidgetKind, build_label_index, generate_widgets_with_options,
    resolve_link_at, resolve_links,
};
use chani_disasm::project::{Attr, Project, SegmentIdx};
use chani_disasm::{Address, MemRef, Operand, decode};

/// An RGB triple from the HTML palette. Rendered either as truecolor or, on
/// terminals without it (e.g. macOS Terminal.app), the nearest xterm-256 index —
/// see [`to_color`].
type Rgb = (u8, u8, u8);

/// VS Code dark background, matching the HTML `body` rule.
const BG: Rgb = (0x1e, 0x1e, 0x1e);
/// Default foreground (`.punct`, plain text).
const FG: Rgb = (0xd4, 0xd4, 0xd4);
/// Focused-widget highlight background (VS Code selection blue). Text keeps its
/// syntax colour on top, so the cursor reads as bright-on-dark rather than
/// inverting to dark-on-bright.
const SEL: Rgb = (0x26, 0x4f, 0x78);
/// Subtle background for the whole row the cursor is on (VS Code current-line
/// highlight), just above [`BG`].
const LINE_HL: Rgb = (0x2a, 0x2d, 0x2e);
/// Footer bar background / foreground.
const FOOTER_BG: Rgb = (0x2d, 0x2d, 0x2d);
const FOOTER_FG: Rgb = (0xa0, 0xa0, 0xa0);
/// Rows of scroll-off kept above/below the cursor when it moves.
const SCROLLOFF: u32 = 2;
/// Rows moved per mouse-wheel notch.
const WHEEL_ROWS: u32 = 3;

/// A point on the back stack: where the cursor was before a jump. Keyed by
/// address + relative line (rather than a raw row) so it stays valid even if the
/// listing is regenerated; `screen_row` reproduces the prior scroll position.
struct NavMark {
    addr: Address,
    rel_line: u32,
    col: u32,
    screen_row: u32,
}

/// What a single-line input prompt does on submit.
#[derive(Clone, Copy, PartialEq, Eq)]
enum PromptKind {
    /// Jump to a `seg:ofs` address or a label name.
    Goto,
    /// Free-text search of the rendered listing.
    Search,
    /// Rename the label at the prompt's address.
    Rename,
}

impl PromptKind {
    /// The leading text shown before the input on the prompt line.
    fn prefix(self) -> &'static str {
        match self {
            PromptKind::Goto => ":",
            PromptKind::Search => "/",
            PromptKind::Rename => "name ",
        }
    }
}

/// Modal segment picker for setting an instruction's `ofs_seg`. `items[0]` is
/// `None` (clear the assumption); the rest are the project's segments in order.
struct OfsSegPicker {
    /// Instruction address being edited.
    addr: Address,
    /// The operand offset, previewed against each candidate segment.
    offset: u16,
    items: Vec<Option<SegmentIdx>>,
    selected: usize,
}

impl OfsSegPicker {
    fn up(&mut self) {
        self.selected = self.selected.saturating_sub(1);
    }

    fn down(&mut self) {
        if self.selected + 1 < self.items.len() {
            self.selected += 1;
        }
    }

    fn choice(&self) -> Option<SegmentIdx> {
        self.items[self.selected]
    }
}

/// Char index of the start of the word at or before `col` in `text`: skip any
/// whitespace immediately left of the cursor, then the word run before it.
/// Drives Ctrl-W and word-left motion.
fn prev_word(text: &str, col: usize) -> usize {
    let chars: Vec<char> = text.chars().collect();
    let mut i = col.min(chars.len());
    while i > 0 && chars[i - 1].is_whitespace() {
        i -= 1;
    }
    while i > 0 && !chars[i - 1].is_whitespace() {
        i -= 1;
    }
    i
}

/// Char index of the start of the next word after `col` in `text`: skip the word
/// run under the cursor, then any whitespace after it. Drives word-right motion.
fn next_word(text: &str, col: usize) -> usize {
    let chars: Vec<char> = text.chars().collect();
    let n = chars.len();
    let mut i = col.min(n);
    while i < n && !chars[i].is_whitespace() {
        i += 1;
    }
    while i < n && chars[i].is_whitespace() {
        i += 1;
    }
    i
}

/// Byte offset of character index `col` in `s` (clamped to `s.len()`).
fn byte_at(s: &str, col: usize) -> usize {
    s.char_indices().nth(col).map(|(b, _)| b).unwrap_or(s.len())
}

/// A single-line text field with a char cursor and readline/emacs-style editing:
/// arrows, Home/End, Ctrl-A/E/B/F/D/H, Ctrl-W/U/K, and word motions
/// (Alt-B/F, Ctrl/Alt-←/→). Used by the goto/search/rename prompt.
#[derive(Default)]
struct LineInput {
    text: String,
    /// Cursor position as a character index into `text`.
    col: usize,
}

impl LineInput {
    fn new(text: String) -> Self {
        let col = text.chars().count();
        LineInput { text, col }
    }

    fn len(&self) -> usize {
        self.text.chars().count()
    }

    fn insert(&mut self, c: char) {
        self.text.insert(byte_at(&self.text, self.col), c);
        self.col += 1;
    }

    fn backspace(&mut self) {
        if self.col > 0 {
            self.text.remove(byte_at(&self.text, self.col - 1));
            self.col -= 1;
        }
    }

    fn delete(&mut self) {
        if self.col < self.len() {
            self.text.remove(byte_at(&self.text, self.col));
        }
    }

    /// Delete the range `[from, self.col)` (char indices), leaving the cursor at
    /// `from`. Shared backing for Ctrl-W and Ctrl-U.
    fn delete_back_to(&mut self, from: usize) {
        let lo = byte_at(&self.text, from);
        let hi = byte_at(&self.text, self.col);
        self.text.replace_range(lo..hi, "");
        self.col = from;
    }

    fn delete_word_back(&mut self) {
        self.delete_back_to(prev_word(&self.text, self.col));
    }

    fn delete_to_start(&mut self) {
        self.delete_back_to(0);
    }

    fn delete_to_end(&mut self) {
        self.text.truncate(byte_at(&self.text, self.col));
    }

    /// Apply one editing key. Esc/Enter (submit/cancel) are handled by the caller.
    fn on_key(&mut self, key: KeyEvent) {
        let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
        let alt = key.modifiers.contains(KeyModifiers::ALT);
        match key.code {
            KeyCode::Left if ctrl || alt => self.col = prev_word(&self.text, self.col),
            KeyCode::Right if ctrl || alt => self.col = next_word(&self.text, self.col),
            KeyCode::Left => self.col = self.col.saturating_sub(1),
            KeyCode::Right if self.col < self.len() => self.col += 1,
            KeyCode::Right => {}
            KeyCode::Home => self.col = 0,
            KeyCode::End => self.col = self.len(),
            KeyCode::Backspace => self.backspace(),
            KeyCode::Delete => self.delete(),
            KeyCode::Char('a') if ctrl => self.col = 0,
            KeyCode::Char('e') if ctrl => self.col = self.len(),
            KeyCode::Char('b') if ctrl => self.col = self.col.saturating_sub(1),
            KeyCode::Char('f') if ctrl && self.col < self.len() => self.col += 1,
            KeyCode::Char('b') if alt => self.col = prev_word(&self.text, self.col),
            KeyCode::Char('f') if alt => self.col = next_word(&self.text, self.col),
            KeyCode::Char('h') if ctrl => self.backspace(),
            KeyCode::Char('d') if ctrl => self.delete(),
            KeyCode::Char('w') if ctrl => self.delete_word_back(),
            KeyCode::Char('u') if ctrl => self.delete_to_start(),
            KeyCode::Char('k') if ctrl => self.delete_to_end(),
            KeyCode::Char(c) if !ctrl && !alt => self.insert(c),
            _ => {}
        }
    }
}

/// A minimal multi-line text buffer for the comment editor: lines plus a
/// `(row, col)` cursor measured in characters.
struct CommentEditor {
    addr: Address,
    lines: Vec<String>,
    row: usize,
    col: usize,
}

impl CommentEditor {
    fn new(addr: Address, text: &str) -> Self {
        let mut lines: Vec<String> = text.lines().map(str::to_string).collect();
        if lines.is_empty() {
            lines.push(String::new());
        }
        let row = lines.len() - 1;
        let col = lines[row].chars().count();
        CommentEditor {
            addr,
            lines,
            row,
            col,
        }
    }

    /// The edited text, lines rejoined with `\n`.
    fn text(&self) -> String {
        self.lines.join("\n")
    }

    /// Byte offset of character index `col` in the current line.
    fn byte_at(&self, col: usize) -> usize {
        let line = &self.lines[self.row];
        line.char_indices()
            .nth(col)
            .map(|(b, _)| b)
            .unwrap_or(line.len())
    }

    fn line_len(&self) -> usize {
        self.lines[self.row].chars().count()
    }

    fn insert(&mut self, c: char) {
        let b = self.byte_at(self.col);
        self.lines[self.row].insert(b, c);
        self.col += 1;
    }

    fn newline(&mut self) {
        let b = self.byte_at(self.col);
        let rest = self.lines[self.row].split_off(b);
        self.lines.insert(self.row + 1, rest);
        self.row += 1;
        self.col = 0;
    }

    fn backspace(&mut self) {
        if self.col > 0 {
            let b = self.byte_at(self.col - 1);
            self.lines[self.row].remove(b);
            self.col -= 1;
        } else if self.row > 0 {
            let cur = self.lines.remove(self.row);
            self.row -= 1;
            self.col = self.line_len();
            self.lines[self.row].push_str(&cur);
        }
    }

    fn delete(&mut self) {
        if self.col < self.line_len() {
            let b = self.byte_at(self.col);
            self.lines[self.row].remove(b);
        } else if self.row + 1 < self.lines.len() {
            let next = self.lines.remove(self.row + 1);
            self.lines[self.row].push_str(&next);
        }
    }

    fn left(&mut self) {
        if self.col > 0 {
            self.col -= 1;
        } else if self.row > 0 {
            self.row -= 1;
            self.col = self.line_len();
        }
    }

    fn right(&mut self) {
        if self.col < self.line_len() {
            self.col += 1;
        } else if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = 0;
        }
    }

    fn up(&mut self) {
        if self.row > 0 {
            self.row -= 1;
            self.col = self.col.min(self.line_len());
        }
    }

    fn down(&mut self) {
        if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = self.col.min(self.line_len());
        }
    }

    /// Word-left within the line; at the line start, fall through to `left`
    /// (crossing into the previous line).
    fn word_left(&mut self) {
        if self.col > 0 {
            self.col = prev_word(&self.lines[self.row], self.col);
        } else {
            self.left();
        }
    }

    /// Word-right within the line; at the line end, fall through to `right`.
    fn word_right(&mut self) {
        if self.col < self.line_len() {
            self.col = next_word(&self.lines[self.row], self.col);
        } else {
            self.right();
        }
    }

    /// Delete the word before the cursor; at the line start, join with the
    /// previous line (like `backspace`).
    fn delete_word_back(&mut self) {
        if self.col == 0 {
            self.backspace();
            return;
        }
        let from = prev_word(&self.lines[self.row], self.col);
        let lo = self.byte_at(from);
        let hi = self.byte_at(self.col);
        self.lines[self.row].replace_range(lo..hi, "");
        self.col = from;
    }

    /// Delete from the line start to the cursor (Ctrl-U).
    fn delete_to_start(&mut self) {
        let hi = self.byte_at(self.col);
        self.lines[self.row].replace_range(..hi, "");
        self.col = 0;
    }

    /// Delete from the cursor to the line end (Ctrl-K).
    fn delete_to_end(&mut self) {
        let lo = self.byte_at(self.col);
        self.lines[self.row].truncate(lo);
    }
}

/// Input mode: normal navigation, a single-line prompt, or the comment editor.
enum Mode {
    Normal,
    Prompt {
        kind: PromptKind,
        input: LineInput,
        /// The address an edit prompt (rename) targets, captured when it opened.
        addr: Option<Address>,
    },
    /// Multi-line comment editor popup.
    Comment(CommentEditor),
    /// Segment picker popup for setting `ofs_seg`.
    OfsSeg(OfsSegPicker),
}

/// One reversible edit: the authoritative attribute at `addr` before and after
/// the change (`None` = no attribute there). Undo restores `before`, redo
/// applies `after`.
struct UndoEntry {
    addr: Address,
    before: Option<Attr>,
    after: Option<Attr>,
    label: String,
}

pub struct App {
    /// Authoritative document — the only state that is saved. Edits mutate this;
    /// it is never analyzed in place.
    base: Project,
    /// Analyzed view derived from `base` (`base.clone().analyze()`), driving the
    /// listing. Rebuilt on every edit/undo.
    project: Project,
    /// Path the document loads from and saves to.
    path: PathBuf,
    labels: HashMap<String, Address>,

    widgets: Vec<Widget>,
    total_rows: u32,
    /// For each row `y`, the slice of `widgets` on that row (in `x` order).
    row_index: Vec<Range<usize>>,
    /// First row that carries each address (where its anchor lives).
    addr_to_row: BTreeMap<Address, u32>,
    /// Owning address of each row, if any (header rows have none).
    row_addr: Vec<Option<Address>>,
    /// First row of each row's address group, for relative-line math.
    group_first: Vec<u32>,
    /// Indices into `widgets` that the cursor can land on, in `(y, x)` order.
    navigable: Vec<usize>,

    /// First visible listing row.
    top_row: u32,
    /// Horizontal scroll offset, in columns.
    left_col: u32,
    /// Index into `navigable` of the focused widget.
    cursor: usize,
    /// Preferred column for vertical cursor motion (editor-style goal column).
    goal_col: u32,
    nav_stack: Vec<NavMark>,

    /// Listing viewport, updated each render (handlers need it for scrolling).
    view_h: u32,
    view_w: u32,

    /// Current input mode (normal navigation vs. an active prompt line).
    mode: Mode,
    /// Rows matched by the last `/` search, in listing order, with the
    /// currently-selected index for `n`/`N` cycling.
    search_matches: Vec<u32>,
    search_idx: usize,
    /// Transient message shown in the footer (e.g. "match 2/7", "no match").
    status: Option<String>,

    /// Undo history. `head` is the number of applied edits (entries
    /// `[0, head)`); `head..` is the redo tail. `saved_at` is `head` at the last
    /// save, so the document is dirty iff `head != saved_at`.
    history: Vec<UndoEntry>,
    head: usize,
    saved_at: usize,
    /// The segment of the most recent `ofs_seg` edit, used to preselect the
    /// picker when the target has no assumption yet.
    last_ofs_seg: Option<SegmentIdx>,
    /// A guarded quit is pending: a second `q` confirms discarding edits.
    confirm_quit: bool,

    /// Whether the terminal supports 24-bit colour; otherwise colours are
    /// quantized to xterm-256 so they render correctly on e.g. Terminal.app.
    truecolor: bool,
    quit: bool,
}

impl App {
    /// Build the app from the authoritative (un-analyzed) project and its path.
    pub fn new(base: Project, path: PathBuf) -> Self {
        let project = analyzed(&base);
        let mut app = App {
            base,
            project,
            path,
            labels: HashMap::new(),
            widgets: Vec::new(),
            total_rows: 0,
            row_index: Vec::new(),
            addr_to_row: BTreeMap::new(),
            row_addr: Vec::new(),
            group_first: Vec::new(),
            navigable: Vec::new(),
            top_row: 0,
            left_col: 0,
            cursor: 0,
            goal_col: 0,
            nav_stack: Vec::new(),
            view_h: 1,
            view_w: 1,
            mode: Mode::Normal,
            search_matches: Vec::new(),
            search_idx: 0,
            status: None,
            history: Vec::new(),
            head: 0,
            saved_at: 0,
            last_ofs_seg: None,
            confirm_quit: false,
            truecolor: supports_truecolor(),
            quit: false,
        };
        app.rebuild_listing();
        app
    }

    /// Recompute the widget grid and all derived indices from the analyzed
    /// `project`. Called after construction and after each re-derive.
    fn rebuild_listing(&mut self) {
        let (mut widgets, total_rows) =
            generate_widgets_with_options(&self.project, LayoutOptions::default());
        // The grid is produced in (y, x) order; sort defensively so the row
        // index and cursor ordering are correct regardless.
        widgets.sort_by_key(|w| (w.y, w.x));

        let row_index = build_row_index(&widgets, total_rows);
        let mut addr_to_row: BTreeMap<Address, u32> = BTreeMap::new();
        for w in &widgets {
            if matches!(w.kind, WidgetKind::Address) {
                addr_to_row.entry((w.seg_idx, w.ofs)).or_insert(w.y);
            }
        }

        let mut row_addr = vec![None; total_rows as usize];
        for (y, range) in row_index.iter().enumerate() {
            row_addr[y] = widgets[range.clone()]
                .iter()
                .find(|w| matches!(w.kind, WidgetKind::Address))
                .map(|w| (w.seg_idx, w.ofs));
        }
        let group_first: Vec<u32> = (0..total_rows)
            .map(|y| match row_addr[y as usize] {
                Some(addr) => addr_to_row.get(&addr).copied().unwrap_or(y),
                None => y,
            })
            .collect();

        let navigable: Vec<usize> = widgets
            .iter()
            .enumerate()
            .filter(|(_, w)| is_navigable(&w.kind))
            .map(|(i, _)| i)
            .collect();

        self.labels = build_label_index(&self.project);
        self.widgets = widgets;
        self.total_rows = total_rows;
        self.row_index = row_index;
        self.addr_to_row = addr_to_row;
        self.row_addr = row_addr;
        self.group_first = group_first;
        self.navigable = navigable;
        if self.cursor >= self.navigable.len() {
            self.cursor = self.navigable.len().saturating_sub(1);
        }
    }

    /// Re-derive the analyzed view from `base` after an edit, keeping the cursor
    /// on the same address and screen line.
    fn rederive(&mut self) {
        let anchor = self.cursor_address();
        let screen_row = self.cursor_yx().0.saturating_sub(self.top_row);

        self.project = analyzed(&self.base);
        self.rebuild_listing();

        if let Some(addr) = anchor
            && let Some(row) = self.row_for_address(addr)
            && let Some(nav) = self.jump_target_nav(row)
        {
            self.cursor = nav;
            let y = self.widgets[self.navigable[nav]].y;
            self.top_row = y.saturating_sub(screen_row).min(self.max_top());
        }
        self.goal_col = self.cursor_yx().1;
        self.ensure_cursor_visible();
    }

    /// The `seg:ofs` of the line the cursor is on.
    fn cursor_address(&self) -> Option<Address> {
        self.focused_widget().map(|w| (w.seg_idx, w.ofs))
    }

    /// The offset that `o` (set `ofs_seg`) would resolve at the cursor: `Some`
    /// only when the cursor sits on an instruction whose decoding has an operand
    /// the assumed segment resolves — an immediate, a direct `[imm]`
    /// displacement, or an `imm32` far pointer. Register-only instructions,
    /// data, labels and comments yield `None`.
    fn cursor_ofs_seg_offset(&self) -> Option<u16> {
        let w = self.focused_widget()?;
        if !matches!(w.kind, WidgetKind::Opcode | WidgetKind::Operand { .. }) {
            return None;
        }
        let seg_val = (self.project.segments[w.seg_idx].start.unwrap_or(0) / 16) as u16;
        let bytes = self.project.bytes_at_seg(w.seg_idx, w.ofs);
        let inst = decode(seg_val, w.ofs as u16, bytes.iter().copied())?;
        (0..inst.arg_count()).find_map(|i| match inst.operand(i) {
            Operand::Imm { value, .. } => Some(value as u16),
            Operand::Mem(MemRef::Direct { ofs, .. }) => Some(ofs),
            Operand::Mem(MemRef::Indirect {
                base: None,
                index: None,
                disp,
                ..
            }) => Some(disp),
            _ => None,
        })
    }

    /// Resolve a palette colour for this terminal.
    fn color(&self, rgb: Rgb) -> Color {
        to_color(rgb, self.truecolor)
    }

    pub fn should_quit(&self) -> bool {
        self.quit
    }

    // ── Cursor / widget helpers ───────────────────────────────────────────────

    fn focused_widget(&self) -> Option<&Widget> {
        self.navigable.get(self.cursor).map(|&i| &self.widgets[i])
    }

    fn cursor_yx(&self) -> (u32, u32) {
        match self.focused_widget() {
            Some(w) => (w.y, w.x),
            None => (0, 0),
        }
    }

    fn max_top(&self) -> u32 {
        self.total_rows.saturating_sub(self.view_h)
    }

    fn cursor_visible(&self) -> bool {
        let (y, _) = self.cursor_yx();
        y >= self.top_row && y < self.top_row + self.view_h
    }

    /// Move `top_row`/`left_col` the minimum needed to bring the cursor into the
    /// viewport, keeping `SCROLLOFF` rows of context where possible.
    fn ensure_cursor_visible(&mut self) {
        let (y, x) = self.cursor_yx();
        let h = self.view_h;
        if y < self.top_row + SCROLLOFF {
            self.top_row = y.saturating_sub(SCROLLOFF);
        } else if y + SCROLLOFF >= self.top_row + h {
            self.top_row = (y + SCROLLOFF + 1).saturating_sub(h);
        }
        self.top_row = self.top_row.min(self.max_top());

        let w = self.view_w;
        if x < self.left_col {
            self.left_col = x;
        } else if x >= self.left_col + w {
            self.left_col = x + 1 - w;
        }
    }

    /// Centre the viewport on the cursor (used when a keyboard move happens
    /// while the cursor is off-screen — one keypress to "come back").
    fn center_on_cursor(&mut self) {
        let (y, _) = self.cursor_yx();
        self.top_row = y.saturating_sub(self.view_h / 2).min(self.max_top());
        self.ensure_cursor_visible();
    }

    /// Set the cursor to the navigable widget on `row` whose column is nearest
    /// `col`. Returns false if the row has no navigable widget.
    fn cursor_to_row_col(&mut self, row: u32, col: u32) -> bool {
        let best = self
            .navigable
            .iter()
            .enumerate()
            .filter(|&(_, &i)| self.widgets[i].y == row)
            .min_by_key(|&(_, &i)| self.widgets[i].x.abs_diff(col));
        match best {
            Some((nav_idx, _)) => {
                self.cursor = nav_idx;
                self.goal_col = col;
                true
            }
            None => false,
        }
    }

    // ── Keyboard ──────────────────────────────────────────────────────────────

    pub fn on_key(&mut self, key: KeyEvent) {
        if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
            self.quit = true;
            return;
        }
        // While a prompt line is open, all keys edit it.
        if matches!(self.mode, Mode::Prompt { .. }) {
            self.on_prompt_key(key);
            return;
        }
        // While the comment editor is open, all keys go to it.
        if matches!(self.mode, Mode::Comment(_)) {
            self.on_comment_key(key);
            return;
        }
        // While the ofs-seg picker is open, all keys go to it.
        if matches!(self.mode, Mode::OfsSeg(_)) {
            self.on_ofs_seg_key(key);
            return;
        }

        // Any normal-mode key clears a stale status message; handlers set a new
        // one as needed. A pending quit-confirmation is cleared by any key other
        // than `q` (so a second `q` still confirms).
        self.status = None;
        if key.code != KeyCode::Char('q') {
            self.confirm_quit = false;
        }
        match key.code {
            KeyCode::Char('q') => self.request_quit(),
            KeyCode::Left => self.move_horizontal(-1),
            KeyCode::Right => self.move_horizontal(1),
            KeyCode::Up => self.move_vertical(-1),
            KeyCode::Down => self.move_vertical(1),
            KeyCode::Enter => self.follow_link(self.cursor_yx().1),
            KeyCode::Esc => self.go_back(),
            KeyCode::PageUp => self.move_page(-1),
            KeyCode::PageDown => self.move_page(1),
            KeyCode::Home => self.move_to_end(-1),
            KeyCode::End => self.move_to_end(1),
            KeyCode::Char(':') | KeyCode::Char('g') => {
                self.begin_prompt(PromptKind::Goto, String::new(), None)
            }
            KeyCode::Char('/') => self.begin_prompt(PromptKind::Search, String::new(), None),
            KeyCode::Char('n') => self.cycle_search(1),
            KeyCode::Char('N') => self.cycle_search(-1),
            // Editing.
            KeyCode::Char('l') => self.begin_rename(),
            KeyCode::Char(';') => self.begin_comment(),
            KeyCode::Char('o') => self.begin_ofs_seg(),
            KeyCode::Char('s') => self.save(),
            KeyCode::Char('u') => self.undo(),
            KeyCode::Char('U') => self.redo(),
            _ => {}
        }
    }

    // ── Prompt (goto / search / rename / comment) ─────────────────────────────

    fn begin_prompt(&mut self, kind: PromptKind, input: String, addr: Option<Address>) {
        let input = LineInput::new(input);
        self.mode = Mode::Prompt { kind, input, addr };
    }

    /// Open a rename prompt for the cursor's address, prefilled with its current
    /// user label (empty when the address only carries an auto-label).
    fn begin_rename(&mut self) {
        if let Some(addr) = self.cursor_address() {
            let prefill = self.base.attrs.get(&addr).and_then(|a| a.name.clone());
            self.begin_prompt(PromptKind::Rename, prefill.unwrap_or_default(), Some(addr));
        }
    }

    /// Open the multi-line comment editor for the cursor's address, prefilled
    /// with its current comment.
    fn begin_comment(&mut self) {
        if let Some(addr) = self.cursor_address() {
            let text = self
                .base
                .attrs
                .get(&addr)
                .and_then(|a| a.comment.clone())
                .unwrap_or_default();
            self.mode = Mode::Comment(CommentEditor::new(addr, &text));
        }
    }

    /// Open the segment picker for the cursor's instruction. No-op (with a
    /// status note) where `ofs_seg` does not apply.
    fn begin_ofs_seg(&mut self) {
        let (Some(offset), Some(addr)) = (self.cursor_ofs_seg_offset(), self.cursor_address())
        else {
            self.status = Some("ofs-seg: not an offset operand here".to_string());
            return;
        };

        // `— none —` (clear) followed by every segment.
        let mut items: Vec<Option<SegmentIdx>> = vec![None];
        items.extend(self.base.segments.indexed_iter().map(|(idx, _)| Some(idx)));

        // Preselect: this instruction's `ofs_seg`, else the last applied, else
        // the first segment.
        let pre = self
            .base
            .attrs
            .get(&addr)
            .and_then(|a| a.ofs_seg)
            .or(self.last_ofs_seg)
            .or_else(|| self.base.segments.indexed_iter().next().map(|(idx, _)| idx));
        let selected = pre
            .and_then(|seg| items.iter().position(|it| *it == Some(seg)))
            .unwrap_or(0);

        self.mode = Mode::OfsSeg(OfsSegPicker {
            addr,
            offset,
            items,
            selected,
        });
    }

    fn on_ofs_seg_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Esc => self.mode = Mode::Normal,
            KeyCode::Up => {
                if let Mode::OfsSeg(p) = &mut self.mode {
                    p.up();
                }
            }
            KeyCode::Down => {
                if let Mode::OfsSeg(p) = &mut self.mode {
                    p.down();
                }
            }
            KeyCode::Enter => {
                if let Mode::OfsSeg(p) = std::mem::replace(&mut self.mode, Mode::Normal) {
                    self.set_ofs_seg(p.addr, p.choice());
                }
            }
            _ => {}
        }
    }

    fn on_comment_key(&mut self, key: KeyEvent) {
        match key.code {
            // Esc discards; Tab saves; Enter inserts a newline.
            KeyCode::Esc => self.mode = Mode::Normal,
            KeyCode::Tab => {
                if let Mode::Comment(ed) = std::mem::replace(&mut self.mode, Mode::Normal) {
                    self.apply_comment(ed.addr, ed.text());
                }
            }
            _ => {
                if let Mode::Comment(ed) = &mut self.mode {
                    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
                    let alt = key.modifiers.contains(KeyModifiers::ALT);
                    match key.code {
                        KeyCode::Enter => ed.newline(),
                        KeyCode::Left if ctrl || alt => ed.word_left(),
                        KeyCode::Right if ctrl || alt => ed.word_right(),
                        KeyCode::Left => ed.left(),
                        KeyCode::Right => ed.right(),
                        KeyCode::Up => ed.up(),
                        KeyCode::Down => ed.down(),
                        KeyCode::Home => ed.col = 0,
                        KeyCode::End => ed.col = ed.line_len(),
                        KeyCode::Backspace => ed.backspace(),
                        KeyCode::Delete => ed.delete(),
                        KeyCode::Char('a') if ctrl => ed.col = 0,
                        KeyCode::Char('e') if ctrl => ed.col = ed.line_len(),
                        KeyCode::Char('b') if ctrl => ed.left(),
                        KeyCode::Char('f') if ctrl => ed.right(),
                        KeyCode::Char('b') if alt => ed.word_left(),
                        KeyCode::Char('f') if alt => ed.word_right(),
                        KeyCode::Char('h') if ctrl => ed.backspace(),
                        KeyCode::Char('d') if ctrl => ed.delete(),
                        KeyCode::Char('w') if ctrl => ed.delete_word_back(),
                        KeyCode::Char('u') if ctrl => ed.delete_to_start(),
                        KeyCode::Char('k') if ctrl => ed.delete_to_end(),
                        KeyCode::Char(c) if !ctrl && !alt => ed.insert(c),
                        _ => {}
                    }
                }
            }
        }
    }

    fn on_prompt_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Esc => self.mode = Mode::Normal,
            KeyCode::Enter => self.submit_prompt(),
            _ => {
                if let Mode::Prompt { input, .. } = &mut self.mode {
                    input.on_key(key);
                }
            }
        }
    }

    fn submit_prompt(&mut self) {
        let Mode::Prompt { kind, input, addr } = std::mem::replace(&mut self.mode, Mode::Normal)
        else {
            return;
        };
        let input = input.text;
        match kind {
            // Goto/search ignore an empty query.
            PromptKind::Goto => {
                let q = input.trim();
                if !q.is_empty() {
                    self.goto(q);
                }
            }
            PromptKind::Search => {
                let q = input.trim();
                if !q.is_empty() {
                    self.search(q);
                }
            }
            // An empty rename is meaningful: it clears the label.
            PromptKind::Rename => {
                if let Some(addr) = addr {
                    self.apply_rename(addr, input);
                }
            }
        }
    }

    // ── Editing / undo / save ─────────────────────────────────────────────────

    /// Apply a rename at `addr` (empty input clears the label).
    fn apply_rename(&mut self, addr: Address, input: String) {
        let new = trimmed_opt(&input);
        let old = self.base.attrs.get(&addr).and_then(|a| a.name.clone());
        if new == old {
            self.status = Some("label unchanged".to_string());
            return;
        }
        let label = match &new {
            Some(n) => format!("rename → {n}"),
            None => "clear label".to_string(),
        };
        let before = self.base.attrs.get(&addr).cloned();
        self.base.set_attr_name(addr, new);
        let after = self.base.attrs.get(&addr).cloned();
        self.commit_edit(addr, before, after, label);
    }

    /// Apply a comment edit at `addr` (empty input clears the comment).
    fn apply_comment(&mut self, addr: Address, input: String) {
        let new = trimmed_opt(&input);
        let old = self.base.attrs.get(&addr).and_then(|a| a.comment.clone());
        if new == old {
            self.status = Some("comment unchanged".to_string());
            return;
        }
        let label = if new.is_some() {
            "edit comment".to_string()
        } else {
            "clear comment".to_string()
        };
        let before = self.base.attrs.get(&addr).cloned();
        self.base.set_attr_comment(addr, new);
        let after = self.base.attrs.get(&addr).cloned();
        self.commit_edit(addr, before, after, label);
    }

    /// Set (or, with `None`, clear) the `ofs16` segment assumption at `addr`.
    /// Remembers the chosen segment as `last_ofs_seg` for the next picker.
    fn set_ofs_seg(&mut self, addr: Address, new: Option<SegmentIdx>) {
        let old = self.base.attrs.get(&addr).and_then(|a| a.ofs_seg);
        if new == old {
            self.status = Some("ofs-seg unchanged".to_string());
            return;
        }
        let label = match new {
            Some(seg) => format!("ofs-seg → {}", self.base.segments[seg].name),
            None => "clear ofs-seg".to_string(),
        };
        let before = self.base.attrs.get(&addr).cloned();
        self.base.set_attr_ofs_seg(addr, new);
        let after = self.base.attrs.get(&addr).cloned();
        if let Some(seg) = new {
            self.last_ofs_seg = Some(seg);
        }
        self.commit_edit(addr, before, after, label);
    }

    /// Record an applied edit (truncating any redo tail), then re-derive.
    fn commit_edit(
        &mut self,
        addr: Address,
        before: Option<Attr>,
        after: Option<Attr>,
        label: String,
    ) {
        self.history.truncate(self.head);
        // A save point in the just-discarded redo tail is now unreachable.
        if self.saved_at > self.head {
            self.saved_at = usize::MAX;
        }
        self.history.push(UndoEntry {
            addr,
            before,
            after,
            label: label.clone(),
        });
        self.head += 1;
        self.rederive();
        self.status = Some(label);
    }

    fn undo(&mut self) {
        if self.head == 0 {
            self.status = Some("nothing to undo".to_string());
            return;
        }
        self.head -= 1;
        let (addr, attr) = {
            let e = &self.history[self.head];
            (e.addr, e.before.clone())
        };
        self.set_attr(addr, attr);
        self.rederive();
        self.status = Some(format!("undo: {}", self.history[self.head].label));
    }

    fn redo(&mut self) {
        if self.head >= self.history.len() {
            self.status = Some("nothing to redo".to_string());
            return;
        }
        let (addr, attr) = {
            let e = &self.history[self.head];
            (e.addr, e.after.clone())
        };
        self.set_attr(addr, attr);
        self.rederive();
        self.status = Some(format!("redo: {}", self.history[self.head].label));
        self.head += 1;
    }

    /// Write or remove the authoritative attribute at `addr`.
    fn set_attr(&mut self, addr: Address, attr: Option<Attr>) {
        match attr {
            Some(a) => {
                self.base.attrs.insert(addr, a);
            }
            None => {
                self.base.attrs.remove(&addr);
            }
        }
    }

    fn dirty(&self) -> bool {
        self.head != self.saved_at
    }

    /// Serialize the authoritative document and write it to disk.
    fn save(&mut self) {
        let mut buf = Vec::new();
        if let Err(e) = self.base.write_to(&mut buf) {
            self.status = Some(format!("save failed: {e}"));
            return;
        }
        match std::fs::write(&self.path, &buf) {
            Ok(()) => {
                self.saved_at = self.head;
                self.confirm_quit = false;
                self.status = Some("saved".to_string());
            }
            Err(e) => self.status = Some(format!("save failed: {e}")),
        }
    }

    /// Quit, guarding unsaved changes: the first `q` while dirty asks for
    /// confirmation; a second `q` (with nothing typed between) discards.
    fn request_quit(&mut self) {
        if self.dirty() && !self.confirm_quit {
            self.confirm_quit = true;
            self.status = Some("unsaved changes — s to save, q to discard".to_string());
        } else {
            self.quit = true;
        }
    }

    /// Resolve a `seg:ofs` address or a label name and jump to it.
    fn goto(&mut self, query: &str) {
        match self.parse_address(query) {
            Some(addr) if self.jump_to_address(addr, true) => {}
            _ => self.status = Some(format!("no address or label '{query}'")),
        }
    }

    /// Resolve a goto target: a `seg:ofs` pair (e.g. `seg000:ca1b`), a label
    /// name, or a bare hex offset — landing in whichever segment the cursor is
    /// currently in. A name that no longer exists but encodes an offset in its
    /// suffix (`data_03977`, `loc_00772`) resolves to that offset, so it snaps to
    /// the nearest preceding line via [`Self::row_for_address`].
    fn parse_address(&self, s: &str) -> Option<Address> {
        if let Some((seg, ofs)) = s.split_once(':') {
            return Some((self.project.segment_by_name(seg)?, parse_hex(ofs)?));
        }
        if let Some(&addr) = self.labels.get(s) {
            return Some(addr);
        }
        // A bare hex offset, or an auto-label-style name whose trailing token is
        // a hex offset (`data_03977` → 3977).
        let ofs = parse_hex(s).or_else(|| parse_hex(s.rsplit('_').next()?))?;
        Some((self.focused_widget()?.seg_idx, ofs))
    }

    /// Free-text search of the whole rendered listing (like `chaniq search`): a
    /// case-insensitive substring match over every row's text — addresses,
    /// mnemonics, operands, comments and all — collected in listing order, then
    /// jump to the first hit.
    fn search(&mut self, query: &str) {
        let q = query.to_lowercase();
        let matches: Vec<u32> = (0..self.total_rows)
            .filter(|&y| self.row_text(y).to_lowercase().contains(&q))
            .collect();

        if matches.is_empty() {
            self.search_matches.clear();
            self.status = Some(format!("no matches for '{query}'"));
            return;
        }
        self.search_matches = matches;
        self.search_idx = 0;
        self.jump_to_row(self.search_matches[0], true);
        self.report_match();
    }

    /// Cycle to the next (`dir > 0`) or previous (`dir < 0`) search match.
    fn cycle_search(&mut self, dir: i64) {
        let n = self.search_matches.len();
        if n == 0 {
            self.status = Some("no active search".to_string());
            return;
        }
        self.search_idx = (self.search_idx as i64 + dir).rem_euclid(n as i64) as usize;
        // No back-stack push while stepping through results.
        self.jump_to_row(self.search_matches[self.search_idx], false);
        self.report_match();
    }

    /// Reconstruct the plain text of row `y` exactly as it is laid out: each
    /// widget placed at its column, padded with spaces. Mirrors the rendered
    /// line that `chaniq search` matches against.
    fn row_text(&self, y: u32) -> String {
        let mut s = String::new();
        let mut col = 0u32;
        for wi in self.row_index[y as usize].clone() {
            let w = &self.widgets[wi];
            if w.x > col {
                s.extend(std::iter::repeat_n(' ', (w.x - col) as usize));
            }
            s.push_str(w.text.as_str());
            col = w.x + w.text.len() as u32;
        }
        s
    }

    /// Jump so that listing row `row` is centred and the cursor lands on it (or,
    /// if that row has no navigable element, the nearest one at/after it).
    fn jump_to_row(&mut self, row: u32, push: bool) {
        if push {
            self.push_mark();
        }
        self.top_row = row.saturating_sub(self.view_h / 2).min(self.max_top());
        if !self.cursor_to_row_col(row, 0) {
            if let Some(ni) = self
                .navigable
                .iter()
                .position(|&i| self.widgets[i].y >= row)
            {
                self.cursor = ni;
            }
        }
        self.goal_col = self.cursor_yx().1;
        self.ensure_cursor_visible();
    }

    fn report_match(&mut self) {
        self.status = Some(format!(
            "match {}/{}",
            self.search_idx + 1,
            self.search_matches.len()
        ));
    }

    /// A keyboard cursor move while the cursor is off-screen first pulls it back
    /// into view instead of moving. Returns true if it consumed the keypress.
    fn recenter_guard(&mut self) -> bool {
        if !self.cursor_visible() {
            self.center_on_cursor();
            true
        } else {
            false
        }
    }

    fn move_horizontal(&mut self, dir: i64) {
        if self.recenter_guard() {
            return;
        }
        let next = self.cursor as i64 + dir;
        if next >= 0 && (next as usize) < self.navigable.len() {
            self.cursor = next as usize;
            self.goal_col = self.cursor_yx().1;
            self.ensure_cursor_visible();
        }
    }

    fn move_vertical(&mut self, dir: i64) {
        if self.recenter_guard() {
            return;
        }
        let (cy, _) = self.cursor_yx();
        let goal = self.goal_col;
        // The nearest navigable row in `dir`, then the column closest to `goal`.
        let target_row = self
            .navigable
            .iter()
            .map(|&i| self.widgets[i].y)
            .filter(|&y| if dir > 0 { y > cy } else { y < cy })
            .min_by_key(|&y| y.abs_diff(cy));
        if let Some(row) = target_row {
            self.cursor_to_row_col(row, goal);
            self.ensure_cursor_visible();
        }
    }

    /// Move the cursor by one viewport height, snapping to the navigable element
    /// nearest the target row at the current goal column.
    fn move_page(&mut self, dir: i64) {
        if self.recenter_guard() {
            return;
        }
        let (cy, _) = self.cursor_yx();
        let goal = self.goal_col;
        let page = self.view_h.max(1);
        let target_y = if dir > 0 {
            cy.saturating_add(page)
        } else {
            cy.saturating_sub(page)
        };
        // Nearest navigable element to the target row (it may not have one), then
        // snap to the goal column on that element's row.
        if let Some((_, &wi)) = self
            .navigable
            .iter()
            .enumerate()
            .min_by_key(|&(_, &i)| self.widgets[i].y.abs_diff(target_y))
        {
            self.cursor_to_row_col(self.widgets[wi].y, goal);
            self.ensure_cursor_visible();
        }
    }

    /// Move the cursor to the first (`dir < 0`) or last (`dir > 0`) navigable
    /// element in the listing.
    fn move_to_end(&mut self, dir: i64) {
        if self.navigable.is_empty() {
            return;
        }
        self.cursor = if dir > 0 { self.navigable.len() - 1 } else { 0 };
        self.goal_col = self.cursor_yx().1;
        self.ensure_cursor_visible();
    }

    /// Scroll the viewport without moving the cursor (decoupled, like the
    /// wheel). The cursor may go off-screen; the next cursor move re-centres it.
    fn scroll_view(&mut self, delta: i64) {
        let top = (self.top_row as i64 + delta).clamp(0, self.max_top() as i64);
        self.top_row = top as u32;
    }

    /// Follow the link under `col` on the focused widget (falling back to the
    /// widget's primary link), keeping the cursor on the same screen line.
    fn follow_link(&mut self, col: u32) {
        let Some(&wi) = self.navigable.get(self.cursor) else {
            return;
        };
        let widget = &self.widgets[wi];
        if let Some(span) = resolve_link_at(&self.project, &self.labels, widget, Some(col)) {
            self.jump_following(span.target);
        }
    }

    /// Jump to `target`'s line, keeping the cursor on the same screen line and
    /// scrolling the contents under it. Snaps to the nearest preceding line when
    /// `target` has no line of its own — a referenced label may sit inside a
    /// larger element (e.g. `data_03977` within the 4-byte `data_03975`).
    fn jump_following(&mut self, target: Address) {
        let Some(anchor_row) = self.row_for_address(target) else {
            return;
        };
        // The anchor row may be a bare label or xref header with nothing
        // navigable on it, so resolve to a real navigable element first.
        let Some(target_nav) = self.jump_target_nav(anchor_row) else {
            return;
        };

        let screen_row = self.cursor_yx().0.saturating_sub(self.top_row);
        self.push_mark();
        self.cursor = target_nav;
        let (ny, nx) = self.cursor_yx();
        self.goal_col = nx;
        self.top_row = ny.saturating_sub(screen_row).min(self.max_top());
        self.ensure_cursor_visible();
    }

    /// Record the current cursor position on the back stack, keyed by address +
    /// relative line so it survives a re-layout.
    fn push_mark(&mut self) {
        let (cy, cx) = self.cursor_yx();
        let screen_row = cy.saturating_sub(self.top_row);
        if let Some(addr) = self.row_addr.get(cy as usize).copied().flatten() {
            self.nav_stack.push(NavMark {
                addr,
                rel_line: cy - self.group_first[cy as usize],
                col: cx,
                screen_row,
            });
        }
    }

    /// Jump the cursor to `addr`'s line, centring the view. When `push`, the
    /// current position is recorded on the back stack first. Returns false if the
    /// address is not present in the listing.
    fn jump_to_address(&mut self, addr: Address, push: bool) -> bool {
        let Some(anchor_row) = self.row_for_address(addr) else {
            return false;
        };
        let Some(target_nav) = self.jump_target_nav(anchor_row) else {
            return false;
        };
        if push {
            self.push_mark();
        }
        self.cursor = target_nav;
        self.goal_col = self.cursor_yx().1;
        self.center_on_cursor();
        true
    }

    /// The listing row for `addr`: the line starting exactly at `addr`, else the
    /// nearest line start before it in the same segment — so an offset inside a
    /// multi-byte instruction or data element (e.g. `seg:3977` within the 4-byte
    /// `data_03975`) snaps back to that element's line. Falls back to the
    /// segment's first line when `addr` precedes it.
    fn row_for_address(&self, addr: Address) -> Option<u32> {
        if let Some((&(seg, _), &row)) = self.addr_to_row.range(..=addr).next_back()
            && seg == addr.0
        {
            return Some(row);
        }
        self.addr_to_row
            .range((addr.0, 0)..)
            .next()
            .filter(|&(&(seg, _), _)| seg == addr.0)
            .map(|(_, &row)| row)
    }

    /// The navigable index to land on when jumping to the address whose group
    /// starts at `anchor_row`: the group's instruction/data line if it has one,
    /// otherwise the first navigable element at or after the anchor row.
    fn jump_target_nav(&self, anchor_row: u32) -> Option<usize> {
        self.navigable
            .iter()
            .position(|&i| {
                let w = &self.widgets[i];
                w.y >= anchor_row
                    && self.group_first[w.y as usize] == anchor_row
                    && matches!(w.kind, WidgetKind::Opcode | WidgetKind::Data)
            })
            .or_else(|| {
                self.navigable
                    .iter()
                    .position(|&i| self.widgets[i].y >= anchor_row)
            })
    }

    fn go_back(&mut self) {
        let Some(mark) = self.nav_stack.pop() else {
            return;
        };
        let Some(&first) = self.addr_to_row.get(&mark.addr) else {
            return;
        };
        let row = (first + mark.rel_line).min(self.total_rows.saturating_sub(1));
        self.top_row = row.saturating_sub(mark.screen_row).min(self.max_top());
        self.cursor_to_row_col(row, mark.col);
        self.ensure_cursor_visible();
    }

    // ── Mouse ─────────────────────────────────────────────────────────────────

    pub fn on_mouse(&mut self, mouse: MouseEvent) {
        match mouse.kind {
            MouseEventKind::ScrollDown => self.scroll_view(WHEEL_ROWS as i64),
            MouseEventKind::ScrollUp => self.scroll_view(-(WHEEL_ROWS as i64)),
            MouseEventKind::Down(MouseButton::Left) => self.click(mouse.column, mouse.row),
            _ => {}
        }
    }

    fn click(&mut self, col: u16, row: u16) {
        if (row as u32) >= self.view_h {
            return; // footer or below the listing
        }
        let doc_row = self.top_row + row as u32;
        let doc_col = self.left_col + col as u32;
        if doc_row >= self.total_rows {
            return;
        }
        // Land the cursor on the navigable widget nearest the click.
        if !self.cursor_to_row_col(doc_row, doc_col) {
            return;
        }
        self.ensure_cursor_visible();
        // Follow only when the click lands exactly on a link (no fall-back to a
        // line's primary link the way keyboard Enter does).
        if let Some(target) = self.link_target_at(doc_row, doc_col) {
            self.jump_following(target);
        }
    }

    /// The link destination at exactly column `col` on row `row`, or `None` when
    /// the column is not on a clickable run.
    fn link_target_at(&self, row: u32, col: u32) -> Option<Address> {
        self.row_index[row as usize].clone().find_map(|wi| {
            resolve_links(&self.project, &self.labels, &self.widgets[wi])
                .into_iter()
                .find(|span| span.contains_col(col))
                .map(|span| span.target)
        })
    }

    // ── Rendering ─────────────────────────────────────────────────────────────

    pub fn render(&mut self, frame: &mut Frame) {
        let [listing_area, footer_area] =
            Layout::vertical([Constraint::Min(0), Constraint::Length(1)]).areas(frame.area());
        self.view_h = listing_area.height as u32;
        self.view_w = listing_area.width as u32;

        let focused = self.navigable.get(self.cursor).copied();
        let last_row = (self.top_row + self.view_h).min(self.total_rows);

        let mut lines: Vec<Line> = Vec::with_capacity(self.view_h as usize);
        for y in self.top_row..last_row {
            lines.push(self.render_row(y, focused));
        }

        let listing = Paragraph::new(lines)
            .style(Style::default().bg(self.color(BG)).fg(self.color(FG)))
            .scroll((0, self.left_col.min(u16::MAX as u32) as u16));
        frame.render_widget(listing, listing_area);
        frame.render_widget(self.footer(), footer_area);

        // Place a real caret at the cursor column of an active prompt line.
        if let Mode::Prompt { kind, input, .. } = &self.mode {
            let caret = (kind.prefix().chars().count() + input.col) as u16;
            frame.set_cursor_position((
                footer_area.x + caret.min(footer_area.width.saturating_sub(1)),
                footer_area.y,
            ));
        }

        if let Mode::Comment(ed) = &self.mode {
            self.render_comment_editor(frame, ed, listing_area);
        }
        if let Mode::OfsSeg(p) = &self.mode {
            self.render_ofs_seg_picker(frame, p, listing_area);
        }
    }

    /// Draw the segment picker: a list on the left, a preview of the offset in
    /// the selected segment on the right.
    fn render_ofs_seg_picker(&self, frame: &mut Frame, p: &OfsSegPicker, area: Rect) {
        let (seg, ofs) = p.addr;
        let title = format!(
            " ofs-seg   {}:{:04x}   ·   offset 0x{:04x} ",
            self.project.segments[seg].name, ofs, p.offset
        );
        let label_for = |it: Option<SegmentIdx>| match it {
            Some(idx) => self.project.segments[idx].name.to_string(),
            None => "— none —".to_string(),
        };

        let name_w = p
            .items
            .iter()
            .map(|&it| label_for(it).chars().count())
            .max()
            .unwrap_or(6) as u16;
        let list_w = (name_w + 4).clamp(10, 24);
        let rows = p.items.len().max(8) as u16;
        let h = (rows + 3).min(area.height.max(1));
        let w = (area.width * 4 / 5).max(48).min(area.width);
        let rect = Rect::new(
            area.x + (area.width.saturating_sub(w)) / 2,
            area.y + (area.height.saturating_sub(h)) / 2,
            w,
            h,
        );

        let panel = Style::default()
            .bg(self.color(FOOTER_BG))
            .fg(self.color(FG));
        let block = Block::bordered().title(title).style(panel);
        let inner = block.inner(rect);
        frame.render_widget(Clear, rect);
        frame.render_widget(block, rect);

        let [content, hint] =
            Layout::vertical([Constraint::Min(1), Constraint::Length(1)]).areas(inner);
        let [list_area, preview_area] =
            Layout::horizontal([Constraint::Length(list_w), Constraint::Min(0)]).areas(content);

        // Segment list with a right divider.
        let list_block = Block::new().borders(Borders::RIGHT).style(panel);
        let list_inner = list_block.inner(list_area);
        frame.render_widget(list_block, list_area);
        let list: Vec<Line> = p
            .items
            .iter()
            .enumerate()
            .map(|(i, &it)| {
                let name = label_for(it);
                if i == p.selected {
                    Line::from(Span::styled(
                        format!("▌{name}"),
                        panel.bg(self.color(SEL)).add_modifier(Modifier::BOLD),
                    ))
                } else {
                    Line::from(format!(" {name}"))
                }
            })
            .collect();
        frame.render_widget(Paragraph::new(list).style(panel), list_inner);

        // Preview of the offset in the selected segment.
        frame.render_widget(
            Paragraph::new(self.ofs_seg_preview(p, preview_area.height)).style(panel),
            preview_area,
        );
        frame.render_widget(
            Paragraph::new(" ↑↓ choose segment    ⏎ apply    esc cancel").style(panel),
            hint,
        );
    }

    /// The preview lines for `‹selected segment›:‹offset›` — the colored listing
    /// around that address, with the covering line marked, or a note for the
    /// clear / out-of-range cases.
    fn ofs_seg_preview(&self, p: &OfsSegPicker, height: u16) -> Vec<Line<'_>> {
        let dim = Style::default().fg(self.color(FOOTER_FG));
        let Some(seg) = p.choice() else {
            return vec![Line::from(Span::styled("(clears the assumption)", dim))];
        };
        let Some(row) = self.row_for_address((seg, p.offset as u32)) else {
            let msg = format!(
                "(nothing at {}:{:04x})",
                self.project.segments[seg].name, p.offset
            );
            return vec![Line::from(Span::styled(msg, dim))];
        };
        let h = height.max(1) as u32;
        let start = row.saturating_sub(1);
        let end = (start + h).min(self.total_rows);
        (start..end)
            .map(|y| {
                let mut line = self.render_row(y, None);
                let marker = if y == row { "▶ " } else { "  " };
                line.spans.insert(
                    0,
                    Span::styled(
                        marker,
                        Style::default()
                            .fg(self.color((0xff, 0xd7, 0x00)))
                            .add_modifier(Modifier::BOLD),
                    ),
                );
                line
            })
            .collect()
    }

    /// Draw the multi-line comment editor as a centred popup over the listing.
    fn render_comment_editor(
        &self,
        frame: &mut Frame,
        ed: &CommentEditor,
        area: ratatui::layout::Rect,
    ) {
        let (seg, ofs) = ed.addr;
        let title = format!(
            " comment @ {}:{:04x}  —  Enter: newline   Tab: save   Esc: cancel ",
            self.project.segments[seg].name, ofs
        );

        // A box wide enough for the title and content, bounded by the viewport.
        let content_w = ed
            .lines
            .iter()
            .map(|l| l.chars().count())
            .max()
            .unwrap_or(0);
        let inner_w = content_w.max(title.chars().count()).max(20) as u16;
        let w = (inner_w + 2).min(area.width.max(4));
        let h = (ed.lines.len() as u16 + 2).min(area.height.max(3));
        let x = area.x + (area.width.saturating_sub(w)) / 2;
        let y = area.y + (area.height.saturating_sub(h)) / 2;
        let rect = ratatui::layout::Rect::new(x, y, w, h);

        let block = ratatui::widgets::Block::bordered().title(title).style(
            Style::default()
                .bg(self.color(FOOTER_BG))
                .fg(self.color(FG)),
        );
        let inner = block.inner(rect);
        frame.render_widget(ratatui::widgets::Clear, rect);
        let body: Vec<Line> = ed.lines.iter().map(|l| Line::raw(l.as_str())).collect();
        frame.render_widget(ratatui::widgets::Paragraph::new(body).block(block), rect);

        // Caret at the editing position (clamped into the inner box).
        let cx = inner.x + (ed.col as u16).min(inner.width.saturating_sub(1));
        let cy = inner.y + (ed.row as u16).min(inner.height.saturating_sub(1));
        frame.set_cursor_position((cx, cy));
    }

    /// Build one styled listing row, placing each widget at its column and
    /// underlining link runs. The focused widget is highlighted with a selection
    /// background; the rest of the cursor's row gets a subtle line highlight.
    fn render_row(&self, y: u32, focused: Option<usize>) -> Line<'_> {
        let on_cursor_row = focused.is_some_and(|fi| self.widgets[fi].y == y);
        let line_bg = on_cursor_row.then_some(LINE_HL);
        let gap_style = match line_bg {
            Some(bg) => Style::default().bg(self.color(bg)),
            None => Style::default(),
        };

        let mut spans: Vec<Span> = Vec::new();
        let mut col = 0u32;
        for wi in self.row_index[y as usize].clone() {
            let widget = &self.widgets[wi];
            if widget.x > col {
                spans.push(Span::styled(
                    " ".repeat((widget.x - col) as usize),
                    gap_style,
                ));
            }
            let is_focus = focused == Some(wi);
            self.push_widget_spans(&mut spans, widget, is_focus, line_bg);
            col = widget.x + widget.text.len() as u32;
        }
        // Extend the line highlight across the rest of the visible width.
        if line_bg.is_some() {
            let end = self.left_col + self.view_w;
            if col < end {
                spans.push(Span::styled(" ".repeat((end - col) as usize), gap_style));
            }
        }
        Line::from(spans)
    }

    /// Emit a widget's text as one or more spans: link runs are underlined, the
    /// focused widget gets a selection background, and widgets on the cursor's
    /// row inherit the subtle line-highlight background.
    fn push_widget_spans<'a>(
        &'a self,
        spans: &mut Vec<Span<'a>>,
        widget: &'a Widget,
        focus: bool,
        line_bg: Option<Rgb>,
    ) {
        let base = Style::default().fg(self.color(rgb_for_kind(&widget.kind)));
        let base = if matches!(widget.kind, WidgetKind::Comment) {
            base.add_modifier(Modifier::ITALIC)
        } else {
            base
        };
        let base = if focus {
            base.bg(self.color(SEL)).add_modifier(Modifier::BOLD)
        } else if let Some(bg) = line_bg {
            base.bg(self.color(bg))
        } else {
            base
        };

        let links = resolve_links(&self.project, &self.labels, widget);
        if links.is_empty() {
            spans.push(Span::styled(widget.text.as_str(), base));
            return;
        }
        let text = widget.text.as_str();
        let mut last = 0usize;
        for link in &links {
            let start = (link.start - widget.x) as usize;
            let end = start + link.len as usize;
            if start > last {
                spans.push(Span::styled(&text[last..start], base));
            }
            spans.push(Span::styled(
                &text[start..end],
                base.add_modifier(Modifier::UNDERLINED),
            ));
            last = end;
        }
        if last < text.len() {
            spans.push(Span::styled(&text[last..], base));
        }
    }

    fn footer(&self) -> Paragraph<'_> {
        let text = match &self.mode {
            // An open prompt owns the footer: prefix + typed text.
            Mode::Prompt { kind, input, .. } => format!("{}{}", kind.prefix(), input.text),
            // The comment editor draws its own popup; keep a short footer hint.
            Mode::Comment(_) => {
                " editing comment — Enter: newline   Tab: save   Esc: cancel ".to_string()
            }
            // The ofs-seg picker draws its own popup.
            Mode::OfsSeg(_) => " choosing ofs-seg — ↑↓ select   ⏎ apply   esc cancel ".to_string(),
            // Otherwise: dirty marker, current address, then status or key hints.
            Mode::Normal => {
                let dirty = if self.dirty() { "*" } else { " " };
                let addr = self
                    .row_addr
                    .get(self.top_row as usize)
                    .and_then(|a| *a)
                    .map(|(seg, ofs)| format!("{}:{:04x}", self.project.segments[seg].name, ofs))
                    .unwrap_or_else(|| "—".to_string());
                match &self.status {
                    Some(msg) => format!("{dirty}{addr}   {msg} "),
                    None => format!(
                        "{dirty}{addr}   ↑↓←→ move   ⏎ follow   esc back   l name   ; comment   o ofs-seg   s save   u undo   : goto   / search  n/N next/prev   q quit "
                    ),
                }
            }
        };
        Paragraph::new(text).style(
            Style::default()
                .bg(self.color(FOOTER_BG))
                .fg(self.color(FOOTER_FG)),
        )
    }
}

/// Whether the cursor can land on a widget of this kind: instruction opcodes
/// and operands, data elements, comments (the header notes), and xref lines
/// (which are links to follow).
fn is_navigable(kind: &WidgetKind) -> bool {
    matches!(
        kind,
        WidgetKind::Opcode
            | WidgetKind::Operand { .. }
            | WidgetKind::Data
            | WidgetKind::Comment
            | WidgetKind::XrefIn
            | WidgetKind::XrefOut
    )
}

/// Map each widget kind to its palette RGB, mirroring the HTML exporter's
/// colours (`widget_class` + the `<style>` block in `disasm.rs`).
fn rgb_for_kind(kind: &WidgetKind) -> Rgb {
    match kind {
        WidgetKind::Address => (0x56, 0x9c, 0xd6),
        WidgetKind::Label | WidgetKind::SegmentDecl => (0xdc, 0xdc, 0xaa),
        WidgetKind::Separator
        | WidgetKind::FileHeader
        | WidgetKind::SegmentHeader
        | WidgetKind::Comment => (0x6a, 0x99, 0x55),
        WidgetKind::Opcode | WidgetKind::AssumeDir => (0xc5, 0x86, 0xc0),
        WidgetKind::Operand { .. } | WidgetKind::StructField { .. } => (0x9c, 0xdc, 0xfe),
        WidgetKind::Punctuation => FG,
        WidgetKind::Data => (0xb5, 0xce, 0xa8),
        WidgetKind::ArrayIndex { .. } => (0xce, 0x91, 0x78),
        WidgetKind::XrefIn | WidgetKind::XrefOut => (0x4e, 0xc9, 0xb0),
    }
}

/// Derive an analyzed view from the authoritative project. `analyze()` is not
/// idempotent in place, so we always start from a fresh clone of `base`.
fn analyzed(base: &Project) -> Project {
    let mut view = base.clone();
    view.analyze();
    view
}

/// Trim `s`; `None` if empty (used to turn a prompt field into a clear).
fn trimmed_opt(s: &str) -> Option<String> {
    let t = s.trim();
    (!t.is_empty()).then(|| t.to_string())
}

/// Parse a hex offset, tolerating a leading `0x` and surrounding whitespace.
fn parse_hex(s: &str) -> Option<u32> {
    u32::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok()
}

/// True if the terminal advertises 24-bit colour via `COLORTERM`. Terminals that
/// don't (e.g. macOS Terminal.app) mis-render truecolor escapes, so we quantize.
fn supports_truecolor() -> bool {
    matches!(
        std::env::var("COLORTERM").as_deref(),
        Ok("truecolor") | Ok("24bit")
    )
}

/// Render a palette RGB as a terminal colour: truecolor where supported,
/// otherwise the nearest xterm-256 palette index.
fn to_color(rgb: Rgb, truecolor: bool) -> Color {
    if truecolor {
        Color::Rgb(rgb.0, rgb.1, rgb.2)
    } else {
        Color::Indexed(nearest_xterm256(rgb))
    }
}

/// The RGB of an xterm-256 palette index in the 6×6×6 colour cube (16–231) or
/// the 24-step grayscale ramp (232–255).
fn xterm256_rgb(i: u8) -> Rgb {
    const LEVELS: [u8; 6] = [0, 95, 135, 175, 215, 255];
    if i < 232 {
        let i = i - 16;
        (
            LEVELS[(i / 36) as usize],
            LEVELS[(i / 6 % 6) as usize],
            LEVELS[(i % 6) as usize],
        )
    } else {
        let v = 8 + (i - 232) * 10;
        (v, v, v)
    }
}

/// Nearest xterm-256 index to an RGB, by squared Euclidean distance over the
/// colour cube and grayscale ramp (indices 16–255).
fn nearest_xterm256((r, g, b): Rgb) -> u8 {
    let dist = |c: Rgb| {
        let d = |a: u8, b: u8| (a as i32 - b as i32).pow(2);
        d(c.0, r) + d(c.1, g) + d(c.2, b)
    };
    (16u16..256)
        .map(|i| i as u8)
        .min_by_key(|&i| dist(xterm256_rgb(i)))
        .unwrap()
}

/// Group widget indices by row: `out[y]` is the contiguous slice of `widgets`
/// (already sorted by `(y, x)`) lying on row `y`.
fn build_row_index(widgets: &[Widget], total_rows: u32) -> Vec<Range<usize>> {
    let mut rows = vec![0usize..0usize; total_rows as usize];
    let mut i = 0;
    while i < widgets.len() {
        let y = widgets[i].y;
        let start = i;
        while i < widgets.len() && widgets[i].y == y {
            i += 1;
        }
        if (y as usize) < rows.len() {
            rows[y as usize] = start..i;
        }
    }
    rows
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;

    use super::*;

    const PROJECT_PATH: &str = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../chani-projects/cryo-dune-3.7-cd-dnsdb.chani"
    );

    /// The authoritative (un-analyzed) project; the App analyzes it itself.
    fn load() -> Project {
        Project::from_project_file(PROJECT_PATH).expect("load project")
    }

    fn new_app() -> App {
        App::new(load(), PathBuf::from(PROJECT_PATH))
    }

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::empty())
    }

    fn key_ctrl(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::CONTROL)
    }

    fn key_alt(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::ALT)
    }

    fn click_at(column: u16, row: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column,
            row,
            modifiers: KeyModifiers::empty(),
        }
    }

    /// Flatten a rendered buffer to text, one row per line.
    fn buffer_text(buf: &ratatui::buffer::Buffer) -> String {
        let area = *buf.area();
        let mut s = String::new();
        for y in 0..area.height {
            for x in 0..area.width {
                s.push_str(buf[(x, y)].symbol());
            }
            s.push('\n');
        }
        s
    }

    /// A navigable widget whose link resolves to a navigable target line on a
    /// different row — the cleanest case to assert follow/back against. Returns
    /// the source nav index and the expected landing nav index.
    fn pick_link(app: &App) -> (usize, usize) {
        for (n, &wi) in app.navigable.iter().enumerate() {
            let w = &app.widgets[wi];
            if let Some(span) = resolve_link_at(&app.project, &app.labels, w, Some(w.x))
                && let Some(&anchor) = app.addr_to_row.get(&span.target)
                && let Some(target_nav) = app.jump_target_nav(anchor)
                && app.widgets[app.navigable[target_nav]].y != w.y
            {
                return (n, target_nav);
            }
        }
        panic!("no followable link found in project");
    }

    /// Type a string then submit, as if entered into an open prompt.
    fn type_and_submit(app: &mut App, s: &str) {
        for c in s.chars() {
            app.on_key(key(KeyCode::Char(c)));
        }
        app.on_key(key(KeyCode::Enter));
    }

    /// A label name whose address is present and reachable in the listing.
    fn a_label(app: &App) -> String {
        for (name, &addr) in &app.labels {
            if let Some(&anchor) = app.addr_to_row.get(&addr)
                && app.jump_target_nav(anchor).is_some()
            {
                return name.to_string();
            }
        }
        panic!("no usable label in project");
    }

    #[test]
    fn renders_navigates_and_follows_links() {
        let mut app = new_app();
        assert!(app.total_rows > 0);
        assert!(!app.navigable.is_empty());

        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        assert_eq!(app.view_h, 39, "40 rows minus a 1-row footer");

        // Cursor motion stays in range and never panics.
        for _ in 0..200 {
            app.on_key(key(KeyCode::Down));
        }
        app.on_key(key(KeyCode::Right));
        app.on_key(key(KeyCode::Up));
        terminal.draw(|f| app.render(f)).unwrap();
        assert!(app.cursor < app.navigable.len());

        // Follow a link: it must move BOTH the cursor and the viewport, landing
        // on the resolved target line and keeping it visible.
        let (link_nav, expected_nav) = pick_link(&app);
        app.cursor = link_nav;
        terminal.draw(|f| app.render(f)).unwrap();
        let origin = app.cursor_yx();

        app.follow_link(origin.1);
        assert_eq!(app.nav_stack.len(), 1);
        assert_eq!(app.cursor, expected_nav, "cursor moved to the target line");
        assert_ne!(app.cursor_yx(), origin, "cursor actually moved");
        assert!(app.cursor_visible(), "target line is on screen");

        app.go_back();
        assert!(app.nav_stack.is_empty());
        assert_eq!(app.cursor_yx(), origin, "esc restored the prior cursor");
        assert!(app.cursor_visible(), "restored cursor is on screen");

        // Esc with an empty stack is a no-op.
        app.go_back();
        assert_eq!(app.cursor_yx(), origin);
    }

    /// Two-level highlight on a dark theme: the focused token gets the strong
    /// selection background, the rest of its row gets the subtle line highlight,
    /// and other rows keep the plain dark background — never inverted.
    #[test]
    fn focused_token_and_line_are_highlighted() {
        let mut app = new_app();
        // Put the cursor on a known, on-screen navigable widget near the top.
        app.cursor = 0;
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        let (cy, cx) = app.cursor_yx();
        let sx = (cx - app.left_col) as u16;
        let sy = (cy - app.top_row) as u16;
        let buf = terminal.backend().buffer();

        // Focused token: selection bg, syntax-coloured (not inverted) fg.
        assert_eq!(
            buf[(sx, sy)].style().bg,
            Some(app.color(SEL)),
            "focused cell uses SEL bg"
        );
        assert_eq!(
            buf[(sx, sy)].style().fg,
            Some(app.color(rgb_for_kind(&app.widgets[app.navigable[0]].kind)))
        );

        // Rest of the cursor row (the address column, never focused): line bg.
        // It also extends past the text to the right edge of the viewport.
        assert_eq!(
            buf[(0, sy)].style().bg,
            Some(app.color(LINE_HL)),
            "cursor row uses the line highlight"
        );
        assert_eq!(
            buf[(app.view_w as u16 - 1, sy)].style().bg,
            Some(app.color(LINE_HL)),
            "line highlight spans the full width"
        );

        // A cell on another row keeps the plain dark background.
        assert_eq!(
            buf[(0, sy + 1)].style().bg,
            Some(app.color(BG)),
            "other rows keep dark bg"
        );
    }

    #[test]
    fn truecolor_falls_back_to_xterm256() {
        // Exact cube colours round-trip to their own index.
        assert_eq!(to_color((0, 0, 0), false), Color::Indexed(16));
        assert_eq!(to_color((255, 255, 255), false), Color::Indexed(231));
        // The dark background lands on the low grayscale ramp, not a bright cube
        // colour (the bug that turned the screen turquoise on Terminal.app).
        let Color::Indexed(bg) = to_color(BG, false) else {
            panic!("expected indexed");
        };
        assert!((232..=255).contains(&bg), "BG maps to gray ramp, got {bg}");
        let (r, g, b) = xterm256_rgb(bg);
        assert!(r < 0x50 && g < 0x50 && b < 0x50, "BG stays dark");
        // Truecolor path is verbatim RGB.
        assert_eq!(
            to_color((0x9c, 0xdc, 0xfe), true),
            Color::Rgb(0x9c, 0xdc, 0xfe)
        );
    }

    #[test]
    fn wheel_scroll_is_decoupled_from_cursor() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        let cursor_before = app.cursor;
        for _ in 0..5 {
            app.on_mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                column: 0,
                row: 0,
                modifiers: KeyModifiers::empty(),
            });
        }
        assert_eq!(app.cursor, cursor_before, "wheel does not move the cursor");
        assert!(app.top_row > 0, "wheel scrolled the viewport");
    }

    #[test]
    fn click_follows_only_on_the_link() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // Put a followable link on screen and find its exact column.
        let (link_nav, _) = pick_link(&app);
        app.cursor = link_nav;
        app.center_on_cursor();
        terminal.draw(|f| app.render(f)).unwrap();

        let w = &app.widgets[app.navigable[link_nav]];
        let span = resolve_link_at(&app.project, &app.labels, w, Some(w.x)).unwrap();
        let srow = (w.y - app.top_row) as u16;
        let link_col = (span.start - app.left_col) as u16;

        // The address column (col 0) on the link's row is never a link: clicking
        // there moves the cursor but does not follow.
        let stack = app.nav_stack.len();
        app.on_mouse(click_at(0, srow));
        assert_eq!(
            app.nav_stack.len(),
            stack,
            "click off the link does not follow"
        );

        // Clicking exactly on the link follows it.
        app.on_mouse(click_at(link_col, srow));
        assert_eq!(app.nav_stack.len(), stack + 1, "click on the link follows");
    }

    #[test]
    fn paging_moves_the_cursor() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // Home / End jump to the first / last navigable element, cursor visible.
        app.on_key(key(KeyCode::End));
        assert_eq!(app.cursor, app.navigable.len() - 1, "End → last element");
        assert!(app.cursor_visible());

        app.on_key(key(KeyCode::Home));
        assert_eq!(app.cursor, 0, "Home → first element");
        assert!(app.cursor_visible());

        // PageDown advances the cursor downward and keeps it on screen.
        let before = app.cursor_yx().0;
        app.on_key(key(KeyCode::PageDown));
        terminal.draw(|f| app.render(f)).unwrap();
        let after = app.cursor_yx().0;
        assert!(
            after > before,
            "PageDown moved cursor down ({before} → {after})"
        );
        assert!(app.cursor_visible(), "cursor stays visible after PageDown");

        // PageUp brings it back up.
        app.on_key(key(KeyCode::PageUp));
        assert!(app.cursor_yx().0 < after, "PageUp moved cursor up");
        assert!(app.cursor_visible());
    }

    #[test]
    fn goto_jumps_by_label_and_address() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // Goto by label name.
        let name = a_label(&app);
        let addr = app.labels[name.as_str()];
        let expected = app.jump_target_nav(app.addr_to_row[&addr]).unwrap();

        app.on_key(key(KeyCode::Char(':')));
        assert!(matches!(app.mode, Mode::Prompt { .. }), "prompt opened");
        type_and_submit(&mut app, &name);
        assert!(matches!(app.mode, Mode::Normal), "prompt closed on submit");
        assert_eq!(app.cursor, expected, "goto landed on the label");
        assert_eq!(app.nav_stack.len(), 1, "goto is undoable");
        assert!(app.cursor_visible());

        // Goto by seg:ofs lands on the same place.
        let (seg, ofs) = addr;
        let spec = format!("{}:{:04x}", app.project.segments[seg].name, ofs);
        app.on_key(key(KeyCode::Char(':')));
        type_and_submit(&mut app, &spec);
        assert_eq!(app.cursor, expected, "goto by seg:ofs matched the label");

        // A bare offset resolves against the segment the cursor is in. The
        // cursor is currently in `seg`, so the bare offset matches the same line.
        assert_eq!(
            app.focused_widget().unwrap().seg_idx,
            seg,
            "cursor is in seg"
        );
        app.on_key(key(KeyCode::Char(':')));
        type_and_submit(&mut app, &format!("{ofs:04x}"));
        assert_eq!(app.cursor, expected, "bare offset used the current segment");

        // An unresolvable target reports an error and does not move.
        let cursor_before = app.cursor;
        app.on_key(key(KeyCode::Char(':')));
        type_and_submit(&mut app, "definitely_not_a_label");
        assert_eq!(app.cursor, cursor_before, "failed goto left cursor put");
        assert!(
            app.status
                .as_deref()
                .unwrap()
                .contains("no address or label")
        );
    }

    #[test]
    fn goto_snaps_to_nearest_preceding_line() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // A line whose next line in the same segment is more than one byte away,
        // i.e. a multi-byte instruction/data element with addressable interior.
        let entries: Vec<(Address, u32)> = app.addr_to_row.iter().map(|(&a, &r)| (a, r)).collect();
        let ((seg, ofs), row) = entries
            .windows(2)
            .find_map(|w| {
                let ((s0, o0), r0) = w[0];
                let ((s1, o1), _) = w[1];
                (s0 == s1 && o1 > o0 + 1).then_some(((s0, o0), r0))
            })
            .expect("project has a multi-byte element");

        let interior = ofs + 1; // inside the element, not itself a line start
        assert!(!app.addr_to_row.contains_key(&(seg, interior)));

        // The resolver snaps the interior offset back to the element's line.
        assert_eq!(app.row_for_address((seg, interior)), Some(row));

        // Goto by an interior seg:ofs snaps to that line.
        let expected = app.jump_target_nav(row).unwrap();
        let spec = format!("{}:{:04x}", app.project.segments[seg].name, interior);
        app.on_key(key(KeyCode::Char(':')));
        type_and_submit(&mut app, &spec);
        assert_eq!(app.cursor, expected, "interior seg:ofs snapped to the line");

        // The cursor is now in `seg`; an auto-label-style name carrying the
        // interior offset (e.g. `data_03977`) snaps the same way.
        app.on_key(key(KeyCode::Char(':')));
        type_and_submit(&mut app, &format!("data_{interior:05x}"));
        assert_eq!(
            app.cursor, expected,
            "auto-label-style name snapped to the line"
        );
    }

    #[test]
    fn following_a_reference_to_an_interior_label_snaps() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // An address inside a multi-byte element — as a referenced label like
        // `data_03977` (covered by `data_03975`) would be — has no line of its
        // own.
        let entries: Vec<(Address, u32)> = app.addr_to_row.iter().map(|(&a, &r)| (a, r)).collect();
        let ((seg, ofs), row) = entries
            .windows(2)
            .find_map(|w| {
                let ((s0, o0), r0) = w[0];
                let ((s1, o1), _) = w[1];
                (s0 == s1 && o1 > o0 + 1).then_some(((s0, o0), r0))
            })
            .expect("project has a multi-byte element");
        let interior = (seg, ofs + 1);
        assert!(!app.addr_to_row.contains_key(&interior));

        // Following a reference to that interior address (what `Enter` on
        // `[data_03977]` does) lands on the covering element's line.
        let expected = app.jump_target_nav(row).unwrap();
        app.jump_following(interior);
        assert_eq!(
            app.cursor, expected,
            "followed reference snapped to the line"
        );
        assert_eq!(app.nav_stack.len(), 1, "follow is undoable");
    }

    #[test]
    fn search_is_full_text_and_cycles() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // Search a value that lives in the disassembly text but is not a label
        // or comment — an opcode mnemonic — proving it searches rendered lines.
        let mnemonic = app
            .widgets
            .iter()
            .find(|w| matches!(w.kind, WidgetKind::Opcode))
            .map(|w| w.text.to_string())
            .expect("project has at least one instruction");

        app.on_key(key(KeyCode::Char('/')));
        type_and_submit(&mut app, &mnemonic);
        assert!(!app.search_matches.is_empty(), "search found the mnemonic");
        assert_eq!(app.search_idx, 0);
        assert_eq!(
            app.status.as_deref(),
            Some(&*format!("match 1/{}", app.search_matches.len()))
        );
        // Every match is a row whose rendered text really contains the query.
        let q = mnemonic.to_lowercase();
        for &y in &app.search_matches {
            assert!(app.row_text(y).to_lowercase().contains(&q));
        }
        let first = app.cursor;

        // n / N cycle through matches (wrapping), updating the index.
        let n = app.search_matches.len();
        app.on_key(key(KeyCode::Char('n')));
        assert_eq!(app.search_idx, 1 % n);
        app.on_key(key(KeyCode::Char('N')));
        assert_eq!(app.search_idx, 0);
        assert_eq!(app.cursor, first, "wrapped back to the first match");

        // A query with no hits clears matches and reports it.
        app.on_key(key(KeyCode::Char('/')));
        type_and_submit(&mut app, "zzz_no_such_text_zzz");
        assert!(app.search_matches.is_empty());
        assert!(app.status.as_deref().unwrap().contains("no matches"));
    }

    /// Put the cursor on an instruction line that carries no user label in the
    /// authoritative document, and return its address.
    fn cursor_on_unlabeled_instruction(app: &mut App) -> Address {
        let nav = app
            .navigable
            .iter()
            .position(|&i| {
                let w = &app.widgets[i];
                matches!(w.kind, WidgetKind::Opcode)
                    && app
                        .base
                        .attrs
                        .get(&(w.seg_idx, w.ofs))
                        .and_then(|a| a.name.as_ref())
                        .is_none()
            })
            .expect("an unlabeled instruction");
        app.cursor = nav;
        app.cursor_address().unwrap()
    }

    #[test]
    fn rename_undo_redo() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);

        assert!(!app.dirty(), "clean on load");
        app.apply_rename(addr, "my_label".to_string());
        assert!(app.dirty(), "edit dirties the document");
        assert_eq!(app.base.attrs[&addr].name.as_deref(), Some("my_label"));
        // The re-derived view reflects the new label.
        assert_eq!(app.labels.get("my_label"), Some(&addr));
        assert_eq!(app.head, 1);

        app.undo();
        assert!(!app.dirty(), "undo back to the save point is clean");
        assert!(
            app.base
                .attrs
                .get(&addr)
                .and_then(|a| a.name.as_ref())
                .is_none()
        );
        assert!(!app.labels.contains_key("my_label"));

        app.redo();
        assert!(app.dirty());
        assert_eq!(app.base.attrs[&addr].name.as_deref(), Some("my_label"));
    }

    #[test]
    fn rename_via_keys_uses_the_prompt() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);

        app.on_key(key(KeyCode::Char('l')));
        assert!(
            matches!(
                app.mode,
                Mode::Prompt {
                    kind: PromptKind::Rename,
                    ..
                }
            ),
            "n opens a rename prompt"
        );
        type_and_submit(&mut app, "kbd_label");
        assert_eq!(app.base.attrs[&addr].name.as_deref(), Some("kbd_label"));
        assert!(matches!(app.mode, Mode::Normal));
    }

    #[test]
    fn comment_edit_then_clear() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);

        app.apply_comment(addr, "a note".to_string());
        assert_eq!(app.base.attrs[&addr].comment.as_deref(), Some("a note"));

        // Whitespace-only input clears the comment and drops the now-empty attr.
        app.apply_comment(addr, "   ".to_string());
        assert!(!app.base.attrs.contains_key(&addr));
        assert_eq!(app.head, 2, "both edits are recorded");
    }

    #[test]
    fn comment_editor_writes_multiline() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);

        // `;` opens the multi-line editor.
        app.on_key(key(KeyCode::Char(';')));
        assert!(matches!(app.mode, Mode::Comment(_)), "; opens the editor");

        // Enter inserts a newline between the two lines; Tab saves.
        for c in "line one".chars() {
            app.on_key(key(KeyCode::Char(c)));
        }
        app.on_key(key(KeyCode::Enter));
        for c in "line two".chars() {
            app.on_key(key(KeyCode::Char(c)));
        }
        app.on_key(key(KeyCode::Tab));
        assert!(matches!(app.mode, Mode::Normal));
        assert_eq!(
            app.base.attrs[&addr].comment.as_deref(),
            Some("line one\nline two"),
            "newline preserved through commit"
        );

        // Re-open: editor is prefilled with the existing lines; Esc cancels.
        app.on_key(key(KeyCode::Char(';')));
        match &app.mode {
            Mode::Comment(ed) => assert_eq!(ed.lines, vec!["line one", "line two"]),
            _ => panic!("editor not open"),
        }
        for c in "zzz".chars() {
            app.on_key(key(KeyCode::Char(c)));
        }
        app.on_key(key(KeyCode::Esc));
        assert_eq!(
            app.base.attrs[&addr].comment.as_deref(),
            Some("line one\nline two"),
            "esc discards editor changes"
        );
    }

    #[test]
    fn line_input_word_boundaries() {
        assert_eq!(prev_word("foo bar baz", 11), 8);
        assert_eq!(prev_word("foo bar baz", 8), 4); // skip the gap, then the word
        assert_eq!(prev_word("foo   bar", 6), 0); // multiple spaces collapse
        assert_eq!(prev_word("", 0), 0);
        assert_eq!(next_word("foo bar baz", 0), 4);
        assert_eq!(next_word("foo bar baz", 4), 8);
        assert_eq!(next_word("foo bar", 4), 7); // last word → end
    }

    #[test]
    fn line_input_readline_editing() {
        let mut li = LineInput::new("hello world".to_string());
        assert_eq!(li.col, 11, "new() lands the cursor at the end");

        // Ctrl-A / Ctrl-E to line ends.
        li.on_key(key_ctrl(KeyCode::Char('a')));
        assert_eq!(li.col, 0);
        li.on_key(key_ctrl(KeyCode::Char('e')));
        assert_eq!(li.col, 11);

        // Ctrl-W deletes the word before the cursor.
        li.on_key(key_ctrl(KeyCode::Char('w')));
        assert_eq!(li.text, "hello ");
        assert_eq!(li.col, 6);

        // Insert in the middle after moving left two chars.
        li.on_key(key(KeyCode::Left));
        li.on_key(key(KeyCode::Left));
        li.insert('X');
        assert_eq!(li.text, "hellXo ");

        // Word motion (Alt-B / Ctrl-Left) and Ctrl-U (delete to start).
        let mut li = LineInput::new("alpha beta gamma".to_string());
        li.on_key(key_alt(KeyCode::Char('b')));
        assert_eq!(li.col, 11, "alt-b to start of last word");
        li.on_key(key_ctrl(KeyCode::Left));
        assert_eq!(li.col, 6, "ctrl-left another word back");
        li.on_key(key_ctrl(KeyCode::Char('u')));
        assert_eq!(li.text, "beta gamma", "ctrl-u deletes to line start");
        assert_eq!(li.col, 0);

        // Ctrl-K deletes to end of line.
        li.on_key(key_ctrl(KeyCode::Char('e')));
        li.on_key(key(KeyCode::Left));
        li.on_key(key(KeyCode::Left));
        li.on_key(key(KeyCode::Left));
        li.on_key(key(KeyCode::Left));
        li.on_key(key_ctrl(KeyCode::Char('k')));
        assert_eq!(li.text, "beta g");
    }

    #[test]
    fn prompt_supports_midline_editing() {
        let mut app = new_app();
        app.on_key(key(KeyCode::Char('/')));
        for c in "helo".chars() {
            app.on_key(key(KeyCode::Char(c)));
        }
        // Move back two and fix the typo: hel|o → hell|o.
        app.on_key(key(KeyCode::Left));
        app.on_key(key(KeyCode::Char('l')));
        match &app.mode {
            Mode::Prompt { input, .. } => {
                assert_eq!(input.text, "hello");
                assert_eq!(input.col, 4, "caret stays before the final char");
            }
            _ => panic!("prompt not open"),
        }
    }

    #[test]
    fn comment_editor_ctrl_word_editing() {
        let mut ed = CommentEditor::new((SegmentIdx::from(0usize), 0), "alpha beta gamma");
        // Ctrl-W removes the last word; cursor starts at end.
        ed.delete_word_back();
        assert_eq!(ed.lines, vec!["alpha beta "]);
        assert_eq!(ed.col, 11);
        // Ctrl-A then Ctrl-K clears the line.
        ed.col = 0;
        ed.delete_to_end();
        assert_eq!(ed.lines, vec![""]);

        // Word motion within a line.
        let mut ed = CommentEditor::new((SegmentIdx::from(0usize), 0), "one two three");
        ed.word_left();
        assert_eq!(ed.col, 8, "word-left to start of last word");
        ed.word_left();
        assert_eq!(ed.col, 4);
        ed.col = 0;
        // At line start, word-left does nothing on a single line.
        ed.word_left();
        assert_eq!((ed.row, ed.col), (0, 0));
    }

    /// Put the cursor on an instruction that takes an `ofs_seg` (has an
    /// immediate or direct-memory operand) and return its address.
    fn cursor_on_ofs_seg_instruction(app: &mut App) -> Address {
        for n in 0..app.navigable.len() {
            if matches!(app.widgets[app.navigable[n]].kind, WidgetKind::Opcode) {
                app.cursor = n;
                if app.cursor_ofs_seg_offset().is_some() {
                    return app.cursor_address().unwrap();
                }
            }
        }
        panic!("no ofs_seg-applicable instruction in project");
    }

    #[test]
    fn ofs_seg_only_on_applicable_instructions() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();

        // A comment/header line is never applicable: `o` does nothing.
        let non_op = (0..app.navigable.len())
            .find(|&n| !matches!(app.widgets[app.navigable[n]].kind, WidgetKind::Opcode));
        if let Some(n) = non_op {
            app.cursor = n;
            assert!(app.cursor_ofs_seg_offset().is_none());
            app.on_key(key(KeyCode::Char('o')));
            assert!(matches!(app.mode, Mode::Normal), "o is a no-op when N/A");
            assert!(app.status.as_deref().unwrap().contains("not an offset"));
        }
    }

    #[test]
    fn ofs_seg_picker_set_preselect_and_clear() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_ofs_seg_instruction(&mut app);
        let seg_idx = app.base.segments.indexed_iter().next().unwrap().0;

        // `o` opens the picker.
        app.on_key(key(KeyCode::Char('o')));
        assert!(matches!(app.mode, Mode::OfsSeg(_)), "o opens the picker");

        // The popup renders without panicking and shows the list + clear option.
        terminal.draw(|f| app.render(f)).unwrap();
        let screen = buffer_text(terminal.backend().buffer());
        assert!(screen.contains("ofs-seg"), "title shown");
        assert!(screen.contains("none"), "clear option shown");

        // Select the first segment and apply.
        let item = match &app.mode {
            Mode::OfsSeg(p) => p.items.iter().position(|it| *it == Some(seg_idx)).unwrap(),
            _ => unreachable!(),
        };
        if let Mode::OfsSeg(p) = &mut app.mode {
            p.selected = item;
        }
        app.on_key(key(KeyCode::Enter));
        assert!(matches!(app.mode, Mode::Normal));
        assert_eq!(app.base.attrs[&addr].ofs_seg, Some(seg_idx));
        assert_eq!(
            app.last_ofs_seg,
            Some(seg_idx),
            "remembers the last applied"
        );
        assert!(app.dirty());

        // Re-open: preselects the instruction's current ofs_seg.
        app.on_key(key(KeyCode::Char('o')));
        match &app.mode {
            Mode::OfsSeg(p) => assert_eq!(p.choice(), Some(seg_idx), "preselects current"),
            _ => panic!("picker not open"),
        }

        // `— none —` (item 0) clears it.
        if let Mode::OfsSeg(p) = &mut app.mode {
            p.selected = 0;
        }
        app.on_key(key(KeyCode::Enter));
        assert!(app.base.attrs.get(&addr).and_then(|a| a.ofs_seg).is_none());

        // With no assumption set, re-opening preselects the last applied.
        app.on_key(key(KeyCode::Char('o')));
        match &app.mode {
            Mode::OfsSeg(p) => assert_eq!(p.choice(), Some(seg_idx), "falls back to last applied"),
            _ => panic!("picker not open"),
        }
    }

    #[test]
    fn save_persists_and_clears_dirty() {
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);
        app.apply_rename(addr, "saved_label".to_string());
        assert!(app.dirty());

        let tmp = std::env::temp_dir().join("chani_tui_save_test.chani");
        app.path = tmp.clone();
        app.save();
        assert!(!app.dirty(), "save clears dirty");

        let text = std::fs::read_to_string(&tmp).unwrap();
        assert!(text.contains("saved_label"), "edit is serialized to disk");
        std::fs::remove_file(&tmp).ok();
    }

    #[test]
    fn quit_is_guarded_while_dirty() {
        // Clean: q quits immediately.
        let mut app = new_app();
        app.on_key(key(KeyCode::Char('q')));
        assert!(app.should_quit());

        // Dirty: first q asks to confirm, a typed key cancels it, q again confirms.
        let mut app = new_app();
        let mut terminal = Terminal::new(TestBackend::new(140, 40)).unwrap();
        terminal.draw(|f| app.render(f)).unwrap();
        let addr = cursor_on_unlabeled_instruction(&mut app);
        app.apply_rename(addr, "x".to_string());

        app.on_key(key(KeyCode::Char('q')));
        assert!(!app.should_quit(), "first q while dirty does not quit");
        assert!(app.confirm_quit);

        app.on_key(key(KeyCode::Down));
        assert!(!app.confirm_quit, "another key cancels the pending quit");

        app.on_key(key(KeyCode::Char('q')));
        assert!(!app.should_quit());
        app.on_key(key(KeyCode::Char('q')));
        assert!(app.should_quit(), "two q in a row discards and quits");
    }
}

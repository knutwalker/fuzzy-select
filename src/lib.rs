//! ## A fuzzy select prompt for the terminal.
//!
//! This crate is a library for creating a fuzzy select prompt for the terminal.
//! It uses [nucleo] as its fuzzy matching engine.
//! The prompt is very simple and not very configurable.
//!
//! ## Usage
//!
//! Add the following to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! fuzzy-select = "0.1"
//! ```
//!
//! ## Example
//! ```no_run
//! # fn main() -> Result<(), fuzzy_select::Error> {
//! use fuzzy_select::FuzzySelect;
//!
//! let options = vec!["foo", "bar", "baz"];
//! let selected = FuzzySelect::new()
//!     .with_prompt("Select something")
//!     .with_options(options)
//!     .select()?;
//!
//! println!("Selected: {:?}", selected);
//! # Ok(())
//! # }
//! ```
use std::{
    borrow::Cow,
    fmt,
    io::{self, StderrLock, Write},
    num::NonZeroU16,
    rc::Rc,
    sync::Arc,
};

use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    style,
    terminal::{self, ClearType},
    QueueableCommand,
};
use nucleo::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Nucleo, Snapshot, Utf32Str,
};

pub use crossterm::style::{Attribute, Attributes, Color, ContentStyle, StyledContent, Stylize};
use unicode_segmentation::UnicodeSegmentation;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, onlyerror::Error)]
pub enum Error {
    #[error("No options to select from")]
    NoOptions,

    #[error("Selection cancelled")]
    Cancelled,

    #[error("Selected index is out of bounds")]
    InvalidSelection,

    #[error("Cannot run with the terminal in non-interactive mode (not a tty)")]
    NonInteractive,

    #[error("Failed to interacto with the terminal: {0}")]
    IoError(#[from] io::Error),
}

/// An item that can be selected in a fuzzy select prompt.
///
/// This trait is implemented for a number of string like types, where
/// the rendered and searched content is identical to the string.
///
/// For more complex items, trait implementation need to provide what
/// content is being searched and how the item is rendered.
///
/// The search content will always be rendered and possibly highlighted
/// with the matched parts of the search query. One can optionally
/// render additional content before and after the search content.
/// This will only be included in the rendering, not in the search.
pub trait Select {
    /// The content that is being used to match against the prompt input.
    fn search_content(&self) -> &str;

    /// Optionally render additional content before the search content when
    /// showing this item.
    fn render_before_content(&self) -> Option<impl fmt::Display + '_>;

    /// Optionally render additional content after the search content when
    /// showing this item.
    fn render_after_content(&self) -> Option<impl fmt::Display + '_>;
}

#[allow(clippy::struct_excessive_bools)]
/// A fuzzy select prompt. See the [module level documentation](crate) for more information.
#[derive(Clone, Debug)]
pub struct FuzzySelect<T> {
    prompt: Option<String>,
    options: Vec<T>,
    initial_selection: u32,
    query: Option<String>,
    filter: bool,
    highlight: bool,
    page_size: Option<NonZeroU16>,
    color: Option<bool>,
    theme: Theme,
    alternate_screen: bool,
    select1: bool,
}

impl<T> Default for FuzzySelect<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> FuzzySelect<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            prompt: None,
            options: Vec::new(),
            initial_selection: 0,
            query: None,
            filter: true,
            highlight: true,
            page_size: None,
            color: None,
            theme: Theme::default(),
            alternate_screen: true,
            select1: false,
        }
    }

    #[must_use]
    pub fn with_prompt(mut self, prompt: impl Into<String>) -> Self {
        self.prompt = Some(prompt.into());
        self
    }

    #[must_use]
    pub fn without_prompt(mut self) -> Self {
        self.prompt = None;
        self
    }

    #[must_use]
    pub fn set_prompt<P: Into<String>>(mut self, prompt: impl Into<Option<P>>) -> Self {
        self.prompt = prompt.into().map(Into::into);
        self
    }

    #[must_use]
    pub fn with_options(mut self, options: Vec<T>) -> Self {
        self.options = options;
        self
    }

    #[must_use]
    pub fn with_options_from_iter(mut self, options: impl IntoIterator<Item = T>) -> Self {
        self.options.extend(options);
        self
    }

    #[must_use]
    pub fn with_options_from_slice(mut self, options: &[T]) -> Self
    where
        T: Clone,
    {
        self.options.extend_from_slice(options);
        self
    }

    #[must_use]
    pub fn add_option(mut self, option: T) -> Self {
        self.options.push(option);
        self
    }

    #[must_use]
    pub fn add_options(mut self, options: impl IntoIterator<Item = T>) -> Self {
        self.options.extend(options);
        self
    }

    #[must_use]
    pub fn with_initial_selection(mut self, initial_selection: u32) -> Self {
        self.initial_selection = initial_selection;
        self
    }

    #[must_use]
    pub fn with_query(mut self, query: impl Into<String>) -> Self {
        self.query = Some(query.into());
        self
    }

    #[must_use]
    pub fn without_query(mut self) -> Self {
        self.query = None;
        self
    }

    #[must_use]
    pub fn set_query<Q: Into<String>>(mut self, query: impl Into<Option<Q>>) -> Self {
        self.query = query.into().map(Into::into);
        self
    }

    #[must_use]
    pub fn with_filter(mut self) -> Self {
        self.filter = true;
        self
    }

    #[must_use]
    pub fn without_filter(mut self) -> Self {
        self.filter = false;
        self
    }

    #[must_use]
    pub fn set_filter(mut self, filter: bool) -> Self {
        self.filter = filter;
        self
    }

    #[must_use]
    pub fn with_select1(mut self) -> Self {
        self.select1 = true;
        self
    }

    #[must_use]
    pub fn without_select1(mut self) -> Self {
        self.select1 = false;
        self
    }

    #[must_use]
    pub fn set_select1(mut self, select1: bool) -> Self {
        self.select1 = select1;
        self
    }

    #[must_use]
    pub fn without_highlight(mut self) -> Self {
        self.highlight = false;
        self
    }

    #[must_use]
    pub fn set_highlight(mut self, highlight: bool) -> Self {
        self.highlight = highlight;
        self
    }

    #[must_use]
    pub fn with_page_size(mut self, page_size: u16) -> Self {
        self.page_size = NonZeroU16::new(page_size);
        self
    }

    #[must_use]
    pub fn with_default_page_size(mut self) -> Self {
        self.page_size = None;
        self
    }

    #[must_use]
    pub fn with_color(mut self, color: bool) -> Self {
        self.color = Some(color);
        self
    }

    #[must_use]
    pub fn with_default_color(mut self) -> Self {
        self.color = None;
        self
    }

    #[must_use]
    pub fn set_color(mut self, color: impl Into<Option<bool>>) -> Self {
        self.color = color.into();
        self
    }

    #[must_use]
    pub fn with_theme(mut self, theme: Theme) -> Self {
        self.theme = theme;
        self
    }

    #[must_use]
    pub fn with_default_theme(mut self) -> Self {
        self.theme = Theme::default();
        self
    }

    #[must_use]
    pub fn without_alternate_screen(mut self) -> Self {
        self.alternate_screen = false;
        self
    }

    #[must_use]
    pub fn with_alternate_screen(mut self) -> Self {
        self.alternate_screen = true;
        self
    }

    #[must_use]
    pub fn set_alternate_screen(mut self, alternate_screen: bool) -> Self {
        self.alternate_screen = alternate_screen;
        self
    }

    /// Runs the fuzzy select prompt and returns the selected item.
    ///
    /// # Errors
    /// - [`Error::NoOptions`] if there are no options to select from.
    ///     Need to call one of the `*option*` methods before calling this.
    /// - [`Error::Cancelled`] if the user cancelled the selection
    ///     (e.g. by hitting Ctrl-C or Esc).
    /// - [`Error::InvalidSelection`] if the default selection index
    ///     is out of bounds based off the provided options.
    /// - [`Error::NonInteractive`] if the terminal is not in interactive mode.
    ///    This is the case if the terminal is not a tty.
    /// - [`Error::IoError`] if there was an error interacting with interacting
    ///     with terminal in general.
    pub fn select(mut self) -> Result<T>
    where
        T: Select,
    {
        if self.options.is_empty() {
            return Err(Error::NoOptions);
        }

        let mut engine = self.init_engine();

        if self.query.is_some() && self.select1 {
            let _status = engine.tick(10);
            let snap = engine.snapshot();
            if snap.matched_item_count() == 1 {
                if let Some(index) = snap.get_matched_item(0) {
                    return Ok(self.options.swap_remove(*index.data));
                }
            }
        }

        let mut prompt = self.init_prompt()?;
        prompt.initial_prompt(self.prompt.as_deref());

        let mut redraw = Redraw::Selection;
        let selected = loop {
            let res = prompt.tick(&self.theme, &mut engine, &self.options, redraw)?;

            match res {
                Ok(r) => redraw = r,
                Err(Stop::Quit) => return Err(Error::Cancelled),
                Err(Stop::Selected(selected)) => break selected,
            }
        };
        drop(prompt);

        let item = *engine
            .snapshot()
            .get_matched_item(selected)
            .ok_or(Error::InvalidSelection)?
            .data;
        let item = self.options.swap_remove(item);

        Ok(item)
    }

    /// Runs the fuzzy select prompt and returns the selected item or None
    /// if no selection was made.
    ///
    /// # Errors
    /// - [`Error::NonInteractive`] if the terminal is not in interactive mode.
    ///    This is the case if the terminal is not a tty.
    /// - [`Error::IoError`] if there was an error interacting with interacting
    ///     with terminal in general.
    pub fn select_opt(self) -> Result<Option<T>>
    where
        T: Select,
    {
        match self.select() {
            Ok(res) => Ok(Some(res)),
            Err(Error::NoOptions | Error::Cancelled | Error::InvalidSelection) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn init_engine(&self) -> Nucleo<usize>
    where
        T: Select,
    {
        let mut engine = Nucleo::<usize>::new(Config::DEFAULT, Arc::new(|| {}), None, 1);

        let injector = engine.injector();

        for (idx, item) in self.options.iter().enumerate() {
            let _ = injector.push(idx, move |_, cols| {
                cols[0] = item.search_content().into();
            });
        }

        if let Some(query) = self.query.as_deref() {
            engine
                .pattern
                .reparse(0, query, CaseMatching::Smart, Normalization::Smart, true);
        }

        engine
    }

    fn init_prompt(&mut self) -> Result<Prompt> {
        let term = Terminal::new(self.color, self.alternate_screen)?;
        let page_size = self
            .page_size
            .map(NonZeroU16::get)
            .or_else(|| terminal::size().ok().map(|(_, h)| h.saturating_sub(1)))
            .unwrap_or(10);
        let highlighter = Highlighter::new(self.highlight && self.color.unwrap_or(true));
        let query = self.query.take().unwrap_or_default();
        let input = Input::new(query);
        let selected = self.initial_selection;
        if selected as usize >= self.options.len() {
            return Err(Error::InvalidSelection);
        }

        Ok(Prompt {
            term,
            scroll_offset: 0,
            selected,
            height: u32::from(page_size),
            active: true,
            filter: self.filter,
            number_of_matches: u32::MAX,
            highlighter,
            input,
        })
    }
}

/// A theme describes how different parts of the prompt are rendered.
#[derive(Copy, Clone, Debug)]
pub struct Theme {
    /// The inidicator to render for the row that is currently selected.
    /// Defaults to `>` in red on a black background.
    pub selected_indicator: StyledContent<char>,

    /// The indicator to render for rows that are not selected.
    /// Defaults to a space on a black background.
    pub indicator: StyledContent<char>,

    /// The style to render the text of the row that is currently selected.
    /// Defaults to a black background.
    pub selected_text: ContentStyle,

    /// The style to render the text of rows that are not selected.
    /// Defaults to non-styled text.
    pub text: ContentStyle,

    /// The style to render the highlighted parts of the text of the row
    /// that is currently selected.
    /// Defaults to dark cyan on black background.
    pub selected_highlight: ContentStyle,

    /// The style to render the highlighted parts of the text of rows
    /// that are not selected.
    /// Defaults to cyan text.
    pub highlight: ContentStyle,
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            selected_indicator: ContentStyle::new().red().on_black().apply('>'),
            indicator: ContentStyle::new().on_black().apply(' '),
            selected_text: ContentStyle::new().white().on_black(),
            text: ContentStyle::new(),
            selected_highlight: ContentStyle::new().dark_cyan().on_black(),
            highlight: ContentStyle::new().cyan(),
        }
    }
}

impl Select for String {
    fn search_content(&self) -> &str {
        self.as_str()
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for &str {
    fn search_content(&self) -> &str {
        self
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Box<str> {
    fn search_content(&self) -> &str {
        self
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Rc<str> {
    fn search_content(&self) -> &str {
        self
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Arc<str> {
    fn search_content(&self) -> &str {
        self
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Cow<'_, str> {
    fn search_content(&self) -> &str {
        self
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

struct Prompt {
    term: Terminal,
    input: Input,
    scroll_offset: u32,
    selected: u32,
    height: u32,
    active: bool,
    filter: bool,
    number_of_matches: u32,
    highlighter: Highlighter,
}

impl Prompt {
    fn initial_prompt(&mut self, prompt: Option<&str>) {
        self.term
            .queue(cursor::MoveTo(0, 0))
            .queue_(terminal::Clear(ClearType::All));

        if let Some(prompt) = prompt {
            self.term.queue_(style::Print(prompt));

            if self.filter {
                self.term.queue_(style::Print(" "));
            } else {
                self.term.queue_(cursor::Hide);
            }
        }

        self.term.queue_(cursor::SavePosition);
    }

    fn tick<T: Select>(
        &mut self,
        theme: &Theme,
        nucleo: &mut Nucleo<usize>,
        items: &[T],
        redraw: Redraw,
    ) -> Result<Result<Redraw, Stop>> {
        if self.active {
            let status = nucleo.tick(10);
            let snap = nucleo.snapshot();

            if status.changed {
                self.scroll_offset = 0;
                self.selected = self
                    .selected
                    .min(snap.matched_item_count().saturating_sub(1));
            }

            self.render_items(theme, snap, items, redraw)?;
        }

        let key = crossterm::event::read()?;
        let changed = self.handle_event(&key);

        let redraw = match changed {
            Changed::Nothing => Redraw::Nothing,
            Changed::Cursor => Redraw::Cursor,
            Changed::Selection => Redraw::Selection,
            Changed::Content => {
                nucleo.pattern.reparse(
                    0,
                    self.input.content(),
                    CaseMatching::Smart,
                    Normalization::Smart,
                    self.input.appending(),
                );

                Redraw::Content
            }
            Changed::Stop(stop) => return Ok(Err(stop)),
        };

        Ok(Ok(redraw))
    }

    fn render_items<T: Select>(
        &mut self,
        theme: &Theme,
        snapshot: &Snapshot<usize>,
        items: &[T],
        redraw: Redraw,
    ) -> Result<()> {
        self.number_of_matches = snapshot.matched_item_count();

        if redraw >= Redraw::Content {
            let end = self
                .scroll_offset
                .saturating_add(self.height)
                .min(snapshot.matched_item_count());
            let start = self.scroll_offset.min(end.saturating_sub(1));

            let matched_items = snapshot.matched_items(start..end).enumerate();

            for (idx, item) in matched_items {
                #[allow(clippy::cast_possible_truncation)]
                let idx = idx as u32 + start;
                let entry = &items[*item.data];

                let (indicator, text, hl) = if idx == self.selected {
                    (
                        theme.selected_indicator,
                        &theme.selected_text,
                        &theme.selected_highlight,
                    )
                } else {
                    (theme.indicator, &theme.text, &theme.highlight)
                };

                self.term
                    .queue(cursor::MoveToNextLine(1))
                    .queue(terminal::Clear(ClearType::CurrentLine))
                    .queue(style::PrintStyledContent(indicator))
                    .queue_(style::PrintStyledContent(text.apply(" ")));

                if let Some(content) = entry.render_before_content() {
                    let _ = self
                        .term
                        .queue(style::PrintStyledContent(text.apply(content)));
                }

                self.highlighter.render(
                    *text,
                    *hl,
                    &mut self.term,
                    snapshot.pattern().column_pattern(0),
                    item.matcher_columns[0].slice(..),
                );

                if let Some(content) = entry.render_after_content() {
                    self.term
                        .queue_(style::PrintStyledContent(text.apply(content)));
                }
            }
        }

        if redraw >= Redraw::Content {
            self.term
                .queue(terminal::Clear(ClearType::FromCursorDown))
                .queue(cursor::RestorePosition)
                .queue(terminal::Clear(ClearType::UntilNewLine))
                .queue_(style::PrintStyledContent(self.input.content().bold()));

            let offset = self.input.cursor_offset_from_end();
            if offset > 0 {
                self.term.queue_(cursor::MoveLeft(offset));
            }
        } else if redraw == Redraw::Cursor {
            self.term.queue_(cursor::RestorePosition);

            let offset = self.input.cursor_offset_from_start();
            if offset > 0 {
                self.term.queue_(cursor::MoveRight(offset));
            }
        }

        if redraw > Redraw::Nothing {
            self.term.flush()?;
        }

        Ok(())
    }

    fn handle_event(&mut self, event: &Event) -> Changed {
        match event {
            Event::Key(KeyEvent {code, modifiers, kind: KeyEventKind::Press, ..}) => return self.handle_key_event(*code, *modifiers),
            Event::FocusLost => self.active = false,
            Event::FocusGained => self.active = true,
            Event::Resize(_, h) => {
                self.height = u32::from(h.saturating_sub(1));
                return Changed::Selection;
            }
            Event::Mouse(_) | Event::Paste(_) => {}
            _ => {}
        };

        Changed::Nothing
    }

    fn handle_key_event(&mut self, code: KeyCode, modifiers: KeyModifiers) -> Changed {
        match (code, modifiers) {
            (KeyCode::Esc, _) => match self.input.clear() {
                Changed::Nothing => Changed::Stop(Stop::Quit),
                otherwise => otherwise,
            },
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => Changed::Stop(Stop::Quit),
            (KeyCode::Enter | KeyCode::Char('\n' | '\r'), _) => {
                Changed::Stop(Stop::Selected(self.selected))
            }
            (KeyCode::Backspace, _) => self.input.delete_left(),
            (KeyCode::Delete, _) => self.input.delete_right(),
            (KeyCode::Home, _) => self.input.move_to_start(),
            (KeyCode::End, _) => self.input.move_to_end(),
            (KeyCode::Left, m) => self
                .input
                .move_by(isize::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1),
            (KeyCode::Right, m) => self
                .input
                .move_by(isize::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1),
            (KeyCode::Up, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1)
            }
            (KeyCode::Down, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1)
            }
            (KeyCode::PageUp, _) => {
                self.scroll_offset = self.scroll_offset.saturating_sub(self.height);
                Changed::Selection
            }
            (KeyCode::PageDown, _) => {
                self.scroll_offset = self.scroll_offset.saturating_add(self.height);
                Changed::Selection
            }
            (KeyCode::Char(c), _) if self.filter => self.input.insert(c),
            _ => Changed::Nothing,
        }
    }

    fn move_selection(&mut self, diff: i32) -> Changed {
        self.selected = self
            .selected
            .saturating_add_signed(diff)
            .min(self.number_of_matches.saturating_sub(1));

        if self.selected < self.scroll_offset {
            self.scroll_offset = self.selected;
        }

        if self.selected >= self.scroll_offset.saturating_add(self.height) {
            self.scroll_offset = self.selected.saturating_sub(self.height).saturating_add(1);
        }

        Changed::Selection
    }
}

#[derive(Clone, Debug)]
struct Input {
    content: String,
    boundaries: Vec<usize>,
    cursor: usize,
    appending: bool,
}

impl Input {
    fn new(content: impl Into<String>) -> Self {
        let content = content.into();
        let boundaries = content
            .grapheme_indices(true)
            .map(|(idx, _)| idx)
            .collect::<Vec<_>>();
        let cursor = boundaries.len();
        Self {
            content,
            boundaries,
            cursor,
            appending: true,
        }
    }

    fn content(&self) -> &str {
        &self.content
    }

    fn appending(&self) -> bool {
        self.appending
    }

    fn cursor_offset_from_start(&self) -> u16 {
        #[allow(clippy::cast_possible_truncation)]
        let cursor = self.cursor.min(usize::from(u16::MAX)) as u16;

        cursor
    }

    fn cursor_offset_from_end(&self) -> u16 {
        #[allow(clippy::cast_possible_truncation)]
        let cursor_offset = self
            .boundaries
            .len()
            .saturating_sub(self.cursor)
            .min(usize::from(u16::MAX)) as u16;

        cursor_offset
    }

    fn clear(&mut self) -> Changed {
        if self.content.is_empty() {
            self.appending = true;
            Changed::Nothing
        } else {
            self.content.clear();
            self.boundaries.clear();
            self.cursor = 0;
            self.appending = false;
            Changed::Content
        }
    }

    fn insert(&mut self, c: char) -> Changed {
        if self.cursor >= self.boundaries.len() {
            self.appending = true;
            self.boundaries.push(self.content.len());
            self.cursor = self.boundaries.len();
            self.content.push(c);
        } else {
            self.appending = false;
            self.content.insert(self.boundaries[self.cursor], c);
            self.boundaries
                .insert(self.cursor, self.boundaries[self.cursor]);
            let len = c.len_utf8();
            self.boundaries[self.cursor + 1..]
                .iter_mut()
                .for_each(|b| *b += len);
            self.cursor += 1;
        }
        Changed::Content
    }

    fn delete_left(&mut self) -> Changed {
        if let Some(pos) = self.cursor.checked_sub(1) {
            let indexes = self.boundaries[pos]..self.index_at_cursor();
            self.content.replace_range(indexes, "");
            let _ = self.boundaries.remove(pos);
            self.cursor = pos;
            self.appending = false;
            Changed::Content
        } else {
            Changed::Nothing
        }
    }

    fn delete_right(&mut self) -> Changed {
        if self.cursor < self.boundaries.len() {
            let start = self.boundaries.remove(self.cursor);
            let indexes = start..self.index_at_cursor();
            self.content.replace_range(indexes, "");
            self.appending = false;
            Changed::Content
        } else {
            Changed::Nothing
        }
    }

    fn move_by(&mut self, diff: isize) -> Changed {
        self.move_to(self.cursor.saturating_add_signed(diff))
    }

    fn move_to(&mut self, cursor: usize) -> Changed {
        let changed = cursor != self.cursor;
        self.cursor = cursor.min(self.boundaries.len());
        if changed {
            Changed::Cursor
        } else {
            Changed::Nothing
        }
    }

    fn move_to_start(&mut self) -> Changed {
        self.move_to(0)
    }

    fn move_to_end(&mut self) -> Changed {
        self.move_to(self.boundaries.len())
    }

    fn index_at_cursor(&self) -> usize {
        self.boundaries
            .get(self.cursor)
            .map_or_else(|| self.content.len(), |&i| i)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Changed {
    Nothing,
    Cursor,
    Content,
    Selection,
    Stop(Stop),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Stop {
    Quit,
    Selected(u32),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Redraw {
    Nothing,
    Cursor,
    Content,
    Selection,
}

struct Highlighter {
    highlight: bool,
    indices: Vec<u32>,
    content: String,
    matcher: Matcher,
}

impl Highlighter {
    fn new(highlight: bool) -> Self {
        Self {
            highlight,
            indices: Vec::new(),
            content: String::new(),
            matcher: Matcher::new(Config::DEFAULT),
        }
    }

    fn render(
        &mut self,
        text: ContentStyle,
        hl: ContentStyle,
        term: &mut Terminal,
        pattern: &Pattern,
        matched_item: Utf32Str<'_>,
    ) {
        if self.highlight {
            let _score = pattern.indices(matched_item, &mut self.matcher, &mut self.indices);
            self.indices.sort_unstable();
            self.indices.dedup();

            let mut indices = self.indices.drain(..).map(|i| i as usize);
            let mut match_idx = indices.next().unwrap_or(usize::MAX);

            for (grapheme_idx, grapheme) in matched_item.chars().enumerate() {
                if grapheme_idx == match_idx {
                    if !self.content.is_empty() {
                        term.queue_(style::PrintStyledContent(text.apply(self.content.as_str())));
                        self.content.clear();
                    }

                    term.queue_(style::PrintStyledContent(hl.apply(grapheme)));
                    match_idx = indices.next().unwrap_or(usize::MAX);
                } else {
                    self.content.push(grapheme);
                }
            }

            if !self.content.is_empty() {
                term.queue_(style::PrintStyledContent(text.apply(self.content.as_str())));
                self.content.clear();
            }
        } else {
            term.queue_(style::PrintStyledContent(text.apply(matched_item)));
        }
    }
}

struct Terminal {
    io: StderrLock<'static>,
    err: Option<Error>,
    alternate_screen: bool,
}

impl Terminal {
    fn new(color: Option<bool>, alternate_screen: bool) -> Result<Self> {
        terminal::enable_raw_mode().map_err(|e| match e.raw_os_error() {
            Some(25 | 6) => Error::NonInteractive,
            _ => e.into(),
        })?;

        if let Some(color) = color {
            style::force_color_output(color);
        }

        let mut io = io::stderr().lock();
        if alternate_screen {
            let _ = io.queue(terminal::EnterAlternateScreen)?;
        }

        Ok(Self {
            io,
            err: None,
            alternate_screen,
        })
    }

    fn queue(&mut self, cmd: impl crossterm::Command) -> &mut Self {
        if let Err(e) = self.io.queue(cmd) {
            if self.err.is_none() {
                self.err = Some(e.into());
            }
        };
        self
    }

    fn queue_(&mut self, cmd: impl crossterm::Command) {
        let _ = self.queue(cmd);
    }

    fn flush(&mut self) -> Result<()> {
        if let Some(e) = self.err.take() {
            return Err(e);
        }

        self.io.flush()?;

        Ok(())
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        self.queue_(cursor::Show);
        if self.alternate_screen {
            let _ = self.queue(terminal::LeaveAlternateScreen).flush();
        }
        let _ = terminal::disable_raw_mode();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_push_at_end() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.insert('b');
        assert_eq!(input.content(), "foob");
        assert_eq!(input.cursor_offset_from_start(), 4);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_push_at_middle() {
        let mut input = Input::new("foo");
        let changed = input.move_by(-1);
        assert_eq!(changed, Changed::Cursor);

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(input.appending());

        let changed = input.insert('b');
        assert_eq!(input.content(), "fobo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_delete_left() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.delete_left();
        assert_eq!(input.content(), "fo");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_delete_left_from_middle() {
        let mut input = Input::new("fob");
        let changed = input.move_by(-1);
        assert_eq!(changed, Changed::Cursor);

        assert_eq!(input.content(), "fob");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(input.appending());

        let changed = input.delete_left();
        assert_eq!(input.content(), "fb");
        assert_eq!(input.cursor_offset_from_start(), 1);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_delete_left_from_start() {
        let mut input = Input::new("fob");
        let changed = input.move_to(0);
        assert_eq!(changed, Changed::Cursor);

        assert_eq!(input.content(), "fob");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 3);
        assert!(input.appending());

        let changed = input.delete_left();
        assert_eq!(input.content(), "fob");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 3);
        assert!(input.appending());

        assert_eq!(changed, Changed::Nothing);
    }

    #[test]
    fn input_delete_right() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.delete_right();
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Nothing);
    }

    #[test]
    fn input_delete_right_from_middle() {
        let mut input = Input::new("fob");
        let changed = input.move_by(-1);
        assert_eq!(changed, Changed::Cursor);

        assert_eq!(input.content(), "fob");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(input.appending());

        let changed = input.delete_right();
        assert_eq!(input.content(), "fo");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_delete_right_from_start() {
        let mut input = Input::new("fob");
        let changed = input.move_to(0);
        assert_eq!(changed, Changed::Cursor);

        assert_eq!(input.content(), "fob");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 3);
        assert!(input.appending());

        let changed = input.delete_right();
        assert_eq!(input.content(), "ob");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 2);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);
    }

    #[test]
    fn input_move_by() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.move_by(-1);
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 2);
        assert_eq!(input.cursor_offset_from_end(), 1);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);

        let changed = input.move_by(1);
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);
    }

    #[test]
    fn input_move_to() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.move_to(1);
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 1);
        assert_eq!(input.cursor_offset_from_end(), 2);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);

        let changed = input.move_to(3);
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);
    }

    #[test]
    fn input_move_to_start_and_end() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.move_to_start();
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 3);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);

        let changed = input.move_to_end();
        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Cursor);
    }

    #[test]
    fn input_clear() {
        let mut input = Input::new("foo");

        assert_eq!(input.content(), "foo");
        assert_eq!(input.cursor_offset_from_start(), 3);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        let changed = input.clear();
        assert_eq!(input.content(), "");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(!input.appending());

        assert_eq!(changed, Changed::Content);

        let changed = input.clear();
        assert_eq!(input.content(), "");
        assert_eq!(input.cursor_offset_from_start(), 0);
        assert_eq!(input.cursor_offset_from_end(), 0);
        assert!(input.appending());

        assert_eq!(changed, Changed::Nothing);
    }
}

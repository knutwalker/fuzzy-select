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
    event::{Event, KeyCode, KeyModifiers},
    style,
    terminal::{self, ClearType},
    QueueableCommand,
};
use nucleo::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Nucleo, Snapshot, Utf32Str, Utf32String,
};

pub use crossterm::style::{Attribute, Attributes, Color, ContentStyle, StyledContent, Stylize};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("No options to select from")]
    NoOptions,

    #[error("Initial selected index is out of bounds")]
    InvalidDefaultSelection,

    #[error("Selection cancelled")]
    Cancelled,

    #[error("Cannot run with the terminal in non-interactive mode (not a tty)")]
    NonInteractive,

    #[error("Failed to interacto with the terminal: {0}")]
    IoError(#[from] io::Error),
}

/// Trait for items that can be selected in a fuzzy select prompt.
///
/// The trait is implemented for a number of string like types, where
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
    fn set_search_content(&self, content: &mut Utf32String);

    /// Optionally render additional content before the search content when
    /// showing this item.
    fn render_before_content(&self) -> Option<impl fmt::Display + '_>;

    /// Optionally render additional content after the search content when
    /// showing this item.
    fn render_after_content(&self) -> Option<impl fmt::Display + '_>;
}

#[derive(Clone, Debug)]
pub struct FuzzySelect<T> {
    options: Vec<T>,
    prompt: Option<String>,
    initial_selection: u32,
    query: Option<String>,
    highlight: bool,
    page_size: Option<NonZeroU16>,
    color: Option<bool>,
    theme: Theme,
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
            options: Vec::new(),
            prompt: None,
            initial_selection: 0,
            query: None,
            highlight: true,
            page_size: None,
            color: None,
            theme: Theme::default(),
        }
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

    /// Runs the fuzzy select prompt and returns the selected item.
    ///
    /// # Errors
    /// - [`Error::NoOptions`] if there are no options to select from.
    ///     Need to call one of the `*option*` methods before calling this.
    /// - [`Error::InvalidDefaultSelection`] if the default selection index
    ///     is out of bounds based off the provided options.
    /// - [`Error::NonInteractive`] if the terminal is not in interactive mode.
    ///    This is the case if the terminal is not a tty.
    /// - [`Error::IoError`] if there was an error interacting with interacting
    ///     with terminal in general.
    pub fn select(mut self) -> Result<Option<T>>
    where
        T: Select,
    {
        if self.options.is_empty() {
            return Err(Error::NoOptions);
        }

        let mut engine = self.init_engine();
        let mut prompt = self.init_prompt()?;
        prompt.initial_prompt(self.prompt.as_deref());

        let selected = loop {
            let res = prompt.tick(&self.theme, &mut engine, &self.options)?;

            match res {
                Some(Stop::Quit) => {
                    return Ok(None);
                }
                Some(Stop::Selected(selected)) => {
                    break selected;
                }
                None => {}
            }
        };
        drop(prompt);

        let item = engine
            .snapshot()
            .get_matched_item(selected)
            .map(|item| item.data)
            .map(|idx| self.options.swap_remove(*idx));

        Ok(item)
    }

    fn init_engine(&self) -> Nucleo<usize>
    where
        T: Select,
    {
        let mut engine = Nucleo::<usize>::new(Config::DEFAULT, Arc::new(|| {}), None, 1);

        let injector = engine.injector();

        for (idx, item) in self.options.iter().enumerate() {
            let _ = injector.push(idx, move |cols| {
                item.set_search_content(&mut cols[0]);
            });
        }

        if let Some(query) = self.query.as_ref() {
            engine
                .pattern
                .reparse(0, query, CaseMatching::Smart, Normalization::Smart, true);
        }

        engine
    }

    fn init_prompt(&mut self) -> Result<Prompt> {
        let term = Terminal::new(self.color)?;
        let page_size = self
            .page_size
            .map(NonZeroU16::get)
            .or_else(|| terminal::size().ok().map(|(_, h)| h.saturating_sub(1)))
            .unwrap_or(10);
        let highlighter = Highlighter::new(self.highlight && self.color.unwrap_or(true));
        let query = self.query.take().unwrap_or_default();
        let cursor_pos = query.len();
        let selected = self.initial_selection;
        if selected as usize >= self.options.len() {
            return Err(Error::InvalidDefaultSelection);
        }

        Ok(Prompt {
            term,
            query,
            cursor_pos,
            scroll_offset: 0,
            selected,
            height: u32::from(page_size),
            active: true,
            appending: true,
            number_of_matches: u32::MAX,
            highlighter,
        })
    }
}

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
            selected_text: ContentStyle::new().on_black(),
            text: ContentStyle::new(),
            selected_highlight: ContentStyle::new().dark_cyan().on_black(),
            highlight: ContentStyle::new().cyan(),
        }
    }
}

impl Select for String {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = self.as_str().into();
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for &str {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = (*self).into();
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Box<str> {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = (&**self).into();
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Rc<str> {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = (&**self).into();
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Arc<str> {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = (&**self).into();
    }

    fn render_before_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }

    fn render_after_content(&self) -> Option<impl fmt::Display + '_> {
        None::<Self>
    }
}

impl Select for Cow<'_, str> {
    fn set_search_content(&self, content: &mut Utf32String) {
        *content = (&**self).into();
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
    query: String,
    cursor_pos: usize,
    scroll_offset: u32,
    selected: u32,
    height: u32,
    active: bool,
    appending: bool,
    number_of_matches: u32,
    highlighter: Highlighter,
}

impl Prompt {
    fn initial_prompt(&mut self, prompt: Option<&str>) {
        self.term
            .queue(cursor::MoveTo(0, 0))
            .queue_(terminal::Clear(ClearType::All));

        if let Some(prompt) = prompt {
            self.term
                .queue(style::Print(prompt))
                .queue_(style::Print(" "));
        }

        self.term.queue_(cursor::SavePosition);
    }

    fn tick<T: Select>(
        &mut self,
        theme: &Theme,
        nucleo: &mut Nucleo<usize>,
        items: &[T],
    ) -> Result<Option<Stop>> {
        if self.active {
            let status = nucleo.tick(10);
            let snap = nucleo.snapshot();

            if status.changed {
                self.scroll_offset = 0;
                self.selected = self
                    .selected
                    .min(snap.matched_item_count().saturating_sub(1));
            }

            self.render_items(theme, snap, items)?;
        }

        let key = crossterm::event::read()?;
        let handled = self.handle_event(&key);

        let query_changed = match handled {
            Handled::Unchanged => false,
            Handled::Changed => true,
            Handled::Stop(stop) => return Ok(Some(stop)),
        };

        if query_changed {
            nucleo.pattern.reparse(
                0,
                &self.query,
                CaseMatching::Smart,
                Normalization::Smart,
                self.appending,
            );
        }

        Ok(None)
    }

    fn render_items<T: Select>(
        &mut self,
        theme: &Theme,
        snapshot: &Snapshot<usize>,
        items: &[T],
    ) -> Result<()> {
        self.number_of_matches = snapshot.matched_item_count();

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

        #[allow(clippy::cast_possible_truncation)]
        let cursor_offset = self
            .query
            .len()
            .saturating_sub(self.cursor_pos)
            .min(usize::from(u16::MAX)) as u16;

        self.term
            .queue(terminal::Clear(ClearType::FromCursorDown))
            .queue(cursor::RestorePosition)
            .queue(terminal::Clear(ClearType::UntilNewLine))
            .queue_(style::PrintStyledContent(self.query.as_str().bold()));

        if cursor_offset > 0 {
            self.term.queue_(cursor::MoveLeft(cursor_offset));
        }

        self.term.flush()?;

        Ok(())
    }

    fn handle_event(&mut self, event: &Event) -> Handled {
        match event {
            Event::Key(key) => return self.handle_key_event(key.code, key.modifiers),
            Event::FocusLost => self.active = false,
            Event::FocusGained => self.active = true,
            Event::Resize(_, h) => self.height = u32::from(h.saturating_sub(1)),
            Event::Mouse(_) | Event::Paste(_) => {}
        };

        Handled::Unchanged
    }

    fn handle_key_event(&mut self, code: KeyCode, modifiers: KeyModifiers) -> Handled {
        match (code, modifiers) {
            (KeyCode::Esc, _) => {
                if self.query.is_empty() {
                    Handled::Stop(Stop::Quit)
                } else {
                    self.query.clear();
                    self.cursor_pos = 0;
                    self.appending = false;
                    Handled::Changed
                }
            }
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => Handled::Stop(Stop::Quit),
            (KeyCode::Enter | KeyCode::Char('\n' | '\r'), _) => {
                Handled::Stop(Stop::Selected(self.selected))
            }
            (KeyCode::Backspace, _) => match self.cursor_pos.checked_sub(1) {
                Some(pos) => {
                    let _ = self.query.remove(pos);
                    self.cursor_pos = pos;
                    self.appending = false;
                    Handled::Changed
                }
                _ => Handled::Unchanged,
            },
            (KeyCode::Delete, _) => {
                if self.cursor_pos < self.query.len() {
                    let _ = self.query.remove(self.cursor_pos);
                    self.appending = false;
                    Handled::Changed
                } else {
                    Handled::Unchanged
                }
            }
            (KeyCode::Home, _) => {
                self.cursor_pos = 0;
                self.appending = self.query.is_empty();
                Handled::Unchanged
            }
            (KeyCode::End, _) => {
                self.cursor_pos = self.query.len();
                self.appending = true;
                Handled::Unchanged
            }
            (KeyCode::Left, m) => {
                self.move_on_line(isize::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1)
            }
            (KeyCode::Right, m) => {
                self.move_on_line(isize::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1)
            }
            (KeyCode::Up, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1)
            }
            (KeyCode::Down, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1)
            }
            (KeyCode::PageUp, _) => {
                self.scroll_offset = self.scroll_offset.saturating_sub(self.height);
                Handled::Unchanged
            }
            (KeyCode::PageDown, _) => {
                self.scroll_offset = self.scroll_offset.saturating_add(self.height);
                Handled::Unchanged
            }
            (KeyCode::Char(c), _) => {
                if self.cursor_pos == self.query.len() {
                    self.appending = true;
                    self.query.push(c);
                } else {
                    self.query.insert(self.cursor_pos, c);
                }
                self.cursor_pos += 1;
                Handled::Changed
            }
            _ => Handled::Unchanged,
        }
    }

    fn move_on_line(&mut self, diff: isize) -> Handled {
        self.cursor_pos = self
            .cursor_pos
            .saturating_add_signed(diff)
            .min(self.query.len());
        self.appending = self.cursor_pos == self.query.len();
        Handled::Unchanged
    }

    fn move_selection(&mut self, diff: i32) -> Handled {
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

        Handled::Unchanged
    }
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Handled {
    Unchanged,
    Changed,
    Stop(Stop),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Stop {
    Quit,
    Selected(u32),
}

struct Terminal {
    io: StderrLock<'static>,
    err: Option<Error>,
}

impl Terminal {
    fn new(color: Option<bool>) -> Result<Self> {
        terminal::enable_raw_mode().map_err(|e| match e.raw_os_error() {
            Some(25 | 6) => Error::NonInteractive,
            _ => e.into(),
        })?;

        if let Some(color) = color {
            style::force_color_output(color);
        }

        let mut io = io::stderr().lock();
        let _ = io.queue(terminal::EnterAlternateScreen)?;

        Ok(Self { io, err: None })
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
        let _ = self.queue(terminal::LeaveAlternateScreen).flush();
        let _ = terminal::disable_raw_mode();
    }
}

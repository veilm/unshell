use std::borrow::Cow::{self, Borrowed, Owned};
use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rustyline::completion::{Completer as CompletionTrait, Pair};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::history::DefaultHistory;
use rustyline::line_buffer::LineBuffer;
use rustyline::{
    At, Cmd, CompletionType, Completer, ConditionalEventHandler, Config, Context, EditMode, Editor,
    Event, EventContext, EventHandler, Helper, Hinter, InputMode, KeyCode, KeyEvent, Modifiers,
    Movement, RepeatCount, Result, Validator, Word,
};

use crate::state::{ReplBinding, ReplCompletionMode, ShellState};
use crate::term::cursor_column;
use crate::{build_prompt, process_line, run_named_function, RunResult, DEFAULT_PROMPT};

const COLOR_RESET: &str = "\x1b[0m";
const COLOR_COMMENT: &str = "\x1b[0;90m";
const COLOR_STRING: &str = "\x1b[1;35m";
const COLOR_KEYWORD: &str = "\x1b[1;31m";
const COLOR_PROMPT: &str = "\x1b[1;32m";
const KEYWORDS: &[&str] = &[
    "alias", "cd", "elif", "else", "eval", "exit", "export", "for", "foreach", "if",
    "set", "unalias",
];

struct FuzzyCompleter {
    start_last: Arc<AtomicBool>,
    mode: ReplCompletionMode,
}

impl CompletionTrait for FuzzyCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>)> {
        if matches!(self.mode, ReplCompletionMode::Off) {
            return Ok((pos, Vec::new()));
        }

        let quote_ctx = quote_context(line, pos);
        let (start, fragment) = if let Some(ctx) = quote_ctx.as_ref() {
            let start = ctx.start + 1;
            (start, &line[start..pos])
        } else {
            word_start(line, pos)
        };
        let (dir_prefix, query) = split_dir_query(fragment);
        let include_hidden = fragment.starts_with('.') || fragment.contains("/.");
        let candidates = list_dir_candidates(&dir_prefix, include_hidden);
        if candidates.is_empty() {
            return Ok((start, candidates));
        }

        if !query.is_empty() {
            let mut prefix_matches: Vec<Pair> = candidates
                .iter()
                .filter(|c| c.display.starts_with(query))
                .cloned()
                .collect();
            if prefix_matches.len() == 1 {
                // Prefer prefix matches to avoid fuzzy surprises like "sour" vs "src".
                finalize_completion_with_context(&mut prefix_matches[0], quote_ctx.as_ref());
                return Ok((start, prefix_matches));
            }
        }

        if matches!(self.mode, ReplCompletionMode::List) {
            return Ok((start, candidates));
        }

        let choices: Vec<String> = candidates.iter().map(|c| c.display.clone()).collect();
        let start_last = self.start_last.swap(false, Ordering::SeqCst);
        match run_fuzzy(&choices, query, start_last) {
            FuzzyOutcome::Selected(sel) => {
                let selected = candidates
                    .iter()
                    .find(|c| c.display == sel)
                    .cloned();
                if let Some(mut choice) = selected {
                    finalize_completion_with_context(&mut choice, quote_ctx.as_ref());
                    Ok((start, vec![choice]))
                } else {
                    Ok((start, candidates))
                }
            }
            FuzzyOutcome::Cancelled => Ok((
                start,
                vec![Pair {
                    display: fragment.to_string(),
                    replacement: fragment.to_string(),
                }],
            )),
            FuzzyOutcome::Unavailable => Ok((start, candidates)),
        }
    }

    fn update(
        &self,
        line: &mut LineBuffer,
        start: usize,
        elected: &str,
        cl: &mut rustyline::Changeset,
    ) {
        line.replace(start..line.pos(), elected, cl);
    }
}

fn quote_completion(value: &str) -> String {
    if !value.chars().any(|ch| ch.is_whitespace()) {
        return value.to_string();
    }
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        if ch == '"' || ch == '\\' {
            out.push('\\');
        }
        out.push(ch);
    }
    out.push('"');
    out
}

fn finalize_completion(choice: &mut Pair) {
    let is_dir = choice.replacement.ends_with('/');
    choice.replacement = quote_completion(&choice.replacement);
    if !is_dir {
        choice.replacement.push(' ');
    }
}

fn finalize_completion_with_context(choice: &mut Pair, quote_ctx: Option<&QuoteContext>) {
    if let Some(ctx) = quote_ctx {
        finalize_completion_in_quote(choice, ctx);
    } else {
        finalize_completion(choice);
    }
}

fn finalize_completion_in_quote(choice: &mut Pair, quote_ctx: &QuoteContext) {
    let is_dir = choice.replacement.ends_with('/');
    if is_dir {
        return;
    }
    if !quote_ctx.has_closing {
        choice.replacement.push(quote_ctx.quote);
        choice.replacement.push(' ');
    }
}
fn word_start<'a>(line: &'a str, pos: usize) -> (usize, &'a str) {
    let mut start = 0;
    for (idx, ch) in line[..pos].char_indices() {
        if ch.is_whitespace() {
            start = idx + ch.len_utf8();
        }
    }
    (start, &line[start..pos])
}

struct QuoteContext {
    start: usize,
    quote: char,
    has_closing: bool,
}

fn quote_context(line: &str, pos: usize) -> Option<QuoteContext> {
    let bytes = line.as_bytes();
    let mut i = 0usize;
    let mut in_quote = None;
    let mut quote_start = 0usize;

    while i < pos && i < bytes.len() {
        let ch = bytes[i];
        if let Some(q) = in_quote {
            if q == b'"' && ch == b'\\' && i + 1 < pos && bytes[i + 1] == b'"' {
                i += 2;
                continue;
            }
            if ch == q {
                in_quote = None;
            }
            i += 1;
            continue;
        }
        if ch == b'\'' || ch == b'"' {
            in_quote = Some(ch);
            quote_start = i;
        }
        i += 1;
    }

    let Some(q) = in_quote else {
        return None;
    };

    let mut j = pos;
    let mut has_closing = false;
    while j < bytes.len() {
        let ch = bytes[j];
        if q == b'"' && ch == b'\\' && j + 1 < bytes.len() && bytes[j + 1] == b'"' {
            j += 2;
            continue;
        }
        if ch == q {
            has_closing = true;
            break;
        }
        j += 1;
    }

    Some(QuoteContext {
        start: quote_start,
        quote: q as char,
        has_closing,
    })
}

fn split_dir_query(fragment: &str) -> (String, &str) {
    match fragment.rfind('/') {
        Some(idx) => {
            let (dir, q) = fragment.split_at(idx + 1);
            (dir.to_string(), q)
        }
        None => (String::new(), fragment),
    }
}

fn list_dir_candidates(dir_prefix: &str, include_hidden: bool) -> Vec<Pair> {
    let dir_path = resolve_dir(dir_prefix);
    let mut entries = Vec::new();

    let Ok(read_dir) = std::fs::read_dir(&dir_path) else {
        return entries;
    };

    for entry in read_dir.flatten() {
        if let Some(name) = entry.file_name().to_str() {
            if !include_hidden && name.starts_with('.') {
                continue;
            }
            let mut replacement = format!("{dir_prefix}{name}");
            if entry.path().is_dir() {
                replacement.push('/');
            }
            entries.push(Pair {
                display: name.to_string(),
                replacement,
            });
        }
    }

    entries.sort_by(|a, b| {
        let a_lower = a.display.to_ascii_lowercase();
        let b_lower = b.display.to_ascii_lowercase();
        a_lower.cmp(&b_lower).then_with(|| a.display.cmp(&b.display))
    });
    entries
}

#[cfg(test)]
mod tests {
    use super::list_dir_candidates;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir() -> PathBuf {
        let base = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        base.join(format!("unshell-repl-{nanos}"))
    }

    #[test]
    fn list_candidates_hides_dotfiles_by_default() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("alpha"), b"").unwrap();
        fs::write(dir.join(".hidden"), b"").unwrap();

        let dir_prefix = format!("{}/", dir.display());
        let candidates = list_dir_candidates(&dir_prefix, false);
        let names: Vec<String> = candidates.into_iter().map(|c| c.display).collect();

        assert_eq!(names, vec!["alpha"]);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn list_candidates_includes_dotfiles_when_requested() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("alpha"), b"").unwrap();
        fs::write(dir.join(".hidden"), b"").unwrap();

        let dir_prefix = format!("{}/", dir.display());
        let candidates = list_dir_candidates(&dir_prefix, true);
        let names: Vec<String> = candidates.into_iter().map(|c| c.display).collect();

        assert_eq!(names, vec![".hidden", "alpha"]);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn list_candidates_sorts_case_insensitive() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("beta"), b"").unwrap();
        fs::write(dir.join("Alpha"), b"").unwrap();
        fs::write(dir.join("alpha"), b"").unwrap();

        let dir_prefix = format!("{}/", dir.display());
        let candidates = list_dir_candidates(&dir_prefix, false);
        let names: Vec<String> = candidates.into_iter().map(|c| c.display).collect();

        assert_eq!(names, vec!["Alpha", "alpha", "beta"]);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn quote_context_reports_open_double_quote() {
        let line = "echo \"docs";
        let pos = line.len();
        let ctx = super::quote_context(line, pos).expect("expected quote context");

        assert_eq!(ctx.quote, '"');
        assert_eq!(ctx.start, 5);
        assert!(!ctx.has_closing);
    }

    #[test]
    fn quote_context_reports_closing_quote() {
        let line = "echo \"docs\"";
        let pos = line.len() - 1;
        let ctx = super::quote_context(line, pos).expect("expected quote context");

        assert_eq!(ctx.quote, '"');
        assert_eq!(ctx.start, 5);
        assert!(ctx.has_closing);
    }

    #[test]
    fn quote_context_handles_single_quotes() {
        let line = "echo 'foo bar'";
        let pos = line.len() - 1;
        let ctx = super::quote_context(line, pos).expect("expected quote context");

        assert_eq!(ctx.quote, '\'');
        assert_eq!(ctx.start, 5);
        assert!(ctx.has_closing);
    }
}

fn resolve_dir(dir_prefix: &str) -> std::path::PathBuf {
    if dir_prefix.starts_with("~/") {
        if let Ok(home) = env::var("HOME") {
            return PathBuf::from(home).join(dir_prefix.trim_start_matches("~/"));
        }
    }

    let path = if dir_prefix.is_empty() { "." } else { dir_prefix };
    std::path::Path::new(path).to_path_buf()
}

enum FuzzyOutcome {
    Selected(String),
    Cancelled,
    Unavailable,
}

fn run_fuzzy(choices: &[String], query: &str, start_last: bool) -> FuzzyOutcome {
    let mut cmd = Command::new("fzf");
    cmd.arg("--height")
        .arg("40%")
        .arg("--margin")
        .arg("0")
        .arg("--gutter")
        .arg(" ")
        .arg("--color")
        .arg("16")
        .arg("--cycle")
        .arg("--no-scrollbar")
        .arg("--color")
        .arg("bg+:-1")
        .arg("--no-info")
        .arg("--no-separator")
        .arg("--reverse")
        .arg("-1") // select only option if only 1 is given
        .arg("--prompt")
        .arg("> ")
        .arg("--exit-0")
        .arg("--print-query")
        .arg("--bind")
        .arg("tab:down,btab:up,alt-k:up,alt-j:down,alt-o:toggle-sort,ctrl-o:toggle-sort,ctrl-j:down,ctrl-k:up")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    if !query.is_empty() {
        cmd.arg("--query").arg(query);
    }
    if start_last {
        cmd.arg("--bind").arg("load:last");
    }

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(_) => return FuzzyOutcome::Unavailable,
    };
    let _ = set_cursor_visible(false);
    let _ = move_cursor_to_eol();
    let _cursor_guard = CursorGuard;

    {
        let stdin = match child.stdin.as_mut() {
            Some(stdin) => stdin,
            None => return FuzzyOutcome::Unavailable,
        };
        for choice in choices {
            let _ = writeln!(stdin, "{choice}");
        }
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(_) => return FuzzyOutcome::Unavailable,
    };

    if output.stdout.is_empty() {
        return FuzzyOutcome::Cancelled;
    }

    let output_text = String::from_utf8_lossy(&output.stdout);
    let mut lines = output_text.lines();
    let _query_line = lines.next();
    let selected = lines.next().unwrap_or("").trim().to_string();

    if selected.is_empty() {
        FuzzyOutcome::Cancelled
    } else {
        FuzzyOutcome::Selected(selected)
    }
}

struct CursorGuard;

impl Drop for CursorGuard {
    fn drop(&mut self) {
        let _ = set_cursor_visible(true);
    }
}

fn move_cursor_to_eol() -> std::io::Result<()> {
    let mut stdout = std::io::stdout();
    stdout.write_all(b"\x1b[999C")?;
    stdout.flush()
}

fn set_cursor_visible(visible: bool) -> std::io::Result<()> {
    let mut stdout = std::io::stdout();
    if visible {
        stdout.write_all(b"\x1b[?25h")?;
    } else {
        stdout.write_all(b"\x1b[?25l")?;
    }
    stdout.flush()
}

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: FuzzyCompleter,
}

struct CompletionEventHandler {
    start_last: bool,
    flag: Arc<AtomicBool>,
}

impl ConditionalEventHandler for CompletionEventHandler {
    fn handle(
        &self,
        _evt: &Event,
        _n: RepeatCount,
        _positive: bool,
        _ctx: &EventContext,
    ) -> Option<Cmd> {
        self.flag.store(self.start_last, Ordering::SeqCst);
        Some(Cmd::Complete)
    }
}

struct CommentAcceptHandler;

impl ConditionalEventHandler for CommentAcceptHandler {
    fn handle(
        &self,
        _evt: &Event,
        _n: RepeatCount,
        _positive: bool,
        ctx: &EventContext,
    ) -> Option<Cmd> {
        if ctx.mode() != EditMode::Vi || ctx.input_mode() != InputMode::Command {
            return None;
        }
        let line = ctx.line();
        let updated = if line.starts_with("# ") {
            line.to_string()
        } else {
            format!("# {line}")
        };
        Some(Cmd::AcceptLineWith(updated))
    }
}
impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut out = String::with_capacity(line.len() + 16);
        let mut in_quote = None;
        let mut word = String::new();
        let mut prev_was_space = true;

        for (idx, ch) in line.char_indices() {
            if let Some(q) = in_quote {
                out.push(ch);
                if ch == q {
                    out.push_str(COLOR_RESET);
                    in_quote = None;
                    prev_was_space = false;
                }
                continue;
            }

            if ch == '#' && prev_was_space {
                flush_word(&mut out, &mut word);
                out.push_str(COLOR_COMMENT);
                out.push_str(&line[idx..]);
                out.push_str(COLOR_RESET);
                return Owned(out);
            }

            if ch == '\'' || ch == '"' {
                flush_word(&mut out, &mut word);
                out.push_str(COLOR_STRING);
                out.push(ch);
                in_quote = Some(ch);
                prev_was_space = false;
                continue;
            }

            if ch.is_whitespace() {
                flush_word(&mut out, &mut word);
                out.push(ch);
                prev_was_space = true;
                continue;
            }

            word.push(ch);
            prev_was_space = false;
        }

        flush_word(&mut out, &mut word);
        if in_quote.is_some() {
            out.push_str(COLOR_RESET);
        }

        if out == line {
            Borrowed(line)
        } else {
            Owned(out)
        }
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Owned(format!("{COLOR_PROMPT}{prompt}{COLOR_RESET}"))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _kind: rustyline::highlight::CmdKind) -> bool {
        true
    }
}

fn flush_word(out: &mut String, word: &mut String) {
    if word.is_empty() {
        return;
    }
    if KEYWORDS.iter().any(|kw| *kw == word.as_str()) {
        out.push_str(COLOR_KEYWORD);
        out.push_str(word);
        out.push_str(COLOR_RESET);
    } else {
        out.push_str(word);
    }
    word.clear();
}

fn resolve_history_path(state: &ShellState) -> Option<PathBuf> {
    if let Some(value) = state.repl.history_file.as_ref() {
        if !value.trim().is_empty() {
            return Some(PathBuf::from(value));
        }
        return None;
    }
    if let Ok(value) = env::var("USH_HISTFILE") {
        if !value.trim().is_empty() {
            return Some(PathBuf::from(value));
        }
    }
    if let Ok(value) = env::var("HISTFILE") {
        if !value.trim().is_empty() {
            return Some(PathBuf::from(value));
        }
    }
    if let Ok(value) = env::var("XDG_DATA_HOME") {
        if !value.trim().is_empty() {
            return Some(PathBuf::from(value).join("unshell").join("histfile"));
        }
    }
    let home = env::var("HOME").ok()?;
    Some(PathBuf::from(home).join(".local").join("share").join("histfile"))
}

fn build_editor(state: &ShellState) -> Result<Editor<ReplHelper, DefaultHistory>> {
    let config_builder = Config::builder()
        .edit_mode(if state.repl.vi_mode { EditMode::Vi } else { EditMode::Emacs })
        .completion_type(CompletionType::List);
    let config = config_builder.build();

    let start_last = Arc::new(AtomicBool::new(false));
    let helper = ReplHelper {
        completer: FuzzyCompleter {
            start_last: start_last.clone(),
            mode: state.repl.completion_mode,
        },
    };

    let mut rl = Editor::with_config(config)?;
    rl.set_auto_add_history(false);
    let _ = rl.enable_bracketed_paste(state.repl.bracketed_paste);
    rl.set_helper(Some(helper));
    apply_bindings(&mut rl, &state.repl.bindings, start_last);
    Ok(rl)
}

fn apply_bindings(
    rl: &mut Editor<ReplHelper, DefaultHistory>,
    bindings: &[ReplBinding],
    start_last: Arc<AtomicBool>,
) {
    let mut saw_btab = false;
    let mut saw_comment_hash = false;
    for binding in bindings {
        if binding.key.trim() == "#" {
            saw_comment_hash = true;
        }
        if binding.action.trim() == "comment-accept" {
            let Some(key_event) = parse_key(&binding.key) else {
                eprintln!("unshell: repl.bind ignored invalid key '{}'", binding.key);
                continue;
            };
            rl.bind_sequence(key_event, EventHandler::Conditional(Box::new(CommentAcceptHandler)));
            continue;
        }
        let Some(key_event) = parse_key(&binding.key) else {
            eprintln!("unshell: repl.bind ignored invalid key '{}'", binding.key);
            continue;
        };
        if key_event.0 == KeyCode::BackTab {
            saw_btab = true;
        }
        if binding.action.trim() == "complete" && key_event.0 == KeyCode::BackTab {
            let handler = CompletionEventHandler {
                start_last: true,
                flag: start_last.clone(),
            };
            rl.bind_sequence(key_event, EventHandler::Conditional(Box::new(handler)));
            continue;
        }
        let Some(cmd) = parse_action(&binding.action) else {
            eprintln!("unshell: repl.bind ignored invalid action '{}'", binding.action);
            continue;
        };
        rl.bind_sequence(key_event, EventHandler::Simple(cmd));
    }
    if !saw_btab {
        let handler = CompletionEventHandler {
            start_last: true,
            flag: start_last,
        };
        rl.bind_sequence(
            KeyEvent(KeyCode::BackTab, Modifiers::NONE),
            EventHandler::Conditional(Box::new(handler)),
        );
    }
    if !saw_comment_hash {
        rl.bind_sequence(
            KeyEvent(KeyCode::Char('#'), Modifiers::NONE),
            EventHandler::Conditional(Box::new(CommentAcceptHandler)),
        );
    }
}

fn parse_key(key: &str) -> Option<KeyEvent> {
    let key = key.trim();
    if key.is_empty() {
        return None;
    }

    if let Some(rest) = key.strip_prefix("ctrl-") {
        let mut chars = rest.chars();
        let ch = chars.next()?;
        if chars.next().is_some() {
            return None;
        }
        return Some(KeyEvent::ctrl(ch));
    }
    if let Some(rest) = key.strip_prefix("alt-") {
        let mut chars = rest.chars();
        let ch = chars.next()?;
        if chars.next().is_some() {
            return None;
        }
        return Some(KeyEvent::alt(ch));
    }

    match key {
        "tab" => Some(KeyEvent(KeyCode::Tab, Modifiers::NONE)),
        "btab" => Some(KeyEvent(KeyCode::BackTab, Modifiers::NONE)),
        "enter" => Some(KeyEvent(KeyCode::Enter, Modifiers::NONE)),
        "esc" => Some(KeyEvent(KeyCode::Esc, Modifiers::NONE)),
        "backspace" => Some(KeyEvent(KeyCode::Backspace, Modifiers::NONE)),
        "left" => Some(KeyEvent(KeyCode::Left, Modifiers::NONE)),
        "right" => Some(KeyEvent(KeyCode::Right, Modifiers::NONE)),
        "up" => Some(KeyEvent(KeyCode::Up, Modifiers::NONE)),
        "down" => Some(KeyEvent(KeyCode::Down, Modifiers::NONE)),
        _ => {
            let mut chars = key.chars();
            let ch = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            Some(KeyEvent(KeyCode::Char(ch), Modifiers::NONE))
        }
    }
}

fn parse_action(action: &str) -> Option<Cmd> {
    let action = action.trim();
    if action.is_empty() {
        return None;
    }

    if let Some(text) = action.strip_prefix("insert:") {
        return Some(Cmd::Insert(1, text.to_string()));
    }
    if let Some(text) = action.strip_prefix("insert ") {
        return Some(Cmd::Insert(1, text.to_string()));
    }

    match action {
        "backward-word" => Some(Cmd::Move(Movement::BackwardWord(1, Word::Emacs))),
        "forward-word" => Some(Cmd::Move(Movement::ForwardWord(1, At::Start, Word::Emacs))),
        "beginning-of-line" => Some(Cmd::Move(Movement::BeginningOfLine)),
        "end-of-line" => Some(Cmd::Move(Movement::EndOfLine)),
        "kill-line" => Some(Cmd::Kill(Movement::EndOfLine)),
        "accept-line" => Some(Cmd::AcceptLine),
        "history-search-backward" => Some(Cmd::HistorySearchBackward),
        "history-search-forward" => Some(Cmd::HistorySearchForward),
        "complete" => Some(Cmd::Complete),
        _ => None,
    }
}

pub fn run_repl(state: &mut ShellState) {
    let mut history_path = resolve_history_path(state);
    let mut last_generation = state.repl.generation;

    let mut rl = match build_editor(state) {
        Ok(rl) => rl,
        Err(err) => {
            eprintln!("unshell: failed to init repl: {err}");
            return;
        }
    };

    if let Some(path) = history_path.as_ref() {
        let _ = rl.load_history(path);
    }

    loop {
        let next_history_path = resolve_history_path(state);
        if state.needs_cursor_check {
            if let Some(column) = cursor_column() {
                if column != 1 {
                    let _ = print_incomplete_marker();
                }
            }
            state.needs_cursor_check = false;
        } else if let Some(column) = cursor_column() {
            if column != 1 {
                let _ = print_incomplete_marker();
            }
        } else if !state.last_output_newline {
            let _ = print_incomplete_marker();
            state.last_output_newline = true;
        }

        if state.repl.generation != last_generation || next_history_path != history_path {
            last_generation = state.repl.generation;
            match build_editor(state) {
                Ok(mut next) => {
                    if let Some(path) = next_history_path.as_ref() {
                        let _ = next.load_history(path);
                    }
                    rl = next;
                    history_path = next_history_path;
                }
                Err(err) => {
                    eprintln!("unshell: failed to reload repl: {err}");
                }
            }
        }

        let prompt = build_prompt(state);
        let prompt = if prompt.is_empty() {
            DEFAULT_PROMPT.to_string()
        } else {
            prompt
        };
        match rl.readline(&prompt) {
            Ok(line) => {
                if !line.trim().is_empty() {
                    let _ = rl.add_history_entry(line.as_str());
                    if let Some(path) = history_path.as_ref() {
                        let _ = rl.append_history(path);
                    }
                    if !run_after_command_hook(&line, state) {
                        break;
                    }
                }
                if !process_line(&line, state) {
                    break;
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("unshell: repl error: {err}");
                break;
            }
        }
    }
}

fn run_after_command_hook(line: &str, state: &mut ShellState) -> bool {
    const HOOK: &str = "unshell_after_command_input";
    let saved_status = state.last_status;
    let args = vec![line.to_string()];
    let result = run_named_function(HOOK, &args, state);
    state.last_status = saved_status;
    let result = match result {
        Ok(result) => result,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };
    match result {
        Some(RunResult::Exit) => false,
        Some(_) | None => true,
    }
}

fn print_incomplete_marker() -> std::io::Result<()> {
    let mut stdout = std::io::stdout();
    stdout.write_all(b"\x1b[7m$\x1b[0m\n")?;
    stdout.flush()
}

use std::borrow::Cow::{self, Borrowed, Owned};
use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use rustyline::completion::{Completer as CompletionTrait, Pair};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::history::DefaultHistory;
use rustyline::line_buffer::LineBuffer;
use rustyline::{
    At, Cmd, CompletionType, Completer, Config, Context, EditMode, Editor, EventHandler, Helper,
    Hinter, KeyCode, KeyEvent, Modifiers, Movement, Result, Validator, Word,
};

use crate::state::{ReplBinding, ShellState};
use crate::process_line;

const PROMPT: &str = "unshell> ";
const COLOR_RESET: &str = "\x1b[0m";
const COLOR_STRING: &str = "\x1b[1;35m";
const COLOR_KEYWORD: &str = "\x1b[1;31m";
const COLOR_PROMPT: &str = "\x1b[1;32m";
const KEYWORDS: &[&str] = &[
    "alias", "cd", "elif", "else", "eval", "exit", "export", "for", "foreach", "if",
    "set", "unalias",
];

#[derive(Default)]
struct FuzzyCompleter {
    command: Vec<String>,
}

impl CompletionTrait for FuzzyCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>)> {
        let (start, fragment) = word_start(line, pos);
        let (dir_prefix, query) = split_dir_query(fragment);
        let candidates = list_dir_candidates(&dir_prefix, ctx);
        if candidates.is_empty() {
            return Ok((start, candidates));
        }

        if self.command.is_empty() {
            return Ok((start, candidates));
        }

        let choices: Vec<String> = candidates.iter().map(|c| c.display.clone()).collect();
        match run_fuzzy(&self.command, &choices, query) {
            FuzzyOutcome::Selected(sel) => {
                let selected = candidates
                    .iter()
                    .find(|c| c.display == sel)
                    .cloned();
                if let Some(mut choice) = selected {
                    choice.replacement.push(' ');
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

fn word_start<'a>(line: &'a str, pos: usize) -> (usize, &'a str) {
    let mut start = 0;
    for (idx, ch) in line[..pos].char_indices() {
        if ch.is_whitespace() {
            start = idx + ch.len_utf8();
        }
    }
    (start, &line[start..pos])
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

fn list_dir_candidates(dir_prefix: &str, _ctx: &Context<'_>) -> Vec<Pair> {
    let dir_path = resolve_dir(dir_prefix);
    let mut entries = Vec::new();

    let Ok(read_dir) = std::fs::read_dir(&dir_path) else {
        return entries;
    };

    for entry in read_dir.flatten() {
        if let Some(name) = entry.file_name().to_str() {
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

    entries
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

fn run_fuzzy(command: &[String], choices: &[String], query: &str) -> FuzzyOutcome {
    let _ = set_cursor_visible(false);
    let _ = move_cursor_to_eol();

    let mut cmd = Command::new(&command[0]);
    if command.len() > 1 {
        cmd.args(&command[1..]);
    }
    cmd.arg("--height")
        .arg("40%")
        .arg("--reverse")
        .arg("--prompt")
        .arg("> ")
        .arg("--exit-0")
        .arg("--print-query")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    if !query.is_empty() {
        cmd.arg("--query").arg(query);
    }

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(_) => return FuzzyOutcome::Unavailable,
    };

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
    let _ = set_cursor_visible(true);

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

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut out = String::with_capacity(line.len() + 16);
        let mut in_quote = None;
        let mut word = String::new();

        for ch in line.chars() {
            if let Some(q) = in_quote {
                out.push(ch);
                if ch == q {
                    out.push_str(COLOR_RESET);
                    in_quote = None;
                }
                continue;
            }

            if ch == '\'' || ch == '"' {
                flush_word(&mut out, &mut word);
                out.push_str(COLOR_STRING);
                out.push(ch);
                in_quote = Some(ch);
                continue;
            }

            if ch.is_whitespace() {
                flush_word(&mut out, &mut word);
                out.push(ch);
                continue;
            }

            word.push(ch);
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

fn history_path() -> Option<PathBuf> {
    if let Ok(value) = env::var("USH_HISTORY") {
        if !value.trim().is_empty() {
            return Some(PathBuf::from(value));
        }
    }
    let home = env::var("HOME").ok()?;
    Some(PathBuf::from(home).join(".ush_history"))
}

fn build_editor(state: &ShellState) -> Result<Editor<ReplHelper, DefaultHistory>> {
    let config = Config::builder()
        .edit_mode(if state.repl.vi_mode { EditMode::Vi } else { EditMode::Emacs })
        .completion_type(CompletionType::List)
        .build();

    let helper = ReplHelper {
        completer: FuzzyCompleter {
            command: state.repl.completion_command.clone(),
        },
    };

    let mut rl = Editor::with_config(config)?;
    rl.set_auto_add_history(false);
    rl.set_helper(Some(helper));
    apply_bindings(&mut rl, &state.repl.bindings);
    Ok(rl)
}

fn apply_bindings(rl: &mut Editor<ReplHelper, DefaultHistory>, bindings: &[ReplBinding]) {
    for binding in bindings {
        let Some(key_event) = parse_key(&binding.key) else {
            eprintln!("unshell: repl.bind ignored invalid key '{}'", binding.key);
            continue;
        };
        let Some(cmd) = parse_action(&binding.action) else {
            eprintln!("unshell: repl.bind ignored invalid action '{}'", binding.action);
            continue;
        };
        rl.bind_sequence(key_event, EventHandler::Simple(cmd));
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
        "enter" => Some(KeyEvent(KeyCode::Enter, Modifiers::NONE)),
        "esc" => Some(KeyEvent(KeyCode::Esc, Modifiers::NONE)),
        "backspace" => Some(KeyEvent(KeyCode::Backspace, Modifiers::NONE)),
        "left" => Some(KeyEvent(KeyCode::Left, Modifiers::NONE)),
        "right" => Some(KeyEvent(KeyCode::Right, Modifiers::NONE)),
        "up" => Some(KeyEvent(KeyCode::Up, Modifiers::NONE)),
        "down" => Some(KeyEvent(KeyCode::Down, Modifiers::NONE)),
        _ => None,
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
    let history_path = history_path();
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
        if state.repl.generation != last_generation {
            last_generation = state.repl.generation;
            match build_editor(state) {
                Ok(mut next) => {
                    if let Some(path) = history_path.as_ref() {
                        let _ = next.load_history(path);
                    }
                    rl = next;
                }
                Err(err) => {
                    eprintln!("unshell: failed to reload repl: {err}");
                }
            }
        }

        match rl.readline(PROMPT) {
            Ok(line) => {
                if !line.trim().is_empty() {
                    let _ = rl.add_history_entry(line.as_str());
                    if let Some(path) = history_path.as_ref() {
                        let _ = rl.save_history(path);
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

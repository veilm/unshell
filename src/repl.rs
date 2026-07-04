use std::borrow::Cow::{self, Borrowed, Owned};
use std::cmp::Ordering as CmpOrdering;
use std::collections::HashSet;
use std::env;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};

use rustyline::completion::{Completer as CompletionTrait, Pair};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::history::DefaultHistory;
use rustyline::line_buffer::LineBuffer;
use rustyline::{
    At, Cmd, Completer, CompletionType, ConditionalEventHandler, Config, Context, EditMode, Editor,
    Event, EventContext, EventHandler, Helper, Hinter, InputMode, KeyCode, KeyEvent, Modifiers,
    Movement, RepeatCount, Result, Validator, Word,
};

use crate::expand::parse_json_string_array;
use crate::parser::parse_args;
use crate::state::{CompletionRule, ReplBinding, ReplCompletionMode, ShellState};
use crate::term::{cursor_column, print_prompt_spacer};
use crate::{
    DEFAULT_PROMPT, RunResult, build_prompt, maybe_auto_refresh_repl, process_line,
    run_named_function,
};

const COLOR_RESET: &str = "\x1b[0m";
const COLOR_COMMENT: &str = "\x1b[0;90m";
const COLOR_STRING: &str = "\x1b[1;35m";
const COLOR_KEYWORD: &str = "\x1b[1;31m";
const COLOR_PROMPT: &str = "\x1b[1;32m";
const COLOR_DIR: &str = "\x1b[32m";
const KEYWORDS: &[&str] = &[
    "alias", "break", "cd", "complete", "continue", "each", "elif", "else", "eval", "exit",
    "export", "for", "foreach", "if", "set", "unalias", "while",
];

struct FuzzyCompleter {
    start_last: Arc<AtomicBool>,
    mode: ReplCompletionMode,
    snapshot: Arc<Mutex<CompletionSnapshot>>,
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
        if fragment.starts_with('$')
            && !matches!(quote_ctx.as_ref(), Some(ctx) if ctx.quote == '\'')
        {
            let vars = {
                let guard = self.snapshot.lock().unwrap();
                guard.vars.clone()
            };
            let candidates = list_var_candidates(&vars, fragment);
            return complete_candidates(
                candidates,
                fragment,
                start,
                fragment,
                quote_ctx.as_ref(),
                self.mode,
                &self.start_last,
            );
        }

        if quote_ctx.is_none()
            && is_command_position(line, start)
            && !fragment.contains('/')
            && !fragment.starts_with('.')
        {
            let commands = {
                let mut guard = self.snapshot.lock().unwrap();
                ensure_command_snapshot(&mut guard);
                guard.commands.clone()
            };
            let candidates = list_command_candidates(&commands, fragment);
            return complete_candidates(
                candidates,
                fragment,
                start,
                fragment,
                quote_ctx.as_ref(),
                self.mode,
                &self.start_last,
            );
        }

        let rules = {
            let guard = self.snapshot.lock().unwrap();
            guard.rules.clone()
        };
        if let Some(candidates) = list_custom_completion_candidates(
            &rules,
            line,
            pos,
            start,
            fragment,
            quote_ctx.as_ref(),
        ) {
            return complete_candidates(
                candidates,
                fragment,
                start,
                fragment,
                quote_ctx.as_ref(),
                self.mode,
                &self.start_last,
            );
        }

        let (expansion_chars, expansion_handler) = {
            let guard = self.snapshot.lock().unwrap();
            (
                guard.expansion_chars.clone(),
                guard.expansion_handler.clone(),
            )
        };
        let file_fragment = filesystem_completion_fragment(
            fragment,
            quote_ctx.as_ref(),
            &expansion_chars,
            &expansion_handler,
        );
        let (dir_prefix, query) = split_dir_query(&file_fragment);
        let include_hidden = file_fragment.starts_with('.') || file_fragment.contains("/.");
        let candidates = list_dir_candidates(&dir_prefix, include_hidden);
        complete_candidates(
            candidates,
            query,
            start,
            fragment,
            quote_ctx.as_ref(),
            self.mode,
            &self.start_last,
        )
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
    if !value
        .chars()
        .any(|ch| ch.is_whitespace() || ch == '[' || ch == ']')
    {
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

fn filesystem_completion_fragment(
    fragment: &str,
    quote_ctx: Option<&QuoteContext>,
    expansion_chars: &HashSet<char>,
    expansion_handler: &[String],
) -> String {
    if quote_ctx.is_some()
        || expansion_chars.is_empty()
        || expansion_handler.is_empty()
        || !fragment.chars().any(|ch| expansion_chars.contains(&ch))
    {
        return fragment.to_string();
    }

    match run_completion_expansion_handler(fragment, expansion_handler) {
        Ok(items) if items.len() == 1 => items[0].clone(),
        Ok(_) => fragment.to_string(),
        Err(err) => {
            eprintln!("unshell: completion expansion failed: {err}");
            fragment.to_string()
        }
    }
}

fn run_completion_expansion_handler(
    fragment: &str,
    expansion_handler: &[String],
) -> std::result::Result<Vec<String>, String> {
    let mut cmd = Command::new(&expansion_handler[0]);
    if expansion_handler.len() > 1 {
        cmd.args(&expansion_handler[1..]);
    }
    let output = cmd
        .arg(fragment)
        .output()
        .map_err(|err| format!("failed to execute expansion handler: {err}"))?;
    if !output.stderr.is_empty() {
        let _ = std::io::stderr().write_all(&output.stderr);
    }
    if !output.status.success() {
        if let Some(code) = output.status.code() {
            return Err(format!("expansion handler failed with status {code}"));
        }
        return Err("expansion handler failed".into());
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|_| "expansion handler output not utf-8".to_string())?;
    parse_json_string_array(&text).map_err(|err| {
        let trimmed = text.trim_end_matches(&['\n', '\r'][..]);
        format!("expansion handler output invalid JSON: {err}: {trimmed}")
    })
}

fn list_custom_completion_candidates(
    rules: &[CompletionRule],
    line: &str,
    pos: usize,
    start: usize,
    fragment: &str,
    quote_ctx: Option<&QuoteContext>,
) -> Option<Vec<Pair>> {
    if is_command_position(line, start) {
        return None;
    }

    let tokens_before = match completion_prefix_tokens(line, start, quote_ctx) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("unshell: completion parse failed: {err}");
            return Some(Vec::new());
        }
    };
    let Some(rule) = rules.iter().find(|rule| rule.match_args == tokens_before) else {
        return None;
    };

    match run_completion_handler(rule, line, pos, fragment, tokens_before.len()) {
        Ok(items) => Some(
            items
                .into_iter()
                .map(|item| Pair {
                    display: item.clone(),
                    replacement: item,
                })
                .collect(),
        ),
        Err(err) => {
            eprintln!("unshell: {err}");
            Some(Vec::new())
        }
    }
}

fn completion_prefix_tokens(
    line: &str,
    start: usize,
    quote_ctx: Option<&QuoteContext>,
) -> std::result::Result<Vec<String>, String> {
    let segment_start = current_segment_start(line, start);
    let prefix_end = quote_ctx.map(|ctx| ctx.start).unwrap_or(start);
    let prefix = line[segment_start.min(prefix_end)..prefix_end].trim();
    if prefix.is_empty() {
        return Ok(Vec::new());
    }
    let tokens = parse_args(prefix)?;
    Ok(tokens
        .into_iter()
        .map(|token| unquote_completion_token(&token))
        .collect())
}

fn current_segment_start(line: &str, end: usize) -> usize {
    let bytes = line.as_bytes();
    let mut idx = 0usize;
    let mut in_double = false;
    let mut in_single = false;
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut brace_depth = 0;
    let mut last_op_end = 0usize;

    while idx < end && idx < bytes.len() {
        let ch = bytes[idx];
        if in_double && ch == b'\\' && idx + 1 < end {
            idx += 2;
            continue;
        }

        match ch {
            b'\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
                idx += 1;
                continue;
            }
            b'"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
                idx += 1;
                continue;
            }
            b'$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if idx + 1 < end && bytes[idx + 1] == b'(' {
                    paren_depth = 1;
                    idx += 2;
                    continue;
                }
            }
            b'[' if !in_double && !in_single && paren_depth == 0 => {
                if idx + 1 < end && bytes[idx + 1].is_ascii_whitespace() {
                    // literal [
                } else {
                    bracket_depth += 1;
                }
                idx += 1;
                continue;
            }
            b'[' if bracket_depth > 0 => {
                bracket_depth += 1;
                idx += 1;
                continue;
            }
            b']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                idx += 1;
                continue;
            }
            b'(' if paren_depth > 0 => {
                paren_depth += 1;
                idx += 1;
                continue;
            }
            b')' if paren_depth > 0 => {
                paren_depth -= 1;
                idx += 1;
                continue;
            }
            b'{' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                brace_depth += 1;
                idx += 1;
                continue;
            }
            b'}' if brace_depth > 0 => {
                brace_depth -= 1;
                idx += 1;
                continue;
            }
            _ => {}
        }

        if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 && brace_depth == 0 {
            if ch == b'|' {
                if idx + 1 < end && bytes[idx + 1] == b'|' {
                    idx += 2;
                } else {
                    idx += 1;
                }
                last_op_end = idx;
                continue;
            }
            if ch == b'&' && idx + 1 < end && bytes[idx + 1] == b'&' {
                idx += 2;
                last_op_end = idx;
                continue;
            }
            if ch == b';' {
                idx += 1;
                last_op_end = idx;
                continue;
            }
        }

        idx += 1;
    }

    last_op_end
}

fn unquote_completion_token(token: &str) -> String {
    if token.len() >= 2 && token.starts_with('\'') && token.ends_with('\'') {
        return token[1..token.len() - 1].to_string();
    }
    if token.len() >= 2 && token.starts_with('"') && token.ends_with('"') {
        return unescape_double_quoted_completion_token(&token[1..token.len() - 1]);
    }
    token.to_string()
}

fn unescape_double_quoted_completion_token(token: &str) -> String {
    let mut out = String::with_capacity(token.len());
    let mut chars = token.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                if next == '"' {
                    out.push('"');
                } else {
                    out.push('\\');
                    out.push(next);
                }
            } else {
                out.push('\\');
            }
        } else {
            out.push(ch);
        }
    }
    out
}

fn run_completion_handler(
    rule: &CompletionRule,
    line: &str,
    pos: usize,
    fragment: &str,
    cword: usize,
) -> std::result::Result<Vec<String>, String> {
    let mut cmd = Command::new(&rule.program);
    cmd.args(&rule.match_args);
    cmd.env("USH_COMP_LINE", line);
    cmd.env("USH_COMP_POINT", pos.to_string());
    cmd.env("USH_COMP_WORD", fragment);
    cmd.env("USH_COMP_CWORD", cword.to_string());
    let output = cmd.output().map_err(|err| {
        format!(
            "failed to execute completion handler '{}': {err}",
            rule.program
        )
    })?;
    if !output.stderr.is_empty() {
        let _ = std::io::stderr().write_all(&output.stderr);
    }
    if !output.status.success() {
        if let Some(code) = output.status.code() {
            return Err(format!(
                "completion handler '{}' failed with status {code}",
                rule.program
            ));
        }
        return Err(format!("completion handler '{}' failed", rule.program));
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|_| format!("completion handler '{}' output not utf-8", rule.program))?;
    parse_json_string_array(&text).map_err(|err| {
        let trimmed = text.trim_end_matches(&['\n', '\r'][..]);
        format!(
            "completion handler '{}' output invalid JSON: {err}: {trimmed}",
            rule.program
        )
    })
}

#[derive(Default)]
struct CompletionSnapshot {
    vars: Vec<String>,
    base_commands: Vec<String>,
    commands: Vec<String>,
    commands_ready: bool,
    rules: Vec<CompletionRule>,
    expansion_chars: HashSet<char>,
    expansion_handler: Vec<String>,
    path: String,
}

fn complete_candidates(
    candidates: Vec<Pair>,
    query: &str,
    start: usize,
    fragment: &str,
    quote_ctx: Option<&QuoteContext>,
    mode: ReplCompletionMode,
    start_last: &AtomicBool,
) -> Result<(usize, Vec<Pair>)> {
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
            finalize_completion_with_context(&mut prefix_matches[0], quote_ctx);
            return Ok((start, prefix_matches));
        }
    }

    if matches!(mode, ReplCompletionMode::List) {
        return Ok((start, candidates));
    }

    let choices: Vec<String> = candidates.iter().map(fzf_display_for_candidate).collect();
    let start_last = start_last.swap(false, Ordering::SeqCst);
    match run_fuzzy(&choices, query, start_last) {
        FuzzyOutcome::Selected(selection) => {
            let selected = selections_to_pairs(&candidates, &selection.selections);
            if selected.is_empty() {
                return Ok((start, candidates));
            }
            let key = selection.key.as_deref();
            if matches!(key, Some("ctrl-a") | Some("alt-a")) && selected.len() >= 2 {
                let prefix = common_prefix(&selected);
                let mut replacement = quote_completion(&format!("{prefix}*"));
                replacement.push(' ');
                return Ok((
                    start,
                    vec![Pair {
                        display: replacement.clone(),
                        replacement,
                    }],
                ));
            }
            if matches!(key, Some("ctrl-s") | Some("alt-s")) && selected.len() >= 2 {
                let suffix = common_suffix(&selected);
                let mut replacement = quote_completion(&format!("*{suffix}"));
                replacement.push(' ');
                return Ok((
                    start,
                    vec![Pair {
                        display: replacement.clone(),
                        replacement,
                    }],
                ));
            }
            if selected.len() == 1 {
                let mut choice = selected[0].clone();
                finalize_completion_with_context(&mut choice, quote_ctx);
                Ok((start, vec![choice]))
            } else {
                let replacement = build_multi_replacement(&selected, quote_ctx);
                Ok((
                    start,
                    vec![Pair {
                        display: replacement.clone(),
                        replacement,
                    }],
                ))
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

fn selections_to_pairs(candidates: &[Pair], selections: &[String]) -> Vec<Pair> {
    let mut out = Vec::with_capacity(selections.len());
    for selection in selections {
        if let Some(choice) = candidates.iter().find(|c| c.display == *selection) {
            out.push(choice.clone());
        } else {
            out.push(Pair {
                display: selection.clone(),
                replacement: selection.clone(),
            });
        }
    }
    out
}

fn build_multi_replacement(selections: &[Pair], quote_ctx: Option<&QuoteContext>) -> String {
    if selections.is_empty() {
        return String::new();
    }
    let last_is_dir = selections
        .last()
        .map(|item| item.replacement.ends_with('/'))
        .unwrap_or(false);

    if let Some(ctx) = quote_ctx {
        if !ctx.has_closing {
            if selections.len() == 1 {
                let mut out = selections[0].replacement.clone();
                if !last_is_dir {
                    out.push(ctx.quote);
                    out.push(' ');
                }
                return out;
            }
            let mut out = selections[0].replacement.clone();
            out.push(ctx.quote);
            out.push(' ');
            let mut first = true;
            for item in &selections[1..] {
                if !first {
                    out.push(' ');
                }
                out.push_str(&quote_completion(&item.replacement));
                first = false;
            }
            if !last_is_dir {
                out.push(' ');
            }
            return out;
        }
    }

    let mut out = String::new();
    for (idx, item) in selections.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        out.push_str(&quote_completion(&item.replacement));
    }
    if !last_is_dir {
        out.push(' ');
    }
    out
}

fn common_prefix(selections: &[Pair]) -> String {
    let mut iter = selections.iter();
    let Some(first) = iter.next() else {
        return String::new();
    };
    let mut prefix = first.replacement.clone();
    for item in iter {
        let next = item.replacement.as_str();
        let mut matched = 0usize;
        for (a, b) in prefix.chars().zip(next.chars()) {
            if a == b {
                matched += a.len_utf8();
            } else {
                break;
            }
        }
        prefix.truncate(matched);
        if prefix.is_empty() {
            break;
        }
    }
    prefix
}

fn common_suffix(selections: &[Pair]) -> String {
    let mut iter = selections.iter();
    let Some(first) = iter.next() else {
        return String::new();
    };
    let mut suffix = first.replacement.clone();
    for item in iter {
        let next = item.replacement.as_str();
        let mut matched = 0usize;
        for (a, b) in suffix.chars().rev().zip(next.chars().rev()) {
            if a == b {
                matched += a.len_utf8();
            } else {
                break;
            }
        }
        let keep = suffix.len().saturating_sub(matched);
        suffix.replace_range(..keep, "");
        if suffix.is_empty() {
            break;
        }
    }
    suffix
}

fn fzf_display_for_candidate(candidate: &Pair) -> String {
    if candidate.replacement.ends_with('/') {
        format!("{COLOR_DIR}{}{COLOR_RESET}", candidate.display)
    } else {
        candidate.display.clone()
    }
}

fn list_var_candidates(vars: &[String], fragment: &str) -> Vec<Pair> {
    let mut out = Vec::new();
    for name in vars {
        let display = format!("${name}");
        if fragment == "$" || display.starts_with(fragment) {
            out.push(Pair {
                display: display.clone(),
                replacement: display,
            });
        }
    }
    out
}

fn list_command_candidates(commands: &[String], fragment: &str) -> Vec<Pair> {
    let mut out = Vec::new();
    for name in commands {
        if fragment.is_empty() || name.starts_with(fragment) {
            out.push(Pair {
                display: name.clone(),
                replacement: name.clone(),
            });
        }
    }
    out
}

fn is_command_position(line: &str, start: usize) -> bool {
    let segment_start = current_segment_start(line, start);
    let segment = line[segment_start.min(start)..start].trim();
    let Ok(tokens) = parse_args(segment) else {
        return segment.is_empty();
    };
    if tokens.is_empty() {
        return true;
    }
    tokens.iter().all(|token| is_assignment_token(token))
}

fn is_assignment_token(token: &str) -> bool {
    let (name, _) = match token.split_once('=') {
        Some((name, value)) if !name.is_empty() => (name, value),
        _ => return false,
    };
    is_valid_var_name(name)
}

fn is_valid_var_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    for ch in chars {
        if !(ch == '_' || ch.is_ascii_alphanumeric()) {
            return false;
        }
    }
    true
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

    entries.sort_by(|a, b| natural_name_cmp(&a.display, &b.display));
    entries
}

// Approximate eza's natural name sorting for filesystem completion.
//
// This fixes common cases like:
// - transfer-4a-20251026
// - transfer-2024-06-10
//
// But it is not identical to eza's natord-based ordering. One observed edge
// case is /home/oboro/media/tmux_rec2/legacy/v2-0-archive, where eza sorts
// names like "%5BP%5D%20dev1" before "0000000000-README.txt", while this
// comparator puts the README first. If exact parity with eza is required, use
// the same natord crate or another shared ordering implementation instead of
// maintaining a hand-rolled comparator here.
fn natural_name_cmp(a: &str, b: &str) -> CmpOrdering {
    let mut a_chunks = NaturalChunks::new(a);
    let mut b_chunks = NaturalChunks::new(b);

    loop {
        match (a_chunks.next(), b_chunks.next()) {
            (None, None) => return a.cmp(b),
            (None, Some(_)) => return CmpOrdering::Less,
            (Some(_), None) => return CmpOrdering::Greater,
            (Some(NaturalChunk::Digits(a_digits)), Some(NaturalChunk::Digits(b_digits))) => {
                let ordering = compare_digit_chunks(a_digits, b_digits);
                if ordering != CmpOrdering::Equal {
                    return ordering;
                }
            }
            (Some(NaturalChunk::Text(a_text)), Some(NaturalChunk::Text(b_text))) => {
                let ordering = a_text
                    .to_ascii_lowercase()
                    .cmp(&b_text.to_ascii_lowercase())
                    .then_with(|| a_text.cmp(b_text));
                if ordering != CmpOrdering::Equal {
                    return ordering;
                }
            }
            (Some(NaturalChunk::Digits(_)), Some(NaturalChunk::Text(_))) => {
                return CmpOrdering::Less;
            }
            (Some(NaturalChunk::Text(_)), Some(NaturalChunk::Digits(_))) => {
                return CmpOrdering::Greater;
            }
        }
    }
}

fn compare_digit_chunks(a: &str, b: &str) -> CmpOrdering {
    let a_trimmed = a.trim_start_matches('0');
    let b_trimmed = b.trim_start_matches('0');
    let a_normalized = if a_trimmed.is_empty() { "0" } else { a_trimmed };
    let b_normalized = if b_trimmed.is_empty() { "0" } else { b_trimmed };

    a_normalized
        .len()
        .cmp(&b_normalized.len())
        .then_with(|| a_normalized.cmp(b_normalized))
        .then_with(|| a.len().cmp(&b.len()))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NaturalChunk<'a> {
    Digits(&'a str),
    Text(&'a str),
}

struct NaturalChunks<'a> {
    text: &'a str,
    offset: usize,
}

impl<'a> NaturalChunks<'a> {
    fn new(text: &'a str) -> Self {
        Self { text, offset: 0 }
    }
}

impl<'a> Iterator for NaturalChunks<'a> {
    type Item = NaturalChunk<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.text.len() {
            return None;
        }

        let start = self.offset;
        let first = self.text[start..].chars().next()?;
        self.offset += first.len_utf8();
        let is_digit = first.is_ascii_digit();

        while self.offset < self.text.len() {
            let ch = self.text[self.offset..].chars().next()?;
            if ch.is_ascii_digit() != is_digit {
                break;
            }
            self.offset += ch.len_utf8();
        }

        let chunk = &self.text[start..self.offset];
        Some(if is_digit {
            NaturalChunk::Digits(chunk)
        } else {
            NaturalChunk::Text(chunk)
        })
    }
}

fn collect_completion_snapshot(state: &ShellState) -> CompletionSnapshot {
    let mut vars = HashSet::new();
    for (name, _) in state.vars.iter() {
        vars.insert(name.clone());
    }
    for frame in state.locals_stack.iter() {
        for name in frame.keys() {
            vars.insert(name.clone());
        }
    }
    for (name, _) in env::vars() {
        vars.insert(name);
    }

    let mut commands = HashSet::new();
    for name in builtin_completion_names() {
        commands.insert(name.to_string());
    }
    for name in state.functions.keys() {
        commands.insert(name.clone());
    }
    for name in state.aliases.keys() {
        commands.insert(name.clone());
    }

    let mut vars: Vec<String> = vars.into_iter().collect();
    let mut commands: Vec<String> = commands.into_iter().collect();
    vars.sort();
    commands.sort();

    let path = env::var("PATH").unwrap_or_default();
    CompletionSnapshot {
        vars,
        base_commands: commands,
        commands: Vec::new(),
        commands_ready: false,
        rules: state.repl.completion_rules.clone(),
        expansion_chars: state.options.expansions_chars.clone(),
        expansion_handler: state.options.expansions_handler.clone(),
        path,
    }
}

fn builtin_completion_names() -> &'static [&'static str] {
    &[
        "alias", "builtin", "break", "cd", "complete", "continue", "def", "each", "elif", "else",
        "eval", "exit", "export", "for", "foreach", "if", "local", "return", "set", "source",
        "unalias", "unset", "while",
    ]
}

fn list_path_commands_with_value(path: &str) -> Vec<String> {
    let mut out = Vec::new();
    for dir in path.split(':') {
        if dir.is_empty() {
            continue;
        }
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if !is_executable(&path) {
                continue;
            }
            if let Some(name) = entry.file_name().to_str() {
                out.push(name.to_string());
            }
        }
    }
    out
}

fn ensure_command_snapshot(snapshot: &mut CompletionSnapshot) {
    let current_path = env::var("PATH").unwrap_or_default();
    if snapshot.commands_ready && snapshot.path == current_path {
        return;
    }
    let mut commands = HashSet::new();
    for name in snapshot.base_commands.iter() {
        commands.insert(name.clone());
    }
    for name in list_path_commands_with_value(&current_path) {
        commands.insert(name);
    }
    let mut commands: Vec<String> = commands.into_iter().collect();
    commands.sort();
    snapshot.commands = commands;
    snapshot.commands_ready = true;
    snapshot.path = current_path;
}

fn is_executable(path: &Path) -> bool {
    let Ok(meta) = std::fs::metadata(path) else {
        return false;
    };
    if !meta.is_file() {
        return false;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        meta.permissions().mode() & 0o111 != 0
    }
    #[cfg(not(unix))]
    {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::{
        QuoteContext, build_multi_replacement, completion_prefix_tokens,
        filesystem_completion_fragment, list_custom_completion_candidates, list_dir_candidates,
        run_completion_handler, unquote_completion_token,
    };
    use crate::state::CompletionRule;
    use rustyline::completion::Pair;
    use std::collections::HashSet;
    use std::fs;
    use std::os::unix::fs::PermissionsExt;
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
    fn list_candidates_sorts_naturally() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("transfer-2024-06-10"), b"").unwrap();
        fs::write(dir.join("transfer-4a-20251026"), b"").unwrap();
        fs::write(dir.join("transfer-2024-09-03"), b"").unwrap();
        fs::write(dir.join("README-2026"), b"").unwrap();

        let dir_prefix = format!("{}/", dir.display());
        let candidates = list_dir_candidates(&dir_prefix, false);
        let names: Vec<String> = candidates.into_iter().map(|c| c.display).collect();

        assert_eq!(
            names,
            vec![
                "README-2026",
                "transfer-4a-20251026",
                "transfer-2024-06-10",
                "transfer-2024-09-03",
            ]
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn filesystem_completion_fragment_uses_single_handler_result() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        let handler = dir.join("expand.sh");
        fs::write(&handler, "#!/bin/sh\nprintf '[\"/expanded/home/\"]'\n").unwrap();
        let mut perms = fs::metadata(&handler).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&handler, perms).unwrap();

        let mut chars = HashSet::new();
        chars.insert('~');
        let fragment = filesystem_completion_fragment(
            "~/",
            None,
            &chars,
            &["sh".to_string(), handler.display().to_string()],
        );

        assert_eq!(fragment, "/expanded/home/");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn filesystem_completion_fragment_does_not_expand_inside_quotes() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        let handler = dir.join("expand.sh");
        fs::write(&handler, "#!/bin/sh\nprintf '[\"/expanded/home/\"]'\n").unwrap();
        let mut perms = fs::metadata(&handler).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&handler, perms).unwrap();

        let mut chars = HashSet::new();
        chars.insert('~');
        let quote_ctx = QuoteContext {
            start: 0,
            quote: '"',
            has_closing: false,
        };
        let fragment = filesystem_completion_fragment(
            "~/",
            Some(&quote_ctx),
            &chars,
            &["sh".to_string(), handler.display().to_string()],
        );

        assert_eq!(fragment, "~/");

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

    #[test]
    fn multi_replacement_quotes_absolute_paths_with_spaces() {
        let selections = vec![
            Pair {
                display: "one".to_string(),
                replacement: "/home/light/example file one.mp4".to_string(),
            },
            Pair {
                display: "two".to_string(),
                replacement: "/home/light/example, file two.mp4".to_string(),
            },
        ];

        assert_eq!(
            build_multi_replacement(&selections, None),
            "\"/home/light/example file one.mp4\" \"/home/light/example, file two.mp4\" "
        );
    }

    #[test]
    fn parse_unknown_option_extracts_flag() {
        let stderr = "unknown option: --no-scrollbar\n";
        assert_eq!(
            super::parse_unknown_option(stderr),
            Some("--no-scrollbar".to_string())
        );
    }

    #[test]
    fn command_position_after_operator() {
        let line = "echo foo | bar";
        let start = line.find("bar").unwrap();
        assert!(super::is_command_position(line, start));
    }

    #[test]
    fn command_position_inline_pipe() {
        let line = "echo foo|bar";
        let start = line.find("bar").unwrap();
        assert!(super::is_command_position(line, start));
    }

    #[test]
    fn not_command_position_after_arg() {
        let line = "echo foo";
        let start = line.find("foo").unwrap();
        assert!(!super::is_command_position(line, start));
    }

    #[test]
    fn command_position_after_assignment() {
        let line = "VAR=1 cmd";
        let start = line.find("cmd").unwrap();
        assert!(super::is_command_position(line, start));
    }

    #[test]
    fn completion_prefix_tokens_handle_quoted_previous_args() {
        let line = r#"git "show diff" "fi"#;
        let quote_ctx = QuoteContext {
            start: line.rfind('"').unwrap(),
            quote: '"',
            has_closing: false,
        };
        let tokens = completion_prefix_tokens(line, quote_ctx.start + 1, Some(&quote_ctx)).unwrap();
        assert_eq!(tokens, vec!["git".to_string(), "show diff".to_string()]);
    }

    #[test]
    fn unquote_completion_token_handles_double_quotes() {
        assert_eq!(
            unquote_completion_token(r#""show \"diff\"""#),
            "show \"diff\"".to_string()
        );
    }

    #[test]
    fn custom_completion_rules_match_exact_prefix_only() {
        let rules = vec![CompletionRule {
            program: "ignored".to_string(),
            match_args: vec!["git".to_string(), "diff".to_string()],
        }];

        assert!(
            list_custom_completion_candidates(&rules, "git diff fo", 11, 9, "fo", None).is_some()
        );
        assert!(
            list_custom_completion_candidates(&rules, "git diff --cached fo", 20, 18, "fo", None)
                .is_none()
        );
    }

    #[test]
    fn completion_handler_receives_match_args_and_env() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        let handler = dir.join("complete-handler.sh");
        fs::write(
            &handler,
            "#!/bin/sh\nprintf '[\"%s|%s|%s|%s|%s\"]' \"$1\" \"$2\" \"$USH_COMP_WORD\" \"$USH_COMP_POINT\" \"$USH_COMP_CWORD\"\n",
        )
        .unwrap();
        let mut perms = fs::metadata(&handler).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&handler, perms).unwrap();

        let rule = CompletionRule {
            program: handler.display().to_string(),
            match_args: vec!["git".to_string(), "diff".to_string()],
        };
        let items = run_completion_handler(&rule, "git diff fo", 11, "fo", 2).unwrap();
        assert_eq!(items, vec!["git|diff|fo|11|2".to_string()]);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn custom_completion_keeps_non_prefix_candidates_for_fzf() {
        let dir = temp_dir();
        fs::create_dir_all(&dir).unwrap();
        let handler = dir.join("complete-handler.sh");
        fs::write(
            &handler,
            "#!/bin/sh\nprintf '[\"xfoo\",\"yfoo\",\"bar\"]'\n",
        )
        .unwrap();
        let mut perms = fs::metadata(&handler).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&handler, perms).unwrap();

        let rules = vec![CompletionRule {
            program: handler.display().to_string(),
            match_args: vec!["ptts".to_string()],
        }];
        let items =
            list_custom_completion_candidates(&rules, "ptts foo", 8, 5, "foo", None).unwrap();
        let replacements: Vec<String> = items.into_iter().map(|item| item.replacement).collect();

        assert_eq!(
            replacements,
            vec!["xfoo".to_string(), "yfoo".to_string(), "bar".to_string()]
        );

        let _ = fs::remove_dir_all(&dir);
    }
}

fn resolve_dir(dir_prefix: &str) -> std::path::PathBuf {
    let path = if dir_prefix.is_empty() {
        "."
    } else {
        dir_prefix
    };
    std::path::Path::new(path).to_path_buf()
}

struct FuzzySelection {
    key: Option<String>,
    selections: Vec<String>,
}

enum FuzzyOutcome {
    Selected(FuzzySelection),
    Cancelled,
    Unavailable,
}

fn run_fuzzy(choices: &[String], query: &str, start_last: bool) -> FuzzyOutcome {
    let mut attempts = 0;
    loop {
        attempts += 1;
        if attempts > 5 {
            return FuzzyOutcome::Unavailable;
        }

        let (output, stderr_path) = match run_fzf_once(choices, query, start_last) {
            Ok(output) => output,
            Err(_) => return FuzzyOutcome::Unavailable,
        };

        if let Some(code) = output.status.code() {
            if code == 130 {
                return FuzzyOutcome::Cancelled;
            }
            if code == 1 {
                return FuzzyOutcome::Cancelled;
            }
            if code == 2 {
                let err = String::from_utf8_lossy(&output.stderr);
                if let Some(option) = parse_unknown_option(&err) {
                    disable_fzf_option(&option);
                    continue;
                }
                if let Some(path) = stderr_path.as_ref() {
                    eprintln!();
                    eprintln!(
                        "unshell: fzf failed; your version may be too old. see {path:?} for details"
                    );
                }
                return FuzzyOutcome::Unavailable;
            }
        }

        if output.stdout.is_empty() {
            return FuzzyOutcome::Cancelled;
        }

        let output_text = String::from_utf8_lossy(&output.stdout);
        let (key, selections) = parse_fzf_output(&output_text);
        if selections.is_empty() {
            return FuzzyOutcome::Cancelled;
        }
        return FuzzyOutcome::Selected(FuzzySelection { key, selections });
    }
}

fn strip_ansi_codes(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            if matches!(chars.peek(), Some('[')) {
                chars.next();
                while let Some(code) = chars.next() {
                    if code.is_ascii_alphabetic() {
                        break;
                    }
                }
                continue;
            }
        }
        out.push(ch);
    }
    out
}

fn parse_fzf_output(output: &str) -> (Option<String>, Vec<String>) {
    let expect_enabled = !fzf_option_disabled("--expect");
    let print_query_enabled = !fzf_option_disabled("--print-query");
    let mut lines = output.lines();
    let mut key = None;
    if print_query_enabled && expect_enabled {
        let _ = lines.next();
        if let Some(line) = lines.next() {
            let line = line.trim();
            if !line.is_empty() {
                key = Some(line.to_string());
            }
        }
    } else {
        if expect_enabled {
            if let Some(line) = lines.next() {
                let line = line.trim();
                if !line.is_empty() {
                    key = Some(line.to_string());
                }
            }
        }
        if print_query_enabled {
            let _ = lines.next();
        }
    }
    let mut selections = Vec::new();
    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let cleaned = strip_ansi_codes(trimmed);
        if !cleaned.is_empty() {
            selections.push(cleaned);
        }
    }
    (key, selections)
}

fn run_fzf_once(
    choices: &[String],
    query: &str,
    start_last: bool,
) -> std::io::Result<(std::process::Output, Option<std::path::PathBuf>)> {
    let args = fzf_args();
    let mut cmd = Command::new("fzf");
    cmd.args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if !query.is_empty() {
        cmd.arg("--query").arg(query);
    }
    if start_last && !fzf_option_disabled("load:last") {
        cmd.arg("--bind").arg("load:last");
    }

    let mut child = cmd.spawn()?;
    let saved_column = cursor_column();
    let _cursor_guard = CursorGuard::new(saved_column);
    let _ = move_cursor_to_eol();

    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::Other, "missing fzf stdin"))?;
        for choice in choices {
            let _ = writeln!(stdin, "{choice}");
        }
    }

    let output = child.wait_with_output()?;
    let stderr_path = write_fzf_stderr(&args, &output.stderr)?;
    Ok((output, stderr_path))
}

fn fzf_disabled_options() -> &'static Mutex<std::collections::HashSet<String>> {
    static OPTIONS: OnceLock<Mutex<std::collections::HashSet<String>>> = OnceLock::new();
    OPTIONS.get_or_init(|| Mutex::new(std::collections::HashSet::new()))
}

fn disable_fzf_option(option: &str) {
    let mut guard = fzf_disabled_options().lock().unwrap();
    guard.insert(option.to_string());
}

fn fzf_option_disabled(option: &str) -> bool {
    let guard = fzf_disabled_options().lock().unwrap();
    guard.contains(option)
}

fn parse_unknown_option(stderr: &str) -> Option<String> {
    for line in stderr.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("unknown option:") {
            let rest = rest.trim();
            let token = rest.split_whitespace().next().unwrap_or("");
            if token.starts_with('-') {
                return Some(token.to_string());
            }
        }
    }
    None
}

fn fzf_args() -> Vec<&'static str> {
    let mut args = Vec::new();
    let opts = [
        "--height",
        "40%",
        "--margin",
        "0",
        "--gutter",
        " ",
        "--color",
        "16",
        "--ansi",
        "--cycle",
        "--no-scrollbar",
        "--color",
        "bg+:-1",
        "--no-info",
        "--no-separator",
        "--reverse",
        "-1",
        "--multi",
        "--prompt",
        "> ",
        "--exit-0",
        "--expect",
        "enter,ctrl-a,alt-a,ctrl-s,alt-s",
        "--print-query",
        "--bind",
        "tab:down,btab:up,ctrl-m:toggle,alt-m:toggle,ctrl-y:select-all+accept,alt-y:select-all+accept,alt-k:up,alt-j:down,alt-o:toggle-sort,ctrl-o:toggle-sort,ctrl-j:down,ctrl-k:up",
    ];
    let mut i = 0;
    while i < opts.len() {
        let opt = opts[i];
        if opt.starts_with('-') {
            if fzf_option_disabled(opt) {
                if i + 1 < opts.len() && !opts[i + 1].starts_with('-') {
                    i += 2;
                } else {
                    i += 1;
                }
                continue;
            }
            args.push(opt);
            if i + 1 < opts.len() && !opts[i + 1].starts_with('-') {
                let val = opts[i + 1];
                if fzf_option_disabled(val) {
                    i += 2;
                    continue;
                }
                args.push(val);
                i += 2;
                continue;
            }
            i += 1;
        } else {
            if !fzf_option_disabled(opt) {
                args.push(opt);
            }
            i += 1;
        }
    }
    args
}

fn write_fzf_stderr(
    args: &[&'static str],
    stderr: &[u8],
) -> std::io::Result<Option<std::path::PathBuf>> {
    if stderr.is_empty() {
        return Ok(None);
    }
    let dir = std::env::temp_dir();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let path = dir.join(format!("ush-fzf-stderr-{nanos}.txt"));
    let mut buffer = Vec::new();
    buffer.extend_from_slice(b"fzf ");
    buffer.extend_from_slice(format_fzf_args(args).as_bytes());
    buffer.extend_from_slice(b"\n\n");
    buffer.extend_from_slice(stderr);
    std::fs::write(&path, buffer)?;
    Ok(Some(path))
}

fn format_fzf_args(args: &[&str]) -> String {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        if arg
            .chars()
            .any(|ch| ch.is_whitespace() || ch == '"' || ch == '\'')
        {
            let escaped = arg.replace('"', "\\\"");
            out.push(format!("\"{escaped}\""));
        } else {
            out.push(arg.to_string());
        }
    }
    out.join(" ")
}

struct CursorGuard {
    column: Option<u16>,
}

impl CursorGuard {
    fn new(column: Option<u16>) -> Self {
        let _ = set_cursor_visible(false);
        Self { column }
    }
}

impl Drop for CursorGuard {
    fn drop(&mut self) {
        let _ = set_cursor_visible(true);
        if let Some(column) = self.column {
            let _ = move_cursor_to_column(column);
        }
    }
}

fn move_cursor_to_column(column: u16) -> std::io::Result<()> {
    let mut stdout = std::io::stdout();
    write!(stdout, "\x1b[{column}G")?;
    stdout.flush()
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

impl ReplHelper {
    fn update_completion_snapshot(&mut self, snapshot: CompletionSnapshot) {
        let mut guard = self.completer.snapshot.lock().unwrap();
        *guard = snapshot;
    }
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

    fn highlight_char(
        &self,
        _line: &str,
        _pos: usize,
        _kind: rustyline::highlight::CmdKind,
    ) -> bool {
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
    Some(
        PathBuf::from(home)
            .join(".local")
            .join("share")
            .join("histfile"),
    )
}

fn build_editor(state: &ShellState) -> Result<Editor<ReplHelper, DefaultHistory>> {
    let config_builder = Config::builder()
        .edit_mode(if state.repl.vi_mode {
            EditMode::Vi
        } else {
            EditMode::Emacs
        })
        .completion_type(CompletionType::List);
    let config = config_builder.build();

    let start_last = Arc::new(AtomicBool::new(false));
    let helper = ReplHelper {
        completer: FuzzyCompleter {
            start_last: start_last.clone(),
            mode: state.repl.completion_mode,
            snapshot: Arc::new(Mutex::new(CompletionSnapshot::default())),
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
            rl.bind_sequence(
                key_event,
                EventHandler::Conditional(Box::new(CommentAcceptHandler)),
            );
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
            eprintln!(
                "unshell: repl.bind ignored invalid action '{}'",
                binding.action
            );
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
        if let Some(helper) = rl.helper_mut() {
            helper.update_completion_snapshot(collect_completion_snapshot(state));
        }
        if state.needs_cursor_check {
            let _ = print_prompt_spacer(&mut std::io::stdout());
            state.needs_cursor_check = false;
        } else if !state.last_output_newline {
            let _ = print_prompt_spacer(&mut std::io::stdout());
            state.last_output_newline = true;
        }

        if state.repl.generation != last_generation || next_history_path != history_path {
            last_generation = state.repl.generation;
            match build_editor(state) {
                Ok(mut next) => {
                    if let Some(path) = next_history_path.as_ref() {
                        let _ = next.load_history(path);
                    }
                    if let Some(helper) = next.helper_mut() {
                        helper.update_completion_snapshot(collect_completion_snapshot(state));
                    }
                    rl = next;
                    history_path = next_history_path;
                }
                Err(err) => {
                    eprintln!("unshell: failed to reload repl: {err}");
                }
            }
        }

        if let Err(err) = maybe_auto_refresh_repl(state) {
            eprintln!("unshell: refresh failed: {err}");
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

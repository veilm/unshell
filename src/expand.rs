use std::io::{self, Write};
use std::process::Command;

use crate::parser::parse_args;
use crate::state::{lookup_var, ShellState};
use crate::workers::run_capture;

pub fn expand_tokens(tokens: Vec<String>, state: &ShellState) -> Result<Vec<String>, String> {
    let mut expanded = Vec::new();

    for token in tokens {
        if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 2 {
            expanded.push(token[1..token.len() - 1].to_string());
            continue;
        }
        if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
            match expand_string_literal(&token[1..token.len() - 1], state) {
                Ok(parts) => expanded.extend(parts),
                Err(err) => return Err(format!("string expansion failed: {err}")),
            }
            continue;
        }
        if token.starts_with("...") {
            let suffix = &token[3..];
            if suffix.is_empty() {
                return Err("spread requires content".into());
            }
            let source = expand_unquoted_token(suffix, state)?;
            let spread_tokens = parse_args(&source)?;
            let spread_expanded = expand_tokens(spread_tokens, state)?;
            expanded.extend(spread_expanded);
            continue;
        }

        let value = expand_unquoted_token(&token, state)?;
        if should_expand_with_handler(&value, state) {
            let replacements = run_expansion_handler(&value, state)?;
            expanded.extend(replacements);
        } else {
            expanded.push(value);
        }
    }

    Ok(expanded)
}

pub fn expand_unquoted_token(token: &str, state: &ShellState) -> Result<String, String> {
    let chars: Vec<char> = token.chars().collect();
    let mut result = String::new();
    let mut idx = 0;

    while idx < chars.len() {
        let ch = chars[idx];

        if ch == '$' && idx + 1 < chars.len() {
            let next = chars[idx + 1];
            if next == '[' {
                if let Some((capture, next_idx)) = read_bracket_capture_chars(&chars, idx + 2) {
                    let value = run_capture(&capture, state)
                        .map_err(|err| format!("capture failed: {err}"))?;
                    result.push_str(&value);
                    idx = next_idx;
                    continue;
                }
            } else if next == '(' {
                if let Some((capture, next_idx)) = read_paren_capture_chars(&chars, idx + 2) {
                    let value = run_capture(&capture, state)
                        .map_err(|err| format!("capture failed: {err}"))?;
                    result.push_str(&value);
                    idx = next_idx;
                    continue;
                }
            } else if is_var_start(next) {
                let (name, next_idx) = read_var_name(&chars, idx + 1);
                let value = lookup_var(state, &name);
                result.push_str(&value);
                idx = next_idx;
                continue;
            }
        }

        if ch == '[' {
            if let Some((capture, next_idx)) = read_bracket_capture_chars(&chars, idx + 1) {
                let value = run_capture(&capture, state)
                    .map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
                idx = next_idx;
                continue;
            }
        }

        result.push(ch);
        idx += 1;
    }

    Ok(result)
}

pub fn expand_string_literal(body: &str, state: &ShellState) -> Result<Vec<String>, String> {
    let mut result = String::new();
    let mut captures = Vec::new();
    let mut chars = body.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '$' {
            if chars.peek() == Some(&'[') {
                chars.next();
                let capture = read_capture(&mut chars)?;
                let value =
                    run_capture(&capture, state).map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
            } else if chars.peek() == Some(&'(') {
                chars.next();
                let capture = read_until_closing(&mut chars, ')')?;
                let value =
                    run_capture(&capture, state).map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
            } else if matches!(chars.peek(), Some(next) if is_var_start(*next)) {
                let name = read_var_name_iter(&mut chars);
                let value = lookup_var(state, &name);
                result.push_str(&value);
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    captures.push(result);
    Ok(captures)
}

pub fn read_bracket_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
    let mut depth = 1;
    let mut content = String::new();

    while idx < chars.len() {
        let ch = chars[idx];
        if ch == '[' {
            depth += 1;
            content.push(ch);
        } else if ch == ']' {
            depth -= 1;
            if depth == 0 {
                return Some((content, idx + 1));
            }
            content.push(ch);
        } else {
            content.push(ch);
        }
        idx += 1;
    }

    None
}

pub fn read_paren_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
    let mut depth = 1;
    let mut content = String::new();

    while idx < chars.len() {
        let ch = chars[idx];
        if ch == '(' {
            depth += 1;
            content.push(ch);
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                return Some((content, idx + 1));
            }
            content.push(ch);
        } else {
            content.push(ch);
        }
        idx += 1;
    }

    None
}

fn read_var_name_iter<I>(chars: &mut std::iter::Peekable<I>) -> String
where
    I: Iterator<Item = char>,
{
    let mut name = String::new();
    if let Some(ch) = chars.next() {
        name.push(ch);
    }

    while let Some(ch) = chars.peek() {
        if is_var_char(*ch) {
            name.push(*ch);
            chars.next();
        } else {
            break;
        }
    }

    name
}

fn read_var_name(chars: &[char], start: usize) -> (String, usize) {
    let mut idx = start + 1;
    while idx < chars.len() && is_var_char(chars[idx]) {
        idx += 1;
    }
    let name: String = chars[start..idx].iter().collect();
    (name, idx)
}

pub fn is_var_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

pub fn is_var_char(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn read_capture<I>(chars: &mut I) -> Result<String, String>
where
    I: Iterator<Item = char>,
{
    let mut depth = 1;
    let mut content = String::new();

    while let Some(ch) = chars.next() {
        match ch {
            '[' => {
                depth += 1;
                content.push(ch);
            }
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Ok(content);
                }
                content.push(ch);
            }
            _ => content.push(ch),
        }
    }

    Err("unterminated capture in string".into())
}

fn read_until_closing<I>(chars: &mut I, closing: char) -> Result<String, String>
where
    I: Iterator<Item = char>,
{
    let mut depth = 1;
    let mut content = String::new();

    while let Some(ch) = chars.next() {
        if ch == closing {
            depth -= 1;
            if depth == 0 {
                return Ok(content);
            }
        } else if ch == '(' && closing == ')' {
            depth += 1;
        }
        content.push(ch);
    }

    Err("unterminated command substitution".into())
}

fn should_expand_with_handler(token: &str, state: &ShellState) -> bool {
    if state.options.expansions_chars.is_empty() {
        return false;
    }
    token.chars().any(|ch| state.options.expansions_chars.contains(&ch))
}

fn run_expansion_handler(token: &str, state: &ShellState) -> Result<Vec<String>, String> {
    if state.options.expansions_handler.is_empty() {
        return Err("expansion handler not configured".into());
    }
    let mut cmd = Command::new(&state.options.expansions_handler[0]);
    if state.options.expansions_handler.len() > 1 {
        cmd.args(&state.options.expansions_handler[1..]);
    }
    let output = cmd
        .arg(token)
        .output()
        .map_err(|err| format!("failed to execute expansion handler: {err}"))?;
    if !output.status.success() {
        let _ = io::stderr().write_all(&output.stderr);
        if let Some(code) = output.status.code() {
            return Err(format!("expansion handler failed with status {code}"));
        }
        return Err("expansion handler failed".into());
    }
    if !output.stderr.is_empty() {
        let _ = io::stderr().write_all(&output.stderr);
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|_| "expansion handler output not utf-8".to_string())?;
    parse_json_string_array(&text).map_err(|err| {
        let trimmed = text.trim_end_matches(&['\n', '\r'][..]);
        format!("expansion handler output invalid JSON: {err}: {trimmed}")
    })
}

fn parse_json_string_array(input: &str) -> Result<Vec<String>, String> {
    let mut chars = input.chars().peekable();
    skip_json_ws(&mut chars);
    if chars.next() != Some('[') {
        return Err("expansion handler output must be a JSON array".into());
    }
    let mut items = Vec::new();

    loop {
        skip_json_ws(&mut chars);
        match chars.peek() {
            Some(']') => {
                chars.next();
                break;
            }
            Some('"') => {
                chars.next();
                let value = parse_json_string(&mut chars)?;
                items.push(value);
                skip_json_ws(&mut chars);
                match chars.peek() {
                    Some(',') => {
                        chars.next();
                    }
                    Some(']') => {
                        chars.next();
                        break;
                    }
                    _ => return Err("expansion handler output malformed".into()),
                }
            }
            _ => return Err("expansion handler output malformed".into()),
        }
    }

    Ok(items)
}

fn skip_json_ws<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) {
    while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
        chars.next();
    }
}

fn parse_json_string<I: Iterator<Item = char>>(
    chars: &mut std::iter::Peekable<I>,
) -> Result<String, String> {
    let mut result = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            '"' => return Ok(result),
            '\\' => {
                let escaped = chars.next().ok_or_else(|| "invalid json escape".to_string())?;
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    'b' => result.push('\u{0008}'),
                    'f' => result.push('\u{000C}'),
                    _ => return Err("unsupported json escape".into()),
                }
            }
            other => result.push(other),
        }
    }
    Err("unterminated json string".into())
}

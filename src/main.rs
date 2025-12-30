use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Command, ExitStatus, Output};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        if let Err(err) = run_script(&args[1]) {
            eprintln!("unshell: failed to run script '{}': {err}", args[1]);
            std::process::exit(1);
        }
        return;
    }

    run_repl();
}

fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut state = ShellState::new();

    loop {
        if print_prompt(&mut stdout).is_err() {
            break;
        }

        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                if !process_line(&line, &mut state) {
                    break;
                }
            }
            Err(err) => {
                eprintln!("unshell: failed to read line: {err}");
                break;
            }
        }
    }
}

fn run_script(path: &str) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;

    let mut ctx = ScriptContext {
        lines,
        state: ShellState::new(),
    };
    ctx.execute()
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;

    Ok(())
}

fn process_line(line: &str, state: &mut ShellState) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return true;
    }

    if trimmed == "exit" {
        return false;
    }

    let args = match parse_args(trimmed) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };

    let expanded = match expand_tokens(args, state) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };

    let (assignments, remaining) = split_assignments(&expanded);
    for (name, value) in assignments {
        state.set_var(&name, value);
    }

    if remaining.is_empty() {
        return true;
    }

    if remaining.len() == 1 && remaining[0] == "exit" {
        return false;
    }

    execute(remaining);
    true
}

struct ShellState {
    vars: HashMap<String, String>,
}

impl ShellState {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn set_var(&mut self, name: &str, value: String) {
        self.vars.insert(name.to_string(), value);
    }
}

fn split_assignments(tokens: &[String]) -> (Vec<(String, String)>, &[String]) {
    let mut assignments = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        if let Some((name, value)) = parse_assignment(&tokens[idx]) {
            assignments.push((name, value));
            idx += 1;
        } else {
            break;
        }
    }

    (assignments, &tokens[idx..])
}

fn parse_assignment(token: &str) -> Option<(String, String)> {
    let mut parts = token.splitn(2, '=');
    let name = parts.next()?;
    let value = parts.next()?;

    if !is_valid_var_name(name) {
        return None;
    }

    Some((name.to_string(), value.to_string()))
}

fn is_valid_var_name(name: &str) -> bool {
    let mut chars = name.chars();
    let first = match chars.next() {
        Some(ch) => ch,
        None => return false,
    };

    if !is_var_start(first) {
        return false;
    }

    for ch in chars {
        if !is_var_char(ch) {
            return false;
        }
    }

    true
}

fn parse_args(line: &str) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;
    let mut in_double = false;
    let mut in_single = false;
    let mut current_quote: Option<char> = None;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' if bracket_depth == 0 && !in_double && !in_single => {
                if !current.is_empty() {
                    return Err("unexpected quote inside token".into());
                }
                in_single = true;
                current_quote = Some('\'');
            }
            '\'' if in_single => {
                in_single = false;
            }
            '"' if bracket_depth == 0 && !in_double => {
                if !current.is_empty() {
                    return Err("unexpected quote inside token".into());
                }
                in_double = true;
                current_quote = Some('"');
            }
            '"' if in_double => {
                in_double = false;
            }
            '\\' if in_double => {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
            }
            c if c.is_whitespace() && bracket_depth == 0 && !in_double && !in_single => {
                if !current.is_empty() {
                    if let Some(quote) = current_quote {
                        args.push(format!("{quote}{current}{quote}"));
                    } else {
                        args.push(std::mem::take(&mut current));
                    }
                    current.clear();
                    current_quote = None;
                }
            }
            c => current.push(c),
        }
    }

    if bracket_depth != 0 {
        return Err("unterminated capture group".into());
    }

    if in_double {
        return Err("unterminated string".into());
    }

    if in_single {
        return Err("unterminated string".into());
    }

    if !current.is_empty() {
        if let Some(quote) = current_quote {
            args.push(format!("{quote}{current}{quote}"));
        } else {
            args.push(current);
        }
    }

    Ok(args)
}

fn expand_tokens(tokens: Vec<String>, state: &ShellState) -> Result<Vec<String>, String> {
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

        expanded.push(expand_unquoted_token(&token, state)?);
    }

    Ok(expanded)
}

fn expand_unquoted_token(token: &str, state: &ShellState) -> Result<String, String> {
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

fn expand_string_literal(body: &str, state: &ShellState) -> Result<Vec<String>, String> {
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

fn read_bracket_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
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

fn read_paren_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
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

fn is_var_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_var_char(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn lookup_var(state: &ShellState, name: &str) -> String {
    if let Some(value) = state.vars.get(name) {
        return value.clone();
    }

    env::var(name).unwrap_or_default()
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

fn run_capture(body: &str, state: &ShellState) -> io::Result<String> {
    let inner = body.trim();
    if inner.is_empty() {
        return Ok(String::new());
    }

    let args = parse_args(inner).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
    if args.is_empty() {
        return Ok(String::new());
    }

    let args = expand_tokens(args, state)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
    if args.is_empty() {
        return Ok(String::new());
    }

    let output = Command::new(&args[0]).args(&args[1..]).output()?;
    trim_capture_output(output)
}

fn trim_capture_output(output: Output) -> io::Result<String> {
    let mut text = String::from_utf8(output.stdout)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "capture output not utf-8"))?;

    if text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }

    Ok(text)
}

struct ScriptContext {
    lines: Vec<String>,
    state: ShellState,
}

struct BlockResult {
    next: usize,
    exit: bool,
}

impl ScriptContext {
    fn execute(&mut self) -> Result<(), String> {
        let mut idx = 0;
        while idx < self.lines.len() {
            let result = self.execute_block(idx, 0, true)?;
            idx = result.next;
            if result.exit {
                break;
            }
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        mut idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<BlockResult, String> {
        while idx < self.lines.len() {
            let raw = &self.lines[idx];
            let (indent, content) = split_indent(raw);
            let trimmed = content.trim();

            if trimmed.is_empty() || trimmed.starts_with('#') {
                idx += 1;
                continue;
            }

            if indent < indent_level {
                break;
            }

            if indent > indent_level {
                return Err(format!("unexpected indent on line {}", idx + 1));
            }

            if let Some(rest) = trimmed.strip_prefix("if ") {
                let condition = rest.trim();
                let run_child = if should_execute {
                    self.evaluate_condition(condition)?
                } else {
                    false
                };

                idx += 1;
                let result =
                    self.execute_block(idx, indent_level + 1, should_execute && run_child)?;
                idx = result.next;

                if result.exit {
                    return Ok(result);
                }
                continue;
            }

            if should_execute && !process_line(trimmed, &mut self.state) {
                return Ok(BlockResult {
                    next: idx + 1,
                    exit: true,
                });
            }

            idx += 1;
        }

        Ok(BlockResult {
            next: idx,
            exit: false,
        })
    }

    fn evaluate_condition(&self, command: &str) -> Result<bool, String> {
        let args = parse_args(command)?;
        if args.is_empty() {
            return Ok(false);
        }
        let expanded = expand_tokens(args, &self.state)?;

        match Command::new(&expanded[0]).args(&expanded[1..]).status() {
            Ok(status) => Ok(status.success()),
            Err(err) => Err(format!(
                "failed to execute condition '{}': {err}",
                expanded[0]
            )),
        }
    }
}

fn split_indent(line: &str) -> (usize, &str) {
    let bytes = line.as_bytes();
    let mut idx = 0;
    while idx < bytes.len() && bytes[idx] == b'\t' {
        idx += 1;
    }
    (idx, &line[idx..])
}

fn print_prompt(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"unshell> ")?;
    stdout.flush()
}

fn execute(args: &[String]) {
    if args.is_empty() {
        return;
    }

    match Command::new(&args[0]).args(&args[1..]).status() {
        Ok(status) => report_status(&args[0], status),
        Err(err) => eprintln!("unshell: failed to execute '{}': {err}", args[0]),
    }
}

fn report_status(cmd: &str, status: ExitStatus) {
    if let Some(code) = status.code() {
        if code != 0 {
            eprintln!("unshell: '{}' exited with status {code}", cmd);
        }
    } else if !status.success() {
        eprintln!("unshell: '{}' terminated by signal", cmd);
    }
}

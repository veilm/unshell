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

    loop {
        if print_prompt(&mut stdout).is_err() {
            break;
        }

        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                if !process_line(&line) {
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

    let mut ctx = ScriptContext { lines };
    ctx.execute()
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;

    Ok(())
}

fn process_line(line: &str) -> bool {
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

    let expanded = match expand_tokens(args) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };

    execute(&expanded);
    true
}

fn parse_args(line: &str) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;
    let mut in_double = false;
    let mut current_quoted = false;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '"' if bracket_depth == 0 && !in_double => {
                if !current.is_empty() {
                    return Err("unexpected quote inside token".into());
                }
                in_double = true;
                current_quoted = true;
            }
            '"' if in_double => {
                in_double = false;
            }
            '\\' if in_double => {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
            }
            '[' if !in_double && bracket_depth == 0 && current.is_empty() => {
                bracket_depth = 1;
                current.push(ch);
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);

                if bracket_depth == 0 {
                    if let Some(next) = chars.peek() {
                        if !next.is_whitespace() && *next != ')' {
                            return Err("capture must be followed by whitespace or )".into());
                        }
                    }
                }
            }
            c if c.is_whitespace() && bracket_depth == 0 && !in_double => {
                if !current.is_empty() {
                    if current_quoted {
                        args.push(format!("\"{current}\""));
                    } else {
                        args.push(std::mem::take(&mut current));
                    }
                    current.clear();
                    current_quoted = false;
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

    if !current.is_empty() {
        if current_quoted {
            args.push(format!("\"{current}\""));
        } else {
            args.push(current);
        }
    }

    Ok(args)
}

fn expand_tokens(tokens: Vec<String>) -> Result<Vec<String>, String> {
    let mut expanded = Vec::new();

    for token in tokens {
        if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
            match expand_string_literal(&token[1..token.len() - 1]) {
                Ok(parts) => expanded.extend(parts),
                Err(err) => return Err(format!("string expansion failed: {err}")),
            }
            continue;
        }

        if let Some(value) = try_capture(&token)? {
            expanded.push(value);
        } else {
            expanded.push(token);
        }
    }

    Ok(expanded)
}

fn expand_string_literal(body: &str) -> Result<Vec<String>, String> {
    let mut result = String::new();
    let mut captures = Vec::new();
    let mut chars = body.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '$' {
            if chars.peek() == Some(&'[') {
                chars.next();
                let capture = read_capture(&mut chars)?;
                let value =
                    run_capture(&capture).map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
            } else if chars.peek() == Some(&'(') {
                chars.next();
                let capture = read_until_closing(&mut chars, ')')?;
                let value =
                    run_capture(&capture).map_err(|err| format!("capture failed: {err}"))?;
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

fn try_capture(token: &str) -> Result<Option<String>, String> {
    if token.starts_with('[') && token.ends_with(']') && token.len() > 2 {
        run_capture(&token[1..token.len() - 1])
            .map(Some)
            .map_err(|err| format!("capture failed: {err}"))
    } else if token.starts_with("$[") && token.ends_with(']') {
        run_capture(&token[2..token.len() - 1])
            .map(Some)
            .map_err(|err| format!("capture failed: {err}"))
    } else if token.starts_with("$(") && token.ends_with(')') {
        run_capture(&token[2..token.len() - 1])
            .map(Some)
            .map_err(|err| format!("capture failed: {err}"))
    } else {
        Ok(None)
    }
}

fn run_capture(body: &str) -> io::Result<String> {
    let inner = body.trim();
    if inner.is_empty() {
        return Ok(String::new());
    }

    let args = parse_args(inner).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
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

            if should_execute && !process_line(trimmed) {
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
        let expanded = expand_tokens(args)?;

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
